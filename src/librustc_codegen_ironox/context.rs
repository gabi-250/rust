#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;

use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutOf, LayoutError, self, TyLayout, Size};
use type_of::LayoutIronOxExt;
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::base::wants_msvc_seh;
use rustc_codegen_ssa::callee::resolve_and_get_fn;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_mir::monomorphize::Instance;
use rustc_target::abi::HasDataLayout;
use rustc::mir::interpret::{Scalar, Allocation};
use std::cell::{Cell, RefCell};
use std::sync::Arc;
use syntax::symbol::LocalInternedString;

use debuginfo::DIScope;
use ir::basic_block::BasicBlock;
use ir::constant::{UnsignedConst, SignedConst};
use ir::instruction::Instruction;
use ir::value::Value;
use ir::type_::{OxType, Type, IxLlcx};
use ir::struct_::OxStruct;
use consts::{self};
use const_cstr::ConstCstr;
use global::Global;

use super::ModuleIronOx;

impl BackendTypes for CodegenCx<'ll, 'tcx> {
    type Value = Value;
    type BasicBlock = BasicBlock;
    type Type = Type;
    type Funclet = ();
    type DIScope = &'ll DIScope;
}

pub struct CodegenCx<'ll, 'tcx: 'll> {
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    pub stats: RefCell<Stats>,
    pub codegen_unit: Arc<CodegenUnit<'tcx>>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, Value>>,
    pub module: RefCell<&'ll mut ModuleIronOx>,
    pub vtables: RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>,
    eh_personality: Cell<Option<Value>>,
    /// All the types defined in this context.
    pub types: RefCell<Vec<OxType>>,
    /// The index of an `OxType` in `types`.
    pub type_cache: RefCell<FxHashMap<OxType, Type>>,
    /// The unsigned constants defined in this context.
    pub u_consts: RefCell<Vec<UnsignedConst>>,
    /// The signed constants defined in this context.
    pub i_consts: RefCell<Vec<SignedConst>>,
    pub struct_consts: RefCell<Vec<OxStruct>>,
    pub const_globals: RefCell<Vec<Global>>,
    pub globals: RefCell<Vec<Global>>,
    pub globals_cache: RefCell<FxHashMap<String, usize>>,
    pub const_globals_cache: RefCell<FxHashMap<Value, Value>>,
    pub const_cstr_cache: RefCell<FxHashMap<LocalInternedString, Value>>,
    pub const_cstrs: RefCell<Vec<ConstCstr>>,
    pub const_casts: RefCell<Vec<Instruction>>,
    pub const_fat_ptrs: RefCell<Vec<(Value, Value)>>,
    pub sym_count: Cell<usize>,
    pub private_globals: RefCell<Vec<Type>>
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn new(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
               codegen_unit: Arc<CodegenUnit<'tcx>>,
               module: &'ll mut ModuleIronOx)
                 -> CodegenCx<'ll, 'tcx> {
        CodegenCx {
            tcx,
            codegen_unit,
            stats: RefCell::new(Stats::default()),
            instances: Default::default(),
            module: RefCell::new(module),
            vtables: Default::default(),
            eh_personality: Default::default(),
            types: Default::default(),
            type_cache: Default::default(),
            u_consts: Default::default(),
            i_consts: Default::default(),
            struct_consts: Default::default(),
            const_globals: Default::default(),
            const_globals_cache: Default::default(),
            globals: Default::default(),
            globals_cache: Default::default(),
            const_cstr_cache: Default::default(),
            const_cstrs: Default::default(),
            const_casts: Default::default(),
            const_fat_ptrs: Default::default(),
            sym_count: Cell::new(0),
            private_globals: Default::default(),
        }
    }

    pub fn ty_size(&self, ty: Type) -> u64 {
        // FIXME: implement
        8
    }

    pub fn get_or_insert_global(&self, name: String, gv: Global) -> Value {
        let mut globals = self.globals.borrow_mut();
        let mut globals_cache = self.globals_cache.borrow_mut();
        if let Some(idx) = globals_cache.get(&name) {
            return Value::Global(*idx);
        }
        let gv_idx = globals.len();
        let gv_idx = *globals_cache.entry(name).or_insert_with(|| {
            globals.push(gv);
            gv_idx
        });
        Value::Global(gv_idx)
    }

    pub fn insert_cstr(&self,
                       c_str: *const u8,
                       len: usize,
                       null_terminated: bool) -> Value {
        let mut const_cstrs = self.const_cstrs.borrow_mut();
        let val = Value::ConstCstr(const_cstrs.len());
        let ty = self.type_ptr_to(self.type_i8());
        const_cstrs.push(ConstCstr::new(ty, c_str, len, null_terminated));
        val
    }

    pub fn get_sym_name(&self, prefix: &str) -> String {
        let count = self.sym_count.get();
        self.sym_count.set(count + 1);
        format!("{}.{}", prefix, count)
    }
}

impl ty::layout::HasTyCtxt<'tcx> for CodegenCx<'ll, 'tcx> {
    fn tcx<'b>(&'b self) -> TyCtxt<'b, 'tcx, 'tcx> {
        self.tcx
    }
}

impl LayoutOf for CodegenCx<'ll, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyLayout = TyLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyLayout {
        self.tcx.layout_of(ty::ParamEnv::reveal_all().and(ty))
            .unwrap_or_else(|e| if let LayoutError::SizeOverflow(_) = e {
                self.sess().fatal(&e.to_string())
            } else {
                bug!("failed to get layout for `{}`: {}", ty, e)
            })
    }
}

impl ty::layout::HasDataLayout for CodegenCx<'ll, 'tcx> {
    fn data_layout(&self) -> &ty::layout::TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl MiscMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn vtables(&self) -> &RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>
    {
        &self.vtables
    }

    fn instances(&self) -> &RefCell<FxHashMap<Instance<'tcx>, Value>> {
        &self.instances
    }

    fn get_fn(&self, instance: Instance<'tcx>) ->
        Value {
        if let Some(ref llfn) = self.instances.borrow().get(&instance) {
            // The function has already been defined
            return **llfn;
        }

        let sym_name = self.tcx.symbol_name(instance).as_str();
        let llfn = if let Some(llfn) = self.get_declared_value(&sym_name) {
            // FIXME:
            llfn
        } else {
            // Otherwise, it probably exists in an external lib...
            let fn_sig = instance.fn_sig(self.tcx);
            let llfn = self.declare_fn(&sym_name, fn_sig);
            // visbility, linkage......
            llfn
        };
        self.instances.borrow_mut().insert(instance, llfn);
        llfn
    }

    fn get_param(&self, llfn: Value, index: c_uint) -> Value {
        let llfn_index = match llfn {
            Value::Function(i) => i,
            _ => bug!("llfn must be a function! Found: {:?}", llfn),
        };
        self.module.borrow().functions[llfn_index].get_param(index as usize)
    }

    fn eh_personality(&self) -> Value {
        if let Some(llpersonality) = self.eh_personality.get() {
            return llpersonality
        }
        let tcx = self.tcx;
        let llfn = match tcx.lang_items().eh_personality() {
            Some(def_id) if !wants_msvc_seh(self.sess()) => {
                resolve_and_get_fn(self, def_id, tcx.intern_substs(&[]))
            }
            _ => {
                let name = if wants_msvc_seh(self.sess()) {
                    unimplemented!("Unsupported platform: MSVC")
                } else {
                    "rust_eh_personality"
                };
                let fty = self.type_variadic_func(&[], self.type_i32());
                self.declare_cfn(name, fty)
            }
        };
        //attributes::apply_target_cpu_attr(self, llfn);
        self.eh_personality.set(Some(llfn));
        llfn
    }

    fn eh_unwind_resume(&self) -> Value {
        unimplemented!("eh_unwind_resume");
    }

    fn sess(&self) -> &Session {
        &self.tcx.sess
    }

    fn check_overflow(&self) -> bool {
        // Don't check for overflow (for now).
        true
    }

    fn stats(&self) -> &RefCell<Stats> {
        &self.stats
    }

    fn consume_stats(self) -> RefCell<Stats> {
        self.stats
    }

    fn codegen_unit(&self) -> &Arc<CodegenUnit<'tcx>> {
        &self.codegen_unit
    }

    fn used_statics(&self) -> &RefCell<Vec<Value>> {
        unimplemented!("used_statics");
    }

    fn set_frame_pointer_elimination(&self, llfn: Value) {
        // FIXME
    }

    fn apply_target_cpu_attr(&self, llfn: Value) {
        // FIXME
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }

}

impl CodegenCx<'ll, 'tcx> {
    /// Return a signed constant which has the specified type, and value.
    fn const_signed(&self, ty: Type, value: i128) -> Value {
        let iconst = SignedConst {
            ty,
            value,
        };
        let mut consts = self.i_consts.borrow_mut();
        consts.push(iconst);
        Value::ConstUint(consts.len() - 1)
    }

    /// Return an unsigned constant which has the specified type, and value.
    fn const_unsigned(&self, ty: Type, value: u128) -> Value {
        let uconst = UnsignedConst {
            ty,
            value,
        };
        let mut consts = self.u_consts.borrow_mut();
        consts.push(uconst);
        Value::ConstUint(consts.len() - 1)
    }
}

// common?
impl ConstMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn const_null(&self, t: Type) -> Value {
        self.const_unsigned(t, 0u128)
    }

    fn const_undef(&self, t: Type) -> Value {
        Value::ConstUndef(t)
    }

    fn const_int(&self, t: Type, i: i64) -> Value {
        unimplemented!("const_int");
    }

    fn const_uint(&self, t: Type, i: u64) -> Value {
        self.const_unsigned(t, i as u128)
    }

    fn const_uint_big(&self, t: Type, u: u128) -> Value {
        self.const_unsigned(t, u)
    }

    fn const_bool(&self, val: bool) -> Value {
        self.const_unsigned(self.type_i1(), val as u128)
    }

    fn const_i32(&self, i: i32) -> Value {
        unimplemented!("const_i32");
    }

    fn const_u32(&self, i: u32) -> Value {
        self.const_unsigned(self.type_i32(), i as u128)
    }

    fn const_u64(&self, i: u64) -> Value {
        unimplemented!("const_u64");
    }

    fn const_usize(&self, i: u64) -> Value {
        let bit_size = self.data_layout().pointer_size.bits();
        let isize_ty = Type::ix_llcx(self,
                                     self.tcx.data_layout.pointer_size.bits());
        if bit_size < 64 {
            // make sure it doesn't overflow
            assert!(i < (1<<bit_size));
        }

        self.const_uint(isize_ty, i)
    }

    fn const_u8(&self, i: u8) -> Value {
        unimplemented!("const_u8");
    }

    fn const_cstr(
        &self,
        s: LocalInternedString,
        null_terminated: bool,
    ) -> Value {
        if let Some(val) = self.const_cstr_cache.borrow().get(&s) {
            return *val;
        }
        // FIXME
        let symbol_name = self.get_sym_name("str");
        let str_val = self.insert_cstr(s.as_ptr() as *const u8,
                                       s.len(),
                                       null_terminated);
        let gv = self.define_global(&symbol_name[..],
                                    self.val_ty(str_val))
            .unwrap_or_else(|| {
                bug!("symbol `{}' is already defined", symbol_name);
            });
        self.const_cstr_cache.borrow_mut().insert(s, gv);
        gv
    }

    fn const_str_slice(&self, s: LocalInternedString) -> Value {
        let len = s.len();
        let cs = consts::ptrcast(self, self.const_cstr(s, false),
            self.type_ptr_to(self.layout_of(self.tcx.mk_str()).ironox_type(self)));
        self.const_fat_ptr(cs, self.const_usize(len as u64))
    }

    fn const_fat_ptr(
        &self,
        ptr: Value,
        meta: Value
    ) -> Value {
        let mut const_fat_ptrs = self.const_fat_ptrs.borrow_mut();
        const_fat_ptrs.push((ptr, meta));
        Value::ConstFatPtr(const_fat_ptrs.len())
    }

    fn const_struct(
        &self,
        elts: &[Value],
        packed: bool
    ) -> Value {
        let mut structs = self.struct_consts.borrow_mut();
        let mut elt_tys = Vec::with_capacity(elts.len());
        for v in elts {
            elt_tys.push(self.val_ty(*v))
        }
        let ty = self.type_struct(&elt_tys[..], packed);
        structs.push(OxStruct::new(elts, ty));
        Value::ConstStruct(structs.len() - 1)
    }

    fn const_array(&self, ty: Type, elts: &[Value]) -> Value {
        unimplemented!("const_array");
    }

    fn const_vector(&self, elts: &[Value]) -> Value {
        unimplemented!("const_vector");
    }

    fn const_bytes(&self, bytes: &[u8]) -> Value {
        unimplemented!("const_bytes");
    }

    fn const_get_elt(&self, v: Value, idx: u64) -> Value {
        unimplemented!("const_get_elt");
    }

    fn const_get_real(&self, v: Value) -> Option<(f64, bool)> {
        unimplemented!("const_get_real");
    }

    fn const_to_uint(&self, v: Value) -> u64 {
        unimplemented!("const_to_uint");
    }

    fn is_const_integral(&self, v: Value) -> bool {
        unimplemented!("is_const_integral");
    }

    fn is_const_real(&self, v: Value) -> bool {
        unimplemented!("is_const_real");
    }

    fn const_to_opt_u128(&self, v: Value, sign_ext: bool) -> Option<u128> {
        match v {
            Value::ConstUint(i) => Some(self.u_consts.borrow()[i].value),
            Value::ConstInt(i) => unimplemented!("signed integers"),
            _ =>  None
        }
    }

    fn const_ptrcast(&self, val: Value, ty: Type) -> Value {
        unimplemented!("const_ptrcast");
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: &layout::Scalar,
        llty: Type,
    ) -> Value {
        let bitsize = if layout.is_bool() { 1 } else { layout.value.size(self).bits() };
        match cv {
            Scalar::Bits { size: 0, .. } => {
                assert_eq!(0, layout.value.size(self).bytes());
                self.const_undef(self.type_ix(0))
            },
            Scalar::Bits { bits, size } => {
                assert_eq!(size as u64, layout.value.size(self).bytes());
                let llval = self.const_uint_big(self.type_ix(bitsize), bits);
                if layout.value == layout::Pointer {
                    unimplemented!("scalar_to_backend: layout::Pointer");
                } else {
                    // FIXME? bitcast llval to llty
                    llval
                }
            },
            Scalar::Ptr(ptr) => {
                unimplemented!("Scalar::Ptr");
            }
        }
    }

    fn from_const_alloc(
        &self,
        layout: TyLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, Value> {
        unimplemented!("from_const_alloc");
    }
}
