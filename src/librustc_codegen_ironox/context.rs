#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutOf, LayoutError, self, TyLayout, Size, VariantIdx};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::base::wants_msvc_seh;
use rustc_codegen_ssa::callee::resolve_and_get_fn;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_mir::monomorphize::Instance;
use rustc_target::abi::HasDataLayout;
use rustc_target::spec::{HasTargetSpec, Target};
use rustc::mir::interpret::{Scalar, AllocKind, Allocation, Pointer, read_target_uint};
use rustc_data_structures::indexed_vec::IndexVec;
use std::cell::{Cell, RefCell};
use std::sync::Arc;
use syntax::ast::Mutability;
use syntax::symbol::LocalInternedString;

use consts::{self};
use debuginfo::DIScope;

use ir::basic_block::BasicBlock;
use ir::bytes::OxConstBytes;
use ir::constant::{OxUnsignedConst, OxSignedConst};
use ir::const_cstr::OxConstStr;
use ir::global::OxGlobal;
use ir::instruction::ConstCast;
use ir::struct_::OxStruct;
use ir::type_::{OxType, Type, IxLlcx};
use ir::value::Value;
use type_of::LayoutIronOxExt;

use super::ModuleIronOx;

impl BackendTypes for CodegenCx<'ll, 'tcx> {
    type Value = Value;
    type BasicBlock = BasicBlock;
    type Type = Type;
    type Funclet = ();
    type DIScope = &'ll DIScope;
}

pub struct CodegenCx<'ll, 'tcx: 'll> {
    /// The typing context of this codegen context.
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    /// The codegen unit compiled using this context. This is None if this `CodegenCx`
    /// is used to compile an allocator module.
    pub codegen_unit: Option<Arc<CodegenUnit<'tcx>>>,
    /// Whether to emit overflow checks.
    pub check_overflow: bool,
    /// The statistics collected while codegnning.
    pub stats: RefCell<Stats>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, Value>>,
    pub module: RefCell<&'ll mut ModuleIronOx>,
    pub vtables: RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>,
    pub personality_fns: RefCell<FxHashMap<Value, Value>>,
    /// All the types defined in this context.
    pub types: RefCell<IndexVec<Type, OxType>>,
    /// The index of an `OxType` in `types`.
    pub type_cache: RefCell<FxHashMap<OxType, Type>>,
    /// The unsigned constants defined in this context.
    pub u_consts: RefCell<Vec<OxUnsignedConst>>,
    /// The signed constants defined in this context.
    pub i_consts: RefCell<Vec<OxSignedConst>>,
    /// All the globals defined so far, including functions.
    pub globals: RefCell<Vec<OxGlobal>>,
    /// Map all the names of the globals defined so far to their index in `globals`.
    pub globals_cache: RefCell<FxHashMap<String, usize>>,
    /// The constant globals (which have a static address).
    pub const_structs: RefCell<Vec<OxStruct>>,
    pub const_structs_cache: RefCell<FxHashMap<(Vec<Value>, bool), Value>>,
    pub const_globals_cache: RefCell<FxHashMap<Value, usize>>,
    pub const_cstr_cache: RefCell<FxHashMap<LocalInternedString, Value>>,
    pub const_cstrs: RefCell<Vec<OxConstStr>>,
    pub const_casts: RefCell<Vec<ConstCast>>,
    pub const_fat_ptrs: RefCell<Vec<(Value, Value)>>,
    pub scalar_lltypes: RefCell<FxHashMap<Ty<'tcx>, Type>>,
    pub lltypes: RefCell<FxHashMap<(Ty<'tcx>, Option<VariantIdx>), Type>>,
    pub bytes: RefCell<Vec<OxConstBytes>>,
    sym_count: Cell<usize>,
    eh_personality: Cell<Option<Value>>,
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn new(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
               codegen_unit: Option<Arc<CodegenUnit<'tcx>>>,
               module: &'ll mut ModuleIronOx)
                 -> CodegenCx<'ll, 'tcx> {
        CodegenCx {
            tcx,
            codegen_unit,
            check_overflow: tcx.sess.overflow_checks(),
            stats: RefCell::new(Stats::default()),
            instances: Default::default(),
            module: RefCell::new(module),
            vtables: Default::default(),
            eh_personality: Default::default(),
            types: Default::default(),
            type_cache: Default::default(),
            u_consts: Default::default(),
            i_consts: Default::default(),
            globals: Default::default(),
            globals_cache: Default::default(),
            const_structs: Default::default(),
            const_structs_cache: Default::default(),
            const_globals_cache: Default::default(),
            const_cstr_cache: Default::default(),
            const_cstrs: Default::default(),
            const_casts: Default::default(),
            const_fat_ptrs: Default::default(),
            sym_count: Cell::new(0),
            personality_fns: Default::default(),
            scalar_lltypes: Default::default(),
            lltypes: Default::default(),
            bytes: Default::default(),
        }
    }

    pub fn insert_cstr(&self,
                       name: &str,
                       c_str: *const u8,
                       len: usize,
                       null_terminated: bool) -> Value {
        let mut const_cstrs = self.const_cstrs.borrow_mut();
        let idx = const_cstrs.len();
        // A C string is a char*.
        let ty = self.type_ptr_to(self.type_i8());
        const_cstrs.push(OxConstStr::new(name.to_string(),
                                        ty,
                                        c_str,
                                        len,
                                        null_terminated));
        Value::ConstCstr(idx)
    }

    /// Generate a unique symbol name that starts with the specified prefix.
    pub fn get_sym_name(&self, prefix: &str) -> String {
        let count = self.sym_count.get();
        self.sym_count.set(count + 1);
        format!(".L{}.{}", prefix, count)
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

impl HasTargetSpec for CodegenCx<'ll, 'tcx> {
    fn target_spec(&self) -> &Target {
        &self.tcx.sess.target.target
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

    fn get_fn(&self, instance: Instance<'tcx>) -> Value {
        let sym_name = self.tcx.symbol_name(instance).as_str();
        if let Some(ref llfn) = self.instances.borrow().get(&instance) {
            // The function has already been defined
            return **llfn;
        }

        let llfn = if let Some(llfn) = self.get_declared_value(&sym_name) {
            // FIXME:
            llfn
        } else {
            // Otherwise, it probably exists in an external lib...
            let fn_sig = instance.fn_sig(self.tcx);
            let llfn = self.declare_fn(&sym_name, fn_sig);
            let instance_def_id = instance.def_id();
            // This is a non-generic function
            self.module.borrow_mut().functions[llfn.fn_idx()]
                .set_is_codegenned(self.tcx.is_codegened_item(instance_def_id));
            // FIXME: visibility, linkage.
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
        self.check_overflow
    }

    fn stats(&self) -> &RefCell<Stats> {
        &self.stats
    }

    fn consume_stats(self) -> RefCell<Stats> {
        self.stats
    }

    fn codegen_unit(&self) -> &Arc<CodegenUnit<'tcx>> {
        if let Some(ref codegen_unit) = self.codegen_unit {
            codegen_unit
        } else {
            panic!("The codegen context has no codegen unit!");
        }
    }

    fn used_statics(&self) -> &RefCell<Vec<Value>> {
        unimplemented!("used_statics");
    }

    fn set_frame_pointer_elimination(&self, _llfn: Value) {
        // FIXME
    }

    fn apply_target_cpu_attr(&self, _llfn: Value) {
        // FIXME
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }

}

impl CodegenCx<'ll, 'tcx> {
    /// Return a signed constant which has the specified type, and value.
    fn const_signed(&self, ty: Type, value: i128) -> Value {
        let iconst = OxSignedConst {
            ty,
            value,
        };
        let mut consts = self.i_consts.borrow_mut();
        consts.push(iconst);
        Value::ConstUint(consts.len() - 1)
    }

    /// Return an unsigned constant which has the specified type, and value.
    fn const_unsigned(&self, ty: Type, value: u128) -> Value {
        let uconst = OxUnsignedConst {
            ty,
            value,
        };
        let mut consts = self.u_consts.borrow_mut();
        consts.push(uconst);
        Value::ConstUint(consts.len() - 1)
    }

    fn const_gep(base_addr: Value, offset: u64) -> Value {
        if let Value::ConstStruct(idx) = base_addr {
            Value::ConstGep { ptr_idx: idx, offset }
        } else {
            bug!("Expected Value::ConstStruct, found {:?}", base_addr)
        }
    }

    fn const_alloc_to_ironox(
        &self,
        alloc: &Allocation,
    ) -> Value {
        // The allocation is represented as a struct.
        let mut struct_elts = Vec::with_capacity(alloc.relocations.len() + 1);
        let dl = self.data_layout();
        let ptr_size = dl.pointer_size.bytes() as usize;
        // The offset into the allocation where we left off...
        let mut last_offset = 0;
        // alloc.relocations are the relocations (pointers) from the `Allocation`.
        // This iterates over the pointers in sorted order.
        for &(offset, ((), alloc_id)) in alloc.relocations.iter() {
            // The offset of the current pointer.
            let offset = offset.bytes();
            assert_eq!(offset as usize as u64, offset);
            let offset = offset as usize;
            // There are some bytes left in between the last pointer codegenned and
            // the current one. Generate the bytes in between:
            if offset > last_offset {
                struct_elts.push(self.const_bytes(&alloc.bytes[last_offset..offset]));
            }
            let ptr_offset = read_target_uint(
                dl.endian,
                &alloc.bytes[offset..(offset + ptr_size)],
            ).expect("const_alloc_to_ironox: could not read relocation pointer") as u64;
            struct_elts.push(self.scalar_to_backend(
                Pointer::new(alloc_id, Size::from_bytes(ptr_offset)).into(),
                &layout::Scalar {
                    value: layout::Primitive::Pointer,
                    valid_range: 0..=!0
                },
                self.type_i8p()
            ));
            // Move to the next byte that has not yet been added to the struct.
            last_offset = offset + ptr_size;
        }
        // If there are more bytes after the last emitted relocation, add them
        // to the struct:
        if alloc.bytes.len() > last_offset {
            struct_elts.push(self.const_bytes(&alloc.bytes[last_offset..]));
        }
        self.const_struct(&struct_elts, true)
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

    fn const_int(&self, _t: Type, _i: i64) -> Value {
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

    fn const_i32(&self, _i: i32) -> Value {
        unimplemented!("const_i32");
    }

    fn const_u32(&self, i: u32) -> Value {
        self.const_unsigned(self.type_i32(), i as u128)
    }

    fn const_u64(&self, _i: u64) -> Value {
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
        self.const_unsigned(self.type_i8(), i as u128)
    }

    fn const_cstr(
        &self,
        s: LocalInternedString,
        null_terminated: bool,
    ) -> Value {
        // If the cstr has already been defined, return its value from the cache.
        if let Some(val) = self.const_cstr_cache.borrow().get(&s) {
            return *val;
        }
        // Create a unique name for this symbol.
        let symbol_name = self.get_sym_name("str");
        let str_val = self.insert_cstr(&symbol_name,
                                       s.as_ptr() as *const u8,
                                       s.len(),
                                       null_terminated);
        let gv = self.define_global(&symbol_name[..],
                                    self.val_ty(str_val))
            .unwrap_or_else(|| {
                bug!("symbol `{}' is already defined", symbol_name);
            });
        match gv {
            Value::Global(idx) => {
                self.globals.borrow_mut()[idx].set_initializer(str_val);
            },
            _ => bug!("expected global, found {:?}", gv),
        };
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
        self.const_struct(&[ptr, meta], false)
    }

    fn const_struct(
        &self,
        elts: &[Value],
        packed: bool
    ) -> Value {
        let name = self.get_sym_name("struct");
        let mut elt_tys = {
            let mut tys = Vec::with_capacity(elts.len());
            for v in elts {
                tys.push(self.val_ty(*v))
            }
            tys
        };
        let mut structs = self.const_structs.borrow_mut();
        let ty = self.type_struct(&elt_tys[..], packed);
        structs.push(OxStruct::new(name.clone(), elts, ty));
        Value::ConstStruct(structs.len() - 1)
    }

    fn const_array(&self, _ty: Type, _elts: &[Value]) -> Value {
        unimplemented!("const_array");
    }

    fn const_vector(&self, _elts: &[Value]) -> Value {
        unimplemented!("const_vector");
    }

    fn const_bytes(&self, bytes: &[u8]) -> Value {
        let mut c_bytes = self.bytes.borrow_mut();
        let name = self.get_sym_name("const_bytes");
        c_bytes.push(OxConstBytes::new(name, bytes));
        Value::ConstBytes(c_bytes.len() - 1)
    }

    fn const_get_elt(&self, _v: Value, _idx: u64) -> Value {
        unimplemented!("const_get_elt");
    }

    fn const_get_real(&self, _v: Value) -> Option<(f64, bool)> {
        unimplemented!("const_get_real");
    }

    fn const_to_uint(&self, _v: Value) -> u64 {
        unimplemented!("const_to_uint");
    }

    fn is_const_integral(&self, v: Value) -> bool {
        match v {
            Value::ConstUint(_) | Value::ConstInt(_) => true,
            _ => false,
        }
    }

    fn is_const_real(&self, _v: Value) -> bool {
        unimplemented!("is_const_real");
    }

    fn const_to_opt_u128(&self, v: Value, _sign_ext: bool) -> Option<u128> {
        match v {
            Value::ConstUint(i) => Some(self.u_consts.borrow()[i].value),
            Value::ConstInt(_) => unimplemented!("signed integers"),
            _ =>  None
        }
    }

    fn const_ptrcast(&self, val: Value, ty: Type) -> Value {
        consts::ptrcast(self, val, ty)
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
                self.const_ptrcast(llval, llty)
            },
            Scalar::Ptr(ptr) => {
                let alloc_kind = self.tcx.alloc_map.lock().get(ptr.alloc_id);
                let base_addr = match alloc_kind {
                    Some(AllocKind::Memory(alloc)) => {
                        let init = self.const_alloc_to_ironox(alloc);
                        if alloc.mutability == Mutability::Mutable {
                            unimplemented!("Mutable allocation");
                        } else {
                            init
                        }
                    }
                    None => bug!("missing allocation {:?}", ptr.alloc_id),
                    _ => unimplemented!("scalar_to_backend({:?})", alloc_kind),
                };
                let val = CodegenCx::const_gep(base_addr, ptr.offset.bytes());
                self.const_ptrcast(val, llty)
            }
        }
    }

    fn from_const_alloc(
        &self,
        layout: TyLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, Value> {
        let const_struct = self.const_alloc_to_ironox(alloc);
        let val = CodegenCx::const_gep(const_struct, offset.bytes());
        // FIXME: get the right ptr in the const struct....
        PlaceRef::new_sized(val, layout, alloc.align)
    }
}
