#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;

use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutOf, LayoutError, self, TyLayout, Size};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::base::wants_msvc_seh;
use rustc_codegen_ssa::callee::resolve_and_get_fn;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_mir::monomorphize::Instance;
use rustc::mir::interpret::{Scalar, Allocation};
use std::cell::{Cell, RefCell};
use std::sync::Arc;
use syntax::symbol::LocalInternedString;

use basic_block::{BasicBlock, BasicBlockData};
use value::Value;
use ironox_type::{Type, LLType};
use function::IronOxFunction;
use debuginfo::DIScope;

use super::ModuleIronOx;

impl BackendTypes for CodegenCx<'ll, 'tcx> {
    type Value = Value;
    type BasicBlock = BasicBlock;
    type Type = Type;
    type Context = ();
    type Funclet = ();
    type DIScope = &'ll DIScope;
}

pub struct IronOxContext<'ll> {
    pub module: &'ll mut ::ModuleIronOx
}

pub struct CodegenCx<'ll, 'tcx: 'll> {
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    pub stats: RefCell<Stats>,
    pub codegen_unit: Arc<CodegenUnit<'tcx>>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, Value>>,
    pub module: RefCell<&'ll mut ModuleIronOx>,
    pub vtables: RefCell<FxHashMap<(Ty<'tcx>, ty::PolyExistentialTraitRef<'tcx>), Value>>,
    eh_personality: Cell<Option<Value>>,
    // XXX
    pub types: RefCell<Vec<LLType>>,
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
        }
    }

    pub fn ty_size(&self, ty: Type) -> u64 {
        // XXX implement
        8
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
        FxHashMap<(Ty<'tcx>, ty::PolyExistentialTraitRef<'tcx>),
            Value>
    > {
        &self.vtables
    }

    fn instances(&self) -> &RefCell<FxHashMap<Instance<'tcx>,
            Value>> {
        &self.instances
    }

    fn get_fn(&self, instance: Instance<'tcx>) ->
        Value {
        eprintln!("Trying to get {:?}", instance);
        if let Some(ref llfn) = self.instances.borrow().get(&instance) {
            // The function has already been defined
            return **llfn;
        }

        let sym_name = self.tcx.symbol_name(instance).as_str();
        let llfn = if let Some(llfn) = self.get_declared_value(&sym_name) {
            // XXX
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
        // XXX return a dummy local
        eprintln!("get local {:?} of {:?}", index, llfn);
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
        unimplemented!("check_overflow");
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

    fn statics_to_rauw(&self) -> &RefCell<Vec<(Value, Value)>> {
        unimplemented!("statics_to_rauw");
    }

    fn used_statics(&self) -> &RefCell<Vec<Value>> {
        unimplemented!("used_statics");
    }

    fn set_frame_pointer_elimination(&self, llfn: Value) {
        // XXX
    }

    fn apply_target_cpu_attr(&self, llfn: Value) {
        // XXX
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }

    fn closure_env_needs_indirect_debuginfo(&self) -> bool {
        unimplemented!("closure_env_needs_indirect_debuginfo");
    }
}

// common?
impl ConstMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn const_null(&self, t: Type) -> Value {
        Value::Const(0)
    }

    fn const_undef(&self, t: Type) -> Value {
        Value::ConstUndef
    }

    fn const_int(&self, t: Type, i: i64) -> Value {
        unimplemented!("");
    }

    fn const_uint(&self, t: Type, i: u64) -> Value {
        unimplemented!("");
    }

    fn const_uint_big(&self, t: Type, u: u128) -> Value {
        Value::BigConst(u)
    }

    fn const_bool(&self, val: bool) -> Value {
        unimplemented!("");
    }

    fn const_i32(&self, i: i32) -> Value {
        unimplemented!("");
    }

    fn const_u32(&self, i: u32) -> Value {
        unimplemented!("");
    }

    fn const_u64(&self, i: u64) -> Value {
        unimplemented!("");
    }

    fn const_usize(&self, i: u64) -> Value {
        Value::Const(i)
    }

    fn const_u8(&self, i: u8) -> Value {
        unimplemented!("");
    }

    fn const_cstr(
        &self,
        s: LocalInternedString,
        null_terminated: bool,
    ) -> Value {
        unimplemented!("");
    }

    fn const_str_slice(&self, s: LocalInternedString) -> Value {
        unimplemented!("");
    }

    fn const_fat_ptr(
        &self,
        ptr: Value,
        meta: Value
    ) -> Value {
        unimplemented!("");
    }

    fn const_struct(
        &self,
        elts: &[Value],
        packed: bool
    ) -> Value {
        unimplemented!("const struct {:?}", elts);
        // XXX calculate the size of the struct
        Value::Const(19)
    }

    fn const_array(&self, ty: Type, elts: &[Value]) -> Value {
        unimplemented!("");
    }

    fn const_vector(&self, elts: &[Value]) -> Value {
        unimplemented!("");
    }

    fn const_bytes(&self, bytes: &[u8]) -> Value {
        unimplemented!("");
    }

    fn const_get_elt(&self, v: Value, idx: u64) -> Value {
        unimplemented!("");
    }

    fn const_get_real(&self, v: Value) -> Option<(f64, bool)> {
        unimplemented!("");
    }

    fn const_to_uint(&self, v: Value) -> u64 {
        unimplemented!("");
    }

    fn is_const_integral(&self, v: Value) -> bool {
        unimplemented!("");
    }

    fn is_const_real(&self, v: Value) -> bool {
        unimplemented!("");
    }

    fn const_to_opt_u128(&self, v: Value, sign_ext: bool) -> Option<u128> {
        unimplemented!("");
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
                // XXX
                llval
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
