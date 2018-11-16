#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutOf, LayoutError, self, TyLayout, Size};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::interfaces::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_mir::monomorphize::Instance;
use rustc::mir::interpret::{Scalar, Allocation};
use std::cell::RefCell;
use std::sync::Arc;
use std::hash::Hash;
use syntax::symbol::LocalInternedString;

use value::Value;
use ironox_type::Type;
use function::IronOxFunction;

pub struct IronOxContext<'ll> {
    pub module: &'ll mut ::ModuleIronOx
}

pub struct IronOxModule {
    pub functions: Vec<IronOxFunction>,
}

impl IronOxModule {
    pub fn new() -> IronOxModule {
        IronOxModule {
            functions: vec![],
        }
    }

    pub fn add_function(&mut self, name: &str, fn_type: Ty<'tcx>) -> Value {
        self.functions.push(IronOxFunction::new(name));
        Value::Function(self.functions.len() - 1)
    }

    // XXX slow
    pub fn get_function(&self, name: &str) -> Option<Value> {
        for (i, f) in self.functions.iter().enumerate() {
            if f.name == name {
                return Some(Value::Function(i))
            }
        }
        return None;
    }
}

pub struct CodegenCx<'ll, 'tcx: 'll, V> {
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    pub stats: RefCell<Stats>,
    pub codegen_unit: Arc<CodegenUnit<'tcx>>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, V>>,
    pub module: RefCell<IronOxModule>,
    pub current_bb: RefCell<Option<Value>>,
    pub vtables: RefCell<FxHashMap<(Ty<'tcx>, ty::PolyExistentialTraitRef<'tcx>), V>>,
}

impl<'ll, 'tcx, Value : Eq+Hash> CodegenCx<'ll, 'tcx, Value> {
    pub fn new(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
           codegen_unit: Arc<CodegenUnit<'tcx>>)
                 -> CodegenCx<'ll, 'tcx, Value> {
        CodegenCx {
            tcx,
            codegen_unit,
            stats: RefCell::new(Stats::default()),
            instances: Default::default(),
            module: RefCell::new(IronOxModule::new()),
            current_bb: RefCell::new(None),
            vtables: Default::default(),
        }
    }
}

impl<'a, 'll: 'a, 'tcx: 'll> CodegenMethods<'a, 'll, 'tcx>
    for CodegenCx<'ll, 'tcx, Value> {}

impl Backend<'ll> for CodegenCx<'ll, 'tcx, Value> {
    type Value = Value;
    type BasicBlock = Value;
    type Type = &'ll Type;
    type Context = IronOxContext<'ll>;
}

impl ty::layout::HasTyCtxt<'tcx> for &'a CodegenCx<'ll, 'tcx, Value> {
    fn tcx<'b>(&'b self) -> TyCtxt<'b, 'tcx, 'tcx> {
        self.tcx
    }
}

impl LayoutOf for &'a CodegenCx<'ll, 'tcx, Value> {
    type Ty = Ty<'tcx>;
    type TyLayout = TyLayout<'tcx>;

    fn layout_of(self, ty: Ty<'tcx>) -> Self::TyLayout {
        self.tcx.layout_of(ty::ParamEnv::reveal_all().and(ty))
            .unwrap_or_else(|e| if let LayoutError::SizeOverflow(_) = e {
                self.sess().fatal(&e.to_string())
            } else {
                bug!("failed to get layout for `{}`: {}", ty, e)
            })
    }
}

impl ty::layout::HasDataLayout for &'a CodegenCx<'ll, 'tcx, Value> {
    fn data_layout(&self) -> &ty::layout::TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl MiscMethods<'ll, 'tcx> for CodegenCx<'ll, 'tcx, Value> {
    fn vtables(&self) -> &RefCell<
        FxHashMap<(Ty<'tcx>, ty::PolyExistentialTraitRef<'tcx>), Value>
    > {
        &self.vtables
    }

    fn instances(&self) -> &RefCell<FxHashMap<Instance<'tcx>, Value>> {
        &self.instances
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Value {
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
            let fn_ty = instance.ty(self.tcx);
            let llfn = self.declare_fn(&sym_name, fn_ty);
            // visbility, linkage......
            llfn
        };
        self.instances.borrow_mut().insert(instance, llfn);
        llfn
    }

    fn get_param(&self, llfn: Value, index: c_uint) -> Value {
        // XXX return a dummy local
        Value::Local(111, 111)
    }

    fn eh_personality(&self) -> Value {
        unimplemented!("eh_personality");
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
        unimplemented!("set_frame_pointer_elimination");
    }

    fn apply_target_cpu_attr(&self, llfn: Value) {
        unimplemented!("apply_target_cpu_attr");
    }

    fn env_alloca_allowed(&self) -> bool {
        unimplemented!("env_alloca_allowed");
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }
}

// common?
impl<'ll, 'tcx : 'll> ConstMethods<'ll, 'tcx> for CodegenCx<'ll, 'tcx, Value> {

    fn const_null(&self, t: &'ll Type) -> Value {
        Value::Const(0)
    }

    fn const_undef(&self, t: &'ll Type) -> Value {
        Value::ConstUndef
    }

    fn const_int(&self, t: &'ll Type, i: i64) -> Value {
        unimplemented!("");
    }

    fn const_uint(&self, t: &'ll Type, i: u64) -> Value {
        unimplemented!("");
    }

    fn const_uint_big(&self, t: &'ll Type, u: u128) -> Value {
        unimplemented!("");
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
        //unimplemented!("");
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
        // XXX calculate the size of the struct
        Value::Const(19)
    }

    fn const_array(&self, ty: &'ll Type, elts: &[Value]) -> Value {
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
        llty: &'ll Type,
    ) -> Value {
        unimplemented!("scalar_to_backend");
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
