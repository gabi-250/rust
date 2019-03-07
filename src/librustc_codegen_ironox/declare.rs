use abi::FnTypeExt;
use context::CodegenCx;
use global::Global;
use ir::type_::Type;
use ir::value::Value;

use rustc::ty::{self, PolyFnSig};
use rustc_codegen_ssa::traits::*;

impl DeclareMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn declare_global(
        &self,
        name: &str, ty: Type
    ) -> Value {
        let gv = Global::new(ty, name.to_string());
        let mut globals = self.globals.borrow_mut();
        let mut globals_cache = self.globals_cache.borrow_mut();

        if let Some(idx) = globals_cache.get(name) {
            return Value::Global(*idx);
        }
        let gv_idx = globals.len();
        let gv_idx = *globals_cache.entry(name.to_string()).or_insert_with(|| {
            globals.push(gv);
            gv_idx
        });
        Value::Global(gv_idx)
    }

    fn declare_cfn(
        &self,
        name: &str,
        fn_type: Type
    ) -> Value {
        self.module.borrow_mut().add_function(self, name, fn_type)
    }

    fn declare_fn(
        &self,
        name: &str,
        sig: PolyFnSig<'tcx>,
    ) -> Value {
        // Normalize the signature.
        let sig = self.tcx.normalize_erasing_late_bound_regions(
            ty::ParamEnv::reveal_all(),
            &sig);
        // Get the IronOx function type that corresponds to this signature.
        let fn_type = self.new_fn_type(sig, &[]).ironox_type(self);
        // Create a function of type fn_type.
        let fn_val = self.declare_cfn(name, fn_type);
        fn_val
    }

    fn define_global(
        &self,
        name: &str,
        ty: Type
    ) -> Option<Value> {
        if self.get_defined_value(name).is_some() {
            None
        } else {
            Some(self.declare_global(name, ty))
        }
    }

    fn define_private_global(&self, ty: Type) -> Value {
        // FIXME: this global should have private linkage.
        let name = self.get_sym_name("priv_glbl");
        self.declare_global(&name, ty)
    }

    fn define_fn(
        &self,
        _name: &str,
        _fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_fn");
    }

    fn define_internal_fn(
        &self,
        _name: &str,
        _fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_internal_fn");
    }

    fn get_declared_value(&self, name: &str) -> Option<Value> {
        if let Some(idx) = self.globals_cache.borrow().get(name) {
            Some(Value::Global(*idx))
        } else {
            None
        }
    }

    fn get_defined_value(&self, name: &str) -> Option<Value> {
        // FIXME: check if the value is a declaration (defined outside
        // of the current translation unit)
        self.get_declared_value(name)
    }
}
