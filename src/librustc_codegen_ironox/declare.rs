use abi::FnTypeExt;
use context::CodegenCx;
use ir::type_::Type;
use ir::value::Value;
use global::Global;

use rustc::ty::{self, PolyFnSig};
use rustc_codegen_ssa::traits::*;

impl DeclareMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn declare_global(
        &self,
        name: &str, ty: Type
    ) -> Value {
        let name = Some(name.to_string());
        let global = Global::new(ty, name);
        self.get_or_insert_global(global)
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
        unimplemented!("define_global");
    }

    fn define_private_global(&self, ty: Type) -> Value {
        unimplemented!("define_private_global");
    }

    fn define_fn(
        &self,
        name: &str,
        fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_fn");
    }

    fn define_internal_fn(
        &self,
        name: &str,
        fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_internal_fn");
    }

    fn get_declared_value(&self, name: &str) -> Option<Value> {
        None
    }

    fn get_defined_value(&self, name: &str) -> Option<Value> {
        // FIXME: check if the value is a declaration (defined outside
        // of the current translation unit)
        self.get_declared_value(name)
    }
}
