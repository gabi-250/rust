use context::CodegenCx;
use ironox_type::Type;
use rustc::ty::{self, PolyFnSig};
use rustc_codegen_ssa::traits::*;
use value::Value;

impl DeclareMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn declare_global(
        &self,
        name: &str, ty: &'ll Type
    ) -> Value {
        unimplemented!("declare_global");
    }

    fn declare_cfn(
        &self,
        name: &str,
        fn_type: &'ll Type
    ) -> Value {
        unimplemented!("declare_cfn");
    }

    fn declare_fn(
        &self,
        name: &str,
        sig: PolyFnSig<'tcx>,
    ) -> Value {
        let sig = self.tcx.normalize_erasing_late_bound_regions(
            ty::ParamEnv::reveal_all(),
            &sig);
        let val = self.module.borrow_mut().add_function(name, sig);
        val
    }

    fn define_global(
        &self,
        name: &str,
        ty: &'ll Type
    ) -> Option<Value> {
        unimplemented!("define_global");
    }

    fn define_private_global(&self, ty: &'ll Type) -> Value {
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
        self.module.borrow().get_function(name)
    }

    fn get_defined_value(&self, name: &str) -> Option<Value> {
        unimplemented!("get_defined_value");
    }
}
