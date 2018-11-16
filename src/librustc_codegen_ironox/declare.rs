use rustc::ty::Ty;

use context::CodegenCx;
use ironox_type::Type;
use rustc_codegen_ssa::interfaces::*;
use value::Value;

impl DeclareMethods<'ll, 'tcx> for CodegenCx<'ll, 'tcx, Value> {

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
        fn_type: Ty<'tcx>,
    ) -> Value {
        eprintln!("Added function {}", name);
        let val = self.module.borrow_mut().add_function(name, fn_type);
        eprintln!("val is {:?}", val);
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
        fn_type: Ty<'tcx>,
    ) -> Value {
        unimplemented!("define_fn");
    }

    fn define_internal_fn(
        &self,
        name: &str,
        fn_type: Ty<'tcx>,
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
