use builder::Builder;
use context::CodegenCx;
use value::Value;

use rustc::ty::Ty;
use rustc_codegen_ssa::traits::{IntrinsicCallMethods, IntrinsicDeclarationMethods};
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_target::abi::call::FnType;
use syntax_pos::Span;

impl IntrinsicCallMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        callee_ty: Ty<'tcx>,
        fn_ty: &FnType<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, Value>],
        llresult: Value,
        span: Span,
    ) {
        unimplemented!("codegen_intrinsic_call");
    }
}

impl IntrinsicDeclarationMethods<'tcx> for CodegenCx<'b, 'tcx> {
    fn get_intrinsic(&self, key: &str) -> Value {
        unimplemented!("get_intrinsic");
    }

    fn declare_intrinsic(
        &self,
        key: &str
    ) -> Option<Value> {
        unimplemented!("declare_intrinsic");
    }
}
