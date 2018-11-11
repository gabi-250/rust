use builder::Builder;
use context::CodegenCx;
use value::Value;

use rustc::ty::Ty;
use rustc_codegen_ssa::interfaces::{IntrinsicCallMethods, IntrinsicDeclarationMethods};
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_target::abi::call::FnType;
use syntax_pos::Span;

impl IntrinsicCallMethods<'a, 'll, 'tcx> for Builder<'a, 'll, 'tcx, &'ll Value> {
    fn codegen_intrinsic_call(
        &mut self,
        callee_ty: Ty<'tcx>,
        fn_ty: &FnType<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, &'ll Value>],
        llresult: &'ll Value,
        span: Span,
    ) {
        unimplemented!("codegen_intrinsic_call");
    }
}

impl IntrinsicDeclarationMethods<'b> for CodegenCx<'b, 'tcx, &'b Value> {
    fn get_intrinsic(&self, key: &str) -> &'b Value {
        unimplemented!("get_intrinsic");
    }

    fn declare_intrinsic(
        &self,
        key: &str
    ) -> Option<&'b Value> {
        unimplemented!("declare_intrinsic");
    }
}
