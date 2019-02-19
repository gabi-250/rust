use builder::Builder;
use context::CodegenCx;
use ir::value::Value;

use rustc::ty::Ty;
use rustc_codegen_ssa::common::IntPredicate;
use rustc_codegen_ssa::traits::{BuilderMethods, ConstMethods, IntrinsicCallMethods};
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
        eprintln!("Ignoring intrinsic call {:?}", callee_ty);
    }

    fn abort(&mut self) {
        unimplemented!("abort");
    }

    fn assume(&mut self, val: Value) {
        unimplemented!("assume");
    }

    fn expect(&mut self, cond: Value, expected: bool) -> Value {
        let expected_val = {
            self.const_bool(!expected)
        };
        self.icmp(IntPredicate::IntEQ, cond, expected_val)
    }
}
