use builder::Builder;
use context::CodegenCx;
use ir::value::Value;

use rustc::hir;
use rustc_codegen_ssa::traits::{AsmMethods, AsmBuilderMethods};
use rustc_codegen_ssa::mir::place::PlaceRef;

impl AsmBuilderMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn codegen_inline_asm(
        &mut self,
        ia: &hir::InlineAsm,
        outputs: Vec<PlaceRef<'tcx, Value>>,
        inputs: Vec<Value>
    ) -> bool {
        unimplemented!("codegen_inline_asm");
    }
}

impl AsmMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn codegen_global_asm(&self, ga: &hir::GlobalAsm) {
        unimplemented!("codegen_global_asm");
    }
}
