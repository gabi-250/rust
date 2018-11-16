use builder::Builder;
use context::CodegenCx;
use value::Value;

use rustc::hir;
use rustc_codegen_ssa::interfaces::{AsmMethods, AsmBuilderMethods};
use rustc_codegen_ssa::mir::place::PlaceRef;

impl AsmBuilderMethods<'a, 'll, 'tcx> for Builder<'a, 'll, 'tcx, Value> {
    fn codegen_inline_asm(
        &mut self,
        ia: &hir::InlineAsm,
        outputs: Vec<PlaceRef<'tcx, Value>>,
        inputs: Vec<Value>
    ) -> bool {
        unimplemented!("codegen_inline_asm");
    }
}

impl AsmMethods for CodegenCx<'ll, 'tcx, Value> {
    fn codegen_global_asm(&self, ga: &hir::GlobalAsm) {
        unimplemented!("codegen_global_asm");
    }
}
