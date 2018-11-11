use rustc_codegen_ssa::interfaces::CodegenObject;

#[derive(Debug, PartialEq)]
pub struct BasicBlock {}

impl<'ll> CodegenObject for &'ll BasicBlock {}
