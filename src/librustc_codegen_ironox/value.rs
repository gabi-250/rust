use rustc_codegen_ssa::interfaces::CodegenObject;
use std::hash::{Hash, Hasher};

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Value {
    Function(usize),
    BasicBlock(usize, usize),
    Local(usize, usize),
    ConstUndef,
    Const(u64),
    Global,
    None,
}

impl<'ll> CodegenObject for Value {}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}
