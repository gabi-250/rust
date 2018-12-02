use rustc_codegen_ssa::traits::CodegenObject;
use std::hash::{Hash, Hasher};
use registers::GPR;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Value {
    Function(usize),
    // (function index, local index)
    Local(usize, usize),
    Register(GPR),
    RbpOffset(isize),
    ConstUndef,
    Const(u64),
    BigConst(u128),
    Global,
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}
