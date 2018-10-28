use rustc_codegen_ssa::interfaces::CodegenObject;
use std::fmt;
use std::hash::{Hash, Hasher};

pub struct Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl<'ll> CodegenObject for &'ll Value {}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("")
        //f.write_str(&llvm::build_string(|s| unsafe {
        //llvm::LLVMRustWriteValueToString(self, s);
        //}).expect("non-UTF8 value description from LLVM"))
    }
}
