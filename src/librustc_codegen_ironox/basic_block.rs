use rustc_codegen_ssa::traits::CodegenObject;

use context::CodegenCx;
use value::Value;

macro_rules! asm {
    ($m:expr, $($args:expr)*) => {
        $(
            $m.instrs.push(format!("{}\n", $args));
        )*
    }
}

/// The index of the parent function, and the index of the basic block
/// in the function.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BasicBlock(pub usize, pub usize);

#[derive(Debug, PartialEq)]
pub struct BasicBlockData {
    pub label: String,
    pub instrs: Vec<String>,
    pub parent: Value,
    pub terminator: Option<String>,
}

impl BasicBlockData {
    pub fn new(cx: &CodegenCx, label: &str, parent: Value) -> BasicBlock {
        let mut bb = BasicBlockData {
            label: label.to_string(),
            instrs: vec![],
            parent: parent,
            terminator: None,
        };
        eprintln!("parent {:?}", parent);
        asm!(bb,
             format!("{}:", label));
        let parent = match parent {
            Value::Function(p) => p,
            _ => bug!("The parent of a basic block has to be a function")
        };
        let bb_index = cx.module.borrow_mut().functions[parent].add_bb(bb);
        BasicBlock(parent, bb_index)
    }

    pub fn br(label: &str) {

    }
}
