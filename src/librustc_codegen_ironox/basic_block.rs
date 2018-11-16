use rustc_codegen_ssa::interfaces::CodegenObject;

use context::CodegenCx;
use value::Value;

macro_rules! asm {
    ($m:expr, $($args:expr)*) => {
        $(
            $m.instrs.push(format!("{}\n", $args));
        )*
    }
}

#[derive(Debug, PartialEq)]
pub struct BasicBlock {
    pub label: String,
    pub instrs: Vec<String>,
    pub parent: Value,
    pub terminator: Option<String>,
}

impl BasicBlock {
    pub fn new(cx: &CodegenCx<Value>, label: &str, parent: Value) -> Value {
        let mut bb = BasicBlock {
            label: label.to_string(),
            instrs: vec![],
            parent: parent,
            terminator: None,
        };
        asm!(bb,
             format!("{}:", label));
        let parent = match parent {
            Value::Function(p) => p,
            _ => bug!("The parent of a basic block has to be a function")
        };
        let bb_index = cx.module.borrow_mut().functions[parent].add_bb(bb);
        Value::BasicBlock(parent, bb_index)
    }
}

impl<'ll> CodegenObject for &'ll BasicBlock {}
