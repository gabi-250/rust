use context::CodegenCx;
use ir::instruction::Instruction;
use ir::function::OxFunction;
use ir::value::Value;

/// The index of the parent function, and the index of the basic block
/// in the function.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BasicBlock(pub usize, pub usize);

/// A basic block.
#[derive(Debug, PartialEq)]
pub struct OxBasicBlock {
    /// The label of the basic block.
    pub label: String,
    /// The index of the basic block in the parent function.
    pub idx: usize,
    /// The instructions in this basic block.
    pub instrs: Vec<Instruction>,
    /// The function the basic block belongs to.
    pub parent: usize,
    /// The terminator of the basic block.
    pub terminator: Option<String>,
}

impl OxBasicBlock {
    pub fn new(cx: &CodegenCx,
               label: &str,
               parent: &OxFunction,
               idx: usize) -> OxBasicBlock {
        OxBasicBlock {
            label: format!("{}_{}", parent.name, label),
            idx,
            instrs: vec![],
            parent: parent.idx,
            terminator: None,
        }
    }
}
