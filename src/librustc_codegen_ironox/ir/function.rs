use context::CodegenCx;
use ir::type_::{OxType, Type};
use ir::value::Value;
use ir::instruction::OxInstruction;
use super::basic_block::{BasicBlock, OxBasicBlock};

use rustc::mir::mono::Linkage;

/// An IronOx function.
#[derive(PartialEq, Debug)]
pub struct OxFunction {
    /// The name of the function.
    pub name: String,
    /// The basic blocks of the function.
    pub basic_blocks: Vec<OxBasicBlock>,
    /// The parameters of the function.
    pub params: Vec<Value>,
    /// The index of the function in the module.
    pub idx: usize,
    /// The type of the function.
    pub ironox_type: Type,
    /// The return type of the function.
    pub ret: Type,
    /// The linkage of the function.
    pub linkage: Linkage,
    /// Whether this function has been defined in the local crate.
    pub is_codegenned: bool,
}

impl OxFunction {
    pub fn new(
        cx: &CodegenCx,
        name: &str,
        idx: usize,
        fn_type: Type) -> OxFunction {
        match cx.types.borrow()[fn_type] {
            OxType::Function { ref args, ref ret } => {
                let mut params = Vec::with_capacity(args.len());
                for (param_idx, ty) in args.iter().enumerate() {
                    params.push(Value::Param { fn_idx: idx, param_idx, ty: *ty });
                }
                let ret = *ret;
                OxFunction {
                    name: name.to_string(),
                    idx,
                    ironox_type: fn_type,
                    basic_blocks: vec![],
                    params,
                    ret,
                    linkage: Linkage::External,
                    is_codegenned: false,
                }
            },
            _ => bug!("Expected OxType::Function, found {:?}", fn_type)
        }
    }

    /// Insert the instruction at a specified position in a basic block.
    pub fn insert_inst(
        &mut self,
        bb_idx: usize,
        inst_idx: usize,
        inst: OxInstruction) {
        assert!(
            bb_idx < self.basic_blocks.len()
                && inst_idx <= self.basic_blocks[bb_idx].instrs.len(),
            "Invalid insertion point!");
        self.basic_blocks[bb_idx].instrs.insert(inst_idx, inst)
    }

    /// Return the specified parameter.
    pub fn get_param(&self, index: usize) -> Value {
        self.params[index]
    }

    /// Add a new basic block to this function.
    ///
    /// The basic block is inserted after the last basic block in the function.
    pub fn add_bb(&mut self, cx: &CodegenCx, label: &str) -> BasicBlock {
        let idx = self.basic_blocks.len();
        let bb = OxBasicBlock::new(cx, label, self, idx);
        self.basic_blocks.push(bb);
        BasicBlock(self.idx, idx)
    }

    pub fn is_declaration(&self) -> bool {
        self.basic_blocks.is_empty()
    }
}
