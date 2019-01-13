// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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

/// A basic block.
#[derive(Debug, PartialEq)]
pub struct BasicBlockData {
    /// The label of the basic block.
    pub label: String,
    /// The x86-64 instructions in this basic block.
    pub instrs: Vec<String>,
    /// The function the basic block belongs to.
    pub parent: Value,
    /// The terminator of the basic block.
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
        asm!(bb,
             format!("{}:", label));
        let parent = match parent {
            Value::Function(p) => p,
            _ => bug!("The parent of a basic block has to be a function")
        };
        // the new basic block is the child of the specified `parent`
        // basic block
        let bb_index = cx.module.borrow_mut().functions[parent].add_bb(bb);
        BasicBlock(parent, bb_index)
    }
}
