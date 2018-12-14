// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::basic_block::BasicBlockData;
use type_::{LLType, Type};
use value::{Instruction, Value};
use context::CodegenCx;

use rustc::ty::FnSig;
use rustc::ty::layout::Align;

/// An IronOx function.
#[derive(PartialEq, Debug)]
pub struct IronOxFunction {
    /// The name of the function.
    pub name: String,
    /// The type of the function.
    pub ironox_type: Type,
    /// The basic blocks of the function.
    pub basic_blocks: Vec<BasicBlockData>,
    pub stack_size: u64,
    // Type, offset from rbp
    pub locals: Vec<(Type, u64)>,
    /// The parameters of the function.
    pub params: Vec<Value>,
    /// The return type of the function.
    pub ret: Value,
}

impl IronOxFunction {
    pub fn new(
        cx: &CodegenCx,
        name: &str,
        fn_type: Type) -> IronOxFunction {
        match cx.types.borrow()[fn_type] {
            LLType::FnType { ref args, ref ret } => {
                let ret = Value::Param(*ret);
                let mut params = Vec::with_capacity(args.len());
                for (index, arg_ty) in args.iter().enumerate() {
                    params.push(Value::Param(*arg_ty));
                }
                IronOxFunction {
                    name: name.to_string(),
                    ironox_type: fn_type,
                    basic_blocks: vec![],
                    params,
                    ret,
                    locals: Default::default(),
                    stack_size: 8,
                }
            },
            _ => bug!("Expected LLFnType, found {}", fn_type)
        }
    }

    /// Insert the instruction at a specified position in a basic block.
    pub fn insert_inst(
        &mut self,
        bb_idx: usize,
        inst_idx: usize,
        inst: Instruction) {
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

    pub fn alloca(
        &mut self,
        cx: &CodegenCx,
        ty: Type,
        name: &str,
        align: Align) -> usize {
        eprintln!("alloca {:?} of type {:?}", name, ty);
        self.insert_local(cx, ty, name)
    }

    fn insert_local(
        &mut self,
        cx: &CodegenCx,
        ty: Type,
        name: &str) -> usize {
        // XXX ignore the alignment and name for now
        // map the local to its offset from rbp
        self.locals.push((ty, self.stack_size));
        self.stack_size += cx.ty_size(ty);
        self.locals.len() - 1
    }

    pub fn local_ty(
        &self,
        local_idx: usize) -> Type {
        self.locals[local_idx].0
    }

    /// Add a new basic block to this function.
    ///
    /// The basic block is inserted after the last basic block in the function.
    pub fn add_bb(&mut self, bb: BasicBlockData) -> usize {
        self.basic_blocks.push(bb);
        self.basic_blocks.len() - 1
    }
}
