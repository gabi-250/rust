// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use basic_block::BasicBlockData;
use ironox_type::{LLType, Type};
use value::Value;
use context::CodegenCx;

use rustc::ty::FnSig;
use rustc::ty::layout::Align;

/// An IronOx function.
#[derive(PartialEq, Debug)]
pub struct IronOxFunction {
    /// The name of the function.
    pub name: String,
    /// The basic blocks of the function.
    pub basic_blocks: Vec<BasicBlockData>,
    /// The parameters of the function.
    pub params: Vec<Value>,
}

impl IronOxFunction {
    pub fn new(
        cx: &CodegenCx,
        name: &str,
        fn_type: Type) -> IronOxFunction {
        match cx.types.borrow()[fn_type] {
            LLType::FnType { ref args, ref ret } => {
                // FIXME: populate params
                let mut params = Vec::with_capacity(args.len());
                IronOxFunction {
                    name: name.to_string(),
                    basic_blocks: vec![],
                    params,
                }
            },
            _ => bug!("Expected LLFnType, found {}", fn_type)
        }
    }

    /// Return the specified parameter.
    pub fn get_param(&self, index: usize) -> Value {
        self.params[index]
    }

    /// Add a new basic block to this function.
    ///
    /// The basic block is inserted after the last basic block in the function.
    pub fn add_bb(&mut self, bb: BasicBlockData) -> usize {
        self.basic_blocks.push(bb);
        self.basic_blocks.len() - 1
    }
}
