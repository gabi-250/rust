// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::basic_block::BasicBlockData;
use type_::{LLType, Type};
use value::Value;
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
