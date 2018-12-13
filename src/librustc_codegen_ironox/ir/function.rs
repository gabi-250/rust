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
use registers::GPR;
use context::CodegenCx;

use rustc::ty::FnSig;
use rustc::ty::layout::Align;

#[derive(PartialEq, Debug)]
pub struct IronOxFunction {
    pub name: String,
    pub ironox_type: Type,
    pub basic_blocks: Vec<BasicBlockData>,
    pub stack_size: u64,
    pub locals: Vec<u64>,
    pub params: Vec<Value>,
    pub ret: Value,
}

impl IronOxFunction {

    pub fn new(
        cx: &CodegenCx,
        name: &str,
        fn_type: Type) -> IronOxFunction {
        eprintln!("new with type {:?}", fn_type);
        match cx.types.borrow()[fn_type] {
            LLType::FnType { ref args, ref ret } => {
                let ret = Value::Param(*ret);
                let mut params = Vec::with_capacity(args.len());
                for (index, arg_ty) in args.iter().enumerate() {
                    eprintln!("adding arg {:?}", *arg_ty);
                    params.push(Value::Param(*arg_ty));
                }
                IronOxFunction {
                    name: name.to_string(),
                    ironox_type: fn_type,
                    basic_blocks: vec![],
                    stack_size: 8,
                    locals: vec![],
                    params,
                    ret,
                }
            },
            _ => bug!("Expected LLFnType, found {}", fn_type)
        }
    }

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

    pub fn zext_local(
        &mut self,
        cx: &CodegenCx,
        val: Value,
        ty: Type) -> usize {
        let local_idx = self.locals.len();
        self.insert_local(cx, ty, &format!("zext_val{:?}", local_idx))
    }

    fn insert_local(
        &mut self,
        cx: &CodegenCx,
        ty: Type,
        name: &str) -> usize {
        // XXX ignore the alignment and name for now
        // map the local to its offset from rbp
        self.locals.push(self.stack_size);
        self.stack_size += cx.ty_size(ty);
        self.locals.len() - 1
    }

    pub fn rbp_offset(
        &self,
        local_idx: usize) -> u64 {
        self.locals[local_idx]
    }

    pub fn add_bb(&mut self, bb: BasicBlockData) -> usize {
        self.basic_blocks.push(bb);
        self.basic_blocks.len() - 1
    }
}
