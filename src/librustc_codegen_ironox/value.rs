// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::hash::{Hash, Hasher};
use context::CodegenCx;
use type_::Type;
use super::ModuleIronOx;

use rustc::ty::layout::Align;
use rustc_codegen_ssa::traits::BaseTypeMethods;

/// The unique identifier of an IronOx value.
///
/// Each enum variant has one or more indices that can be used to retrieve the
/// value from the context.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    /// The index of an `IronOxFunction` function in the module.
    Function(usize),
    /// A `(function index, local index)` pair that can be used to retrieve a
    /// local value.
    Local(usize, usize),
    /// An uninitialized constant. This is just a wrapper around a `Type`.
    ConstUndef(Type),
    /// The index of an `UnsignedConst` in `u_consts`.
    ConstUint(usize),
    /// The index of a `SignedConst` in `i_consts`.
    ConstInt(usize),
    /// The index of an `IronOxStruct` in the `structs` vec from `ModuleIronOx`.
    ConstStruct(usize),
    /// The parameter of an `IronOxFunction`. This is just a wrapper around a
    /// `Type`. A parameter is an (index, type) pair, where 'index' is the
    /// index of the parameter in the list of parameters of the function.
    Param(usize, Type),
    /// An instruction: (functiton index, basic block index, instruction index).
    Instruction(usize, usize, usize),
    /// A placeholder for unimplemented Values. This variant will be removed.
    None,
}

/// An IronOx instruction.
#[derive(PartialEq, Debug, Eq, Hash)]
pub enum Instruction {
    /// Call(fn_idx, args). Emit a call to the function found at index `fn_idx`
    /// in the `functions` vector of the module.
    Call(usize, Vec<Value>),
    /// Allocate space on the stack for a variable of a particular type and
    /// alignment.
    Alloca(String, Type, Align),
    /// Cast a value to a type.
    Cast(Value, Type),
    /// Add two values and return the result.
    Add(Value, Value),
    /// Currently serves as a no-op. This variant will be removed.
    None,
}

impl Instruction {
    /// Return the `Type` of the value that would result from evaluating this
    /// instruction.
    pub fn val_ty(&self, cx: &CodegenCx, module: &ModuleIronOx) -> Type {
        match *self {
            Instruction::Alloca(_, ty, _) => ty,
            Instruction::Cast(_, ty) => ty,
            Instruction::Add(v1, v2) => {
                let ty = cx.val_ty(v1);
                assert_eq!(ty, cx.val_ty(v2));
                ty
            },
            Instruction::Call(fn_idx, _) => cx.module.borrow().functions[fn_idx].ret,
            Instruction::None => bug!("None does not have a type"),
        }
    }
}
