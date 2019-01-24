// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::hash::{Hash, Hasher};
use type_::Type;

use rustc::ty::layout::Align;

/// The unique identifier of an IronOx value.
///
/// Each enum variant has one or more indices that can be used to retrieve the
/// value from the context.
#[derive(PartialEq, Copy, Clone, Eq, Debug, Hash)]
pub enum Value {
    /// The index of an `IronOxFunction` function in the module.
    Function(usize),
    /// An unspecified constant. This is just a wrapper around a `Type`.
    ConstUndef(Type),
    /// The index of an `UnsignedConst` in `u_consts`.
    ConstUint(usize),
    /// The index of a `SignedConst` in `i_consts`.
    ConstInt(usize),
    /// The index of a private (unnamed) global value that in `private_globals`.
    Global(usize),
    /// The index of an `IronOxStruct` in the `structs` vec from `ModuleIronOx`.
    ConstStruct(usize),
    /// The parameter of an `IronOxFunction`. This is just a wrapper around a `Type`.
    Param(usize, Type),
    /// An instruction: (functiton index, basic block index, instruction index).
    Instruction(usize, usize, usize),
    StructPtr(usize),
    None,
}

#[derive(PartialEq, Debug, Eq, Hash)]
pub enum Instruction {
    /// Store (ptr, value).
    Store(Value, Value),
    /// An unconditional branch to a label.
    Br(String),
    CondBr(Value, String, String),
    /// Return instruction.
    Ret(Option<Value>),
    /// Call (function, args)
    Call(usize, Vec<Value>),
    Alloca(String, Type, u64, Align),
    Cast(Value, Type),
    Add(Value, Value),
    Sub(Value, Value),
    Eq(Value, Value),
    Lt(Value, Value),
}

impl Instruction {
    pub fn is_branch(&self) -> bool {
        match *self {
            Instruction::Br(..) | Instruction::CondBr(..) => true,
            _ => false,
        }
    }
}
