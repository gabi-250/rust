// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::hash::{Hash, Hasher};
use type_::Type;

/// The unique identifier of an IronOx value.
///
/// Each enum variant has one or more indices that can be used to retrieve the
/// value from the context.
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Value {
    /// The index of an `IronOxFunction` function in the module.
    Function(usize),
    /// A `(function index, local index)` pair that can be used to retrieve a
    /// local value.
    Local(usize, usize),
    /// An unspecified constant. This is just a wrapper around a `Type`.
    ConstUndef,
    BigConst(u128),
    Global,
    /// The index of an `IronOxStruct` in the `structs` vec from `ModuleIronOx`.
    ConstStruct(usize),
    /// The parameter of an `IronOxFunction`. This is just a wrapper around a `Type`.
    Param(Type),
    /// A placeholder for unimplemented Values. This variant will be removed.
    None,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Instruction {
    // FIXME: implement
    None,
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}
