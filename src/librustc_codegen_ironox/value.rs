// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::hash::{Hash, Hasher};
use registers::GPR;
use type_::Type;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum Value {
    Function(usize),
    // (function index, local index)
    Local(usize, usize),
    Register(GPR),
    RbpOffset(isize),
    ConstUndef,
    Const(u64),
    BigConst(u128),
    Global(Type),
    // the position in the structs Vec from ModuleIronOx
    ConstStruct(usize),
    Param(Type),
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}
