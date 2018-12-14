// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use type_::Type;

#[derive(Debug)]
pub struct UnsignedConst {
    pub ty: Type,
    pub value: u128,
    pub allowed_range: (u128, u128),
}

#[derive(Debug)]
pub struct SignedConst {
    pub ty: Type,
    pub value: i128,
    pub allowed_range: (i128, i128),
}
