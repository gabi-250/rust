// Copyright 2018 Gabriela-Alexandra Moldovan
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[allow(dead_code)]
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum GPR {
    RAX,
    RBX,
    RCX,
    RDX,
    RBP,
    RSP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}
impl ToString for GPR {
    fn to_string(&self) -> String {
        match self {
            GPR::RAX => "%rax".to_string(),
            GPR::RBX => "%rbx".to_string(),
            GPR::RCX => "%rcx".to_string(),
            GPR::RDX => "%rdx".to_string(),
            GPR::RBP => "%rbp".to_string(),
            GPR::RSP => "%rsp".to_string(),
            GPR::RSI => "%rsi".to_string(),
            GPR::RDI => "%rdi".to_string(),
            GPR::R8 => "%r8".to_string(),
            GPR::R9 => "%r9".to_string(),
            GPR::R10 => "%r10".to_string(),
            GPR::R11 => "%r11".to_string(),
            GPR::R12 => "%r12".to_string(),
            GPR::R13 => "%r13".to_string(),
            GPR::R14 => "%r14".to_string(),
            GPR::R15 => "%r15".to_string(),
        }
    }
}
