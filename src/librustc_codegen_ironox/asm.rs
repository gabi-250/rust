// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use builder::Builder;
use context::CodegenCx;
use value::Value;

use rustc::hir;
use rustc_codegen_ssa::traits::{AsmMethods, AsmBuilderMethods};
use rustc_codegen_ssa::mir::place::PlaceRef;

impl AsmBuilderMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn codegen_inline_asm(
        &mut self,
        ia: &hir::InlineAsm,
        outputs: Vec<PlaceRef<'tcx, &'ll Value>>,
        inputs: Vec<&'ll Value>
    ) -> bool {
        unimplemented!("codegen_inline_asm");
    }
}

impl AsmMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn codegen_global_asm(&self, ga: &hir::GlobalAsm) {
        unimplemented!("codegen_global_asm");
    }
}
