// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
//
use builder::Builder;
use context::CodegenCx;
use value::Value;

use rustc::ty::Ty;
use rustc_codegen_ssa::traits::{IntrinsicCallMethods, IntrinsicDeclarationMethods};
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_target::abi::call::FnType;
use syntax_pos::Span;

impl IntrinsicCallMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        callee_ty: Ty<'tcx>,
        fn_ty: &FnType<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, &'ll Value>],
        llresult: &'ll Value,
        span: Span,
    ) {
        unimplemented!("codegen_intrinsic_call");
    }
}

impl IntrinsicDeclarationMethods<'tcx> for CodegenCx<'b, 'tcx> {
    fn get_intrinsic(&self, key: &str) -> &'b Value {
        unimplemented!("get_intrinsic");
    }

    fn declare_intrinsic(
        &self,
        key: &str
    ) -> Option<&'b Value> {
        unimplemented!("declare_intrinsic");
    }
}
