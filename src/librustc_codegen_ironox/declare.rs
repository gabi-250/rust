// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use context::CodegenCx;
use ironox_type::Type;
use rustc::ty::PolyFnSig;
use rustc_codegen_ssa::traits::*;
use value::Value;

impl DeclareMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn declare_global(
        &self,
        name: &str, ty: &'ll Type
    ) -> &'ll Value {
        unimplemented!("declare_global");
    }

    fn declare_cfn(
        &self,
        name: &str,
        fn_type: &'ll Type
    ) -> &'ll Value {
        unimplemented!("declare_cfn");
    }

    fn declare_fn(
        &self,
        name: &str,
        sig: PolyFnSig<'tcx>,
    ) -> &'ll Value {
        unimplemented!("declare_fn");
    }

    fn define_global(
        &self,
        name: &str,
        ty: &'ll Type
    ) -> Option<&'ll Value> {
        unimplemented!("define_global");
    }

    fn define_private_global(&self, ty: &'ll Type) -> &'ll Value {
        unimplemented!("define_private_global");
    }

    fn define_fn(
        &self,
        name: &str,
        fn_sig: PolyFnSig<'tcx>,
    ) -> &'ll Value {
        unimplemented!("define_fn");
    }

    fn define_internal_fn(
        &self,
        name: &str,
        fn_sig: PolyFnSig<'tcx>,
    ) -> &'ll Value {
        unimplemented!("define_internal_fn");
    }

    fn get_declared_value(&self, name: &str) -> Option<&'ll Value> {
        unimplemented!("get_declared_value");
    }

    fn get_defined_value(&self, name: &str) -> Option<&'ll Value> {
        unimplemented!("get_defined_value");
    }
}
