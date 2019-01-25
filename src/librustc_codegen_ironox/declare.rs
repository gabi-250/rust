// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use abi::FnTypeExt;
use context::CodegenCx;
use type_::Type;
use value::Value;

use rustc::ty::{self, PolyFnSig};
use rustc_codegen_ssa::traits::*;

impl DeclareMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn declare_global(
        &self,
        name: &str, ty: Type
    ) -> Value {
        unimplemented!("declare_global");
    }

    fn declare_cfn(
        &self,
        name: &str,
        fn_type: Type
    ) -> Value {
        let mut module = self.module.borrow_mut();
        eprintln!("Declare cfun {} of type {:?}", name,
                  module.icx.types[fn_type]);
        let fn_val = module.add_function(self, name, fn_type);
        module.icx.named_globals.insert(name.to_string(), fn_val);
        fn_val
    }

    fn declare_fn(
        &self,
        name: &str,
        sig: PolyFnSig<'tcx>,
    ) -> Value {
        // Normalize the signature.
        let sig = self.tcx.normalize_erasing_late_bound_regions(
            ty::ParamEnv::reveal_all(),
            &sig);
        // Get the IronOx function type that corresponds to this signature.
        let fn_type = self.new_fn_type(sig, &[]).ironox_type(self);
        // Create a function of type fn_type.
        let fn_val = self.declare_cfn(name, fn_type);
        fn_val
    }

    fn define_global(
        &self,
        name: &str,
        ty: Type
    ) -> Option<Value> {
        unimplemented!("define_global");
    }

    fn define_private_global(&self, ty: Type) -> Value {
        self.module.borrow_mut().add_private_global(ty)
    }

    fn define_fn(
        &self,
        name: &str,
        fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_fn");
    }

    fn define_internal_fn(
        &self,
        name: &str,
        fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_internal_fn");
    }

    fn get_declared_value(&self, name: &str) -> Option<Value> {
        eprintln!("all functions {:?}", self.module.borrow().functions);
        eprintln!("get_declared_value {:?}", name);
        None
    }

    fn get_defined_value(&self, name: &str) -> Option<Value> {
        // FIXME: check if the value is a declaration (defined outside
        // of the current translation unit)
        self.get_declared_value(name)
    }
}
