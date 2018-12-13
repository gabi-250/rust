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
use value::Value;
use type_::Type;

use rustc_codegen_ssa::traits::*;
use rustc::hir::def_id::DefId;
use rustc::ty::layout::Align;


impl StaticMethods for CodegenCx<'ll, 'tcx> {

    fn static_addr_of(
        &self,
        cv: Value,
        align: Align,
        kind: Option<&str>,
    ) -> Value {
        if let Some(&global) = self.global_cache.borrow().get(&cv) {
            return global;
        }
        let global = match kind {
            Some(kind) if !self.tcx.sess.fewer_names() => {
                unimplemented!("static_addr_of_mut");
            },
            _ => {
                self.define_private_global(self.val_ty(cv))
            }
        };
        self.global_cache.borrow_mut().insert(cv, global);
        global
    }

    fn codegen_static(
        &self,
        def_id: DefId,
        is_mutable: bool,
    ) {
        unimplemented!("codegen_static");
    }
}
