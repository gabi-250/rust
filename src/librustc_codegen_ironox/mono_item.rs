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
use rustc::hir::def_id::DefId;
use rustc::mir::mono::{Linkage, Visibility};
use rustc_mir::monomorphize::Instance;
use value::Value;
use rustc_codegen_ssa::traits::*;

pub use rustc::mir::mono::MonoItem;

pub use rustc_mir::monomorphize::item::MonoItemExt as BaseMonoItemExt;

impl PreDefineMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn predefine_static(&self,
                        def_id: DefId,
                        linkage: Linkage,
                        visibility: Visibility,
                        symbol_name: &str) {
        unimplemented!("predefine_static");
    }

    fn predefine_fn(&self,
                    instance: Instance<'tcx>,
                    linkage: Linkage,
                    visibility: Visibility,
                    symbol_name: &str) {

        // insert an empty value for now
        self.instances.borrow_mut().insert(instance, &Value {});
    }
}
