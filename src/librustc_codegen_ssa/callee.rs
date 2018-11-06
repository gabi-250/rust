// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use interfaces::*;
use rustc::ty::{self, Ty};
use rustc::ty::layout::{LayoutOf, TyLayout, HasTyCtxt};
use rustc::ty::subst::Substs;
use rustc::hir::def_id::DefId;

pub fn resolve_and_get_fn<'a, 'll: 'a, 'tcx: 'll, Cx : 'a +  CodegenMethods<'a, 'll, 'tcx>>(
    cx: &Cx,
    def_id: DefId,
    substs: &'tcx Substs<'tcx>,
) -> Cx::Value
    where &'a Cx : LayoutOf<Ty = Ty<'tcx>, TyLayout = TyLayout<'tcx>> + HasTyCtxt<'tcx>
{
    cx.get_fn(
        ty::Instance::resolve(
            *cx.tcx(),
            ty::ParamEnv::reveal_all(),
            def_id,
            substs
        ).unwrap()
    )
}
