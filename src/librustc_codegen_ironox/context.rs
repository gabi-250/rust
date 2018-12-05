// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutOf, LayoutError, self, TyLayout, Size};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_mir::monomorphize::Instance;
use rustc::mir::interpret::{Scalar, Allocation};
use std::cell::RefCell;
use std::sync::Arc;
use syntax::symbol::LocalInternedString;

use basic_block::BasicBlock;
use value::Value;
use ironox_type::Type;
use debuginfo::DIScope;

impl BackendTypes for CodegenCx<'ll, 'tcx> {
    type Value = &'ll Value;
    type BasicBlock = &'ll BasicBlock;
    type Type = &'ll Type;
    type Context = ();
    type Funclet = ();

    type DIScope = &'ll DIScope;
}

pub struct CodegenCx<'ll, 'tcx: 'll> {
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    pub stats: RefCell<Stats>,
    pub codegen_unit: Arc<CodegenUnit<'tcx>>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, &'ll Value>>,
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn new(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
           codegen_unit: Arc<CodegenUnit<'tcx>>,
           module: &'ll ::ModuleIronOx)
                 -> CodegenCx<'ll, 'tcx> {
        CodegenCx {
            tcx,
            codegen_unit,
            stats: RefCell::new(Stats::default()),
            instances: Default::default(),
        }
    }
}

impl ty::layout::HasTyCtxt<'tcx> for CodegenCx<'ll, 'tcx> {
    fn tcx<'b>(&'b self) -> TyCtxt<'b, 'tcx, 'tcx> {
        self.tcx
    }
}

impl LayoutOf for CodegenCx<'ll, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyLayout = TyLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyLayout {
        self.tcx.layout_of(ty::ParamEnv::reveal_all().and(ty))
            .unwrap_or_else(|e| if let LayoutError::SizeOverflow(_) = e {
                self.sess().fatal(&e.to_string())
            } else {
                bug!("failed to get layout for `{}`: {}", ty, e)
            })
    }
}

impl ty::layout::HasDataLayout for CodegenCx<'ll, 'tcx> {
    fn data_layout(&self) -> &ty::layout::TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl MiscMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn vtables(&self) -> &RefCell<
        FxHashMap<(Ty<'tcx>, ty::PolyExistentialTraitRef<'tcx>), &'ll Value>
    > {
        unimplemented!("vtables");
    }

    fn instances(&self) -> &RefCell<FxHashMap<Instance<'tcx>, &'ll Value>> {
        &self.instances
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> &'ll Value {
        unimplemented!("get_fn");
    }

    fn get_param(&self, llfn: &'ll Value, index: c_uint) -> &'ll Value {
        unimplemented!("get_param");
    }

    fn eh_personality(&self) -> &'ll Value {
        unimplemented!("eh_personality");
    }

    fn eh_unwind_resume(&self) -> &'ll Value {
        unimplemented!("eh_unwind_resume");
    }

    fn sess(&self) -> &Session {
        &self.tcx.sess
    }

    fn check_overflow(&self) -> bool {
        unimplemented!("check_overflow");
    }

    fn stats(&self) -> &RefCell<Stats> {
        &self.stats
    }

    fn consume_stats(self) -> RefCell<Stats> {
        self.stats
    }

    fn codegen_unit(&self) -> &Arc<CodegenUnit<'tcx>> {
        &self.codegen_unit
    }

    fn statics_to_rauw(&self) -> &RefCell<Vec<(&'ll Value, &'ll Value)>> {
        unimplemented!("statics_to_rauw");
    }

    fn used_statics(&self) -> &RefCell<Vec<&'ll Value>> {
        unimplemented!("used_statics");
    }

    fn set_frame_pointer_elimination(&self, llfn: &'ll Value) {
        unimplemented!("set_frame_pointer_elimination");
    }

    fn apply_target_cpu_attr(&self, llfn: &'ll Value) {
        unimplemented!("apply_target_cpu_attr");
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }

    fn closure_env_needs_indirect_debuginfo(&self) -> bool {
        unimplemented!("closure_env_needs_indirect_debuginfo");
    }
}

// common?

impl ConstMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn const_null(&self, t: &'ll Type) -> &'ll Value {
        unimplemented!("const_null");
    }

    fn const_undef(&self, t: &'ll Type) -> &'ll Value {
        unimplemented!("const_undef");
    }

    fn const_int(&self, t: &'ll Type, i: i64) -> &'ll Value {
        unimplemented!("const_int");
    }

    fn const_uint(&self, t: &'ll Type, i: u64) -> &'ll Value {
        unimplemented!("const_uint");
    }

    fn const_uint_big(&self, t: &'ll Type, u: u128) -> &'ll Value {
        unimplemented!("const_uint_big");
    }

    fn const_bool(&self, val: bool) -> &'ll Value {
        unimplemented!("const_bool");
    }

    fn const_i32(&self, i: i32) -> &'ll Value {
        unimplemented!("const_i32");
    }

    fn const_u32(&self, i: u32) -> &'ll Value {
        unimplemented!("const_u32");
    }

    fn const_u64(&self, i: u64) -> &'ll Value {
        unimplemented!("const_u64");
    }

    fn const_usize(&self, i: u64) -> &'ll Value {
        unimplemented!("const_usize");
    }

    fn const_u8(&self, i: u8) -> &'ll Value {
        unimplemented!("const_u8");
    }

    fn const_cstr(
        &self,
        s: LocalInternedString,
        null_terminated: bool,
    ) -> &'ll Value {
        unimplemented!("const_cstr");
    }

    fn const_str_slice(&self, s: LocalInternedString) -> &'ll Value {
        unimplemented!("const_str_slice");
    }

    fn const_fat_ptr(
        &self,
        ptr: &'ll Value,
        meta: &'ll Value
    ) -> &'ll Value {
        unimplemented!("const_fat_ptr");
    }

    fn const_struct(
        &self,
        elts: &[&'ll Value],
        packed: bool
    ) -> &'ll Value {
        unimplemented!("const_struct");
    }

    fn const_array(&self, ty: &'ll Type, elts: &[&'ll Value]) -> &'ll Value {
        unimplemented!("const_array");
    }

    fn const_vector(&self, elts: &[&'ll Value]) -> &'ll Value {
        unimplemented!("const_vector");
    }

    fn const_bytes(&self, bytes: &[u8]) -> &'ll Value {
        unimplemented!("const_bytes");
    }

    fn const_get_elt(&self, v: &'ll Value, idx: u64) -> &'ll Value {
        unimplemented!("const_get_elt");
    }

    fn const_get_real(&self, v: &'ll Value) -> Option<(f64, bool)> {
        unimplemented!("const_get_real");
    }

    fn const_to_uint(&self, v: &'ll Value) -> u64 {
        unimplemented!("const_to_uint");
    }

    fn is_const_integral(&self, v: &'ll Value) -> bool {
        unimplemented!("is_const_integral");
    }

    fn is_const_real(&self, v: &'ll Value) -> bool {
        unimplemented!("is_const_real");
    }

    fn const_to_opt_u128(&self, v: &'ll Value, sign_ext: bool) -> Option<u128> {
        unimplemented!("const_to_opt_u128");
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: &layout::Scalar,
        llty: &'ll Type,
    ) -> &'ll Value {
        unimplemented!("scalar_to_backend");
    }

    fn from_const_alloc(
        &self,
        layout: TyLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, &'ll Value> {
        unimplemented!("from_const_alloc");
    }
}
