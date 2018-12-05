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
use builder::Builder;
use value::Value;

use rustc::hir::def_id::CrateNum;
use rustc::mir;
use rustc::ty::{self, Ty};
use rustc_codegen_ssa::debuginfo::{FunctionDebugContext, VariableAccess, MirDebugScope,
    VariableKind};
use rustc_codegen_ssa::traits::{DebugInfoMethods, DebugInfoBuilderMethods};
use rustc_data_structures::indexed_vec::IndexVec;
use rustc_mir::monomorphize::Instance;
use syntax_pos;
use syntax::ast;

pub struct DIScope {}

impl DebugInfoMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn create_vtable_metadata(
        &self,
        ty: Ty<'tcx>,
        vtable: Self::Value,
    ) {
        unimplemented!("create_vtable_metadata");
    }

    fn create_function_debug_context(
        &self,
        instance: Instance<'tcx>,
        sig: ty::FnSig<'tcx>,
        llfn: &'ll Value,
        mir: &mir::Mir,
    ) -> FunctionDebugContext<&'ll DIScope> {
        return FunctionDebugContext::DebugInfoDisabled;
    }

    fn create_mir_scopes(
        &self,
        mir: &mir::Mir,
        debug_context: &FunctionDebugContext<&'ll DIScope>,
    ) -> IndexVec<mir::SourceScope, MirDebugScope<&'ll DIScope>> {
        IndexVec::new()
    }

    fn extend_scope_to_file(
        &self,
        scope_metadata: &'ll DIScope,
        file: &syntax_pos::SourceFile,
        defining_crate: CrateNum,
    ) -> &'ll DIScope {
        unimplemented!("extend_scope_to_file");
    }

    fn debuginfo_finalize(&self) {
        // do nothing
    }

    fn debuginfo_upvar_decls_ops_sequence(&self, byte_offset_of_var_in_env: u64) -> [i64; 4] {
        unimplemented!("debuginfo_upvar_decls_ops_sequence");
    }
}

impl<'a, 'll: 'a, 'tcx: 'll> DebugInfoBuilderMethods<'tcx>
    for Builder<'a, 'll, 'tcx>
{
    fn declare_local(
        &mut self,
        dbg_context: &FunctionDebugContext<&'ll DIScope>,
        variable_name: ast::Name,
        variable_type: Ty<'tcx>,
        scope_metadata: &'ll DIScope,
        variable_access: VariableAccess<'_, &'ll Value>,
        variable_kind: VariableKind,
        span: syntax_pos::Span,
    ) {
        unimplemented!("declare_local");
    }

    fn set_source_location(
        &mut self,
        debug_context: &FunctionDebugContext<&'ll DIScope>,
        scope: Option<&'ll DIScope>,
        span: syntax_pos::Span,
    ) {
        unimplemented!("set_source_location");
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        unimplemented!("insert_reference_to_gdb_debug_scripts_section_global");
    }
}
