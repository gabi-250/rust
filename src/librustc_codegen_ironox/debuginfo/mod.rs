use context::CodegenCx;
use builder::Builder;
use value::Value;

use rustc::hir::def_id::CrateNum;
use rustc::mir;
use rustc::ty::{self, Ty};
use rustc_codegen_ssa::debuginfo::{FunctionDebugContext, VariableAccess, MirDebugScope,
    VariableKind};
use rustc_codegen_ssa::interfaces::{DebugInfoMethods, DebugInfoBuilderMethods};
use rustc_data_structures::indexed_vec::IndexVec;
use rustc_mir::monomorphize::Instance;
use syntax_pos;
use syntax::ast;

pub struct DIScope {}

impl<'ll, 'tcx: 'll> DebugInfoMethods<'ll, 'tcx> for CodegenCx<'ll, 'tcx, &'ll Value> {
    type DIScope = &'ll DIScope;

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
        llfn: Self::Value,
        mir: &mir::Mir,
    ) -> FunctionDebugContext<Self::DIScope> {
        return FunctionDebugContext::DebugInfoDisabled;
    }

    fn create_mir_scopes(
        &self,
        mir: &mir::Mir,
        debug_context: &FunctionDebugContext<Self::DIScope>,
    ) -> IndexVec<mir::SourceScope, MirDebugScope<Self::DIScope>> {
        IndexVec::new()
    }

    fn extend_scope_to_file(
        &self,
        scope_metadata: Self::DIScope,
        file: &syntax_pos::SourceFile,
        defining_crate: CrateNum,
    ) -> Self::DIScope {
        unimplemented!("extend_scope_to_file");
    }

    fn debuginfo_finalize(&self) {
        // do nothing
    }

    fn debuginfo_upvar_decls_ops_sequence(&self, byte_offset_of_var_in_env: u64) -> [i64; 4] {
        unimplemented!("debuginfo_upvar_decls_ops_sequence");
    }
}

impl<'a, 'll: 'a, 'tcx: 'll> DebugInfoBuilderMethods<'a, 'll, 'tcx>
    for Builder<'a, 'll, 'tcx, &'ll Value>
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
