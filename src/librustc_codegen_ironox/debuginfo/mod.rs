use context::CodegenCx;
use builder::Builder;
use ir::value::Value;

use rustc::hir::def_id::CrateNum;
use rustc::mir;
use rustc::session::config::DebugInfo;
use rustc::ty::{self, Ty};
use rustc_codegen_ssa::debuginfo::{FunctionDebugContext, VariableAccess, MirDebugScope,
    VariableKind};
use rustc_codegen_ssa::traits::{DebugInfoMethods, DebugInfoBuilderMethods};
use rustc_data_structures::indexed_vec::IndexVec;
use rustc_mir::monomorphize::Instance;
use syntax_pos::{self, BytePos};
use syntax::ast;

pub struct DIScope {}

impl DebugInfoMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn create_vtable_metadata(
        &self,
        _ty: Ty<'tcx>,
        _vtable: Self::Value,
    ) {
        // FIXME do nothing for now
    }

    fn create_function_debug_context(
        &self,
        _instance: Instance<'tcx>,
        _sig: ty::FnSig<'tcx>,
        _llfn: Value,
        _mir: &mir::Mir,
    ) -> FunctionDebugContext<Self::DIScope> {
        if self.tcx.sess.opts.debuginfo == DebugInfo::None {
            return FunctionDebugContext::DebugInfoDisabled;
        } else {
            unimplemented!("create_function_debug_context");
        }
    }

    fn create_mir_scopes(
        &self,
        mir: &mir::Mir,
        debug_context: &FunctionDebugContext<&'ll DIScope>,
    ) -> IndexVec<mir::SourceScope, MirDebugScope<&'ll DIScope>> {
        let null_scope = MirDebugScope {
            scope_metadata: None,
            file_start_pos: BytePos(0),
            file_end_pos: BytePos(0)
        };
        let scopes = IndexVec::from_elem(null_scope, &mir.source_scopes);
        if let FunctionDebugContext::DebugInfoDisabled = *debug_context {
            return scopes;
        } else {
            unimplemented!("create_mir_scopes");
        }
    }

    fn extend_scope_to_file(
        &self,
        _scope_metadata: &'ll DIScope,
        _file: &syntax_pos::SourceFile,
        _defining_crate: CrateNum,
    ) -> &'ll DIScope {
        unimplemented!("extend_scope_to_file");
    }

    fn debuginfo_finalize(&self) {
        // do nothing
    }

    fn debuginfo_upvar_decls_ops_sequence(
        &self,
        _byte_offset_of_var_in_env: u64
    ) -> [i64; 4] {
        unimplemented!("debuginfo_upvar_decls_ops_sequence");
    }
}

impl<'a, 'll: 'a, 'tcx: 'll> DebugInfoBuilderMethods<'tcx>
    for Builder<'a, 'll, 'tcx>
{
    fn declare_local(
        &mut self,
        _dbg_context: &FunctionDebugContext<&'ll DIScope>,
        _variable_name: ast::Name,
        _variable_type: Ty<'tcx>,
        _scope_metadata: &'ll DIScope,
        _variable_access: VariableAccess<'_, Value>,
        _variable_kind: VariableKind,
        _span: syntax_pos::Span,
    ) {
        unimplemented!("declare_local");
    }

    fn set_source_location(
        &mut self,
        _debug_context: &FunctionDebugContext<&'ll DIScope>,
        _scope: Option<&'ll DIScope>,
        _span: syntax_pos::Span,
    ) {
        // do nothing
    }

    fn insert_reference_to_gdb_debug_scripts_section_global(&mut self) {
        // do nothing
    }
}
