use rustc::ty::TyCtxt;
use syntax_pos::symbol::InternedString;
use rustc::mir::mono::Stats;

pub fn compile_codegen_unit<'ll, 'tcx>(_tcx: TyCtxt<'ll, 'tcx, 'tcx>,
                                  _cgu_name: InternedString)
                                  -> Stats {
    unimplemented!("compile_codegen_unit");
}
