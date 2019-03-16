use super::{IronOxCodegenBackend, ModuleIronOx};
use rustc::mir::mono::Stats;
use rustc::ty::TyCtxt;
use rustc_codegen_ssa::base::maybe_create_entry_wrapper;
use rustc_codegen_ssa::back::write::submit_codegened_module_to_llvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mono_item::MonoItemExt;
use rustc_mir::monomorphize::partitioning::CodegenUnitExt;
use syntax_pos::symbol::InternedString;

use builder::Builder;
use x86_64::asm_printer::AsmPrinter;
use context::CodegenCx;

pub fn compile_codegen_unit<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cgu_name: InternedString,
) -> Stats {
    let dep_node = tcx.codegen_unit(cgu_name).codegen_dep_node(tcx);
    let ((stats, module), _) = tcx.dep_graph.with_task(dep_node,
                                                       tcx,
                                                       cgu_name,
                                                       codegen_ironox_module);
    let cost = 0; // FIXME may need to compute the 'cost'
    submit_codegened_module_to_llvm(&IronOxCodegenBackend(()), tcx, module, cost);
    return stats;
}

fn codegen_ironox_module<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cgu_name: InternedString,
) -> (Stats, ModuleCodegen<ModuleIronOx>) {
    let backend = IronOxCodegenBackend(());
    let cgu = tcx.codegen_unit(cgu_name);
    let mut ironox_module = backend.new_metadata(tcx.sess, &cgu_name.as_str());
    let (stats, asm) = {
        let cx = CodegenCx::new(tcx, Some(cgu), &mut ironox_module);
        let mono_items = cx.codegen_unit()
                           .items_in_deterministic_order(cx.tcx);
        for &(mono_item, (linkage, visibility)) in &mono_items {
            mono_item.predefine::<Builder>(&cx, linkage, visibility);
        }
        for &(mono_item, _) in &mono_items {
            mono_item.define::<Builder>(&cx);
        }
        maybe_create_entry_wrapper::<Builder>(&cx);
        AsmPrinter::new(cx).codegen()
    };
    ironox_module.asm = Some(asm);
    (stats, ModuleCodegen {
        name: cgu_name.to_string(),
        module_llvm: ironox_module,
        kind: ModuleKind::Regular,
    })
}
