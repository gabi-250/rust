use super::{IronOxCodegenBackend, ModuleIronOx};
use rustc::mir::mono::Stats;
use rustc::ty::TyCtxt;
use rustc_codegen_ssa::back::write::submit_codegened_module_to_llvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};
use syntax_pos::symbol::InternedString;

pub fn compile_codegen_unit<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cgu_name: InternedString,
) -> Stats {
    let cgu = tcx.codegen_unit(cgu_name);

    for (&mono_item, &_linkage) in cgu.items() {
        eprintln!("Mono item is {:?}", mono_item);
    }

    let codegened_module = ModuleIronOx {
        asm: "mov $5, %rax".to_string(),
    };
    let module = ModuleCodegen {
        name: cgu_name.to_string(),
        module_llvm: codegened_module,
        kind: ModuleKind::Regular,
    };
    submit_codegened_module_to_llvm(&IronOxCodegenBackend(()), tcx, module, 0 as u64);
    Default::default()
}
