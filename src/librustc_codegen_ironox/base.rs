use super::{IronOxCodegenBackend, ModuleIronOx};
use rustc::mir::mono::Stats;
use rustc::ty::TyCtxt;
use rustc_codegen_ssa::back::write::submit_codegened_module_to_llvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};
use rustc_mir::monomorphize::MonoItem;
use syntax_pos::symbol::InternedString;

use ::back::write;

pub fn compile_codegen_unit<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cgu_name: InternedString,
) -> Stats {
    let cgu = tcx.codegen_unit(cgu_name);
    let mut codegened_module = ModuleIronOx {
        asm: "".to_string(),
    };
    for (&mono_item, &_linkage) in cgu.items() {
        match mono_item {
            MonoItem::Fn(inst) => {
                eprintln!("Processing function {:?}", tcx.symbol_name(inst).as_str());
                // create the function
                //eprintln!("Arg count is {:?}", mir.arg_count);
                write::emit_function(tcx, &mut codegened_module, inst);
            },
            _ => {
                // don't care
                eprintln!("Other mono item");
            }
        }
    }

    let module = ModuleCodegen {
        name: cgu_name.to_string(),
        module_llvm: codegened_module,
        kind: ModuleKind::Regular,
    };
    submit_codegened_module_to_llvm(&IronOxCodegenBackend(()), tcx, module, 0 as u64);
    Default::default()
}
