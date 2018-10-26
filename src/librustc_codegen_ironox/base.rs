use rustc::ty::TyCtxt;
use syntax_pos::symbol::InternedString;
use rustc::mir::mono::Stats;
//use rustc_mir::monomorphize::MonoItem;
use rustc_mir::monomorphize::partitioning::CodegenUnitExt;
use std::cell::RefCell;
use context::CodegenCx;
use value::Value;
use super::{IronOxCodegenBackend, ModuleIronOx};
use rustc_codegen_ssa::back::write::submit_codegened_module_to_llvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};

pub fn compile_codegen_unit<'ll, 'tcx>(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
                                  cgu_name: InternedString)
                                  -> Stats {
    let cgu = tcx.codegen_unit(cgu_name);
    let _dep_node = cgu.codegen_dep_node(tcx);
    let module = ModuleCodegen {
        name: cgu_name.to_string(),
        module_llvm: ModuleIronOx {},
        kind: ModuleKind::Regular,
    };
    submit_codegened_module_to_llvm(&IronOxCodegenBackend(()), tcx,
                                    module, 0 as u64);

    //for (&mono_item, &linkage) in cgu.items() {
        //eprintln!("Mono item {:?}", mono_item);
        //match mono_item {
            //MonoItem::Fn(inst) => {
                //let def_id = inst.def_id();
                //let mir = tcx.instance_mir(inst.def);
                //for bb in mir.basic_blocks() {
                    //eprintln!("Statements {:?}", bb.statements);
                //}
            //},
            //_ => {
                //eprintln!("Other mono");
            //}
        //}
    //}

    let cx = CodegenCx {
        tcx,
        stats: RefCell::new(Stats::default()),
        codegen_unit: cgu,
        value: Value {}
    };
    Default::default()
}
