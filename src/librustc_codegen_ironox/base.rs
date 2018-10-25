use rustc::ty::TyCtxt;

use std::any::Any;
use std::sync::mpsc;


use rustc::hir::def_id::LOCAL_CRATE;
use rustc::ty::query::Providers;

use rustc_codegen_utils::base;

use rustc_mir::monomorphize::item::MonoItem;
use syntax_pos::symbol::InternedString;
use rustc_mir::monomorphize::partitioning::CodegenUnitExt;

#[allow(dead_code)]
pub struct OngoingCodegen {
    pub crate_name: String,
    //crate_name: Symbol,
    //crate_hash: Svh,
    //metadata: EncodedMetadata,
    //output_filenames: Arc<OutputFilenames>,
}

fn compile_codegen_unit<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>,
                                  cgu_name: InternedString) {
    let cgu = tcx.codegen_unit(cgu_name);
    let _dep_node = cgu.codegen_dep_node(tcx);

    for (&mono_item, &linkage) in cgu.items() {
        eprintln!("Mono item: {:?}", mono_item);
        eprintln!("{:?}", linkage);
        match mono_item {
            MonoItem::Fn(inst) => {
                eprintln!("Function!");
                let def_id = inst.def_id();
                eprintln!("Def ID: {:?}", def_id);
                let mir = tcx.instance_mir(inst.def);
                for bb in mir.basic_blocks() {
                    eprintln!("Statements: {:?}", bb.statements);
                }
                eprintln!("----------------------");
            }
            _ => {
                eprintln!("Other mono item");
                eprintln!("----------------------");
            }
        }
    }
}

pub fn codegen_crate<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>,
                               _rx: mpsc::Receiver<Box<dyn Any + Send>>)
                               -> OngoingCodegen {
    let codegen_units =
        tcx.collect_and_partition_mono_items(LOCAL_CRATE).1;

    let codegen_units = (*codegen_units).clone();
    for cgu in codegen_units.into_iter() {
        compile_codegen_unit(tcx, *cgu.name());
    }
    return OngoingCodegen {
        crate_name: "dummy-crate".to_string()
    };
}

pub fn provide(providers: &mut Providers) {
    base::provide(providers);
}

pub fn provide_extern(providers: &mut Providers) {
    base::provide_extern(providers);
}

