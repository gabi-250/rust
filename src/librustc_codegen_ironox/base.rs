use rustc::ty::TyCtxt;

use std::any::Any;
use std::sync::mpsc;


use rustc::hir::def_id::LOCAL_CRATE;
use rustc::ty::query::Providers;

use rustc_codegen_utils::base;

#[allow(dead_code)]
pub struct OngoingCodegen {
    pub crate_name: String,
}

fn compile_codegen_unit<'a, 'tcx>(_tcx: TyCtxt<'a, 'tcx, 'tcx>,
                                  cgu_name: InternedString) {
    eprintln!("{:?}", cgu_name);
}

pub fn codegen_crate<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>,
                               _rx: mpsc::Receiver<Box<dyn Any + Send>>)
                               -> OngoingCodegen {


    let codegen_units =
        tcx.collect_and_partition_mono_items(LOCAL_CRATE).1;


    let codegen_units = (*codegen_units).clone();
    for cgu in codegen_units.into_iter() {

        compile_codegen_unit(tcx, *cgu.name());
        for (&mono_item, &linkage) in cgu.items() {
            eprintln!("{:?}", mono_item);
            eprintln!("{:?}", linkage);
        }
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

