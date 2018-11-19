use rustc::middle::cstore::EncodedMetadata;
use rustc::session::config::OutputFilenames;
use rustc::ty::TyCtxt;
use rustc_data_structures::svh::Svh;
use syntax_pos::symbol::Symbol;

use std::any::Any;
use std::sync::Arc;
use std::sync::mpsc;

#[allow(dead_code)]
pub struct OngoingCodegen {
    crate_name: Symbol,
    crate_hash: Svh,
    metadata: EncodedMetadata,
    output_filenames: Arc<OutputFilenames>,
}

pub fn codegen_crate<'a, 'tcx>(_tcx: TyCtxt<'a, 'tcx, 'tcx>,
                               _rx: mpsc::Receiver<Box<dyn Any + Send>>)
                               -> OngoingCodegen {
    unimplemented!("base::codegen_crate");
}
