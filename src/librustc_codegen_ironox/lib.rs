#![feature(box_syntax)]
#![feature(optin_builtin_traits)]

extern crate rustc;
extern crate rustc_mir;
extern crate rustc_target;
#[macro_use]
extern crate rustc_data_structures;
extern crate rustc_codegen_utils;
extern crate syntax_pos;

mod metadata;

use std::any::Any;
use std::sync::mpsc;

use rustc::hir::def_id::LOCAL_CRATE;
use rustc::dep_graph::DepGraph;
use rustc::middle::cstore::MetadataLoader;
use rustc::session::{CompileIncomplete, Session};
use rustc::session::config::{OutputFilenames, PrintRequest};
use rustc::ty::{self, TyCtxt};
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use rustc_codegen_utils::target_features::{all_known_features, X86_WHITELIST};
use rustc_data_structures::sync::Lrc;
use rustc_mir::monomorphize::collector;
use rustc_mir::monomorphize::item::MonoItem;

mod back {
    pub use rustc_codegen_utils::symbol_export;
    pub use rustc_codegen_utils::symbol_names;
}

mod base;

pub struct IronOxCodegenBackend(());

impl !Send for IronOxCodegenBackend {}
impl !Sync for IronOxCodegenBackend {}

pub fn target_feature_whitelist(sess: &Session)
    -> &'static [(&'static str, Option<&'static str>)] {
    match &*sess.target.target.arch {
        "x86" | "x86_64" => X86_WHITELIST,
        _ => &[],
    }
}

impl IronOxCodegenBackend {
    pub fn new() -> Box<dyn CodegenBackend> {
        box IronOxCodegenBackend(())
    }
}

impl CodegenBackend for IronOxCodegenBackend {
    fn init(&self, _sess: &Session) {}

    fn print(&self, _req: PrintRequest, _sess: &Session) {}

    fn print_passes(&self) {}

    fn print_version(&self) {}

    fn metadata_loader(&self) -> Box<dyn MetadataLoader + Sync> {
        box metadata::IronOxMetadataLoader
    }

    fn provide(&self, providers: &mut ty::query::Providers) {
        providers.target_features_whitelist = |tcx, cnum| {
            assert_eq!(cnum, LOCAL_CRATE);
            if tcx.sess.opts.actually_rustdoc {
                Lrc::new(
                    all_known_features()
                        .map(|(a, b)| (a.to_string(), b.map(|s| s.to_string())))
                        .collect(),
                )
            } else {
                Lrc::new(
                    target_feature_whitelist(tcx.sess)
                        .iter()
                        .map(|&(a, b)| (a.to_string(), b.map(|s| s.to_string())))
                        .collect(),
                )
            }
        };
        back::symbol_names::provide(providers);
        back::symbol_export::provide(providers);
    }

    fn provide_extern(&self, providers: &mut ty::query::Providers) {
        back::symbol_export::provide_extern(providers);
    }

    fn codegen_crate<'a, 'tcx>(
        &self,
        tcx: TyCtxt<'a, 'tcx, 'tcx>,
        rx: mpsc::Receiver<Box<dyn Any + Send>>,
    ) -> Box<dyn Any> {
        // Print the MIR
        for mono_item in collector::collect_crate_mono_items(
            tcx, collector::MonoItemCollectionMode::Eager).0 {
            match mono_item {
                MonoItem::Fn(inst) => {
                    let def_id = inst.def_id();
                    eprintln!("Def ID: {:?}", def_id);
                    let mir = tcx.instance_mir(inst.def);
                    for bb in mir.basic_blocks() {
                        eprintln!("Statements: {:?}", bb.statements);
                    }
                }
                _ => {}
            }
        }
        box base::codegen_crate(tcx, rx)
    }

    fn join_codegen_and_link(
        &self,
        ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
        _dep_graph: &DepGraph,
        _outputs: &OutputFilenames,
    ) -> Result<(), CompileIncomplete> {
        let _ongoing_codegen = ongoing_codegen.downcast::<::base::OngoingCodegen>()
            .expect("Expected the iron-ox OngoingCodegen!");
        unimplemented!("join_codegen_and_link");
    }
}

#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    IronOxCodegenBackend::new()
}
