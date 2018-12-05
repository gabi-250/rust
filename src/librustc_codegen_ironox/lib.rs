// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(box_syntax)]
#![feature(libc)]
#![feature(in_band_lifetimes)]
#![feature(optin_builtin_traits)]
#![allow(unused)]

extern crate rustc_errors;
#[macro_use] extern crate rustc;
extern crate rustc_allocator;
extern crate rustc_mir;
extern crate rustc_target;
#[macro_use]
extern crate rustc_data_structures;
extern crate rustc_codegen_ssa;
extern crate rustc_codegen_utils;
extern crate syntax_pos;
extern crate rustc_errors as errors;
extern crate libc;
extern crate syntax;


use std::sync::{mpsc, Arc};
use rustc::hir::def_id::LOCAL_CRATE;
use rustc::dep_graph::DepGraph;
use rustc::middle::cstore::MetadataLoader;
use rustc::session::{CompileIncomplete, Session};
use rustc::session::config::{OutputFilenames, OutputType, PrintRequest};
use rustc::ty::{self, TyCtxt};
use rustc_allocator::{ALLOCATOR_METHODS, AllocatorTy};
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use rustc_codegen_utils::target_features::{all_known_features, X86_WHITELIST};
use rustc_data_structures::sync::Lrc;
use rustc_mir::monomorphize::collector;
use rustc_mir::monomorphize::item::MonoItem;
use rustc_codegen_ssa::CompiledModule;
use rustc_codegen_ssa::ModuleCodegen;
use rustc_errors::{FatalError, Handler};
use rustc::mir::mono::Stats;
use rustc_codegen_ssa::traits::{ExtraBackendMethods, WriteBackendMethods,
                                ThinBufferMethods, ModuleBufferMethods};
use rustc::middle::cstore::EncodedMetadata;
use rustc::middle::allocator::AllocatorKind;
use syntax_pos::symbol::InternedString;
use rustc_codegen_ssa::back::write::{CodegenContext, ModuleConfig};
use rustc_codegen_ssa::back::lto::{SerializedModule, LtoModuleCodegen, ThinModule};
use rustc::dep_graph::WorkProduct;
use rustc::util::time_graph::Timeline;
use std::any::Any;

use std::process::{Command, Stdio};
use std::io::Write;

mod back {
    pub use rustc_codegen_utils::symbol_names;
    pub mod write;
}

mod abi;
mod asm;
mod base;
mod basic_block;
mod builder;
mod consts;
mod context;
mod debuginfo;
mod declare;
mod intrinsic;
mod ironox_type;
mod metadata;
mod mono_item;
mod value;

#[derive(Clone)]
pub struct IronOxCodegenBackend(());

impl Clone for TargetMachineIronOx {
    fn clone(&self) -> Self {
        panic!()
    }
}

impl ExtraBackendMethods for IronOxCodegenBackend {
    fn new_metadata(&self, _sess: &Session, _mod_name: &str) -> ModuleIronOx {
        ModuleIronOx { asm: "".to_string() }
    }

    fn write_metadata<'b, 'gcx>(
        &self,
        tcx: TyCtxt<'b, 'gcx, 'gcx>,
        _metadata: &ModuleIronOx
    ) -> EncodedMetadata {
        let metadata = tcx.encode_metadata();
        metadata
    }

    fn codegen_allocator(&self, _tcx: TyCtxt, _mods: &ModuleIronOx, _kind: AllocatorKind) {
        // FIXME
    }

    fn compile_codegen_unit<'ll, 'tcx: 'll>(
        &self,
        tcx: TyCtxt<'ll, 'tcx, 'tcx>,
        cgu_name: InternedString
    ) -> Stats {
        base::compile_codegen_unit(tcx, cgu_name)
    }

    fn target_machine_factory(
        &self,
        _sess: &Session,
        _find_features: bool,
    ) -> Arc<dyn Fn() -> Result<TargetMachineIronOx, String> + Send + Sync> {
        Arc::new(move || Ok(TargetMachineIronOx {}))
    }

    fn target_cpu<'b>(&self, _sess: &'b Session) -> &'b str {
        unimplemented!("target_cpu");
    }
}

pub struct ModuleIronOx {
    asm: String
}

pub struct ModuleBufferIronOx {}

impl ModuleBufferMethods for ModuleBufferIronOx {
    fn data(&self) -> &[u8] {
        unimplemented!("data");
    }
}

pub struct ContextIronOx {}
pub struct TargetMachineIronOx {}
pub struct ThinDataIronOx {}
pub struct ThinBufferIronOx {}

impl ThinBufferMethods for ThinBufferIronOx {
    fn data(&self) -> &[u8] {
        unimplemented!("data");
    }
}

impl WriteBackendMethods for IronOxCodegenBackend {
    type Module = ModuleIronOx;
    type ModuleBuffer = ModuleBufferIronOx;
    type Context = ContextIronOx;
    type TargetMachine = TargetMachineIronOx;
    type ThinData = ThinDataIronOx;
    type ThinBuffer = ThinBufferIronOx;

    fn print_pass_timings(&self) {
        unimplemented!("print_pass_timings");
    }

    fn run_lto(
        _cgcx: &CodegenContext<Self>,
        _modules: Vec<ModuleCodegen<Self::Module>>,
        _cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
        _timeline: &mut Timeline
    ) -> Result<(Vec<LtoModuleCodegen<Self>>, Vec<WorkProduct>), FatalError> {
        unimplemented!("run_lto");
    }

    unsafe fn optimize(
        _cgcx: &CodegenContext<Self>,
        _diag_handler: &Handler,
        _module: &ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
        _timeline: &mut Timeline
    ) -> Result<(), FatalError> {
        Ok(())
    }

    unsafe fn optimize_thin(
        _cgcx: &CodegenContext<Self>,
        _thin: &mut ThinModule<Self>,
        _timeline: &mut Timeline
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        unimplemented!("optimize_thin");
    }

    unsafe fn codegen(
        cgcx: &CodegenContext<Self>,
        _diag_handler: &Handler,
        module: ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
        _timeline: &mut Timeline
    ) -> Result<CompiledModule, FatalError> {
        // FIXME fix this
        if true || config.no_integrated_as {
            let object = cgcx.output_filenames
                .temp_path(OutputType::Object, Some(&module.name));
            let filename = object.to_str().unwrap().to_string();
            let mut cmd = Command::new("as").arg("-o").arg(filename)
                .stdin(Stdio::piped()).spawn().expect("failed to run as");
            {
                let stdin = cmd.stdin.as_mut().expect("failed to open stdin");
                stdin.write_all(module.module_llvm.asm.as_bytes())
                    .expect("failed to write to stdin");
            }
            Ok(CompiledModule {
                name: module.name.clone(),
                kind: module.kind,
                object: Some(object),
                bytecode: None,
                bytecode_compressed: None,
            })
        } else {
            unimplemented!("ironox does not have an integrated assembler!");
        }
    }

    fn run_lto_pass_manager(
        _cgcx: &CodegenContext<Self>,
        _module: &ModuleCodegen<Self::Module>,
        _config: &ModuleConfig,
        _thin: bool
    ) {
        unimplemented!("run_lto_pass_manager");
    }
}

unsafe impl<'a> Send for IronOxCodegenBackend {}
unsafe impl<'a> Sync for IronOxCodegenBackend {}

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
        rustc_codegen_ssa::back::symbol_export::provide(providers);
        rustc_codegen_ssa::base::provide_both(providers);
    }

    fn provide_extern(&self, providers: &mut ty::query::Providers) {
        rustc_codegen_ssa::back::symbol_export::provide_extern(providers);
    }

    fn codegen_crate<'a, 'tcx>(
        &self,
        tcx: TyCtxt<'a, 'tcx, 'tcx>,
        rx: mpsc::Receiver<Box<dyn Any + Send>>,
    ) -> Box<dyn Any> {
        box rustc_codegen_ssa::base::codegen_crate(IronOxCodegenBackend(()), tcx, rx)
    }

    fn join_codegen_and_link(
        &self,
        _ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
        _dep_graph: &DepGraph,
        _outputs: &OutputFilenames,
    ) -> Result<(), CompileIncomplete> {
        Ok(())
    }
}

#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    IronOxCodegenBackend::new()
}
