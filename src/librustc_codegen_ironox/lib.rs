#![feature(box_syntax)]
#![feature(cell_update)]
#![feature(crate_visibility_modifier)]
#![feature(libc)]
#![feature(in_band_lifetimes)]
#![feature(optin_builtin_traits)]
#![feature(rustc_attrs)]
#![feature(step_trait)]
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
extern crate tempfile;
extern crate rustc_demangle;
extern crate serialize as rustc_serialize; // used for newtype_index

use std::sync::{mpsc, Arc};
use rustc::hir::def_id::LOCAL_CRATE;
use rustc::dep_graph::DepGraph;
use rustc::middle::cstore::MetadataLoader;
use rustc::session::{CompileIncomplete, Session};
use rustc::session::config::{self, OutputFilenames, OutputType, PrintRequest};
use rustc::ty::{self, TyCtxt};
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use rustc_codegen_utils::target_features::{all_known_features, X86_WHITELIST};
use rustc_data_structures::sync::Lrc;
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

mod back {
    pub use rustc_codegen_utils::symbol_names;
    pub mod link;
    pub mod write;
}

mod ir {
    pub mod basic_block;
    pub mod bytes;
    pub mod constant;
    pub mod const_cstr;
    pub mod function;
    pub mod global;
    pub mod instruction;
    pub mod struct_;
    pub mod type_;
    pub mod value;
}

mod abi;
mod allocator;
mod asm;
mod base;
mod builder;
mod consts;
mod context;
mod debuginfo;
mod declare;
mod intrinsic;
mod type_of;
mod metadata;
mod mono_item;
mod x86_64 {
    pub mod asm_printer;
    pub mod fn_printer;
    pub mod gas_directive;
    pub mod instruction;
    pub mod register;
}

use context::CodegenCx;
use ir::basic_block::BasicBlock;
use ir::type_::Type;
use ir::value::Value;
use ir::function::OxFunction;

#[derive(Clone)]
pub struct IronOxCodegenBackend(());

impl Clone for TargetMachineIronOx {
    fn clone(&self) -> Self {
        panic!()
    }
}

impl ExtraBackendMethods for IronOxCodegenBackend {
    fn new_metadata(&self, tcx: TyCtxt<'_, '_, '_>, mod_name: &str) -> ModuleIronOx {
        ModuleIronOx::new()
    }

    fn write_metadata<'b, 'gcx>(
        &self,
        tcx: TyCtxt<'b, 'gcx, 'gcx>,
        _metadata: &mut ModuleIronOx
    ) -> EncodedMetadata {
        let mut emit_metadata = false;
        for ty in tcx.sess.crate_types.borrow() {
            match *ty {
                config::CrateType::Rlib => emit_metadata = true,
                config::CrateType::Dylib |
                config::CrateType::ProcMacro => unimplemented!("compressed metadata"),
                _ => {}
            }
        }
        if emit_metadata {
            tcx.encode_metadata()
        } else {
            EncodedMetadata::new()
        }
    }

    fn codegen_allocator<'b, 'gcx>(
        &self,
        tcx: TyCtxt<'b, 'gcx, 'gcx>,
        mods: &'b mut ModuleIronOx,
        kind: AllocatorKind) {
        allocator::codegen(tcx, mods, kind);
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

#[derive(Debug)]
pub struct ModuleIronOx {
    /// The functions defined in this module
    pub functions: Vec<OxFunction>,
    /// The x86-64 program which corresponds to the module. This field is only
    /// initialised after the code emitter processes the module.
    pub asm: Option<String>,
}

impl ModuleIronOx {
    /// Create an empty module.
    pub fn new() -> ModuleIronOx {
        ModuleIronOx {
            functions: Default::default(),
            asm: None,
        }
    }

    pub fn bb_label(&self, bb: BasicBlock) -> String {
        self.functions[bb.0].basic_blocks[bb.1].label.to_string()
    }

    /// Get the function at index `fn_idx`.
    pub fn get_function(&mut self, fn_idx: usize) -> &mut OxFunction {
        &mut self.functions[fn_idx]
    }

    /// Add a new function of a particular `Type`.
    pub fn add_function(
        &mut self,
        cx: &CodegenCx,
        name: &str,
        fn_type: Type) -> Value {
        let llfn = self.functions.iter().find(|x| x.name == name).map(|f| f.idx);
        if let Some(idx) = llfn {
            Value::Function(idx)
        } else {
            let idx = self.functions.len();
            self.functions.push(OxFunction::new(cx, name, idx, fn_type));
            Value::Function(idx)
        }
    }
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

    fn run_fat_lto(
        _cgcx: &CodegenContext<Self>,
        _modules: Vec<ModuleCodegen<Self::Module>>,
        _timeline: &mut Timeline
    ) -> Result<LtoModuleCodegen<Self>, FatalError> {
        unimplemented!("run_fat_lto");
    }

    fn run_thin_lto(
        _cgcx: &CodegenContext<Self>,
        _modules: Vec<(String, Self::ThinBuffer)>,
        _cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
        _timeline: &mut Timeline
    ) -> Result<(Vec<LtoModuleCodegen<Self>>, Vec<WorkProduct>), FatalError> {
        unimplemented!("run_thin_lto");
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
        diag_handler: &Handler,
        module: ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
        timeline: &mut Timeline
    ) -> Result<CompiledModule, FatalError> {
        back::write::codegen(cgcx, diag_handler, module, config, timeline)
    }

    fn prepare_thin(
        _cgcx: &CodegenContext<Self>,
        _module: ModuleCodegen<Self::Module>
    ) -> (String, Self::ThinBuffer) {
        unimplemented!("prepare_thin");
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
        ongoing_codegen: Box<dyn Any>,
        sess: &Session,
        _dep_graph: &DepGraph,
        outputs: &OutputFilenames,
    ) -> Result<(), CompileIncomplete> {
        let (codegen_results, _work_products) =
            ongoing_codegen.downcast::
                <rustc_codegen_ssa::back::write::OngoingCodegen<IronOxCodegenBackend>>()
                .expect("Expected IronOxCodegenBackend's OngoingCodegen, found Box<Any>")
                .join(sess);
        if sess.opts.debugging_opts.incremental_info {
            bug!("IronOx does not support incremental compilation");
        }

        sess.compile_status()?;
        // No need to link unless this is an executable
        if !sess.opts.output_types.keys().any(|&i| i == OutputType::Exe ||
                                                   i == OutputType::Metadata) {
            return Ok(());
        }
        back::link::link_binary(sess, &codegen_results,
                                outputs, &codegen_results.crate_name.as_str());
        Ok(())
    }
}

#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    IronOxCodegenBackend::new()
}
