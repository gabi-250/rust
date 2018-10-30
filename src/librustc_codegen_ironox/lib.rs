#![feature(box_syntax)]
#![feature(optin_builtin_traits)]
#![allow(unused_variables)]
extern crate rustc;
extern crate rustc_allocator;
extern crate rustc_mir;
extern crate rustc_target;
#[macro_use]
extern crate rustc_data_structures;
extern crate rustc_codegen_ssa;
extern crate rustc_codegen_utils;
extern crate syntax_pos;
extern crate rustc_errors as errors;
extern crate faerie;
extern crate target_lexicon;
extern crate x86asm;

mod metadata;

use faerie::{ArtifactBuilder, Decl};
use std::any::Any;
use std::sync::{mpsc, Arc};
use rustc::hir::def_id::LOCAL_CRATE;
use rustc::dep_graph::DepGraph;
use rustc::middle::cstore::MetadataLoader;
use rustc::session::{CompileIncomplete, Session};
use rustc::session::config::{OutputFilenames, OutputType, PrintRequest};
use rustc::ty::{self, TyCtxt};
use rustc_codegen_utils::codegen_backend::CodegenBackend;
use rustc_codegen_utils::target_features::{all_known_features, X86_WHITELIST};
use rustc_data_structures::sync::Lrc;
use rustc::mir::mono::Stats;
use rustc_codegen_ssa::interfaces::{ExtraBackendMethods, WriteBackendMethods,
                                    ThinBufferMethods, ModuleBufferMethods};
use rustc::middle::cstore::EncodedMetadata;
use rustc::middle::allocator::AllocatorKind;
use syntax_pos::symbol::InternedString;
use rustc_codegen_ssa::back::write::{CodegenContext, ModuleConfig, DiagnosticHandlers};
use rustc_codegen_ssa::back::lto::{SerializedModule, LtoModuleCodegen, ThinModule};
use rustc::dep_graph::WorkProduct;
use rustc::util::time_graph::Timeline;
use errors::{FatalError, Handler};
use rustc_codegen_ssa::ModuleCodegen;
use rustc_codegen_ssa::CompiledModule;
use target_lexicon::{Architecture, Vendor, OperatingSystem, Environment, BinaryFormat, Triple};
use std::fs::File;

mod back {
    pub use rustc_codegen_utils::symbol_names;
}

mod base;
mod value;
mod context;

#[derive(Clone)]
pub struct IronOxCodegenBackend(());

impl Clone for TargetMachineIronOx {
    fn clone(&self) -> Self {
        panic!()
    }
}

impl ExtraBackendMethods for IronOxCodegenBackend {
    fn thin_lto_available(&self) -> bool {
        false
    }

    fn pgo_available(&self) -> bool {
        false
    }

    fn new_metadata(&self, sess: &Session, mod_name: &str) -> ModuleIronOx {
        ModuleIronOx { bytes: vec![] }
    }

    fn write_metadata<'b, 'gcx>(
        &self,
        tcx: TyCtxt<'b, 'gcx, 'gcx>,
        metadata: &ModuleIronOx
    ) -> EncodedMetadata {
        let metadata = tcx.encode_metadata();
        metadata
    }

    fn codegen_allocator(&self, tcx: TyCtxt, mods: &ModuleIronOx, kind: AllocatorKind) {
        // XXX
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
        sess: &Session,
        find_features: bool
    ) -> Arc<dyn Fn() ->
        Result<TargetMachineIronOx, String> + Send + Sync> {
        Arc::new(move || { Ok(TargetMachineIronOx {}) })
    }

    fn target_cpu<'b>(&self, sess: &'b Session) -> &'b str {
        unimplemented!("");
    }
}

pub struct ModuleIronOx {
    bytes: Vec<u8>
}
pub struct ModuleBufferIronOx {}

impl ModuleBufferMethods for ModuleBufferIronOx {
    fn data(&self) -> &[u8] {
        unimplemented!("");
    }
}

pub struct ContextIronOx {}
pub struct TargetMachineIronOx {}
pub struct ThinDataIronOx {}
pub struct ThinBufferIronOx {}

impl ThinBufferMethods for ThinBufferIronOx {
    fn data(&self) -> &[u8] {
        unimplemented!("");
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
        unimplemented!("");
    }

    fn run_lto(
        cgcx: &CodegenContext<Self>,
        modules: Vec<ModuleCodegen<Self::Module>>,
        cached_modules: Vec<(SerializedModule<Self::ModuleBuffer>, WorkProduct)>,
        timeline: &mut Timeline
    ) -> Result<(Vec<LtoModuleCodegen<Self>>, Vec<WorkProduct>), FatalError> {
        unimplemented!("");
    }

    fn new_diagnostic_handlers<'a>(
        cgcx: &'a CodegenContext<Self>,
        handler: &'a Handler,
        llcx: &'a Self::Context
    ) -> DiagnosticHandlers<'a, Self> {
        unimplemented!("");
    }

    fn drop_diagnostic_handlers<'a>(diag : &mut DiagnosticHandlers<'a, Self>) {
        unimplemented!("");
    }

    unsafe fn optimize(
        cgcx: &CodegenContext<Self>,
        diag_handler: &Handler,
        module: &ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
        timeline: &mut Timeline
    ) -> Result<(), FatalError> {
        Ok(())
    }

    unsafe fn optimize_thin(
        cgcx: &CodegenContext<Self>,
        thin: &mut ThinModule<Self>,
        timeline: &mut Timeline
    ) -> Result<ModuleCodegen<Self::Module>, FatalError> {
        unimplemented!("");
    }

    unsafe fn codegen(
        cgcx: &CodegenContext<Self>,
        diag_handler: &Handler,
        module: ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
        timeline: &mut Timeline
    ) -> Result<CompiledModule, FatalError> {
        let object = cgcx.output_filenames
            .temp_path(OutputType::Object, Some(&module.name));
        let target = Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Unknown,
            operating_system: OperatingSystem::Unknown,
            environment: Environment::Unknown,
            binary_format: BinaryFormat::Elf,
        };
        let filename = object.to_str().unwrap().to_string();
        let file = File::create(&filename).expect("failed to create file");
        let mut obj = ArtifactBuilder::new(target).name(filename).finish();
        // Add dummy declarations
        obj.declarations([
            ("str.1",    Decl::CString { global: false }),
            ("deadbeef", Decl::Function { global: true }),
        ].into_iter().cloned()).map_err(
            |e| diag_handler.fatal(&format!("{:?}", e)))?;
        obj.define("str.1", b"hello".to_vec()).map_err(
            |e| diag_handler.fatal(&format!("failed to define string: {:?}", e)))?;
        obj.define("deadbeef", module.module_llvm.bytes).map_err(
            |e| diag_handler.fatal(&format!("failed to define string: {:?}", e)))?;
        obj.write(file).map_err(|_| diag_handler.fatal("failed to write file"))?;
        eprintln!("Compiling {:?}", object);
        Ok(CompiledModule {
            name: module.name.clone(),
            kind: module.kind,
            object: Some(object),
            bytecode: None,
            bytecode_compressed: None,
        })
    }

    fn run_lto_pass_manager(
        cgcx: &CodegenContext<Self>,
        module: &ModuleCodegen<Self::Module>,
        config: &ModuleConfig,
        thin: bool
    ) {
        unimplemented!("");
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

impl<'a> CodegenBackend for IronOxCodegenBackend {
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
        rustc_codegen_ssa::base::provide(providers);
    }

    fn provide_extern(&self, providers: &mut ty::query::Providers) {
        rustc_codegen_ssa::back::symbol_export::provide_extern(providers);
        rustc_codegen_ssa::base::provide_extern(providers);
    }

    fn codegen_crate<'b, 'tcx>(
        &self,
        tcx: TyCtxt<'b, 'tcx, 'tcx>,
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
        //let _ongoing_codegen = ongoing_codegen.downcast::<::base::OngoingCodegen>()
            //.expect("Expected the iron-ox OngoingCodegen!");
        Ok(())
    }
}

#[no_mangle]
pub fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    IronOxCodegenBackend::new()
}
