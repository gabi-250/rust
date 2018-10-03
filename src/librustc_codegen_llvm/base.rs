// Copyright 2012-2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Codegen the completed AST to the LLVM IR.
//!
//! Some functions here, such as codegen_block and codegen_expr, return a value --
//! the result of the codegen to LLVM -- while others, such as codegen_fn
//! and mono_item, are called only for the side effect of adding a
//! particular definition to the LLVM IR output we're producing.
//!
//! Hopefully useful general knowledge about codegen:
//!
//!   * There's no way to find out the Ty type of a Value.  Doing so
//!     would be "trying to get the eggs out of an omelette" (credit:
//!     pcwalton).  You can, instead, find out its llvm::Type by calling val_ty,
//!     but one llvm::Type corresponds to many `Ty`s; for instance, tup(int, int,
//!     int) and rec(x=int, y=int, z=int) will have the same llvm::Type.

use super::ModuleLlvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};
use rustc_codegen_ssa::base::maybe_create_entry_wrapper;
use super::LlvmCodegenBackend;

use back::write;
use llvm;
use metadata;
use rustc::mir::mono::{Linkage, Visibility, Stats};
use rustc::middle::cstore::{EncodedMetadata};
use rustc::ty::TyCtxt;
use rustc::middle::exported_symbols;
use rustc::session::config::{self, DebugInfo};
use builder::Builder;
use common;
use context::CodegenCx;
use monomorphize::Instance;
use monomorphize::partitioning::{CodegenUnit, CodegenUnitExt};
use rustc_codegen_utils::symbol_names_test;
use time_graph;
use mono_item::{MonoItem, MonoItemExt};
use type_of::LayoutLlvmExt;
use rustc::util::nodemap::FxHashMap;
use CrateInfo;
use rustc_data_structures::small_c_str::SmallCStr;

use rustc_codegen_ssa::interfaces::*;

use std::any::Any;
use std::cmp;
use std::ffi::CString;
use std::i32;
use std::ops::{Deref, DerefMut};
use std::sync::mpsc;
use std::time::{Instant, Duration};
use syntax_pos::Span;
use syntax_pos::symbol::InternedString;
use rustc::hir::CodegenFnAttrs;

use value::Value;



pub(crate) fn write_metadata<'a, 'gcx>(
    tcx: TyCtxt<'a, 'gcx, 'gcx>,
    llvm_module: &ModuleLlvm
) -> EncodedMetadata {
    use std::io::Write;
    use flate2::Compression;
    use flate2::write::DeflateEncoder;

    let (metadata_llcx, metadata_llmod) = (&*llvm_module.llcx, llvm_module.llmod());

    #[derive(PartialEq, Eq, PartialOrd, Ord)]
    enum MetadataKind {
        None,
        Uncompressed,
        Compressed
    }

    let kind = tcx.sess.crate_types.borrow().iter().map(|ty| {
        match *ty {
            config::CrateType::Executable |
            config::CrateType::Staticlib |
            config::CrateType::Cdylib => MetadataKind::None,

            config::CrateType::Rlib => MetadataKind::Uncompressed,

            config::CrateType::Dylib |
            config::CrateType::ProcMacro => MetadataKind::Compressed,
        }
    }).max().unwrap_or(MetadataKind::None);

    if kind == MetadataKind::None {
        return EncodedMetadata::new();
    }

    let metadata = tcx.encode_metadata();
    if kind == MetadataKind::Uncompressed {
        return metadata;
    }

    assert!(kind == MetadataKind::Compressed);
    let mut compressed = tcx.metadata_encoding_version();
    DeflateEncoder::new(&mut compressed, Compression::fast())
        .write_all(&metadata.raw_data).unwrap();

    let llmeta = common::bytes_in_context(metadata_llcx, &compressed);
    let llconst = common::struct_in_context(metadata_llcx, &[llmeta], false);
    let name = exported_symbols::metadata_symbol_name(tcx);
    let buf = CString::new(name).unwrap();
    let llglobal = unsafe {
        llvm::LLVMAddGlobal(metadata_llmod, common::val_ty(llconst), buf.as_ptr())
    };
    unsafe {
        llvm::LLVMSetInitializer(llglobal, llconst);
        let section_name = metadata::metadata_section_name(&tcx.sess.target.target);
        let name = SmallCStr::new(section_name);
        llvm::LLVMSetSection(llglobal, name.as_ptr());

        // Also generate a .section directive to force no
        // flags, at least for ELF outputs, so that the
        // metadata doesn't get loaded into memory.
        let directive = format!(".section {}", section_name);
        let directive = CString::new(directive).unwrap();
        llvm::LLVMSetModuleInlineAsm(metadata_llmod, directive.as_ptr())
    }
    return metadata;
}

pub struct ValueIter<'ll> {
    cur: Option<&'ll Value>,
    step: unsafe extern "C" fn(&'ll Value) -> Option<&'ll Value>,
}

impl Iterator for ValueIter<'ll> {
    type Item = &'ll Value;

    fn next(&mut self) -> Option<&'ll Value> {
        let old = self.cur;
        if let Some(old) = old {
            self.cur = unsafe { (self.step)(old) };
        }
        old
    }
}

pub fn iter_globals(llmod: &'ll llvm::Module) -> ValueIter<'ll> {
    unsafe {
        ValueIter {
            cur: llvm::LLVMGetFirstGlobal(llmod),
            step: llvm::LLVMGetNextGlobal,
        }
    }
}

/// A curious wrapper structure whose only purpose is to call `codegen_aborted`
/// when it's dropped abnormally.
///
/// In the process of working on rust-lang/rust#55238 a mysterious segfault was
/// stumbled upon. The segfault was never reproduced locally, but it was
/// suspected to be releated to the fact that codegen worker threads were
/// sticking around by the time the main thread was exiting, causing issues.
///
/// This structure is an attempt to fix that issue where the `codegen_aborted`
/// message will block until all workers have finished. This should ensure that
/// even if the main codegen thread panics we'll wait for pending work to
/// complete before returning from the main thread, hopefully avoiding
/// segfaults.
///
/// If you see this comment in the code, then it means that this workaround
/// worked! We may yet one day track down the mysterious cause of that
/// segfault...
struct AbortCodegenOnDrop(Option<OngoingCodegen>);

impl AbortCodegenOnDrop {
    fn into_inner(mut self) -> OngoingCodegen {
        self.0.take().unwrap()
    }
}

impl Deref for AbortCodegenOnDrop {
    type Target = OngoingCodegen;
}

fn assert_and_save_dep_graph<'ll, 'tcx>(tcx: TyCtxt<'ll, 'tcx, 'tcx>) {
    time(tcx.sess,
         "assert dep graph",
         || rustc_incremental::assert_dep_graph(tcx));

    time(tcx.sess,
         "serialize dep graph",
         || rustc_incremental::save_dep_graph(tcx));
}

fn collect_and_partition_mono_items<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cnum: CrateNum,
) -> (Arc<DefIdSet>, Arc<Vec<Arc<CodegenUnit<'tcx>>>>)
{
    assert_eq!(cnum, LOCAL_CRATE);

    let collection_mode = match tcx.sess.opts.debugging_opts.print_mono_items {
        Some(ref s) => {
            let mode_string = s.to_lowercase();
            let mode_string = mode_string.trim();
            if mode_string == "eager" {
                MonoItemCollectionMode::Eager
            } else {
                if mode_string != "lazy" {
                    let message = format!("Unknown codegen-item collection mode '{}'. \
                                           Falling back to 'lazy' mode.",
                                          mode_string);
                    tcx.sess.warn(&message);
                }

                MonoItemCollectionMode::Lazy
            }
        }
        None => {
            if tcx.sess.opts.cg.link_dead_code {
                MonoItemCollectionMode::Eager
            } else {
                MonoItemCollectionMode::Lazy
            }
        }
    };

    let (items, inlining_map) =
        time(tcx.sess, "monomorphization collection", || {
            collector::collect_crate_mono_items(tcx, collection_mode)
    });

    tcx.sess.abort_if_errors();

    ::rustc_mir::monomorphize::assert_symbols_are_distinct(tcx, items.iter());

    let strategy = if tcx.sess.opts.incremental.is_some() {
        PartitioningStrategy::PerModule
    } else {
        PartitioningStrategy::FixedUnitCount(tcx.sess.codegen_units())
    };

    let codegen_units = time(tcx.sess, "codegen unit partitioning", || {
        partitioning::partition(tcx,
                                items.iter().cloned(),
                                strategy,
                                &inlining_map)
            .into_iter()
            .map(Arc::new)
            .collect::<Vec<_>>()
    });

    let mono_items: DefIdSet = items.iter().filter_map(|mono_item| {
        match *mono_item {
            MonoItem::Fn(ref instance) => Some(instance.def_id()),
            MonoItem::Static(def_id) => Some(def_id),
            _ => None,
        }
    }).collect();

    fn deref(&self) -> &OngoingCodegen {
        self.0.as_ref().unwrap()
    }
}

impl DerefMut for AbortCodegenOnDrop {
    fn deref_mut(&mut self) -> &mut OngoingCodegen {
        self.0.as_mut().unwrap()
    }
}

impl Drop for AbortCodegenOnDrop {
    fn drop(&mut self) {
        if let Some(codegen) = self.0.take() {
            codegen.codegen_aborted();
        }
    }
}

fn assert_and_save_dep_graph<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>) {
    time(tcx.sess,
         "assert dep graph",
         || rustc_incremental::assert_dep_graph(tcx));

    time(tcx.sess,
         "serialize dep graph",
         || rustc_incremental::save_dep_graph(tcx));
}

impl CrateInfo {
    pub fn new(tcx: TyCtxt) -> CrateInfo {
        let mut info = CrateInfo {
            panic_runtime: None,
            compiler_builtins: None,
            profiler_runtime: None,
            sanitizer_runtime: None,
            is_no_builtins: Default::default(),
            native_libraries: Default::default(),
            used_libraries: tcx.native_libraries(LOCAL_CRATE),
            link_args: tcx.link_args(LOCAL_CRATE),
            crate_name: Default::default(),
            used_crates_dynamic: cstore::used_crates(tcx, LinkagePreference::RequireDynamic),
            used_crates_static: cstore::used_crates(tcx, LinkagePreference::RequireStatic),
            used_crate_source: Default::default(),
            wasm_imports: Default::default(),
            lang_item_to_crate: Default::default(),
            missing_lang_items: Default::default(),
        };
        let lang_items = tcx.lang_items();

        let load_wasm_items = tcx.sess.crate_types.borrow()
            .iter()
            .any(|c| *c != config::CrateType::Rlib) &&
            tcx.sess.opts.target_triple.triple() == "wasm32-unknown-unknown";

        if load_wasm_items {
            info.load_wasm_imports(tcx, LOCAL_CRATE);
        }

        let crates = tcx.crates();

        let n_crates = crates.len();
        info.native_libraries.reserve(n_crates);
        info.crate_name.reserve(n_crates);
        info.used_crate_source.reserve(n_crates);
        info.missing_lang_items.reserve(n_crates);

        for &cnum in crates.iter() {
            info.native_libraries.insert(cnum, tcx.native_libraries(cnum));
            info.crate_name.insert(cnum, tcx.crate_name(cnum).to_string());
            info.used_crate_source.insert(cnum, tcx.used_crate_source(cnum));
            if tcx.is_panic_runtime(cnum) {
                info.panic_runtime = Some(cnum);
            }
            if tcx.is_compiler_builtins(cnum) {
                info.compiler_builtins = Some(cnum);
            }
            if tcx.is_profiler_runtime(cnum) {
                info.profiler_runtime = Some(cnum);
            }
            if tcx.is_sanitizer_runtime(cnum) {
                info.sanitizer_runtime = Some(cnum);
            }
            if tcx.is_no_builtins(cnum) {
                info.is_no_builtins.insert(cnum);
            }
            if load_wasm_items {
                info.load_wasm_imports(tcx, cnum);
            }
            let missing = tcx.missing_lang_items(cnum);
            for &item in missing.iter() {
                if let Ok(id) = lang_items.require(item) {
                    info.lang_item_to_crate.insert(item, id.krate);
                }
            }

            // No need to look for lang items that are whitelisted and don't
            // actually need to exist.
            let missing = missing.iter()
                .cloned()
                .filter(|&l| !weak_lang_items::whitelisted(tcx, l))
                .collect();
            info.missing_lang_items.insert(cnum, missing);
        }

        return info
    }

    fn load_wasm_imports(&mut self, tcx: TyCtxt, cnum: CrateNum) {
        self.wasm_imports.extend(tcx.wasm_import_module_map(cnum).iter().map(|(&id, module)| {
            let instance = Instance::mono(tcx, id);
            let import_name = tcx.symbol_name(instance);

            (import_name.to_string(), module.clone())
        }));
    }
}

fn is_codegened_item(tcx: TyCtxt, id: DefId) -> bool {
    let (all_mono_items, _) =
        tcx.collect_and_partition_mono_items(LOCAL_CRATE);
    all_mono_items.contains(&id)
}

=======
pub fn compile_codegen_unit<'ll, 'tcx>(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
                                  cgu_name: InternedString)
                                  -> Stats {
    let start_time = Instant::now();

    let dep_node = tcx.codegen_unit(cgu_name).codegen_dep_node(tcx);
    let ((stats, module), _) = tcx.dep_graph.with_task(dep_node,
                                                       tcx,
                                                       cgu_name,
                                                       module_codegen);
    let time_to_codegen = start_time.elapsed();

    // We assume that the cost to run LLVM on a CGU is proportional to
    // the time we needed for codegenning it.
    let cost = time_to_codegen.as_secs() * 1_000_000_000 +
               time_to_codegen.subsec_nanos() as u64;

    write::submit_codegened_module_to_llvm(tcx,
                                           module,
                                           cost);
    return stats;

    fn module_codegen<'ll, 'tcx>(
        tcx: TyCtxt<'ll, 'tcx, 'tcx>,
        cgu_name: InternedString)
        -> (Stats, ModuleCodegen<ModuleLlvm>)
    {
        let backend = LlvmCodegenBackend(());
        let cgu = tcx.codegen_unit(cgu_name);
        // Instantiate monomorphizations without filling out definitions yet...
        let llvm_module = backend.new_metadata(tcx.sess, &cgu_name.as_str());
        let stats = {
            let cx = CodegenCx::new(tcx, cgu, &llvm_module);
            let mono_items = cx.codegen_unit
                               .items_in_deterministic_order(cx.tcx);
            for &(mono_item, (linkage, visibility)) in &mono_items {
                mono_item.predefine::<Builder<&Value>>(&cx, linkage, visibility);
            }

            // ... and now that we have everything pre-defined, fill out those definitions.
            for &(mono_item, _) in &mono_items {
                mono_item.define::<Builder<&Value>>(&cx);
            }

            // If this codegen unit contains the main function, also create the
            // wrapper here
            maybe_create_entry_wrapper::<Builder<&Value>>(&cx);

            // Run replace-all-uses-with for statics that need it
            for &(old_g, new_g) in cx.statics_to_rauw().borrow().iter() {
                cx.static_replace_all_uses(old_g, new_g)
            }

            // Create the llvm.used variable
            // This variable has type [N x i8*] and is stored in the llvm.metadata section
            if !cx.used_statics().borrow().is_empty() {
                cx.create_used_variable()
            }

            // Finalize debuginfo
            if cx.sess().opts.debuginfo != DebugInfo::None {
                cx.debuginfo_finalize();
            }

            cx.consume_stats().into_inner()
        };

        (stats, ModuleCodegen {
            name: cgu_name.to_string(),
            module_llvm: llvm_module,
            kind: ModuleKind::Regular,
        })
    }
}

pub fn provide_both(providers: &mut Providers) {
    providers.dllimport_foreign_items = |tcx, krate| {
        let module_map = tcx.foreign_modules(krate);
        let module_map = module_map.iter()
            .map(|lib| (lib.def_id, lib))
            .collect::<FxHashMap<_, _>>();

        let dllimports = tcx.native_libraries(krate)
            .iter()
            .filter(|lib| {
                if lib.kind != cstore::NativeLibraryKind::NativeUnknown {
                    return false
                }
                let cfg = match lib.cfg {
                    Some(ref cfg) => cfg,
                    None => return true,
                };
                attr::cfg_matches(cfg, &tcx.sess.parse_sess, None)
            })
            .filter_map(|lib| lib.foreign_module)
            .map(|id| &module_map[&id])
            .flat_map(|module| module.foreign_items.iter().cloned())
            .collect();
        Lrc::new(dllimports)
    };

    providers.is_dllimport_foreign_item = |tcx, def_id| {
        tcx.dllimport_foreign_items(def_id.krate).contains(&def_id)
    }
}

pub fn set_link_section(llval: &Value, attrs: &CodegenFnAttrs) {
    let sect = match attrs.link_section {
        Some(name) => name,
        None => return,
    };
    unsafe {
        let buf = SmallCStr::new(&sect.as_str());
        llvm::LLVMSetSection(llval, buf.as_ptr());
    }
}

pub fn linkage_to_llvm(linkage: Linkage) -> llvm::Linkage {
    match linkage {
        Linkage::External => llvm::Linkage::ExternalLinkage,
        Linkage::AvailableExternally => llvm::Linkage::AvailableExternallyLinkage,
        Linkage::LinkOnceAny => llvm::Linkage::LinkOnceAnyLinkage,
        Linkage::LinkOnceODR => llvm::Linkage::LinkOnceODRLinkage,
        Linkage::WeakAny => llvm::Linkage::WeakAnyLinkage,
        Linkage::WeakODR => llvm::Linkage::WeakODRLinkage,
        Linkage::Appending => llvm::Linkage::AppendingLinkage,
        Linkage::Internal => llvm::Linkage::InternalLinkage,
        Linkage::Private => llvm::Linkage::PrivateLinkage,
        Linkage::ExternalWeak => llvm::Linkage::ExternalWeakLinkage,
        Linkage::Common => llvm::Linkage::CommonLinkage,
    }
}

pub fn visibility_to_llvm(linkage: Visibility) -> llvm::Visibility {
    match linkage {
        Visibility::Default => llvm::Visibility::Default,
        Visibility::Hidden => llvm::Visibility::Hidden,
        Visibility::Protected => llvm::Visibility::Protected,
    }
}
