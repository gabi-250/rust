use rustc::hir::def_id::{CrateNum, DefId, LOCAL_CRATE};
use rustc_mir::monomorphize::collector::{self, MonoItemCollectionMode};
use rustc_mir::monomorphize::partitioning::{self, PartitioningStrategy, CodegenUnit};
use rustc::util::nodemap::{FxHashMap, DefIdSet};
use rustc::mir::mono::{Linkage};
use rustc::util::common::{time};
use rustc::mir::mono::{MonoItem};
use rustc::ty::query::Providers;
use rustc_mir::monomorphize::MonoItemExt;
use rustc::middle::cstore;
use syntax::attr;
use rustc_data_structures::sync::Lrc;
use rustc::ty::TyCtxt;

use std::sync::Arc;

fn collect_and_partition_mono_items<'a, 'tcx>(
    tcx: TyCtxt<'a, 'tcx, 'tcx>,
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

    if tcx.sess.opts.debugging_opts.print_mono_items.is_some() {
        let mut item_to_cgus: FxHashMap<_, Vec<_>> = Default::default();

        for cgu in &codegen_units {
            for (&mono_item, &linkage) in cgu.items() {
                item_to_cgus.entry(mono_item)
                            .or_default()
                            .push((cgu.name().clone(), linkage));
            }
        }

        let mut item_keys: Vec<_> = items
            .iter()
            .map(|i| {
                let mut output = i.to_string(tcx);
                output.push_str(" @@");
                let mut empty = Vec::new();
                let cgus = item_to_cgus.get_mut(i).unwrap_or(&mut empty);
                cgus.as_mut_slice().sort_by_key(|&(ref name, _)| name.clone());
                cgus.dedup();
                for &(ref cgu_name, (linkage, _)) in cgus.iter() {
                    output.push_str(" ");
                    output.push_str(&cgu_name.as_str());

                    let linkage_abbrev = match linkage {
                        Linkage::External => "External",
                        Linkage::AvailableExternally => "Available",
                        Linkage::LinkOnceAny => "OnceAny",
                        Linkage::LinkOnceODR => "OnceODR",
                        Linkage::WeakAny => "WeakAny",
                        Linkage::WeakODR => "WeakODR",
                        Linkage::Appending => "Appending",
                        Linkage::Internal => "Internal",
                        Linkage::Private => "Private",
                        Linkage::ExternalWeak => "ExternalWeak",
                        Linkage::Common => "Common",
                    };

                    output.push_str("[");
                    output.push_str(linkage_abbrev);
                    output.push_str("]");
                }
                output
            })
            .collect();

        item_keys.sort();

        for item in item_keys {
            println!("MONO_ITEM {}", item);
        }
    }

    (Arc::new(mono_items), Arc::new(codegen_units))
}

fn is_codegened_item(tcx: TyCtxt, id: DefId) -> bool {
    let (all_mono_items, _) =
        tcx.collect_and_partition_mono_items(LOCAL_CRATE);
    all_mono_items.contains(&id)
}

pub fn provide(providers: &mut Providers) {
    providers.collect_and_partition_mono_items =
        collect_and_partition_mono_items;

    providers.is_codegened_item = is_codegened_item;

    providers.codegen_unit = |tcx, name| {
        let (_, all) = tcx.collect_and_partition_mono_items(LOCAL_CRATE);
        all.iter()
            .find(|cgu| *cgu.name() == name)
            .cloned()
            .unwrap_or_else(|| panic!("failed to find cgu with name {:?}", name))
    };

    provide_extern(providers);
}

pub fn provide_extern(providers: &mut Providers) {
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
    };
}
