//! All functions here are copied from
//! https://github.com/rust-lang/rust/blob/14ea6e50c1534a23cb51375552c14568db9ee130/src/librustc_codegen_llvm/back/link.rs

use rustc::hir::def_id::CrateNum;
use rustc::middle::cstore::METADATA_FILENAME;
use rustc::session::Session;
use rustc::session::config::{self, OutputFilenames, OutputType};
use rustc::session::config::Lto;
use rustc::session::search_paths::PathKind;
use rustc_codegen_ssa::CodegenResults;
use rustc_codegen_ssa::back::command::Command;
use rustc_codegen_ssa::back::linker::Linker;
use rustc_codegen_ssa::back::link::{remove, ignored_for_lto, linker_and_flavor,
                                    get_linker};
pub use rustc_codegen_utils::link::{find_crate_name, filename_for_input,
                                    default_output_for_target,
                                    invalid_output_for_target,
                                    filename_for_metadata, out_filename,
                                    check_file_is_writeable};
use rustc_data_structures::fx::FxHashSet;
use rustc::middle::cstore::{NativeLibrary, NativeLibraryKind};
use rustc::middle::dependency_format::Linkage;
use rustc_target::spec::{PanicStrategy, RelroLevel, LinkerFlavor};
use tempfile::{Builder as TempFileBuilder, TempDir};

use std::ascii;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Command as ShellCommand, Output, Stdio};
use std::str;
use syntax::attr;

fn get_file_path(sess: &Session, name: &str) -> PathBuf {
    let fs = sess.target_filesearch(PathKind::Native);
    let file_path = fs.get_lib_path().join(name);
    if file_path.exists() {
        return file_path
    }
    for search_path in fs.search_paths() {
        let file_path = search_path.dir.join(name);
        if file_path.exists() {
            return file_path
        }
    }
    PathBuf::from(name)
}

pub(crate) fn link_binary(sess: &Session,
                          codegen_results: &CodegenResults,
                          outputs: &OutputFilenames,
                          crate_name: &str) -> Vec<PathBuf> {
    let mut out_filenames = Vec::new();
    for &crate_type in sess.crate_types.borrow().iter() {
        // Ignore executable crates if we have -Z no-codegen, as they will error.
        let output_metadata = sess.opts.output_types.contains_key(&OutputType::Metadata);
        if (sess.opts.debugging_opts.no_codegen || !sess.opts.output_types.should_codegen()) &&
           !output_metadata &&
           crate_type == config::CrateType::Executable {
            continue;
        }
        let out_files = link_binary_output(sess,
                                           codegen_results,
                                           crate_type,
                                           outputs,
                                           crate_name);
        out_filenames.extend(out_files);
    }

    if !sess.opts.cg.save_temps {
        if sess.opts.output_types.should_codegen() {
            for obj in codegen_results.modules.iter()
                .filter_map(|m| m.object.as_ref()) {
                remove(sess, obj);
            }
        }
        // FIXME: the metadata module is never emitted
        if let Some(ref allocator) = codegen_results.allocator_module {
            if let Some(ref obj) = allocator.object {
                remove(sess, obj);
            }
            if let Some(ref bc) = allocator.bytecode_compressed {
                remove(sess, bc);
            }
        }
    }
    out_filenames
}


fn link_binary_output(sess: &Session,
                      codegen_results: &CodegenResults,
                      crate_type: config::CrateType,
                      outputs: &OutputFilenames,
                      crate_name: &str) -> Vec<PathBuf> {
    let mut out_filenames = vec![];
    if outputs.outputs.contains_key(&OutputType::Metadata) {
        let out_filename = filename_for_metadata(sess, crate_name, outputs);
        // To avoid races with another rustc process scanning the output directory,
        // we need to write the file somewhere else and atomically move it to its
        // final destination, with a `fs::rename` call. In order for the rename to
        // always succeed, the temporary file needs to be on the same filesystem,
        // which is why we create it inside the output directory specifically.
        let metadata_tmpdir = TempFileBuilder::new()
            .prefix("rmeta")
            .tempdir_in(out_filename.parent().unwrap())
            .unwrap_or_else(|err| sess.fatal(&format!("couldn't create a temp dir: {}", err)));
        let metadata = emit_metadata(sess, codegen_results, &metadata_tmpdir);
        if let Err(e) = fs::rename(metadata, &out_filename) {
            sess.fatal(&format!("failed to write {}: {}", out_filename.display(), e));
        }
        out_filenames.push(out_filename);
    }

    let tmpdir = TempFileBuilder::new().prefix("rustc").tempdir().unwrap_or_else(|err|
        sess.fatal(&format!("couldn't create a temp dir: {}", err)));

    if outputs.outputs.should_codegen() {
        let out_filename = out_filename(sess, crate_type, outputs, crate_name);
        match crate_type {
            config::CrateType::Rlib => {
                link_rlib(sess,
                          crate_type,
                          &out_filename,
                          codegen_results,
                          &tmpdir);
            }
            config::CrateType::Staticlib => {
                link_staticlib(sess, codegen_results, &out_filename, &tmpdir);
            }
            _ => {
                link_natively(sess, crate_type, &out_filename, codegen_results, tmpdir.path());
            }
        }
        out_filenames.push(out_filename);
    }

    if sess.opts.cg.save_temps {
        let _ = tmpdir.into_path();
    }

    out_filenames
}

fn link_staticlib(_sess: &Session,
                  _codegen_results: &CodegenResults,
                  _out_filename: &Path,
                  _tempdir: &TempDir) {
    unimplemented!("link_staticlib");
}

fn link_natively(sess: &Session,
                 crate_type: config::CrateType,
                 out_filename: &Path,
                 codegen_results: &CodegenResults,
                 tmpdir: &Path) {
    let (linker, flavor) = linker_and_flavor(sess);
    let (pname, mut cmd) = get_linker(sess, &linker, flavor);

    if let Some(args) = sess.target.target.options.pre_link_args.get(&flavor) {
        cmd.args(args);
    }
    if let Some(args) = sess.target.target.options.pre_link_args_crt.get(&flavor) {
        if sess.crt_static() {
            cmd.args(args);
        }
    }
    if let Some(ref args) = sess.opts.debugging_opts.pre_link_args {
        cmd.args(args);
    }
    cmd.args(&sess.opts.debugging_opts.pre_link_arg);

    let pre_link_objects = if crate_type == config::CrateType::Executable {
        &sess.target.target.options.pre_link_objects_exe
    } else {
        bug!("Unsupported crate type: {:?}", crate_type);
    };
    for obj in pre_link_objects {
        cmd.arg(get_file_path(sess, obj));
    }
    if crate_type == config::CrateType::Executable && sess.crt_static() {
        for obj in &sess.target.target.options.pre_link_objects_exe_crt {
            cmd.arg(get_file_path(sess, obj));
        }
    }

    if sess.target.target.options.is_like_emscripten {
        cmd.arg("-s");
        cmd.arg(if sess.panic_strategy() == PanicStrategy::Abort {
            "DISABLE_EXCEPTION_CATCHING=1"
        } else {
            "DISABLE_EXCEPTION_CATCHING=0"
        });
    }

    {
        // FIXME: hard-coded target
        let target_cpu = "x86-64";
        let mut linker = codegen_results.linker_info.to_linker(
            cmd, &sess, flavor, target_cpu);
        link_args(&mut *linker, flavor, sess, crate_type, tmpdir,
                  out_filename, codegen_results);
        cmd = linker.finalize();
    }
    if let Some(args) = sess.target.target.options.late_link_args.get(&flavor) {
        cmd.args(args);
    }
    for obj in &sess.target.target.options.post_link_objects {
        cmd.arg(get_file_path(sess, obj));
    }
    if sess.crt_static() {
        for obj in &sess.target.target.options.post_link_objects_crt {
            cmd.arg(get_file_path(sess, obj));
        }
    }
    if let Some(args) = sess.target.target.options.post_link_args.get(&flavor) {
        cmd.args(args);
    }
    for &(ref k, ref v) in &sess.target.target.options.link_env {
        cmd.env(k, v);
    }

    // May have not found libraries in the right formats.
    sess.abort_if_errors();
    let prog = exec_linker(sess, &mut cmd, out_filename, tmpdir);
    let output = match prog {
        Ok(ref output) => output,
        Err(_) => panic!("An error occurred when linking"),
    };
    let mut out = output.stderr.clone();
    out.extend(&output.stdout);
    let out = String::from_utf8_lossy(&out);

    if sess.target.target.options.linker_is_gnu &&
       flavor != LinkerFlavor::Ld &&
       (out.contains("unrecognized command line option") ||
        out.contains("unknown argument")) &&
       out.contains("-no-pie") &&
       cmd.get_args().iter().any(|e| e.to_string_lossy() == "-no-pie") {
        panic!(
            "Linker does not support -no-pie command line option. Retrying without.");
    }
    match prog {
        Ok(prog) => {
            fn escape_string(s: &[u8]) -> String {
                str::from_utf8(s).map(|s| s.to_owned())
                    .unwrap_or_else(|_| {
                        let mut x = "Non-UTF-8 output: ".to_string();
                        x.extend(s.iter()
                                  .flat_map(|&b| ascii::escape_default(b))
                                  .map(char::from));
                        x
                    })
            }
            if !prog.status.success() {
                let mut output = prog.stderr.clone();
                output.extend_from_slice(&prog.stdout);
                sess.struct_err(&format!("linking with `{}` failed: {}",
                                         pname.display(),
                                         prog.status))
                    .note(&format!("{:?}", &cmd))
                    .note(&escape_string(&output))
                    .emit();
                sess.abort_if_errors();
            }
        },
        Err(e) => {
            let linker_not_found = e.kind() == io::ErrorKind::NotFound;

            let mut linker_error = {
                if linker_not_found {
                    sess.struct_err(&format!("linker `{}` not found", pname.display()))
                } else {
                    sess.struct_err(&format!("could not exec the linker `{}`",
                                             pname.display()))
                }
            };

            linker_error.note(&e.to_string());

            if !linker_not_found {
                linker_error.note(&format!("{:?}", &cmd));
            }

            linker_error.emit();
            sess.abort_if_errors();
        }
    }
}

fn exec_linker(_sess: &Session, cmd: &mut Command, _out_filename: &Path, _tmpdir: &Path)
    -> io::Result<Output>
{
    if !cmd.very_likely_to_exceed_some_spawn_limit() {
        match cmd.command().stdout(Stdio::piped()).stderr(Stdio::piped()).spawn() {
            Ok(child) => {
                let output = child.wait_with_output();
                return output;
            }
            Err(ref e) if e.raw_os_error() == Some(::libc::E2BIG) => {
                panic!("command line to linker was too big: {}", e);
            }
            Err(e) => return Err(e)
        }
    } else {
        panic!("very_likely_to_exceed_some_spawn_limit is true!");
    }
}

fn link_args(cmd: &mut dyn Linker,
             flavor: LinkerFlavor,
             sess: &Session,
             crate_type: config::CrateType,
             tmpdir: &Path,
             out_filename: &Path,
             codegen_results: &CodegenResults) {

    // Linker plugins should be specified early in the list of arguments
    cmd.cross_lang_lto();

    // The default library location, we need this to find the runtime.
    // The location of crates will be determined as needed.
    let lib_path = sess.target_filesearch(PathKind::All).get_lib_path();

    // target descriptor
    let t = &sess.target.target;

    cmd.include_path(&lib_path.to_path_buf());
    for obj in codegen_results.modules.iter().filter_map(|m| m.object.as_ref()) {
        cmd.add_object(obj);
    }
    cmd.output_filename(out_filename);

    // If we're building a dynamic library then some platforms need to make sure
    // that all symbols are exported correctly from the dynamic library.
    if crate_type != config::CrateType::Executable ||
       sess.target.target.options.is_like_emscripten {
        cmd.export_symbols(tmpdir, crate_type);
    }

    // When linking a dynamic library, we put the metadata into a section of the
    // executable. This metadata is in a separate object file from the main
    // object file, so we link that in here.
    if crate_type == config::CrateType::Dylib ||
       crate_type == config::CrateType::ProcMacro {
        if let Some(obj) = codegen_results.metadata_module.object.as_ref() {
            cmd.add_object(obj);
        }
    }

    let obj = codegen_results.allocator_module
        .as_ref()
        .and_then(|m| m.object.as_ref());
    if let Some(obj) = obj {
        // FIXME: codegen_allocator....
        cmd.add_object(obj);
    }

    // Try to strip as much out of the generated object by removing unused
    // sections if possible. See more comments in linker.rs
    if !sess.opts.cg.link_dead_code {
        let keep_metadata = crate_type == config::CrateType::Dylib;
        cmd.gc_sections(keep_metadata);
    }

    let used_link_args = &codegen_results.crate_info.link_args;

    if crate_type == config::CrateType::Executable {
        let mut position_independent_executable = false;

        if t.options.position_independent_executables {
            let empty_vec = Vec::new();
            let args = sess.opts.cg.link_args.as_ref().unwrap_or(&empty_vec);
            let more_args = &sess.opts.cg.link_arg;
            let mut args = args.iter().chain(more_args.iter()).chain(used_link_args.iter());

            let reloc_model_arg = match sess.opts.cg.relocation_model {
                Some(ref s) => &s[..],
                None => &sess.target.target.options.relocation_model[..],
            };

            if reloc_model_arg == "pic"
                && !sess.crt_static() && !args.any(|x| *x == "-static") {
                position_independent_executable = true;
            }
        }

        if position_independent_executable {
            cmd.position_independent_executable();
        } else {
            // recent versions of gcc can be configured to generate position
            // independent executables by default. We have to pass -no-pie to
            // explicitly turn that off. Not applicable to ld.
            if sess.target.target.options.linker_is_gnu
                && flavor != LinkerFlavor::Ld {
                cmd.no_position_independent_executable();
            }
        }
    }

    let relro_level = match sess.opts.debugging_opts.relro_level {
        Some(level) => level,
        None => t.options.relro_level,
    };
    match relro_level {
        RelroLevel::Full => {
            cmd.full_relro();
        },
        RelroLevel::Partial => {
            cmd.partial_relro();
        },
        RelroLevel::Off => {
            cmd.no_relro();
        },
        RelroLevel::None => {
        },
    }

    //// Pass optimization flags down to the linker.
    //cmd.optimize();

    //// Pass debuginfo flags down to the linker.
    //cmd.debuginfo();

    // We want to, by default, prevent the compiler from accidentally leaking in
    // any system libraries, so we may explicitly ask linkers to not link to any
    // libraries by default. Note that this does not happen for windows because
    // windows pulls in some large number of libraries and I couldn't quite
    // figure out which subset we wanted.
    //
    // This is all naturally configurable via the standard methods as well.
    if !sess.opts.cg.default_linker_libraries.unwrap_or(false) &&
        t.options.no_default_libraries
    {
        cmd.no_default_libraries();
    }

    // Take careful note of the ordering of the arguments we pass to the linker
    // here. Linkers will assume that things on the left depend on things to the
    // right. Things on the right cannot depend on things on the left. This is
    // all formally implemented in terms of resolving symbols (libs on the right
    // resolve unknown symbols of libs on the left, but not vice versa).
    //
    // For this reason, we have organized the arguments we pass to the linker as
    // such:
    //
    //  1. The local object that LLVM just generated
    //  2. Local native libraries
    //  3. Upstream rust libraries
    //  4. Upstream native libraries
    //
    // The rationale behind this ordering is that those items lower down in the
    // list can't depend on items higher up in the list. For example nothing can
    // depend on what we just generated (e.g., that'd be a circular dependency).
    // Upstream rust libraries are not allowed to depend on our local native
    // libraries as that would violate the structure of the DAG, in that
    // scenario they are required to link to them as well in a shared fashion.
    //
    // Note that upstream rust libraries may contain native dependencies as
    // well, but they also can't depend on what we just started to add to the
    // link line. And finally upstream native libraries can't depend on anything
    // in this DAG so far because they're only dylibs and dylibs can only depend
    // on other dylibs (e.g., other native deps).
    add_local_native_libraries(cmd, sess, codegen_results);
    add_upstream_rust_crates(cmd, sess, codegen_results, crate_type, tmpdir);
    add_upstream_native_libraries(cmd, sess, codegen_results, crate_type);

    // Tell the linker what we're doing.
    if crate_type != config::CrateType::Executable {
        cmd.build_dylib(out_filename);
    }
    if crate_type == config::CrateType::Executable && sess.crt_static() {
        cmd.build_static_executable();
    }

    if sess.opts.debugging_opts.pgo_gen.is_some() {
        cmd.pgo_gen();
    }

    // Finally add all the linker arguments provided on the command line along
    // with any #[link_args] attributes found inside the crate
    if let Some(ref args) = sess.opts.cg.link_args {
        cmd.args(args);
    }
    cmd.args(&sess.opts.cg.link_arg);
    cmd.args(&used_link_args);

}

fn relevant_lib(sess: &Session, lib: &NativeLibrary) -> bool {
    // FIXME:
    match lib.cfg {
        Some(ref cfg) => attr::cfg_matches(cfg, &sess.parse_sess, None),
        None => true,
    }
}

fn add_upstream_rust_crates(cmd: &mut dyn Linker,
                            sess: &Session,
                            codegen_results: &CodegenResults,
                            crate_type: config::CrateType,
                            tmpdir: &Path) {
    // All of the heavy lifting has previously been accomplished by the
    // dependency_format module of the compiler. This is just crawling the
    // output of that module, adding crates as necessary.
    //
    // Linking to a rlib involves just passing it to the linker (the linker
    // will slurp up the object files inside), and linking to a dynamic library
    // involves just passing the right -l flag.

    let formats = sess.dependency_formats.borrow();
    let data = formats.get(&crate_type).unwrap();

    // Invoke get_used_crates to ensure that we get a topological sorting of
    // crates.
    let deps = &codegen_results.crate_info.used_crates_dynamic;
    //panic!("deps are {:?}", deps);
    //
    let mut group_end = None;
    let mut group_start = None;
    let mut end_with = FxHashSet::default();
    let info = &codegen_results.crate_info;
    for &(cnum, _) in deps.iter().rev() {
        if let Some(missing) = info.missing_lang_items.get(&cnum) {
            end_with.extend(missing.iter().cloned());
            if end_with.len() > 0 && group_end.is_none() {
                group_end = Some(cnum);
            }
        }
        end_with.retain(|item| info.lang_item_to_crate.get(item) != Some(&cnum));
        if end_with.len() == 0 && group_end.is_some() {
            group_start = Some(cnum);
            break
        }
    }

    // If we didn't end up filling in all lang items from upstream crates then
    // we'll be filling it in with our crate. This probably means we're the
    // standard library itself, so skip this for now.
    if group_end.is_some() && group_start.is_none() {
        group_end = None;
    }

    //let mut compiler_builtins = None;

    for &(cnum, _) in deps.iter() {
        if group_start == Some(cnum) {
            cmd.group_start();
        }

        // We may not pass all crates through to the linker. Some crates may
        // appear statically in an existing dylib, meaning we'll pick up all the
        // symbols from the dylib.
        let src = &codegen_results.crate_info.used_crate_source[&cnum];
        match data[cnum.as_usize() - 1] {
            _ if codegen_results.crate_info.profiler_runtime == Some(cnum) => {
                unimplemented!("add_static_crate");
            }
            _ if codegen_results.crate_info.sanitizer_runtime == Some(cnum) => {
                unimplemented!("link_sanitizer_runtime");
            }
            // compiler-builtins are always placed last to ensure that they're
            // linked correctly.
            //_ if codegen_results.crate_info.compiler_builtins == Some(cnum) => {
                //assert!(compiler_builtins.is_none());
                //compiler_builtins = Some(cnum);
            //}
            Linkage::NotLinked |
            Linkage::IncludedFromDylib => {}
            Linkage::Static => {
                add_static_crate(cmd, sess, codegen_results, tmpdir, crate_type, cnum);
            }
            Linkage::Dynamic => {
                add_dynamic_crate(cmd, sess, &src.dylib.as_ref().unwrap().0)
            }
        }

        if group_end == Some(cnum) {
            cmd.group_end();
        }
    }

    fn are_upstream_rust_objects_already_included(sess: &Session) -> bool {
        match sess.lto() {
            Lto::Fat => true,
            Lto::Thin => {
                // If we defer LTO to the linker, we haven't run LTO ourselves, so
                // any upstream object files have not been copied yet.
                !sess.opts.debugging_opts.cross_lang_lto.enabled()
            }
            Lto::No |
            Lto::ThinLocal => false,
        }
    }

    fn add_static_crate(cmd: &mut dyn Linker,
                        sess: &Session,
                        codegen_results: &CodegenResults,
                        tmpdir: &Path,
                        crate_type: config::CrateType,
                        cnum: CrateNum) {
        let src = &codegen_results.crate_info.used_crate_source[&cnum];
        let cratepath = &src.rlib.as_ref().unwrap().0;

        // See the comment above in `link_staticlib` and `link_rlib` for why if
        // there's a static library that's not relevant we skip all object
        // files.
        let native_libs = &codegen_results.crate_info.native_libraries[&cnum];
        let skip_native = native_libs.iter().any(|lib| {
            lib.kind == NativeLibraryKind::NativeStatic && !relevant_lib(sess, lib)
        });

        if (!are_upstream_rust_objects_already_included(sess) ||
            ignored_for_lto(sess, &codegen_results.crate_info, cnum)) &&
           crate_type != config::CrateType::Dylib &&
           !skip_native {
            cmd.link_rlib(&cratepath.to_path_buf());
            return
        }

        let dst = tmpdir.join(cratepath.file_name().unwrap());
        if crate_type == config::CrateType::Dylib &&
            codegen_results.crate_info.compiler_builtins != Some(cnum) {
            cmd.link_whole_rlib(&dst.to_path_buf());
        } else {
            cmd.link_rlib(&dst.to_path_buf());
        }

    }

    // Converts a library file-stem into a cc -l argument
    fn unlib<'a>(config: &config::Config, stem: &'a str) -> &'a str {
        if stem.starts_with("lib") && !config.target.options.is_like_windows {
            &stem[3..]
        } else {
            stem
        }
    }

    // Same thing as above, but for dynamic crates instead of static crates.
    fn add_dynamic_crate(cmd: &mut dyn Linker, sess: &Session, cratepath: &Path) {
        // If we're performing LTO, then it should have been previously required
        // that all upstream rust dependencies were available in an rlib format.
        //assert!(!are_upstream_rust_objects_already_included(sess));

        // Just need to tell the linker about where the library lives and
        // what its name is
        let parent = cratepath.parent();
        if let Some(dir) = parent {
            cmd.include_path(&dir.to_path_buf());
        }
        let filestem = cratepath.file_stem().unwrap().to_str().unwrap();
        cmd.link_rust_dylib(&unlib(&sess.target, filestem),
                            parent.unwrap_or(Path::new("")));
    }
}

fn add_upstream_native_libraries(cmd: &mut dyn Linker,
                                 sess: &Session,
                                 codegen_results: &CodegenResults,
                                 crate_type: config::CrateType) {
    // Be sure to use a topological sorting of crates because there may be
    // interdependencies between native libraries. When passing -nodefaultlibs,
    // for example, almost all native libraries depend on libc, so we have to
    // make sure that's all the way at the right (liblibc is near the base of
    // the dependency chain).
    //
    // This passes RequireStatic, but the actual requirement doesn't matter,
    // we're just getting an ordering of crate numbers, we're not worried about
    // the paths.
    let formats = sess.dependency_formats.borrow();
    let data = formats.get(&crate_type).unwrap();

    let crates = &codegen_results.crate_info.used_crates_static;

    for &(cnum, _) in crates {
        for lib in codegen_results.crate_info.native_libraries[&cnum].iter() {
            let name = match lib.name {
                Some(ref l) => l,
                None => continue,
            };
            if !relevant_lib(sess, &lib) {
                continue
            }
            match lib.kind {
                NativeLibraryKind::NativeUnknown => cmd.link_dylib(&name.as_str()),
                NativeLibraryKind::NativeFramework => cmd.link_framework(&name.as_str()),
                NativeLibraryKind::NativeStaticNobundle => {
                    // Link "static-nobundle" native libs only if the crate they originate from
                    // is being linked statically to the current crate.  If it's linked dynamically
                    // or is an rlib already included via some other dylib crate, the symbols from
                    // native libs will have already been included in that dylib.
                    if data[cnum.as_usize() - 1] == Linkage::Static {
                        cmd.link_staticlib(&name.as_str())
                    }
                },
                // ignore statically included native libraries here as we've
                // already included them when we included the rust library
                // previously
                NativeLibraryKind::NativeStatic => {}
            }
        }
    }
}

fn add_local_native_libraries(cmd: &mut dyn Linker,
                              sess: &Session,
                              codegen_results: &CodegenResults) {
    let filesearch = sess.target_filesearch(PathKind::All);
    for search_path in filesearch.search_paths() {
        match search_path.kind {
            PathKind::Framework => { cmd.framework_path(&search_path.dir); }
            _ => { cmd.include_path(&search_path.dir.to_path_buf()); }
        }
    }

    let relevant_libs = codegen_results.crate_info.used_libraries.iter().filter(|l| {
        relevant_lib(sess, l)
    });

    // This, or: let search_path = archive_search_paths(sess);
    let search_path = sess.target_filesearch(PathKind::Native).search_path_dirs();
    for lib in relevant_libs {
        let name = match lib.name {
            Some(ref l) => l,
            None => continue,
        };
        match lib.kind {
            NativeLibraryKind::NativeUnknown => cmd.link_dylib(&name.as_str()),
            NativeLibraryKind::NativeFramework => cmd.link_framework(&name.as_str()),
            NativeLibraryKind::NativeStaticNobundle => cmd.link_staticlib(&name.as_str()),
            NativeLibraryKind::NativeStatic => cmd.link_whole_staticlib(&name.as_str(),
                                                                        &search_path)
        }
    }
}

fn link_rlib(sess: &'a Session,
             _crate_type: config::CrateType,
             out_filename: &Path,
             codegen_results: &CodegenResults,
             tmpdir: &TempDir) {
    // Instead of putting the metadata in an object file section, rlibs
    // contain the metadata in a separate file.
    let metadata_filename = emit_metadata(sess, codegen_results, tmpdir);
    // The files to be added to the rlib.
    let mut ar_files = Vec::new();
    // Add all the object files produced.
    for obj in codegen_results.modules.iter().filter_map(|m| m.object.as_ref()) {
        ar_files.push(obj)
    }

    for lib in codegen_results.crate_info.used_libraries.iter() {
        match lib.kind {
            NativeLibraryKind::NativeStatic => {}
            NativeLibraryKind::NativeStaticNobundle |
            NativeLibraryKind::NativeFramework |
            NativeLibraryKind::NativeUnknown => continue,
        }
        if let Some(name) = lib.name {
            unimplemented!("add native lib: {}", name.as_str());
        }
    }

    ar_files.push(&metadata_filename);

    // FIXME: handle compressed bytecode?

    let mut cmd = ShellCommand::new("ar");
    cmd.arg("cr").arg(out_filename).args(ar_files);
    if cmd.status().is_err() {
        sess.struct_err(&format!("failed to execute: {:?}", cmd)).emit();
        sess.abort_if_errors();
    }
}


fn emit_metadata<'a>(
    sess: &'a Session,
    codegen_results: &CodegenResults,
    tmpdir: &TempDir
) -> PathBuf {
    let out_filename = tmpdir.path().join(METADATA_FILENAME);
    let result = fs::write(&out_filename, &codegen_results.metadata.raw_data);
    if let Err(e) = result {
        sess.fatal(&format!("failed to write {}: {}", out_filename.display(), e));
    }
    out_filename
}
