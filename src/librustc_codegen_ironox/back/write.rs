use errors::{Handler, FatalError};
use rustc::session::config::OutputType;
use rustc::util::time_graph::Timeline;
use rustc_codegen_ssa::{CompiledModule, ModuleCodegen, ModuleKind};
use rustc_codegen_ssa::back::write::{CodegenContext, ModuleConfig, run_assembler};

use std::fs;
use std::path::PathBuf;

use IronOxCodegenBackend;
use ModuleIronOx;

/// Write the assembler instructions from a `module` to the specified `path`.
fn write_module(
    module: &ModuleIronOx,
    path: &PathBuf,
    diag_handler: &Handler) {
    // If this is a regular module, write it to a file. For 'metadata' and
    // 'allocator' modules `asm` is always `None`. `Metadata`/`allocator`
    // modules are ignored for now. We don't crash when compiling
    // `metadata`/`allocator` modules, because for each `regular` module, there
    // is a `metadata` module that needs to be compiled.
    if let Some(ref asm) = module.asm {
        if let Err(e) = fs::write(path, asm) {
            diag_handler.err(&format!("failed to write asm file {}", e));
        }
    }
}

pub unsafe fn codegen(
    cgcx: &CodegenContext<IronOxCodegenBackend>,
    diag_handler: &Handler,
    module: ModuleCodegen<ModuleIronOx>,
    config: &ModuleConfig,
    timeline: &mut Timeline
) -> Result<CompiledModule, FatalError> {
    // FIXME: emit_bc_compressed
    if config.obj_is_bitcode || config.emit_bc {
        unimplemented!("IronOx can't emit bitcode!");
    }

    let module_name = module.name.clone();
    let module_name = Some(&module_name[..]);
    let obj_path =
        cgcx.output_filenames.temp_path(OutputType::Object, module_name);
    match module.kind {
        ModuleKind::Regular | ModuleKind::Allocator => {
            // The path to the assembly file in which to write the module.
            let asm_path =
                cgcx.output_filenames.temp_path(OutputType::Assembly, module_name);
            write_module(&module.module_llvm, &asm_path, diag_handler);
            // `no_integrated_as` must be explicitly set to 'true' for all 'regular'
            // modules. For 'metadata' and 'allocator' modules, `no_integrated_as` is
            // always set to `false`. `metadata` and `allocator` modules are not
            // assembled.
            if config.emit_obj {
                // The path to the object file in which to assemble the module.
                let obj_path =
                    cgcx.output_filenames.temp_path(OutputType::Object, module_name);
                // Run the assembler to produce the object file from the assembly.
                run_assembler(cgcx, diag_handler, &asm_path, &obj_path);
                if !config.emit_asm && !cgcx.save_temps {
                    fs::remove_file(&asm_path);
                }
            }
        },
        ModuleKind::Metadata => {
            eprintln!("not emitting: {:?}", obj_path);
        }
    }
    Ok(module.into_compiled_module(config.emit_obj,
                                   config.emit_bc,
                                   config.emit_bc_compressed,
                                   &cgcx.output_filenames))
}
