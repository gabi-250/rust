#![allow(dead_code)]
use rustc::ty::{self, TyCtxt, Instance};
use rustc::mir::{BasicBlock, BasicBlockData, Local, Place, Rvalue, StatementKind,
                 TerminatorKind, Operand};
use rustc::session::config::OutputType;
use rustc::util::time_graph::Timeline;
use rustc_codegen_ssa::{ModuleCodegen, CompiledModule};
use rustc_codegen_ssa::back::write::{CodegenContext, ModuleConfig, run_assembler};
use syntax_pos::symbol::LocalInternedString;
use errors::{Handler, FatalError};

use ModuleIronOx;
use IronOxCodegenBackend;

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

macro_rules! asm {
    ($m:expr, $($args:expr)*) => {
        $(
            $m.asm.push_str(&format!("{}\n", $args));
        )*
    }
}

fn write_module(
    module: &ModuleIronOx,
    path: &PathBuf,
    diag_handler: &Handler) {
    if let Err(e) = fs::write(path, module.asm()) {
        diag_handler.err(&format!("failed to write asm file {}", e));
    }
}

pub unsafe fn codegen(
    cgcx: &CodegenContext<IronOxCodegenBackend>,
    diag_handler: &Handler,
    module: ModuleCodegen<ModuleIronOx>,
    config: &ModuleConfig,
    timeline: &mut Timeline
) -> Result<CompiledModule, FatalError> {
    if config.no_integrated_as {
        eprintln!("config is {:?}", config.no_integrated_as);
        let module_name = Some(&module.name[..]);

        let assembly =
            cgcx.output_filenames.temp_path(OutputType::Assembly, module_name);
        write_module(&module.module_llvm, &assembly, diag_handler);


        let object = cgcx.output_filenames
            .temp_path(OutputType::Object, module_name);
        let filename = object.to_str().unwrap().to_string();
        eprintln!("Compiling {:?}", object);
        run_assembler(cgcx, diag_handler, &assembly, &object);
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
