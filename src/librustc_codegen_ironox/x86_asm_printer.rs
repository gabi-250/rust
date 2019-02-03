use super::ModuleIronOx;

use context::CodegenCx;
use ir::value::Value;
use ir::instruction::Instruction;

use rustc::mir::mono::Stats;
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::traits::MiscMethods;
use std::cell::{Cell, RefCell};

/// The result of evaluating an `Instruction`.
#[derive(Debug)]
pub struct InstrAsm {
    /// The sequence of assembly instructions the instruction is compiled to.
    asm: String,
    /// The register/memory address that contains the result of the instruction.
    result: String,
}

/// A module that contains the context for generating machine instructions from
/// a `ModuleIronOx`.
pub struct ModuleAsm<'ll, 'tcx> {
    /// The codegen context, which also contains the `ModuleIronOx` to be compiled.
    cx: CodegenCx<'ll, 'tcx>,
    /// A mapping from high-level instructions to the assembly they compile to.
    /// If the same instruction is used in two different expressions, it is
    /// often enough to retrieve its result from this mapping (it does not always
    /// have to be recompiled).
    compiled_insts: RefCell<FxHashMap<Value, InstrAsm>>,
    /// A mapping from function parameters to their location on the stack.
    /// Currently, parameters are always pushed to and retrieved from the stack.
    compiled_params: RefCell<FxHashMap<Value, InstrAsm>>,
}

impl ModuleAsm<'ll, 'tcx> {
    pub fn new(cx: CodegenCx<'ll, 'tcx>) -> ModuleAsm<'ll, 'tcx> {
        ModuleAsm {
            cx,
            compiled_insts: Default::default(),
            compiled_params: Default::default(),
        }
    }

    /// Return the x86-64 instructions that correspond to this module.
    pub fn compile(&mut self) -> String {
        unimplemented!("compile");
    }
}

pub fn compile(mut module: ModuleAsm) -> (Stats, String) {
    let asm = module.compile();
    (module.cx.consume_stats().into_inner(), asm)
}
