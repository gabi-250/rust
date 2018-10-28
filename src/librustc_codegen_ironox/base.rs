use super::{IronOxCodegenBackend, ModuleIronOx};
use rustc::mir::mono::Stats;
use rustc::ty::TyCtxt;
use rustc_codegen_ssa::back::write::submit_codegened_module_to_llvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};
use syntax_pos::symbol::InternedString;

use std::io::Cursor;
use x86asm::{InstructionWriter, Mnemonic, Mode, Operand, Reg};

pub fn compile_codegen_unit<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cgu_name: InternedString,
) -> Stats {
    let cgu = tcx.codegen_unit(cgu_name);
    let buffer = Cursor::new(Vec::new());
    let mut writer = InstructionWriter::new(buffer, Mode::Protected);
    let _bytes_written = writer
        .write2(
            Mnemonic::MOV,
            Operand::Direct(Reg::EAX),
            Operand::Literal32(7),
        ).unwrap();
    let codegened_module = ModuleIronOx {
        bytes: writer.get_inner_writer_ref().get_ref().to_vec(),
    };
    let module = ModuleCodegen {
        name: cgu_name.to_string(),
        module_llvm: codegened_module,
        kind: ModuleKind::Regular,
    };
    submit_codegened_module_to_llvm(&IronOxCodegenBackend(()), tcx, module, 0 as u64);
    Default::default()
}
