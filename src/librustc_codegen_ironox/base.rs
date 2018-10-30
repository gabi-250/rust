use super::{IronOxCodegenBackend, ModuleIronOx};
use rustc::mir::mono::Stats;
use rustc::mir::StatementKind;
use rustc::ty::TyCtxt;
use rustc_codegen_ssa::back::write::submit_codegened_module_to_llvm;
use rustc_codegen_ssa::{ModuleCodegen, ModuleKind};
use rustc_mir::monomorphize::MonoItem;
use syntax_pos::symbol::InternedString;

use std::collections::HashMap;
use std::io::Cursor;
use x86asm::{InstructionWriter, Mnemonic, Mode, Operand, Reg};

pub fn compile_codegen_unit<'ll, 'tcx>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    cgu_name: InternedString,
) -> Stats {
    let cgu = tcx.codegen_unit(cgu_name);

    let buffer = Cursor::new(Vec::new());
    let mut writer = InstructionWriter::new(buffer, Mode::Long);
    let mut locals = HashMap::new();
    for (&mono_item, &linkage) in cgu.items() {
        eprintln!("Mono item is {:?}", mono_item);
        match mono_item {
            MonoItem::Fn(inst) => {
                let mir = tcx.instance_mir(inst.def);
                eprintln!("Arg count is {:?}", mir.arg_count);
                let mut stack_size = 0;
                for (i, bb) in mir.basic_blocks().iter().enumerate() {
                    eprintln!("bb{}", i);
                    for stmt in &bb.statements {
                        eprintln!("\tStatement: {:?}", stmt);
                        match stmt.kind {
                            StatementKind::StorageLive(a) => {
                                eprintln!("\t\tLive: {:?}",a);
                                let _bytes_written = writer
                                    .write1(
                                        Mnemonic::PUSH,
                                        Operand::Literal32(0),
                                    ).unwrap();
                                stack_size += 8;
                                locals.insert(a, stack_size);
                            },
                            StatementKind::StorageDead(a) => {
                                eprintln!("\t\tDead: {:?}", a);
                                let _bytes_written = writer
                                    .write2(
                                        Mnemonic::ADD,
                                        Operand::Direct(Reg::RSP),
                                        Operand::Literal32(8),
                                    ).unwrap();
                                stack_size -= 8;
                                locals.remove(&a);
                            },
                            StatementKind::Assign(ref place, ref rv) => {
                                eprintln!("\t\t{:?} = {:?}", place, rv);
                            },
                            _ => {
                                eprintln!("\t\tOther statement");
                            }
                        }
                    }
                    eprintln!("\tTerminator: {:?}", bb.terminator());
                }
            },
            _ => {
                eprintln!("Other mono item");
            }
        }
    }

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
