#![allow(dead_code)]
use rustc::ty::{self, TyCtxt, Instance};
use rustc::mir::{BasicBlock, BasicBlockData, Local, Place, Rvalue, StatementKind,
                 TerminatorKind, Operand};
use syntax_pos::symbol::LocalInternedString;

use super::super::ModuleIronOx;

use std::collections::HashMap;

macro_rules! asm {
    ($m:expr, $($args:expr)*) => {
        $(
            $m.asm.push_str(&format!("{}\n", $args));
        )*
    }
}

pub fn emit_function<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                                 module: &mut ModuleIronOx,
                                 inst: Instance<'tcx>) {
    let mangled_name = tcx.symbol_name(inst).as_str();
    asm!(module,
         format!(".type {fn_name},@function", fn_name=mangled_name)
         format!("{fn_name}:", fn_name=mangled_name)
    );

    let mir = tcx.instance_mir(inst.def);
    let mut local_map: HashMap<Local, u64> = HashMap::new();

    // Calculate the size of the stack
    let locals = &mir.local_decls;
    let mut stack_size = 0;
    for (local, decl) in locals.iter_enumerated().skip(mir.arg_count + 1) {
        let ty = decl.ty;
        //eprintln!("Local is {:?} {:?}", decl, ty.sty);
        let size = tcx.layout_of(ty::ParamEnv::reveal_all().and(ty))
            .ok().map_or(0, |x| x.size.bytes());
        stack_size += size;
        if size != 0 {
            // map local to its offset from rbp
            local_map.insert(local, stack_size + 8 /* the size of rbp */);
        }
    }
    emit_prologue(module, stack_size);
    eprintln!("The locals are {:?}", local_map);

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        emit_bb(tcx,
                module,
                mangled_name,
                bb_data,
                bb,
                &local_map);
    }

    emit_epilogue(module, stack_size);
    eprintln!("The size of the stack is {}", stack_size);
}

pub fn emit_bb<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                           module: &mut ModuleIronOx,
                           mangled_name: LocalInternedString,
                           bb_data: &BasicBlockData,
                           bb: BasicBlock,
                           locals: &HashMap<Local, u64>) {
    for stmt in &bb_data.statements {
        match stmt.kind {
            StatementKind::Assign(ref place, ref value) => {
                eprintln!("{:?} = {:?}", place, value);
                emit_assign(module, place, value, locals);
            },
            _ => {
                //unimplemented!("Statement kind: {:?}", stmt.kind);
            }
        }
    }

    let terminator = bb_data.terminator();
    eprintln!("Emitting {:?}", bb);
    eprintln!("\t{:?}", bb_data);
    eprintln!("Terminator is {:?}", terminator);
    asm!(module,
         format!("{}_{:?}:\n", mangled_name, bb)
    );
    let mut target_bb = None;
    match terminator.kind {
        TerminatorKind::Goto{ target: t } => {
            target_bb = Some(format!("{}_{:?}", mangled_name, t));
        },
        TerminatorKind::Call{ func: ref f, args:_,  destination: ref dest,
                              cleanup: _, from_hir_call: _ } => {
            target_bb = match dest {
                Some(d) => Some(format!("{}_{:?}", mangled_name, d.1)),
                None => panic!("No terminator for {:?}", bb_data)
            };
            eprintln!("{:?}", f);
            match f {
                Operand::Constant(ref c) => {
                    match c.literal.ty.sty {
                        ty::FnDef(def_id, _) => {
                            eprintln!("f is {:?}", f);
                            //let mangled_name = tcx.symbol_name(inst).as_str();
                        },
                        _ => {
                            unimplemented!("Cannot call {:?}", c.literal.ty.sty);
                        }
                    }
                    //asm!(module,
                         //format!("call {}") );
                },
                _ => {
                    panic!("Function call: move or copy");
                }
            }
            //let mangled_name = tcx.symbol_name(f).as_str();
            //asm!(module,
                 //"call {}", );
        },
        TerminatorKind::Return => {
            // do nothing for now
        },
        _ => {
            unimplemented!("term kind {:?}", terminator.kind);
        }
    }
    eprintln!("Emitting jmp {:?}", target_bb);
    if let Some(target) = target_bb {
        asm!(module,
             format!("jmp {}", target));
    }
}

fn emit_assign(module: &mut ModuleIronOx,
               place: &Place,
               value: &Rvalue,
               locals: &HashMap<Local, u64>) {
    match place {
        Place::Local(local) => {
            // find position on stack
            let stack_loc = match locals.get(local) {
                Some(l) => l,
                None => panic!("Could not find local {:?}", local)
            };
            eprintln!("Local is at {}(%rbp)", stack_loc);
            match *value {
                Rvalue::Use(ref operand) => {
                    match operand {
                        _ => {}
                        //asm!(module,
                             //format!("mov {},(%rbp), {},(%rbp)");
                    }
                },
                _ => {
                    eprintln!("Other rvalue: {:?}.\nNot emitting", *value);
                }
            };
        },
        _ => {
            eprintln!("Other place {:?}", place);
        }
    }
}

pub fn emit_prologue(module: &mut ModuleIronOx,
                     stack_size: u64) {
    asm!(module,
         "push %rbp"
         "mov %rsp, %rbp"
         format!("sub {}, %rsp", stack_size));
}

pub fn emit_epilogue(module: &mut ModuleIronOx,
                     stack_size: u64) {
    asm!(module,
         format!("add {}, %rsp", stack_size)
         "leave"
         "ret"
    );
}
