use rustc::ty::{self, Ty, TypeFoldable, TyCtxt, Instance};
use rustc::ty::subst::Substs;
use rustc::mir::{BasicBlock, BasicBlockData, Local, Place, Rvalue, StatementKind,
                 TerminatorKind, Operand, ProjectionElem};
use rustc_data_structures::indexed_vec::Idx;
use syntax_pos::symbol::LocalInternedString;

use super::super::ModuleIronOx;

use std::collections::HashMap;

use back::registers::GPR;

macro_rules! asm {
    ($m:expr, $($args:expr)*) => {
        $(
            $m.asm.push_str(&format!("{}\n", $args));
        )*
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum MachineLocal {
    Register(GPR),
    RbpOffset(isize),
}

impl ToString for MachineLocal {
    fn to_string(&self) -> String {
        match self {
            MachineLocal::Register(r) => r.to_string(),
            MachineLocal::RbpOffset(bytes) => format!("{}(%rbp)", bytes),
        }
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
    let mut local_map: HashMap<Local, MachineLocal> = HashMap::new();

    // Calculate the size of the stack
    let locals = &mir.local_decls;
    let mut stack_size = get_local_size(
        tcx,
        locals.iter().nth(0).unwrap().ty,
        inst.substs);
    // the return address the local _0:
    local_map.insert(Local::new(0),
                     MachineLocal::RbpOffset(stack_size as isize));

    // function arguments:
    for (local, decl) in locals.iter_enumerated().skip(1).take(mir.arg_count) {
        let ty = decl.ty;
        let index = local.index();
        if index == 1 {
            local_map.insert(local, MachineLocal::Register(GPR::RDI));
        } else if index == 2 {
            local_map.insert(local, MachineLocal::Register(GPR::RSI));
        } else if index == 3 {
            local_map.insert(local, MachineLocal::Register(GPR::RDX));
        } else if index == 4 {
            local_map.insert(local, MachineLocal::Register(GPR::RCX));
        } else if index == 5 {
            local_map.insert(local, MachineLocal::Register(GPR::R8));
        } else if index == 6 {
            local_map.insert(local, MachineLocal::Register(GPR::R9));
        } else {
            unimplemented!("Function args on the stack");
        }
    }

    for (local, decl) in locals.iter_enumerated().skip(mir.arg_count + 1) {
        let ty = decl.ty;
        let size = get_local_size(tcx, ty, inst.substs);
        stack_size += size;
        if size != 0 {
            // map local to its offset from rbp
            local_map.insert(local, MachineLocal::RbpOffset(stack_size as isize));
        }
    }
    emit_prologue(module, stack_size);

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        emit_bb(tcx,
                module,
                mangled_name,
                bb_data,
                bb,
                &local_map);
    }

    emit_epilogue(module, stack_size);
}

fn monomorphize<'ll, 'tcx, T> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                               ty: &T,
                               param_substs: &'tcx Substs<'tcx>) -> T
    where T: TypeFoldable<'tcx>
{
    tcx.subst_and_normalize_erasing_regions(
        param_substs,
        ty::ParamEnv::reveal_all(),
        ty,
    )
}

fn get_local_size<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                              ty: Ty<'tcx>,
                              param_substs: &'tcx Substs<'tcx>) -> u64 {
    let ty = monomorphize(tcx, &ty, param_substs);
    tcx.layout_of(ty::ParamEnv::reveal_all().and(&ty)).unwrap().size.bytes()
}

pub fn emit_bb<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                           module: &mut ModuleIronOx,
                           mangled_name: LocalInternedString,
                           bb_data: &'tcx BasicBlockData,
                           bb: BasicBlock,
                           locals: &HashMap<Local, MachineLocal>) {
    for stmt in &bb_data.statements {
        match stmt.kind {
            StatementKind::Assign(ref place, ref value) => {
                emit_assign(tcx, module, place, value, locals);
            },
            StatementKind::StorageLive(_) |
            StatementKind::StorageDead(_) => {
                // nothing to do
            },
            _ => {
                unimplemented!("Statement kind: {:?}", stmt.kind);
            }
        }
    }

    let terminator = bb_data.terminator();
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
                None => { /* this is a non-terminating call */ None }
            };
            let op = get_operand(tcx, &f, locals);
            asm!(module,
                 format!("call {}", op));
        },
        TerminatorKind::Drop { ref location, target, unwind } => {
            target_bb = Some(format!("{}_{:?}", mangled_name, target));
        },
        TerminatorKind::Return => {
            // do nothing for now
        },
        TerminatorKind::Resume => {
            // do nothing for now...
        },
        _ => {
            unimplemented!("term kind {:?}", terminator.kind);
        }
    }
    if let Some(target) = target_bb {
        asm!(module,
             format!("jmp {}", target));
    }
}

pub fn get_place<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                             place: &Place,
                             locals: &HashMap<Local, MachineLocal>) -> String {
    match place {
        Place::Local(local) => {
            // find position on stack
            let local = locals.get(local).expect("Could not find local");

            local.to_string()
        },
        Place::Projection(ref proj) => {
            let base = get_place(tcx, &proj.base, locals);
            match proj.elem {
                ProjectionElem::Field(f, t) => {
                    // XXX get the size of t
                    let index = f.index();
                    let t = 8;
                    format!("({base}, {index}, {scale})",
                            base=base,
                            index=index,
                            scale=t)
                },
                ProjectionElem::Deref => {
                    format!("({base})", base=base)
                },
                _ => {
                    unimplemented!("Projection element {:?}", proj.elem);
                }
            }
        },
        _ => {
            unimplemented!("Place {:?}", place);
        }
    }
}

/*
fn fully_evaluate<'ll, 'tcx>(
    tcx: TyCtxt <'ll, 'tcx, 'tcx>,
    constant: &'tcx ty::Const<'tcx>,
) -> Result<&'tcx ty::Const<'tcx>, Lrc<ConstEvalErr<'tcx>>> {
    match constant.val {
        ConstValue::Unevaluated(def_id, ref substs) => {
            let param_env = ty::ParamEnv::reveal_all();
            let instance = ty::Instance::resolve(tcx, param_env, def_id, substs).unwrap();
            let cid = GlobalId {
                instance,
                promoted: None,
            };
            tcx.const_eval(param_env.and(cid))
        },
        _ => Ok(constant),
    }
}

pub fn eval_mir_constant<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                                     constant: &mir::Constant<'tcx>)
    -> Result<&'tcx ty::Const<'tcx>, Lrc<ConstEvalErr<'tcx>>> {

        let c = tcx.subst_and_normalize_erasing_regions(
            self.param_substs, // XXX ???
            ty::ParamEnv::reveal_all(),
            constant,
        )
        let c = monomorphize(tcx, constant.literal.ty, &constant.literal);
        fully_evaluate(tcx, &c)
}*/

pub fn get_operand<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                               operand: &'tcx Operand,
                               locals: &HashMap<Local, MachineLocal>) -> String {
    match operand {
        Operand::Copy(ref place) => {
            get_place(tcx, &place, locals)
        }
        Operand::Constant(ref c) => {
            // XXX c has type parameters
            // let c = eval_mir_constant(tcx, c).unwrap();
            match c.literal.ty.sty {
                ty::FnDef(def_id, substs) => {
                    let fn_name = tcx.def_symbol_name(def_id);
                    eprintln!("fn name is {:?}", fn_name);
                    let instance = Instance::mono(tcx, def_id);
                    eprintln!("{:?}", instance);
                    let fn_name = tcx.symbol_name(instance);
                    eprintln!("fn name is {:?}", fn_name);
                    fn_name.to_string()
                },
                _ => {
                    unimplemented!("Cannot call {:?}", c.literal.ty.sty);
                }
            }
        },
        Operand::Move(place) => {
            get_place(tcx, place, locals)
        },
    }
}

pub fn get_rvalue<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                              value: &'tcx Rvalue,
                              locals: &HashMap<Local, MachineLocal>) -> String {
    match *value {
        Rvalue::Use(ref operand) => {
            get_operand(tcx, &operand, locals)
        },
        Rvalue::Cast(kind, ref operand, ty) => {
            get_operand(tcx, &operand, locals)
        },
        Rvalue::Ref(_, _, ref place) => {
            get_place(tcx, &place, locals)
        },
        _ => {
            unimplemented!("Rvalue: {:?}", *value);
        }
    }
}

pub fn emit_assign<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>,
                               module: &mut ModuleIronOx,
                               place: &Place,
                               value: &'tcx Rvalue,
                               locals: &HashMap<Local, MachineLocal>) {
    let place = get_place(tcx, place, locals);
    let value = get_rvalue(tcx, value, locals);
    asm!(module,
         format!("mov {}, %rax", value)
         format!("mov %rax, {}", place));
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
