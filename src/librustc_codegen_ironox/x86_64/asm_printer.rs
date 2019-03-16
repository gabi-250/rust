use context::CodegenCx;
use ir::basic_block::OxBasicBlock;
use ir::function::OxFunction;
use ir::instruction::{CompOp, OxInstruction};
use ir::type_::{OxType, ScalarType, Type};
use ir::value::Value;

use rustc::mir::mono::{Stats, Visibility};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::traits::{BaseTypeMethods, MiscMethods};
use std::cell::RefCell;

use x86_64::gas_directive::{BigNum, GasDirective, GasType};
use x86_64::instruction::MachineInst;
use x86_64::register::GeneralPurposeReg::{self, *};
use x86_64::register::{Location, Operand, Register, SubRegister, AccessMode,
                      operand_access_mode, access_mode};


const FN_ARG_REGS: [GeneralPurposeReg; 6] = [RDI, RSI, RDX, RCX, R8, R9];

/// The result of evaluating an `Instruction`.
#[derive(Clone, Debug)]
pub struct CompiledInst {
    /// The sequence of assembly instructions the instruction is compiled to.
    asm: Vec<MachineInst>,
    /// The register/memory address that contains the result of the instruction.
    result: Option<Operand>,
}

impl CompiledInst {
    /// Create a new `CompiledInst` from a vector of machine instructions.
    pub fn new(asm: Vec<MachineInst>, result: Operand) -> CompiledInst {
        CompiledInst { asm, result: Some(result) }
    }

    /// Create a `CompiledInst` with a result, and without any other machine
    /// instructions.
    pub fn with_result(result: Operand) -> CompiledInst {
        CompiledInst { asm: vec![], result: Some(result) }
    }

    /// Create a `CompiledInst` with a list of machine instructions, but without
    /// a result.
    pub fn with_instructions(asm: Vec<MachineInst>) -> CompiledInst {
        CompiledInst { asm, result: None }
    }
}

/// A module that contains the context for generating machine instructions from
/// a `ModuleIronOx`.
pub struct FunctionPrinter<'a, 'll, 'tcx> {
    /// The codegen context, which also contains the `ModuleIronOx` to be compiled.
    cx: &'a CodegenCx<'ll, 'tcx>,
    /// A mapping from high-level instructions to the assembly they compile to.
    /// If the same instruction is used in two different expressions, it is
    /// often enough to retrieve its result from this mapping (it does not always
    /// have to be recompiled).
    compiled_insts: RefCell<FxHashMap<Value, CompiledInst>>,
    /// A mapping from function parameters to their location on the stack.
    /// Currently, parameters are always pushed to and retrieved from the stack.
    compiled_params: RefCell<FxHashMap<Value, CompiledInst>>,
    inst_stack_loc: RefCell<FxHashMap<OxInstruction, CompiledInst>>,
}

impl FunctionPrinter<'a, 'll, 'tcx> {
    /// Create a new function printer.
    fn new(cx: &'a CodegenCx<'ll, 'tcx>) -> FunctionPrinter<'a, 'll, 'tcx> {
        FunctionPrinter {
            cx,
            compiled_insts: Default::default(),
            compiled_params: Default::default(),
            inst_stack_loc: Default::default(),
        }
    }

    fn codegen_function(&self, f: &OxFunction) -> Vec<MachineInst> {
        let mut asm = vec![MachineInst::Label(f.name.clone())];
        let (mut param_movs, stack_size) = self.compute_stack_size(&f);
        asm.append(&mut FunctionPrinter::emit_prologue(stack_size));
        for param in &f.params {
            let mut inst_asm = self.codegen_value(*param);
            asm.append(&mut inst_asm.asm);
        }
        asm.append(&mut param_movs);
        for bb in &f.basic_blocks {
            asm.push(MachineInst::Label(bb.label.clone()));
            asm.append(&mut self.codegen_block(&bb));
        }
        asm
    }

    fn get_func_arg(idx: usize) -> GeneralPurposeReg {
        if idx < 6 {
            return FN_ARG_REGS[idx];
        } else {
            unimplemented!("function arg no {}", idx);
        }
    }

    fn codegen_value(&self, value: Value) -> CompiledInst {
        let module = self.cx.module.borrow();
        match value {
            Value::ConstUint(idx) => {
                let imm_size = self.cx.val_ty(value).size(&self.cx.types.borrow());
                let acc_mode = access_mode(imm_size as u64);
                let value = self.cx.u_consts.borrow()[idx].value;
                let result = Operand::Immediate(value as isize, acc_mode);
                CompiledInst::with_result(result)
            }
            Value::Param(..) => {
                if let Some(instr_asm) = self.compiled_params.borrow().get(&value) {
                    let result = instr_asm.result.clone();
                    CompiledInst::with_result(result.unwrap())
                } else {
                    bug!("param should've already been compiled!");
                }
            }
            Value::Instruction(..) => {
                // If the instruction has already been compiled, return its
                // cached result.
                if let Some(inst_asm) = self.compiled_insts.borrow().get(&value) {
                    // if this instruction is a call, store its result (if it has
                    // one)
                    let compiled_inst = if let Some(ref result) = inst_asm.result {
                        CompiledInst::with_result(result.clone())
                    } else {
                        CompiledInst::with_instructions(vec![])
                    };
                    return compiled_inst;
                }
                self.codegen_instruction(value)
            }
            Value::Global(idx) => {
                let global = &self.cx.globals.borrow()[idx];
                let result = Operand::Loc(Location::RipOffset(global.name.clone()));
                CompiledInst::with_result(result)
            }
            Value::Cast(idx) => {
                self.codegen_value(self.cx.const_casts.borrow()[idx].value)
            }
            Value::Function(idx) => {
                // Move the result to the stack, and assume its size is 8.
                let function_name = if !module.functions[idx].is_codegenned() {
                    format!("{}@PLT", &module.functions[idx].name)
                } else {
                    format!("{}(%rip)", &module.functions[idx].name)
                };
                // %rax
                let result = Location::from(Register::direct(RAX));
                let asm = vec![
                    MachineInst::lea(Operand::Sym(function_name), result.clone())];
                CompiledInst::new(asm, Operand::from(result))
            }
            Value::ConstGep { ptr_idx, offset } => {
                let const_struct = &self.cx.const_structs.borrow()[ptr_idx];
                let const_struct = format!("{}(%rip)", const_struct.name);
                let const_struct = Operand::Sym(const_struct);
                // FIXME: this clobbers r11
                let r11 = Register::direct(R11);
                let asm = vec![
                    MachineInst::lea(const_struct, r11),
                    MachineInst::add(
                        Operand::Immediate(offset as isize, AccessMode::Full), r11),
                ];
                CompiledInst::new(asm, Operand::Loc(Location::Reg(r11)))
            }
            _ => {
                unimplemented!("codegen_value({:?})", value);
            }
        }
    }

    fn codegen_and(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::And { lhs, rhs } = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.codegen_value(*lhs);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.codegen_value(*rhs);
            asm.append(&mut instr_asm2.asm);
            // the size in bits
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg1 = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            let reg2 = Register::direct(SubRegister::reg(RCX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(instr_asm1.result.unwrap(), reg1),
                MachineInst::mov(instr_asm2.result.unwrap(), reg2),
                MachineInst::and(reg2, reg1),
            ]);
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(reg1, result.clone()));
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::And, found {:?}", inst);
        }
    }


    fn codegen_add(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Add { lhs, rhs } = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.codegen_value(*lhs);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.codegen_value(*rhs);
            asm.append(&mut instr_asm2.asm);
            // The size in bits.
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg1 = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            let reg2 = Register::direct(SubRegister::reg(RCX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(instr_asm1.result.unwrap(), reg1),
                MachineInst::mov(instr_asm2.result.unwrap(), reg2),
                MachineInst::add(reg2, reg1),
            ]);
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(reg1, result.clone()));
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Add, found {:?}", inst);
        }
    }

    fn codegen_sub(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Sub { lhs, rhs } = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.codegen_value(*lhs);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.codegen_value(*rhs);
            asm.append(&mut instr_asm2.asm);
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg1 = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            let reg2 = Register::direct(SubRegister::reg(RCX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(instr_asm1.result.unwrap(), reg1),
                MachineInst::mov(instr_asm2.result.unwrap(), reg2),
                MachineInst::sub(reg2, reg1),
            ]);
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(reg1, result.clone()));
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Sub, found {:?}", inst);
        }
    }

    fn codegen_not(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Not(v) = inst {
            let mut asm = vec![];
            let mut instr_asm = self.codegen_value(*v);
            asm.append(&mut instr_asm.asm);
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            let mask = Register::direct(SubRegister::reg(RCX, access_mode(size)));
            let result = self.precompiled_result(inst);
            asm.extend(vec![
                MachineInst::mov(instr_asm.result.unwrap(), reg),
                MachineInst::xor(mask, mask),
                MachineInst::mov(Operand::Immediate(1, AccessMode::Full), reg),
                MachineInst::not(mask),
                MachineInst::not(reg),
                MachineInst::xor(mask, reg),
                MachineInst::mov(reg, result.clone()),
            ]);
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Not, found {:?}", inst);
        }
    }

    fn codegen_call(&self, inst: &OxInstruction) -> CompiledInst {
        let (callee, args) = match inst {
            OxInstruction::Call { callee, ref args } |
            OxInstruction::Invoke { callee, ref args, .. } => (callee, args),
            _ => bug!("Expected invoke or call, found {:?}", inst)
        };
        let mut asm = vec![];
        for arg in args.iter() {
            // Compile everything before actually passing the argument to the
            // called function (this enusres we don't clobber any registers we
            // actually need in the function call).
            self.codegen_value(*arg);
        }
        // Prepare the function arguments.
        for (idx, arg) in args.iter().take(6).enumerate() {
            let mut instr_asm = self.codegen_value(*arg);
            asm.append(&mut instr_asm.asm);
            let param = FunctionPrinter::get_func_arg(idx);
            // FIXME: always handle globals correctly
            if let Some(Operand::Loc(Location::RipOffset(_))) = instr_asm.result.clone() {
                // globals are always treated as addresses
                asm.push(MachineInst::lea(instr_asm.result.unwrap(),
                                          Register::direct(param)));
            } else {
                asm.push(MachineInst::mov(instr_asm.result.unwrap(),
                                          Register::direct(param)));
            }
        }
        let remaining_args: Vec<&Value> = args.iter().skip(6).collect();
        let mut total_arg_size = 0;

        for arg in remaining_args.iter().rev() {
            let arg_size = self.cx.val_ty(**arg).size(&self.cx.types.borrow());
            total_arg_size += arg_size / 8;
        }
        let padding = total_arg_size % 16;
        if remaining_args.len() > 0 {
            // Align the stack:
            asm.push(MachineInst::sub(
                Operand::Immediate(padding as isize, AccessMode::Full),
                Register::direct(RSP)));
        }

        // The remaining arguments are pushed to the stack in reverse order.
        for arg in remaining_args.iter().rev() {
            let mut instr_asm = self.codegen_value(**arg);
            asm.append(&mut instr_asm.asm);
            let arg_size = self.cx.val_ty(**arg).size(&self.cx.types.borrow());
            let acc_mode = access_mode(arg_size);
            let rax = Register::direct(SubRegister::reg(RAX, acc_mode));

            if let Some(Operand::Loc(Location::RipOffset(_))) = instr_asm.result.clone() {
                // globals are always treated as addresses
                asm.push(MachineInst::lea(instr_asm.result.unwrap(), rax));
            } else {
                asm.push(MachineInst::mov(instr_asm.result.unwrap(), rax));
            }
            asm.push(MachineInst::push(rax));
        }
        let remaining_args = remaining_args.len();
        match callee {
            Value::Function(idx) => {
                let module = self.cx.module.borrow();
                // Move the result to the stack, and assume its size is 8.
                let fn_name = if !module.functions[*idx].is_codegenned() {
                    format!("{}@PLT", &module.functions[*idx].name)
                } else {
                    module.functions[*idx].name.clone()
                };
                asm.push(MachineInst::call(fn_name.clone()));
                if total_arg_size > 0 {
                    // Pop all the pushed arguments:
                    asm.push(
                        MachineInst::add(
                            Operand::Immediate((total_arg_size + padding) as isize,
                                               AccessMode::Full),
                            Register::direct(RSP)));
                }
                let ret_ty = inst.val_ty(self.cx);
                // If the function doesn't return anything, carry on.
                if let OxType::Void = self.cx.types.borrow()[ret_ty] {
                    CompiledInst::with_instructions(asm)
                } else {
                    let result = self.precompiled_result(inst);
                    let acc_mode = result.access_mode();
                    match acc_mode {
                        AccessMode::Large(bytes) if bytes <= 16 => {
                            let am_full = AccessMode::Full;
                            let offset = result.rbp_offset();
                            let result1 = Location::RbpOffset(offset, am_full);
                            let am2 = access_mode(8 * (bytes - 8));
                            let result2 = Location::RbpOffset(offset + 8, am2);
                            asm.extend(vec![
                                MachineInst::mov(
                                    Register::direct(
                                        SubRegister::reg(RAX, AccessMode::Full)),
                                    result1),
                                // Load RDX at result + 8
                                MachineInst::mov(
                                    Register::direct(
                                        SubRegister::reg(RDX, am2)),
                                    result2),
                            ]);
                        },
                        AccessMode::Large(bytes) => {
                            unimplemented!("AccessMode::Large({}))", bytes);
                        },
                        _ => {
                            asm.push(MachineInst::mov(
                                Register::direct(SubRegister::reg(RAX, acc_mode)),
                                result.clone()));
                        }
                    };
                    CompiledInst::new(asm, result)
                }
            },
            Value::Instruction(_, _, _) => {
                let ptr = self.codegen_value(*callee);
                let result = ptr.result.clone().unwrap();
                let acc_mode = result.access_mode();
                let reg = Register::direct(SubRegister::reg(RAX, acc_mode));
                // dereference result
                asm.extend(vec![
                    MachineInst::call(result.clone().deref()),
                    MachineInst::mov(reg, result.clone()),
                ]);
                CompiledInst::new(asm, result)
            },
            _ => unimplemented!("call to {:?}", callee),
        }
    }

    fn codegen_icmp(&self, inst: &OxInstruction) -> CompiledInst {
        match *inst {
            OxInstruction::Icmp { lhs, rhs, op } => {
                // These instructions have a boolean result.
                let cmp_res = self.precompiled_result(inst);
                let mut asm = vec![];
                let mut instr_asm1 = self.codegen_value(lhs);
                asm.append(&mut instr_asm1.asm);
                let mut instr_asm2 = self.codegen_value(rhs);
                asm.append(&mut instr_asm2.asm);
                let result1 = instr_asm1.result.unwrap();
                let result2 = instr_asm2.result.unwrap();
                // Move the value into a register and compare it with the second value.
                let reg = Register::direct(
                    SubRegister::reg(RAX, operand_access_mode(&result1, &result2)));
                // FIXME: check the instruction and set RCX to cmp result.
                // mov cmp result to cmp_res
                asm.extend(vec![
                    MachineInst::mov(result1, reg),
                    MachineInst::cmp(result2, reg),
                ]);

                let reg = Register::direct(SubRegister::reg(RAX, AccessMode::Low8));
                let set = match op {
                    CompOp::Eq => MachineInst::sete(reg),
                    CompOp::Ne => MachineInst::setne(reg),
                    CompOp::Ugt => MachineInst::seta(reg),
                    CompOp::Sgt => MachineInst::setg(reg),
                    CompOp::Uge => MachineInst::setae(reg),
                    CompOp::Sge => MachineInst::setge(reg),
                    CompOp::Ult => MachineInst::setb(reg),
                    CompOp::Slt => MachineInst::setl(reg),
                    CompOp::Ule => MachineInst::setbe(reg),
                    CompOp::Sle => MachineInst::setle(reg),
                };
                asm.extend(vec![
                    set,
                    MachineInst::mov(reg, cmp_res.clone()),
                ]);
                CompiledInst::new(asm, cmp_res)
            },
            _ => bug!("expected a comparison instruction, found {:?}", inst),
        }
    }

    fn codegen_condbr(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::CondBr { cond, then_bb, else_bb } = inst {
            let mut asm = vec![];
            match cond {
                Value::Instruction(..) => {
                    let cond_val = self.codegen_instruction(*cond).result.unwrap();
                    let true_reg = Register::direct(
                        SubRegister::reg(RCX, AccessMode::Low8));
                    let cond_reg = Register::direct(
                        SubRegister::reg(RDI, AccessMode::Low8));
                    asm.extend(vec![
                        // Check if the condition is true
                        MachineInst::mov(
                            Operand::Immediate(1, AccessMode::Low8), true_reg),
                        MachineInst::mov(cond_val, cond_reg),
                        // Compile the result of evaluating the condition with 1.
                        MachineInst::cmp(cond_reg, true_reg),
                        // If the condition is true.
                        MachineInst::je(then_bb.to_string()),
                    ]);
                    asm.push(MachineInst::jmp(else_bb.to_string()));
                    CompiledInst::with_instructions(asm)
                }
                _ => {
                    bug!("cond must be a Value::Instruction, not {:?}", cond);
                }
            }
        } else {
            bug!("expected OxInstruction::CondBr, found {:?}", inst);
        }
    }

    fn codegen_br(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Br(target) = inst {
            CompiledInst::with_instructions(
                vec![MachineInst::jmp(target.to_string())])
        } else {
            bug!("expected OxInstruction::Br, found {:?}", inst);
        }
    }

    fn codegen_store(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Store { ptr, val } = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.codegen_value(*ptr);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.codegen_value(*val);
            asm.append(&mut instr_asm2.asm);
            let ptr_result = instr_asm1.result.clone().unwrap();
            let val_result = instr_asm2.result.unwrap();
            let val_am = val_result.access_mode();
            match val_am {
                AccessMode::Large(16) => {
                    match (&val_result, &ptr_result) {
                        (Operand::Loc(Location::RbpOffset(val_offset, _sam)),
                         Operand::Loc(Location::RbpOffset(_dst_offset, dam))) => {
                            let am_full = AccessMode::Full;
                            // Move two quad words from val to dst.
                            let reg =
                                Register::direct(SubRegister::reg(RCX, am_full));
                            let val_word1 = Operand::Loc(
                                Location::RbpOffset(*val_offset, am_full));
                            let val_word2 = Operand::Loc(
                                Location::RbpOffset(val_offset + 8, am_full));
                            // check if ptr result is a ptr?
                            asm.extend(vec![
                                // Move the first word.
                                MachineInst::mov(ptr_result.clone(),
                                                 Register::direct(RAX)),
                                MachineInst::mov(val_word1, reg),
                                MachineInst::mov(reg, Register::indirect(RAX))
                            ]);
                            if let AccessMode::Large(16) = dam {
                                asm.extend(vec![
                                    // Move the second word.
                                    MachineInst::add(Operand::Immediate(8, am_full),
                                                     Register::direct(RAX)),
                                    MachineInst::mov(val_word2, reg),
                                    MachineInst::mov(reg, Register::indirect(RAX)),
                                ]);
                            }
                         },
                         _ => {
                            bug!("Invalid val-dst pair: ({:?}, {:?})",
                                 val_result, ptr_result)
                         }
                    }
                },
                AccessMode::Large(bytes) => {
                    unimplemented!("AccessMode::Large({})", bytes)
                },
                _ => {
                    let access_mode = val_result.access_mode();
                    let reg =
                        Register::direct(SubRegister::reg(RCX, access_mode));
                    asm.extend(vec![
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::mov(ptr_result, Register::direct(RAX)),
                        MachineInst::mov(val_result, reg),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                    ]);
                }
            }
            CompiledInst::new(asm, instr_asm1.result.unwrap())
        } else {
            bug!("expected OxInstruction::Store, found {:?}", inst);
        }
    }

    fn codegen_load(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Load { ptr, .. } = inst {
            let mut asm = vec![];
            let mut instr_asm = self.codegen_value(*ptr);
            // FIXME:
            asm.append(&mut instr_asm.asm);
            let inst_size =
                inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let acc_mode = access_mode(inst_size as u64);
            let result = self.precompiled_result(inst);
            match acc_mode {
                AccessMode::Large(16) => {
                    asm.extend(vec![
                        MachineInst::mov(instr_asm.result.unwrap(),
                                         Register::direct(RAX)),
                        // Copy the first quad-word.
                        MachineInst::mov(Register::indirect(RAX),
                                         Register::direct(RDI)),
                        // Copy the second quad-word.
                        MachineInst::add(Operand::Immediate(8, AccessMode::Full),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::indirect(RAX),
                                         Register::direct(RSI)),

                        MachineInst::lea(result.clone(),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::direct(RDI),
                                         Register::indirect(RAX)),
                        // Move the second word...
                        MachineInst::add(Operand::Immediate(8, AccessMode::Full),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::direct(RSI),
                                         Register::indirect(RAX)),
                    ]);

                },
                AccessMode::Large(_) => bug!("Unsupported mode: {:?}", acc_mode),
                _ => {
                    asm.extend(vec![
                        MachineInst::mov(instr_asm.result.unwrap(),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::indirect(RAX),
                                         Register::direct(SubRegister::reg(RAX,
                                                                           acc_mode))),
                        MachineInst::mov(Register::direct(SubRegister::reg(RAX,
                                                                           acc_mode)),
                                         result.clone()),
                    ]);
                }
            }
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Load, found {:?}", inst);
        }
    }

    fn codegen_checkoverflow(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::CheckOverflow { signed, .. } = inst {
            let result = self.precompiled_result(inst);
            // Get register %dl.
            let reg = Register::direct(SubRegister::reg(RDX, AccessMode::Low8));
            let set_inst = if *signed {
                MachineInst::setno(reg)
            } else {
                MachineInst::setnb(reg)
            };
            let asm = vec![
                set_inst,
                MachineInst::mov(reg, result.clone()),
            ];
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::CheckOverflow, found {:?}", inst);
        }
    }

    fn codegen_cast(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Cast { val, ty } = inst {
            let stack_loc = self.precompiled_result(inst);
            let mut v = self.codegen_value(*val);
            if let Some(mut op) = v.result {
                let old_acc_mode = op.access_mode();
                let val_size = ty.size(&self.cx.types.borrow());
                let new_acc_mode = access_mode(val_size);
                if old_acc_mode < new_acc_mode {
                    // This is a widening cast. When switching to a larger
                    // subregister for example (%ax -> %rax), make sure there
                    // is no junk leftover in the higher order bits.
                    let reg_new = Register::direct(
                        SubRegister::reg(RAX, new_acc_mode));
                    let reg_old = Register::direct(
                        SubRegister::reg(RAX, old_acc_mode));
                    v.asm.extend(vec![
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::NOP,
                        MachineInst::xor(reg_new, reg_new),
                        MachineInst::mov(op, reg_old),
                        MachineInst::mov(reg_new, stack_loc.clone()),
                    ]);
                } else {
                    let old_acc_mode = op.access_mode();
                    let rax = Register::direct(SubRegister::reg(RAX, old_acc_mode));
                    v.asm.extend(vec![
                        MachineInst::mov(op, rax),
                        MachineInst::mov(rax, stack_loc.clone())
                    ]);
                }
                v.result = Some(stack_loc);
            }
            v
        } else {
            bug!("expected OxInstruction::Cast, found {:?}", inst);
        }
    }

    fn codegen_ret(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Ret(value) = inst {
            let mut asm = vec![];
            if let Some(val) = value {
                asm.push(
                    MachineInst::xor(Register::direct(RAX), Register::direct(RAX)));
                let mut instr_asm = self.codegen_value(*val);
                asm.append(&mut instr_asm.asm);
                let result = instr_asm.result.unwrap();
                let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
                if size <= 64 {
                    let reg_full =
                        Register::direct(SubRegister::reg(RAX, AccessMode::Full));
                    let reg =
                        Register::direct(SubRegister::reg(RAX, access_mode(size)));
                    asm.extend(vec![
                        // If we aren't using the entire %rax, make sure there
                        // is no junk left over in the higher order bits.
                        MachineInst::xor(reg_full, reg_full),
                        MachineInst::mov(result, reg),
                    ]);
                } else if size <= 128 {
                    // The return value is placed in RAX:RDX
                    let rax = Register::direct(RAX);
                    // RAX already covers 64 bits...
                    let am2 = access_mode(size - 64);

                    let rdx = Operand::Loc(Location::Reg(
                            Register::direct(SubRegister::reg(RDX, am2))));

                    let offset = result.rbp_offset();
                    let am_full = AccessMode::Full;
                    let result_lo = Location::RbpOffset(offset, am_full);
                    let result_hi = Location::RbpOffset(offset + 8, am2);
                    asm.extend(vec![
                        MachineInst::mov(result_lo, rax),
                        MachineInst::mov(result_hi, rdx),
                    ]);

                } else {
                    bug!("Unsupported return value size: {:?}", size);
                }
                asm.append(&mut FunctionPrinter::emit_epilogue());
                CompiledInst::with_instructions(asm)
            } else {
                asm.append(&mut FunctionPrinter::emit_epilogue());
                CompiledInst::with_instructions(asm)
            }
        } else {
            bug!("expected OxInstruction::Ret, found {:?}", inst);
        }
    }

    fn codegen_alloca(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Alloca { .. }= inst {
            let result = self.precompiled_result(inst);
            CompiledInst::with_result(result)
        } else {
            bug!("expected OxInstruction::Alloca, found {:?}", inst);
        }
    }

    fn codegen_unreachable(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Unreachable = inst {
            CompiledInst::with_instructions(vec![MachineInst::UD2])
        } else {
            bug!("expected OxInstruction::Unreachable, found {:?}", inst);
        }
    }

    fn codegen_extractvalue(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::ExtractValue { agg, idx } = inst {
            let types = self.cx.types.borrow();
            let agg_ty = self.cx.val_ty(*agg);
            match types[agg_ty] {
                OxType::StructType { .. } => {
                    let offset = types[agg_ty].offset(*idx, &types);
                    assert!(offset % 8 == 0);
                    let offset = offset / 8;
                    let agg_val = self.codegen_value(*agg).result.unwrap();
                    let stack_loc = self.precompiled_result(inst);
                    let asm = vec![
                        MachineInst::lea(agg_val, Register::direct(RAX)),
                        MachineInst::add(Operand::Immediate(offset as isize,
                                                            AccessMode::Full),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::indirect(RAX),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::direct(RAX),
                                         stack_loc.clone()),
                    ];
                    CompiledInst::new(asm, stack_loc)
                },
                _ => {
                    unimplemented!("extract value at idx {:?} from agg {:?}",
                                   idx, self.cx.val_ty(*agg));
                }
            }
        } else {
            bug!("expected OxInstruction::ExtractValue, found {:?}", inst);
        }
    }

    fn codegen_insertvalue_struct(
        &self,
        agg_dst: Operand,
        agg: Value,
        v: Value,
        idx: u64
    ) -> CompiledInst {
        let types = self.cx.types.borrow();
        let agg_ty = self.cx.val_ty(agg);
        let members = match types[agg_ty] {
            OxType::StructType { ref members, .. } => members,
            ref ty => bug!("Expected OxType::StructType, found {:?}", ty),
        };

        // The offset within the aggregated of the member at
        // position `idx`.
        let offset = types[agg_ty].offset(idx, &types) as isize;
        assert!(offset % 8 == 0);
        let offset = offset / 8;

        //let agg_val = self.codegen_value(*agg);
        let compiled_v = self.codegen_value(v).result.unwrap();
        let member_rbp_offset = agg_dst.rbp_offset() + offset;
        let member_rbp_offset = Location::RbpOffset(member_rbp_offset,
                                                    AccessMode::Full);
        let member_am = access_mode(members[idx as usize].size(&types));
        match agg {
            // FIXME? an aggregate is only undefined if it is a Value::ConstUndef.
            // This is because storing an undefined value is a no-op.
            Value::ConstUndef(_) => {
                let reg =
                    Register::direct(SubRegister::reg(RCX, member_am));
                let asm = vec![
                    MachineInst::mov(compiled_v, reg),
                    MachineInst::lea(member_rbp_offset,
                    // Store the result on the stack.
                                     Register::direct(RAX)),
                    MachineInst::mov(reg, Register::indirect(RAX)),
                ];
                CompiledInst::new(asm, agg_dst)
            },
            Value::Instruction(fn_idx, bb_idx, idx) => {
                let inst = &self.cx.module.borrow().functions[fn_idx]
                    .basic_blocks[bb_idx].instrs[idx];
                let compiled_insts = self.compiled_insts.borrow();
                let reg =
                    Register::direct(SubRegister::reg(RCX, member_am));
                if let Some(old_struct) = compiled_insts.get(&agg) {
                    let old_struct_loc = old_struct.result.clone().unwrap();
                    let asm = vec![
                        MachineInst::mov(old_struct_loc, reg),
                        MachineInst::mov(reg, agg_dst.clone()),
                        MachineInst::xor(reg, reg),
                        MachineInst::mov(compiled_v, reg),
                        MachineInst::lea(member_rbp_offset,
                                         Register::direct(RAX)),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                    ];
                    CompiledInst::new(asm, agg_dst)
                } else {
                    bug!("OxInstruction {:?} has not yet been compiled", inst);
                }
            },
            _ => unimplemented!("InsertValue into {:?} {:?}", agg,
                                self.cx.val_ty(agg)),
        }
    }

    fn codegen_insertvalue(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::InsertValue { agg, elt, idx } = inst {
            let types = self.cx.types.borrow();
            let agg_ty = self.cx.val_ty(*agg);
            match types[agg_ty] {
                OxType::StructType { .. } => {
                    let agg_dst = self.precompiled_result(inst);
                    self.codegen_insertvalue_struct(agg_dst, *agg, *elt, *idx)
                }
                ref ty => unimplemented!("Insert into ty {:?}", ty),
            }
        } else {
            bug!("expected OxInstruction::Unreachable, found {:?}", inst);
        }
    }

    fn codegen_structgep(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::StructGep { ptr: agg, idx } = inst {
            let types = self.cx.types.borrow();
            let agg_ty = self.cx.val_ty(*agg);
            // Agg is a pointer to a struct. To get its members, we must get the
            // pointee of the pointer (the pointee is the struct itself).
            let offset = types[agg_ty.pointee_ty(&types)].offset(*idx, &types);

            assert!(offset % 8 == 0);
            let offset = offset / 8;
            let agg_val = self.codegen_value(*agg).result.unwrap();
            let stack_loc = self.precompiled_result(inst);
            // FIXME:
            let asm = vec![
                MachineInst::NOP,
                // Move the pointer from agg_val to %rax.
                MachineInst::mov(agg_val, Register::direct(RAX)),
                // Find the offset of the field to get.
                MachineInst::add(Operand::Immediate(offset as isize,
                                                    AccessMode::Full),
                                 Register::direct(RAX)),
                // Store the result on the stack.
                MachineInst::mov(Register::direct(RAX),
                                 stack_loc.clone()),
            ];
            CompiledInst::new(asm, stack_loc)
        } else {
            bug!("expected OxInstruction::StructGep, found {:?}", inst);
        }
    }

    fn codegen_gep(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Gep { ptr: agg, indices, inbounds } = inst {
            let types = self.cx.types.borrow();
            let agg_ty = self.cx.val_ty(*agg);
            // FIXME: normally, the indices of a GEP must be constants (they need
            // to be known at compile-time). However if an index is used to
            // index into an array, it doesn't have to be a constants (because
            // the type of the resulting GEP can be known statically anyway).
            let mut asm = Vec::new();
            let mut cur_agg_ty = agg_ty;
            let indices: Vec<(Type, Operand)> =
                indices.iter().enumerate().map(|(i, idx_val)| {
                let index_val = self.codegen_value(*idx_val);
                asm.extend(index_val.asm);
                cur_agg_ty = cur_agg_ty.ty_at_idx(i as u64, &types);
                (cur_agg_ty, index_val.result.unwrap())

            }).collect();

            let stack_loc = self.precompiled_result(inst);
            // First, get the base address of the pointer.
            let agg_val = self.codegen_value(*agg).result.unwrap();
            asm.extend(vec![
                MachineInst::mov(agg_val, Register::direct(RAX)),
                MachineInst::mov(Register::direct(RAX), stack_loc.clone())
            ]);

            let agg_pointee = agg_ty.pointee_ty(&types);
            match types[agg_pointee] {
                OxType::Array { .. } => {
                    for (ty, index_val) in indices {
                        // Find out the size of the struct:
                        let ty_size = ty.size(&types);
                        assert!(ty_size % 8 == 0);
                        let ty_size = ty_size as u64 / 8;
                        // %rcx initially contains the base address of the array.
                        let rcx = Register::direct(RCX);
                        // %rax stores the number of bytes to add to %rcx to get
                        // to the desired 'element'.
                        let rax = Register::direct(RAX);
                        asm.extend(vec![
                            MachineInst::mov(stack_loc.clone(), rcx),
                            MachineInst::mov(index_val, rax),
                            MachineInst::mov(Operand::Immediate(ty_size as isize,
                                                                AccessMode::Full),
                                             Register::direct(RSI)),
                            MachineInst::mul(Register::direct(RSI)),
                            MachineInst::add(rax, rcx),
                            MachineInst::mov(rcx, stack_loc.clone()),
                        ]);
                    }
                },
                OxType::StructType { .. } if indices.len() > 1 => {
                    unimplemented!("gep({:?}) with indices {:?}", agg_ty, indices);
                }
                _ => {
                    // An i8*, struct* etc
                    assert_eq!(indices.len(), 1);
                    let (ty, ref index_val) = indices[0];
                    let ty_size = ty.size(&types);
                    assert!(ty_size % 8 == 0);
                    let ty_size = ty_size as u64 / 8;
                    // %rcx initially contains the base address of the array.
                    let rcx = Register::direct(RCX);
                    // %rax stores the number of bytes to add to %rcx to get
                    // to the desired 'element'.
                    let rax = Register::direct(RAX);
                    // FIXME: duplication
                    asm.extend(vec![
                        MachineInst::mov(stack_loc.clone(), rcx),
                        MachineInst::mov(index_val.clone(), rax),
                        MachineInst::mov(Operand::Immediate(ty_size as isize,
                                                            AccessMode::Full),
                                         Register::direct(RSI)),
                        MachineInst::mul(Register::direct(RSI)),
                        MachineInst::add(rax, rcx),
                        MachineInst::mov(rcx, stack_loc.clone()),
                    ]);
                }
            }
            CompiledInst::new(asm, stack_loc)
        } else {
            bug!("expected OxInstruction::Gep, found {:?}", inst);
        }
    }

    fn codegen_invoke(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Invoke { .. } = inst {
            // Emit the call
            self.codegen_call(inst)
            // FIXME: skip the resume for now
            //unimplemented!("Invoke {:?}, {:?}", then, catch);
        } else {
            bug!("expected OxInstruction::Invoke, found {:?}", inst);
        }
    }

    fn codegen_resume(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Resume(_) = inst {
            // FIXME: crash for now.
            CompiledInst::with_instructions(vec![MachineInst::UD2])
        } else {
            bug!("expected OxInstruction::Resume, found {:?}", inst);
        }
    }

    fn codegen_landingpad(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::LandingPad { .. } = inst {
            // FIXME: crash for now.
            let landing_pad = self.precompiled_result(inst);
            CompiledInst::new(vec![MachineInst::UD2], landing_pad)
        } else {
            bug!("expected OxInstruction::LandingPad, found {:?}", inst);
        }
    }

    fn codegen_mul(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Mul { lhs, rhs, signed } = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.codegen_value(*lhs);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.codegen_value(*rhs);
            asm.append(&mut instr_asm2.asm);
            // The size in bits.
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg1 = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            let reg2 = Register::direct(SubRegister::reg(RCX, access_mode(size)));
            // The result of mul is actually in RDX:RAX. However, we only care
            // about the low order bits from %rax, because multiplying two
            // values of a particular bit width (type) always results in a value
            // of that bit width (type).
            let mul_inst = if *signed {
                MachineInst::imul(reg2)
            } else {
                MachineInst::mul(reg2)
            };
            let result = self.precompiled_result(inst);
            asm.extend(vec![
                MachineInst::mov(instr_asm1.result.unwrap(), reg1),
                MachineInst::mov(instr_asm2.result.unwrap(), reg2),
                mul_inst,
                MachineInst::mov(reg1, result.clone()),
            ]);
            CompiledInst::new(asm, result)

        } else {
            bug!("expected OxInstruction::Mul, found {:?}", inst);
        }
    }

    fn codegen_switch(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Switch { value, default, cases } = inst {
            let compiled_v = self.codegen_value(*value);
            let v_stack_loc = compiled_v.result.clone().unwrap();
            // Load the value and compare it with each of the cases
            let acc_mode = access_mode(
                self.cx.val_ty(*value).size(&self.cx.types.borrow()));
            let reg = Register::direct(SubRegister::reg(RAX, acc_mode));
            let mut asm = vec![MachineInst::mov(v_stack_loc, reg)];
            let module = &self.cx.module.borrow();
            for (v, bb) in cases {
                let compiled_v = self.codegen_value(*v);
                asm.extend(compiled_v.asm);
                let compiled_v = compiled_v.result.clone().unwrap();
                let acc_mode = access_mode(
                    self.cx.val_ty(*v).size(&self.cx.types.borrow()));
                let v_reg = Register::direct(SubRegister::reg(RCX, acc_mode));
                let bb = &module.functions[bb.0].basic_blocks[bb.1];
                asm.extend(vec![
                    MachineInst::mov(compiled_v, v_reg),
                    MachineInst::cmp(reg, v_reg),
                    MachineInst::je(bb.label.clone()),
                ]);
            }
            let default = &module.functions[default.0].basic_blocks[default.1];
            asm.push(MachineInst::jmp(default.label.clone()));
            CompiledInst::with_instructions(asm)
        } else {
            bug!("expected OxInstruction::Switch, found {:?}", inst);
        }
    }

    fn codegen_select(&self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Select { cond, then_val, else_val } = inst {
            match cond {
                // FIXME: assert that the condition is either an i1 or a vector
                // of i1.
                Value::Instruction(..) => {
                    let mut asm = Vec::new();
                    let compiled_cond = self.codegen_instruction(*cond).result.unwrap();
                    let true_lbl = self.cx.get_sym_name("select_true");
                    let done_lbl = self.cx.get_sym_name("select_done");
                    let mut add_asm_and_get_result =
                        |v: Value, asm: &mut Vec<MachineInst>| {
                            let v = self.codegen_value(v);
                            asm.extend(v.asm);
                            v.result.unwrap()
                        };
                    // then_val and else_val have the same type/access mode.
                    let acc_mode = access_mode(
                        self.cx.val_ty(*then_val).size(&self.cx.types.borrow()));
                    let then_val = add_asm_and_get_result(*then_val, &mut asm);
                    let else_val = add_asm_and_get_result(*else_val, &mut asm);
                    let reg = Register::direct(SubRegister::reg(RAX, acc_mode));
                    // The stack location where the result of this instruction
                    // will be stored.
                    let stack_loc = self.precompiled_result(inst);
                    let cond_reg = Register::direct(
                        SubRegister::reg(RCX, AccessMode::Low8));
                    asm.extend(vec![
                        // Check if the condition is true
                        MachineInst::mov(then_val.clone(), cond_reg),
                        MachineInst::cmp(compiled_cond, cond_reg),
                        MachineInst::je(true_lbl.clone()),
                        MachineInst::mov(else_val, reg),
                        MachineInst::mov(reg, stack_loc.clone()),
                        MachineInst::jmp(done_lbl.clone()),
                        MachineInst::Label(true_lbl),
                        MachineInst::mov(then_val, reg),
                        MachineInst::mov(reg, stack_loc.clone()),
                        MachineInst::Label(done_lbl),
                    ]);
                    CompiledInst::new(asm, stack_loc)
                }
                _ => {
                    bug!("cond must be a Value::Instruction, not {:?}", cond);
                }
            }
        } else {
            bug!("expected OxInstruction::Select, found {:?}", inst);
        }
    }

    fn codegen_instruction(&self, inst_v: Value) -> CompiledInst {
        let module = self.cx.module.borrow();
        let inst = if let Value::Instruction(fn_idx, bb_idx, idx) = inst_v {
            &module.functions[fn_idx].basic_blocks[bb_idx].instrs[idx]
        } else {
            bug!("can only compile a Value::Instruction; found {:?}", inst_v);
        };
        if let Some(instr_asm) = self.compiled_insts.borrow().get(&inst_v) {
            return instr_asm.clone();
        }
        let instr_asm = match inst {
            OxInstruction::Br(..) => self.codegen_br(inst),
            OxInstruction::Ret(..) => self.codegen_ret(inst),
            OxInstruction::Store { .. } => self.codegen_store(inst),
            OxInstruction::Add { .. } => self.codegen_add(inst),
            OxInstruction::Sub { .. } => self.codegen_sub(inst),
            OxInstruction::Mul { .. } => self.codegen_mul(inst),
            OxInstruction::Call { .. }=> self.codegen_call(inst),
            OxInstruction::Alloca { .. } => self.codegen_alloca(inst),
            OxInstruction::Icmp { .. } => self.codegen_icmp(inst),
            OxInstruction::CondBr { .. } => self.codegen_condbr(inst),
            OxInstruction::Load { .. } => self.codegen_load(inst),
            OxInstruction::CheckOverflow { .. } => self.codegen_checkoverflow(inst),
            OxInstruction::Cast { .. }=> self.codegen_cast(inst),
            OxInstruction::Unreachable => self.codegen_unreachable(inst),
            OxInstruction::ExtractValue { .. } => self.codegen_extractvalue(inst),
            OxInstruction::InsertValue { .. } => self.codegen_insertvalue(inst),
            OxInstruction::StructGep { .. } => self.codegen_structgep(inst),
            OxInstruction::Not(..) => self.codegen_not(inst),
            OxInstruction::And { .. } => self.codegen_and(inst),
            OxInstruction::Invoke {..} => self.codegen_invoke(inst),
            OxInstruction::Resume(..)=> self.codegen_resume(inst),
            OxInstruction::LandingPad { .. } => self.codegen_landingpad(inst),
            OxInstruction::Select { .. } => self.codegen_select(inst),
            OxInstruction::Switch { .. } => self.codegen_switch(inst),
            OxInstruction::Gep { .. } => self.codegen_gep(inst),
        };
        let compiled_inst = if let Some(ref result) = instr_asm.result {
            CompiledInst::new(instr_asm.asm.clone(), result.clone())
        } else {
            CompiledInst::with_instructions(instr_asm.asm.clone())
        };
        self.compiled_insts.borrow_mut().insert(inst_v, compiled_inst);
        instr_asm
    }

    fn precompiled_result(&self, inst: &OxInstruction) -> Operand {
        self.inst_stack_loc.borrow()[inst].result.clone().unwrap().clone()
    }

    fn already_compiled(&self, value: Value) -> bool {
        self.compiled_insts.borrow().get(&value).is_some()
    }

    pub fn codegen_block(&self, bb: &OxBasicBlock) -> Vec<MachineInst> {
        let mut asm = vec![];
        for (inst_idx, _) in bb.instrs.iter().enumerate() {
            let inst = &bb.instrs[inst_idx];
            if self.already_compiled(Value::Instruction(bb.parent, bb.idx, inst_idx)) {
                // Always compile branching instructions.
                if inst.is_branch() {
                    let mut instr_asm = self.codegen_instruction(
                        Value::Instruction(bb.parent, bb.idx, inst_idx));
                    asm.append(&mut instr_asm.asm);
                }
            } else {
                let mut instr_asm = self.codegen_instruction(
                    Value::Instruction(bb.parent, bb.idx, inst_idx));
                asm.append(&mut instr_asm.asm);
            }
        }
        asm
    }

    fn emit_prologue(stack_size: usize) -> Vec<MachineInst> {
        vec![
            MachineInst::push(Register::direct(RBP)),
            MachineInst::mov(Register::direct(RSP), Register::direct(RBP)),
            MachineInst::sub(Operand::Immediate(stack_size as isize, AccessMode::Full),
                             Register::direct(RSP)),
        ]
    }

    fn emit_epilogue() -> Vec<MachineInst> {
        vec![
            MachineInst::LEAVE,
            MachineInst::RET,
        ]
    }

    fn compute_stack_size(&self, f: &OxFunction) -> (Vec<MachineInst>, usize) {
        let mut size = 0;
        let mut param_movs = vec![];

        for (idx, param) in f.params.iter().take(6).enumerate() {
            // FIXME:
            let param_size =
                self.cx.val_ty(*param).size(&self.cx.types.borrow()) as isize;
            size += param_size / 8;
            let acc_mode = access_mode(param_size as u64);
            let result = Location::RbpOffset(-size, acc_mode);
            let reg = FunctionPrinter::get_func_arg(idx);
            let reg = Register::direct(SubRegister::reg(reg, acc_mode));
            param_movs.push(MachineInst::mov(reg, result.clone()));
            // load from param doesn't work
            let instr_asm =
                CompiledInst::new(param_movs.clone(), Operand::from(result));
            self.compiled_params.borrow_mut().insert(*param, instr_asm);
        }

        let remaining_params: Vec<&Value> = f.params.iter().skip(6).collect();
        // The first argument is at rbp + 16.
        let mut pos_rbp_offset = 16;
        // The remaining arguments are pushed to the stack in reverse order.
        for param in remaining_params.iter().rev() {
            let param_size =
                self.cx.val_ty(**param).size(&self.cx.types.borrow()) as isize;
            let acc_mode = access_mode(param_size as u64);
            let result = Location::RbpOffset(pos_rbp_offset, acc_mode);
            let instr_asm =
                CompiledInst::with_result(Operand::from(result));
            self.compiled_params.borrow_mut().insert(**param, instr_asm);
            pos_rbp_offset += param_size / 8;
        }

        for bb in &f.basic_blocks {
            for inst in &bb.instrs {
                let instr_asm = match inst {
                    OxInstruction::Add { .. } |
                    OxInstruction::Sub { .. } |
                    OxInstruction::Mul { .. } |
                    OxInstruction::And { .. } |
                    OxInstruction::Load { .. } => {
                        let inst_size =
                            inst.val_ty(self.cx).size(&self.cx.types.borrow());

                        let acc_mode = access_mode(inst_size as u64);
                        size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-size, acc_mode);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::Call { .. } | OxInstruction::Invoke { .. } => {
                        let ret_ty = inst.val_ty(self.cx);
                        // If the function doesn't return anything, carry on.
                        if let OxType::Void = self.cx.types.borrow()[ret_ty] {
                            continue;
                        }
                        let inst_size = ret_ty.size(&self.cx.types.borrow());
                        let result = if inst_size <= 64 {
                            let acc_mode = access_mode(inst_size as u64);
                            // FIXME: check non void ret
                            size += (inst_size / 8) as isize;
                            Location::RbpOffset(-size, acc_mode)
                        } else if inst_size <= 128 {
                            // Split the result in two: the first half goes in %rax,
                            // and the second in %rdx
                            size += (inst_size / 8) as isize;
                            Location::RbpOffset(-size,
                                                AccessMode::Large(inst_size / 8))
                        } else {
                            unimplemented!("return value size: {}", inst_size);
                        };
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::Alloca { ty, .. } => {
                        let types = &self.cx.types.borrow();
                        let inst_size =
                            ty.pointee_ty(types).size(types);
                        let acc_mode = access_mode(inst_size);
                        size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-size, acc_mode);
                        size += 8;
                        let result_addr = Location::RbpOffset(-size, AccessMode::Full);
                        let asm = vec![
                            MachineInst::lea(result, Register::direct(RAX)),
                            MachineInst::mov(Register::direct(RAX),
                                             result_addr.clone()),
                        ];
                        // FIXME: This is not a param move.
                        param_movs.extend(asm.clone());
                        CompiledInst::new(asm.clone(), Operand::from(result_addr))
                    }
                    OxInstruction::ExtractValue { agg, idx } => {
                        let types = self.cx.types.borrow();
                        let agg_ty = self.cx.val_ty(*agg);
                        let elt_size = match types[agg_ty] {
                            OxType::StructType { ref members, .. } => {
                                members[*idx as usize].size(&types)
                            },
                            ref ty => unimplemented!("ExtractValue({:?})", ty),
                        };
                        size += elt_size as isize / 8;
                        let acc_mode = access_mode(elt_size);
                        let result = Location::RbpOffset(-size, acc_mode);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::StructGep { .. }| OxInstruction::Gep { .. } => {
                        // The size of a pointer
                        size += 8;
                        let result = Location::RbpOffset(-size, AccessMode::Full);

                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::Not(v) => {
                        let val_size =
                            self.cx.val_ty(*v).size(&self.cx.types.borrow()) as isize;
                        size += val_size / 8;
                        let result = Location::RbpOffset(-size, AccessMode::Full);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::InsertValue { agg, .. } => {
                        let agg_size = self.cx.val_ty(*agg).
                            size(&self.cx.types.borrow()) as isize;
                        size += agg_size / 8;
                        let result =
                            Location::RbpOffset(-size, AccessMode::Full);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::Select { then_val, else_val, .. }=> {
                        let ty = self.cx.val_ty(*then_val);
                        assert_eq!(ty, self.cx.val_ty(*else_val));
                        let val_size = ty.size(&self.cx.types.borrow()) as isize;
                        let acc_mode = access_mode(val_size as u64);
                        size += val_size / 8;
                        let result =
                            Location::RbpOffset(-size, acc_mode);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::LandingPad {ty, .. } => {
                        let pad_size = ty.size(&self.cx.types.borrow()) as isize;
                        let acc_mode = access_mode(pad_size as u64);
                        size += pad_size / 8;
                        let result =
                            Location::RbpOffset(-size, acc_mode);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::Icmp { .. } |
                    OxInstruction::CheckOverflow { .. } => {
                        // allocate a bool:
                        let inst_size = 8;
                        let acc_mode = access_mode(inst_size);
                        size += inst_size as isize / 8;
                        let result =
                            Location::RbpOffset(-size, acc_mode);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    OxInstruction::Cast { ty, .. } => {
                        let inst_size = ty.size(&self.cx.types.borrow());
                        let acc_mode = access_mode(inst_size);
                        size += inst_size as isize / 8;
                        let result =
                            Location::RbpOffset(-size, acc_mode);
                        CompiledInst::with_result(Operand::from(result))
                    }
                    _ => continue,
                };
                self.inst_stack_loc.borrow_mut().insert((*inst).clone(), instr_asm);
            }
        }
        if size % 16 != 0 {
            size += 16 - size % 16;
        }
        (param_movs, size as usize)
    }
}

pub struct AsmPrinter<'ll, 'tcx> {
    /// The codegen context, which also contains the `ModuleIronOx` to be compiled.
    cx: CodegenCx<'ll, 'tcx>,
    codegenned_consts: RefCell<Vec<Value>>,
}

impl AsmPrinter<'ll, 'tcx> {
    pub fn new(cx: CodegenCx<'ll, 'tcx>) -> AsmPrinter<'ll, 'tcx> {
        AsmPrinter { cx, codegenned_consts: Default::default() }
    }

    fn constant_value(&self, v: Value) -> String {
        match v {
            Value::ConstCstr(idx) => self.cx.const_cstrs.borrow()[idx].name.clone(),
            Value::Global(idx) => {
                self.constant_value(self.cx.globals.borrow()[idx].initializer.unwrap())
            },
            Value::ConstGep { ptr_idx, offset } if offset == 0 => {
                // FIXME: handle non-zero offsets
                let name = self.cx.const_structs.borrow()[ptr_idx].name.clone();
                name
            }
            _ => unimplemented!("value of {:?}", v),
        }
    }

    fn codegen_const_global(&self, c: Value) -> Vec<MachineInst> {
        let mut asm = vec![];
        match c {
            Value::ConstFatPtr(idx) => {
                let (v1, v2) = self.cx.const_fat_ptrs.borrow()[idx];
                asm.append(&mut self.codegen_const_global(v1));
                asm.append(&mut self.codegen_const_global(v2));
            }
            Value::ConstUint(idx) => {
                let u_const = self.cx.u_consts.borrow()[idx];
                let directive = self.declare_scalar(u_const.ty, u_const.value);
                asm.push(MachineInst::Directive(directive));
            }
            Value::Cast(idx) => {
                let cast_inst = &self.cx.const_casts.borrow()[idx];
                let name = self.constant_value(cast_inst.value);
                asm.push(MachineInst::Directive(GasDirective::Quad(
                            vec![BigNum::Sym(name)])));
            }
            Value::ConstStruct(idx) => {
                let const_struct = &self.cx.const_structs.borrow()[idx];

                self.codegenned_consts.borrow_mut().push(c);
                asm.push(MachineInst::Label(const_struct.name.clone()));
                for c in &const_struct.components {
                    asm.extend(self.codegen_const_global(*c));
                }
            }
            Value::Function(idx) => {
                let name = self.cx.module.borrow().functions[idx].name.clone();
                asm.push(MachineInst::Directive(GasDirective::Quad(
                            vec![BigNum::Sym(name)])));
            }
            Value::ConstCstr(idx) => {
                let c_str = &self.cx.const_cstrs.borrow()[idx];
                let const_str = self.get_str(c_str.ptr, c_str.len);
                asm.extend(vec![
                    MachineInst::Directive(
                        GasDirective::Type(c_str.name.clone(), GasType::Object)),
                    MachineInst::Directive(
                        GasDirective::Size(c_str.name.clone(), c_str.len)),
                    MachineInst::Label(c_str.name.clone()),
                    MachineInst::Directive(GasDirective::Ascii(vec![const_str])),
                ]);
            }
            Value::ConstBytes(idx) => {
                let const_bytes = &self.cx.bytes.borrow()[idx];
                asm.extend(vec![
                    MachineInst::Label(const_bytes.name.clone()),
                    MachineInst::Directive(
                        GasDirective::Byte(const_bytes.bytes.clone())),
                    MachineInst::Directive(
                        GasDirective::Size(const_bytes.name.clone(),
                                           const_bytes.len())),
                ]);
            },
            _ => unimplemented!("codegen_const_global({:?})", c),
        };
        asm
    }

    fn get_str(&self, ptr: *const u8, len: usize) -> String {
        let mut c_str = String::with_capacity(len);
        for i in 0..len {
            unsafe {
                c_str.push(*ptr.offset(i as isize) as char);
            }
        }
        c_str
    }

    fn declare_scalar(&self, ty: Type, value: u128) -> GasDirective {
        match self.cx.types.borrow()[ty] {
            OxType::Scalar(ScalarType::I32) => GasDirective::Long(vec![value as u32]),
            OxType::Scalar(ScalarType::I64) => GasDirective::Quad(
                vec![BigNum::Immediate(value as u64)]),
            _ => unimplemented!("type of {:?}", ty),
        }
    }

    fn declare_globals(&self) -> Vec<MachineInst> {
        let mut asm = vec![
            MachineInst::Directive(GasDirective::Section(".rodata".to_string()))];
        for global in self.cx.globals.borrow().iter().rev() {
            asm.push(MachineInst::Label(global.name.clone()));
            match global.initializer {
                Some(v) => {
                    let codegen = {
                        !self.codegenned_consts.borrow().contains(&v)
                    };
                    if codegen {
                        asm.extend(self.codegen_const_global(v));
                        self.codegenned_consts.borrow_mut().push(v);
                    }
                }
                None => bug!("no initializer found for {:?}", global),
            };
        }
        for (i, _c_struct) in self.cx.const_structs.borrow().iter().enumerate() {
            let v = Value::ConstStruct(i);
            let codegen = {
                !self.codegenned_consts.borrow().contains(&v)
            };
            if codegen {
                asm.extend(self.codegen_const_global(v));
                self.codegenned_consts.borrow_mut().push(v);
            }
        }
        asm
    }


    fn declare_functions(&self) -> Vec<MachineInst> {
        let mut asm: Vec<MachineInst> = vec![
            MachineInst::Directive(GasDirective::Text)];
        let module = self.cx.module.borrow();
        for f in &module.functions {
            asm.extend(vec![
                MachineInst::Directive(GasDirective::Global(f.name.clone())),
                MachineInst::Directive(GasDirective::Type(f.name.clone(),
                                                          GasType::Function)),
            ]);
            match f.visibility {
                Visibility::Hidden => {
                    asm.push(
                        MachineInst::Directive(GasDirective::Hidden(f.name.clone())));
                },
                Visibility::Protected => {
                    asm.push(
                        MachineInst::Directive(GasDirective::Protected(f.name.clone())));
                }
                Visibility::Default => {
                    //do nothing
                },
            }
        }
        asm
    }

    /// Consume the printer, and return the codegen result.
    ///
    /// The codegen result is a string that contains the x86-64 program that
    /// corresponds to the module from the `CodegenCx` of this printer.
    pub fn codegen(self) -> (Stats, String) {
        self.pprint();
        let mut asm = String::new();
        // Define the globals.
        for globl in &self.declare_globals() {
            asm.push_str(&globl.to_string());
        }
        // Declare the functions.
        for decl in &self.declare_functions() {
            asm.push_str(&decl.to_string());
        }
        for f in &self.cx.module.borrow().functions {
            if !f.is_declaration() {
                let mut fn_asm = String::new();
                let asm_insts  = FunctionPrinter::new(&self.cx).codegen_function(&f);
                for inst in asm_insts {
                    fn_asm.push_str(&format!("{}", inst));
                }
                asm.push_str(&fn_asm);
            }
        }
        (self.cx.consume_stats().into_inner(), asm)
    }
}
