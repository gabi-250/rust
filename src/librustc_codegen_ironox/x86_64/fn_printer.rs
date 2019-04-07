use context::CodegenCx;
use ir::basic_block::OxBasicBlock;
use ir::function::OxFunction;
use ir::instruction::{CompOp, OxInstruction};
use ir::type_::{OxType, Type};
use ir::value::Value;

use x86_64::instruction::MachineInst;
use x86_64::register::GeneralPurposeReg::{self, *};
use x86_64::register::{Location, Operand, Register, SubRegister, AccessMode,
                      operand_access_mode, access_mode};

use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::traits::BaseTypeMethods;

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
    compiled_insts: FxHashMap<Value, CompiledInst>,
    /// A mapping from function parameters to their location on the stack.
    /// Currently, parameters are always pushed to and retrieved from the stack.
    arg_loc: FxHashMap<Value, Operand>,
    /// The location on the stack where each `OxInstruction` should be placed.
    inst_loc: FxHashMap<OxInstruction, Operand>,
    /// The amount of stack space to allocate for this function.
    stack_size: isize,
}

impl FunctionPrinter<'a, 'll, 'tcx> {
    /// Create a new function printer.
    pub fn new(cx: &'a CodegenCx<'ll, 'tcx>) -> FunctionPrinter<'a, 'll, 'tcx> {
        FunctionPrinter {
            cx,
            compiled_insts: Default::default(),
            arg_loc: Default::default(),
            inst_loc: Default::default(),
            stack_size: 0,
        }
    }

    /// Return the `MachineInst` of function `f`.
    pub fn codegen_function(mut self, f: &OxFunction) -> Vec<MachineInst> {
        let mut asm = vec![MachineInst::Label(f.name.clone())];
        let mut arg_movs = self.map_args_to_stack_locs(f);
        let mut allocas = self.map_insts_to_stack_locs(f);
        self.align_stack();
        asm.append(&mut self.emit_prologue());
        asm.append(&mut arg_movs);
        asm.append(&mut allocas);
        for bb in &f.basic_blocks {
            asm.append(&mut self.codegen_block(&bb));
        }
        asm
    }

    /// Return the general purpose register in which the argument at position `idx`
    /// is stored. This panics if the index is greater than 6.
    fn fn_arg_reg(idx: usize) -> GeneralPurposeReg {
        if idx < 6 {
            return FN_ARG_REGS[idx];
        } else {
            bug!("Argument number {} should be on the stack", idx);
        }
    }

    fn codegen_value(&mut self, value: Value) -> CompiledInst {
        let module = self.cx.module.borrow();
        match value {
            Value::ConstUint(idx) => {
                let imm_size = self.cx.val_size(value);
                let acc_mode = access_mode(imm_size as u64);
                let value = self.cx.u_consts.borrow()[idx].value;
                let result = Operand::Immediate(value as isize, acc_mode);
                CompiledInst::with_result(result)
            }
            Value::Param { .. } => {
                if let Some(result) = self.arg_loc.get(&value) {
                    CompiledInst::with_result(result.clone())
                } else {
                    bug!("param should've already been compiled!");
                }
            }
            Value::Instruction { .. } => {
                // If the instruction has already been compiled, return its
                // cached result.
                if let Some(inst_asm) = self.compiled_insts.get(&value) {
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
            Value::ConstCast(idx) => {
                self.codegen_value(self.cx.const_casts.borrow()[idx].value)
            }
            Value::Function(idx) => {
                // Move the result to the stack, and assume its size is 8.
                let function_name = if !module.functions[idx].is_codegenned {
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

    fn codegen_and(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::And { lhs, rhs } = inst {
            let mut asm = vec![];
            let mut comp_inst1 = self.codegen_value(*lhs);
            asm.append(&mut comp_inst1.asm);
            let mut comp_inst2 = self.codegen_value(*rhs);
            asm.append(&mut comp_inst2.asm);
            // the size in bits
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let rax = Register::direct(SubRegister::new(RAX, access_mode(size)));
            let rcx = Register::direct(SubRegister::new(RCX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(comp_inst1.result.unwrap(), rax),
                MachineInst::mov(comp_inst2.result.unwrap(), rcx),
                MachineInst::and(rcx, rax),
            ]);
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(rax, result.clone()));
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::And, found {:?}", inst);
        }
    }

    fn codegen_add(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Add { lhs, rhs } = inst {
            let mut asm = vec![];
            let mut comp_inst1 = self.codegen_value(*lhs);
            asm.append(&mut comp_inst1.asm);
            let mut comp_inst2 = self.codegen_value(*rhs);
            asm.append(&mut comp_inst2.asm);
            // the size in bits
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let rax = Register::direct(SubRegister::new(RAX, access_mode(size)));
            let rcx = Register::direct(SubRegister::new(RCX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(comp_inst1.result.unwrap(), rax),
                MachineInst::mov(comp_inst2.result.unwrap(), rcx),
                MachineInst::add(rcx, rax),
            ]);
            // FIXME:
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(rax, result.clone()));
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Add, found {:?}", inst);
        }
    }

    fn codegen_sub(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Sub { lhs, rhs } = inst {
            let mut asm = vec![];
            let mut comp_inst1 = self.codegen_value(*lhs);
            asm.append(&mut comp_inst1.asm);
            let mut comp_inst2 = self.codegen_value(*rhs);
            asm.append(&mut comp_inst2.asm);
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let rax = Register::direct(SubRegister::new(RAX, access_mode(size)));
            let rcx = Register::direct(SubRegister::new(RCX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(comp_inst1.result.unwrap(), rax),
                MachineInst::mov(comp_inst2.result.unwrap(), rcx),
                MachineInst::sub(rcx, rax),
            ]);
            // FIXME:
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(rax, result.clone()));
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Sub, found {:?}", inst);
        }
    }

    fn codegen_not(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Not(v) = inst {
            let mut asm = vec![];
            let mut comp_inst = self.codegen_value(*v);
            asm.append(&mut comp_inst.asm);
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg = Register::direct(SubRegister::new(RAX, access_mode(size)));
            let mask = Register::direct(SubRegister::new(RCX, access_mode(size)));
            let result = self.precompiled_result(inst);
            asm.extend(vec![
                MachineInst::mov(comp_inst.result.unwrap(), reg),
                MachineInst::xor(mask, mask),
                MachineInst::mov(Operand::Immediate(1, AccessMode::Full), mask),
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

    fn codegen_neg(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Neg(v) = inst {
            let mut asm = vec![];
            let mut comp_inst = self.codegen_value(*v);
            asm.append(&mut comp_inst.asm);
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg = Register::direct(SubRegister::new(RAX, access_mode(size)));
            let result = self.precompiled_result(inst);
            asm.extend(vec![
                MachineInst::mov(comp_inst.result.unwrap(), reg),
                MachineInst::neg(reg),
                MachineInst::mov(reg, result.clone()),
            ]);
            CompiledInst::new(asm, result)
        } else {
            bug!("expected OxInstruction::Neg, found {:?}", inst);
        }
    }

    fn codegen_args(&mut self, args: &Vec<Value>) -> (Vec<MachineInst>, isize) {
        let mut asm = vec![];
        for arg in args.iter() {
            // Compile everything before actually passing the argument to the
            // called function (this enusres we don't clobber any registers we
            // actually need in the function call).
            self.codegen_value(*arg);
        }
        // Prepare the function arguments. The first 6 go in registers.
        for (idx, arg) in args.iter().take(6).enumerate() {
            let mut comp_inst = self.codegen_value(*arg);
            asm.append(&mut comp_inst.asm);
            let param = FunctionPrinter::fn_arg_reg(idx);
            // FIXME: always handle globals correctly
            if let Some(Operand::Loc(Location::RipOffset(_))) = comp_inst.result.clone() {
                // globals are always treated as addresses
                asm.push(MachineInst::lea(comp_inst.result.unwrap(),
                                          Register::direct(param)));
            } else {
                asm.push(MachineInst::mov(comp_inst.result.unwrap(),
                                          Register::direct(param)));
            }
        }
        let remaining_args: Vec<&Value> = args.iter().skip(6).collect();
        let total_arg_size = 8 * remaining_args.len() as isize;

        let padding = total_arg_size % 16;
        if padding > 0 {
            // Align the stack on a 16-byte boundary.
            asm.push(MachineInst::sub(
                Operand::Immediate(padding as isize, AccessMode::Full),
                Register::direct(RSP)));
        }
        // The remaining arguments are pushed to the stack in reverse order.
        for arg in remaining_args.iter().rev() {
            let mut comp_inst = self.codegen_value(**arg);
            asm.append(&mut comp_inst.asm);
            let arg_size = self.cx.val_size(**arg);
            let acc_mode = access_mode(arg_size);
            let rax = Register::direct(SubRegister::new(RAX, acc_mode));
            if let Some(Operand::Loc(Location::RipOffset(_))) = comp_inst.result.clone() {
                // Globals are always assumed to be addresses...
                asm.push(MachineInst::lea(comp_inst.result.unwrap(), rax));
            } else {
                asm.push(MachineInst::mov(comp_inst.result.unwrap(), rax));
            }
            // `push` always pushed a 64-bit value, so push the full %rax register.
            asm.push(MachineInst::push(Register::direct(RAX)));
        }
        (asm, total_arg_size + padding)
    }

    fn codegen_call_ret(&mut self, inst: &OxInstruction) -> CompiledInst {
        let mut asm = vec![];
        let result = self.precompiled_result(inst);
        let acc_mode = result.access_mode();
        // Check the size of the return value.
        match acc_mode {
            // A return value that fits in registers %rax and %rdx.
            AccessMode::Large(bytes) if bytes <= 16 => {
                let offset = result.rbp_offset();
                let result_low = Location::RbpOffset(offset, AccessMode::Full);
                // This represents the size of the register that will contain the
                // bits left after moving the lower 64 bits into %rax.
                let acc_mode_high = access_mode(8 * (bytes - 8));
                let result_high = Location::RbpOffset(offset + 8, acc_mode_high);
                let rax = Register::direct(SubRegister::new(RAX, AccessMode::Full));
                let rdx = Register::direct(SubRegister::new(RDX, acc_mode_high));
                asm.extend(vec![
                    // Move the low order bits of the result in %rax.
                    MachineInst::mov(rax, result_low),
                    // Move the remaining bits of the result in %rdx.
                    MachineInst::mov(rdx, result_high),
                ]);
            },
            AccessMode::Large(bytes) => {
                bug!("Return value of size: {} bytes", bytes);
            },
            _ => {
                // The return value is small enough to fit in %rax.
                asm.push(MachineInst::mov(
                    Register::direct(SubRegister::new(RAX, acc_mode)),
                    result.clone()));
            }
        };
        CompiledInst::new(asm, result)
    }

    fn codegen_call(&mut self, inst: &OxInstruction) -> CompiledInst {
        let (callee, args) = match inst {
            OxInstruction::Call { callee, ref args } |
            OxInstruction::Invoke { callee, ref args, .. } => (callee, args),
            _ => bug!("Expected invoke or call, found {:?}", inst)
        };
        let (mut asm, total_arg_size) = self.codegen_args(args);
        // Emit the call
        match callee {
            Value::Function(idx) => {
                let module = self.cx.module.borrow();
                let fn_name = if !module.functions[*idx].is_codegenned {
                    format!("{}@PLT", &module.functions[*idx].name)
                } else {
                    module.functions[*idx].name.clone()
                };
                asm.push(MachineInst::call(fn_name.clone()));
            },
            Value::Instruction { .. } => {
                let ptr = self.codegen_value(*callee);
                let result = ptr.result.clone().unwrap();
                // Dereference the value.
                asm.extend(vec![
                    MachineInst::call(result.clone().deref()),
                ]);
            },
            _ => unimplemented!("call to {:?}", callee),
        };
        // Clean up the stack after the call.
        if total_arg_size > 0 {
            // Pop all the pushed arguments:
            asm.push(
                MachineInst::add(
                    Operand::Immediate(total_arg_size, AccessMode::Full),
                    Register::direct(RSP)));
        }
        let ret_ty = inst.val_ty(self.cx);
        // If the function doesn't return anything, return.
        if let OxType::Void = self.cx.types.borrow()[ret_ty] {
            CompiledInst::with_instructions(asm)
        } else {
            // Load the return value, and store it on the stack.
            let ret_insts = self.codegen_call_ret(inst);
            asm.extend(ret_insts.asm);
            CompiledInst::new(asm, ret_insts.result.unwrap())
        }
    }

    fn codegen_icmp(&mut self, inst: &OxInstruction) -> CompiledInst {
        match *inst {
            OxInstruction::Icmp { lhs, rhs, op } => {
                // These instructions have a boolean result.
                let cmp_res = self.precompiled_result(inst);
                let mut asm = vec![];
                let mut comp_lhs = self.codegen_value(lhs);
                asm.append(&mut comp_lhs.asm);
                let mut comp_rhs = self.codegen_value(rhs);
                asm.append(&mut comp_rhs.asm);
                let result_lhs = comp_lhs.result.unwrap();
                let result_rhs = comp_rhs.result.unwrap();
                let acc_mode = operand_access_mode(&result_lhs, &result_rhs);
                // Move the value into a register an compare it with the second value.
                let rax = Register::direct(SubRegister::new(RAX, acc_mode));
                asm.extend(vec![
                    MachineInst::mov(result_lhs, rax),
                    MachineInst::cmp(result_rhs, rax),
                ]);
                let rax = Register::direct(SubRegister::new(RAX, AccessMode::Low8));
                let set = match op {
                    CompOp::Eq => MachineInst::sete(rax),
                    CompOp::Ne => MachineInst::setne(rax),
                    CompOp::Ugt => MachineInst::seta(rax),
                    CompOp::Sgt => MachineInst::setg(rax),
                    CompOp::Uge => MachineInst::setae(rax),
                    CompOp::Sge => MachineInst::setge(rax),
                    CompOp::Ult => MachineInst::setb(rax),
                    CompOp::Slt => MachineInst::setl(rax),
                    CompOp::Ule => MachineInst::setbe(rax),
                    CompOp::Sle => MachineInst::setle(rax),
                };
                asm.extend(vec![
                    set,
                    MachineInst::mov(rax, cmp_res.clone()),
                ]);
                CompiledInst::new(asm, cmp_res)
            },
            _ => bug!("expected a comparison instruction, found {:?}", inst),
        }
    }

    fn codegen_condbr(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::CondBr { cond, then_bb, else_bb } = inst {
            let mut asm = vec![];
            let cond_val = self.codegen_value(*cond).result.unwrap();
            let true_reg = Register::direct(
                SubRegister::new(RCX, AccessMode::Low8));
            let cond_reg = Register::direct(
                SubRegister::new(RDI, AccessMode::Low8));
            let then_label = self.cx.module.borrow().bb_label(*then_bb);
            let else_label = self.cx.module.borrow().bb_label(*else_bb);
            asm.extend(vec![
                // Check if the condition is true
                MachineInst::mov(
                    Operand::Immediate(1, AccessMode::Low8), true_reg),
                MachineInst::mov(cond_val, cond_reg),
                // Compile the result of evaluating the condition with 1.
                MachineInst::cmp(cond_reg, true_reg),
                // If the condition is true.
                MachineInst::je(then_label),
            ]);
            asm.push(MachineInst::jmp(else_label));
            CompiledInst::with_instructions(asm)
        } else {
            bug!("expected OxInstruction::CondBr, found {:?}", inst);
        }
    }

    fn codegen_br(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Br(target) = inst {
            let label = self.cx.module.borrow().bb_label(*target);
            CompiledInst::with_instructions(
                vec![MachineInst::jmp(label)])
        } else {
            bug!("expected OxInstruction::Br, found {:?}", inst);
        }
    }

    fn codegen_store(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Store { ptr, val } = inst {
            let mut asm = vec![];
            let mut comp_ptr = self.codegen_value(*ptr);
            asm.append(&mut comp_ptr.asm);
            let mut comp_val = self.codegen_value(*val);
            asm.append(&mut comp_val.asm);
            let ptr_result = comp_ptr.result.clone().unwrap();
            let val_result = comp_val.result.unwrap();
            let val_am = val_result.access_mode();
            match val_am {
                AccessMode::Large(16) => {
                    let val_offset = val_result.rbp_offset();
                    let am_full = AccessMode::Full;
                    // Move two quad words from val to dst.
                    let reg =
                        Register::direct(SubRegister::new(RCX, am_full));
                    let val_word1 = Operand::Loc(
                        Location::RbpOffset(val_offset, am_full));
                    let val_word2 = Operand::Loc(
                        Location::RbpOffset(val_offset + 8, am_full));
                    // Check if ptr.result is a ptr?
                    asm.extend(vec![
                        // Move the first word.
                        MachineInst::mov(ptr_result.clone(),
                                         Register::direct(RAX)),
                        MachineInst::mov(val_word1, reg),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                        // Move the second word.
                        MachineInst::add(Operand::Immediate(8, am_full),
                                         Register::direct(RAX)),
                        MachineInst::mov(val_word2, reg),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                    ]);
                },
                AccessMode::Large(bytes) => {
                    unimplemented!("AccessMode::Large({})", bytes)
                },
                _ => {
                    let access_mode = val_result.access_mode();
                    let reg =
                        Register::direct(SubRegister::new(RCX, access_mode));
                    asm.extend(vec![
                        MachineInst::mov(ptr_result, Register::direct(RAX)),
                        MachineInst::mov(val_result, reg),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                    ]);
                }
            }
            CompiledInst::new(asm, comp_ptr.result.unwrap())
        } else {
            bug!("expected OxInstruction::Store, found {:?}", inst);
        }
    }

    fn codegen_load(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Load { ptr, .. } = inst {
            let mut asm = vec![];
            let mut comp_inst = self.codegen_value(*ptr);
            // FIXME:
            asm.append(&mut comp_inst.asm);
            let inst_size =
                inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let acc_mode = access_mode(inst_size as u64);
            let result = self.precompiled_result(inst);
            match acc_mode {
                AccessMode::Large(16) => {
                    asm.extend(vec![
                        MachineInst::mov(comp_inst.result.unwrap(),
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
                        MachineInst::mov(comp_inst.result.unwrap(),
                                         Register::direct(RAX)),
                        MachineInst::mov(Register::indirect(RAX),
                                         Register::direct(SubRegister::new(RAX,
                                                                           acc_mode))),
                        MachineInst::mov(Register::direct(SubRegister::new(RAX,
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

    fn codegen_checkoverflow(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::CheckOverflow { signed, .. } = inst {
            let result = self.precompiled_result(inst);
            // Get register %dl.
            let reg = Register::direct(SubRegister::new(RDX, AccessMode::Low8));
            let set_inst = if *signed {
                MachineInst::seto(reg)
            } else {
                MachineInst::setb(reg)
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

    fn codegen_cast(&mut self, inst: &OxInstruction) -> CompiledInst {
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
                        SubRegister::new(RAX, new_acc_mode));
                    let reg_old = Register::direct(
                        SubRegister::new(RAX, old_acc_mode));
                    v.asm.extend(vec![
                        MachineInst::xor(reg_new, reg_new),
                        MachineInst::mov(op, reg_old),
                        MachineInst::mov(reg_new, stack_loc.clone()),
                    ]);
                } else {
                    let old_acc_mode = op.access_mode();
                    let rax = Register::direct(SubRegister::new(RAX, old_acc_mode));
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

    fn codegen_ret(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Ret(value) = inst {
            let mut asm = vec![];
            if let Some(val) = value {
                let mut comp_inst = self.codegen_value(*val);
                asm.append(&mut comp_inst.asm);
                let result = comp_inst.result.unwrap();
                let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
                if size <= 64 {
                    let reg_full =
                        Register::direct(SubRegister::new(RAX, AccessMode::Full));
                    let reg =
                        Register::direct(SubRegister::new(RAX, access_mode(size)));
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
                            Register::direct(SubRegister::new(RDX, am2))));

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

    fn codegen_alloca(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Alloca { .. }= inst {
            let result = self.precompiled_result(inst);
            CompiledInst::with_result(result)
        } else {
            bug!("expected OxInstruction::Alloca, found {:?}", inst);
        }
    }

    fn codegen_unreachable(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Unreachable = inst {
            CompiledInst::with_instructions(vec![MachineInst::Ud2])
        } else {
            bug!("expected OxInstruction::Unreachable, found {:?}", inst);
        }
    }

    fn codegen_extractvalue(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::ExtractValue { agg, idx } = inst {
            let types = self.cx.types.borrow();
            let agg_ty = self.cx.val_ty(*agg);
            match types[agg_ty] {
                OxType::Struct { .. } => {
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
        &mut self,
        agg_dst: Operand,
        agg: Value,
        v: Value,
        idx: u64
    ) -> CompiledInst {
        let types = self.cx.types.borrow();
        let agg_ty = self.cx.val_ty(agg);
        let members = match types[agg_ty] {
            OxType::Struct { ref members, .. } => members,
            ref ty => bug!("Expected OxType::Struct, found {:?}", ty),
        };

        // The offset within the aggregated of the member at
        // position `idx`.
        let offset = types[agg_ty].offset(idx, &types) as isize;
        assert!(offset % 8 == 0);
        let offset = offset / 8;

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
                    Register::direct(SubRegister::new(RCX, member_am));
                let asm = vec![
                    MachineInst::mov(compiled_v, reg),
                    MachineInst::lea(member_rbp_offset,
                    // Store the result on the stack.
                                     Register::direct(RAX)),
                    MachineInst::mov(reg, Register::indirect(RAX)),
                ];
                CompiledInst::new(asm, agg_dst)
            },
            Value::Instruction { fn_idx, bb_idx, idx } => {
                let inst = &self.cx.module.borrow().functions[fn_idx]
                    .basic_blocks[bb_idx].instrs[idx];
                let reg =
                    Register::direct(SubRegister::new(RCX, member_am));
                if let Some(old_struct) = self.compiled_insts.get(&agg) {
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

    fn codegen_insertvalue(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::InsertValue { agg, elt, idx } = inst {
            let types = self.cx.types.borrow();
            let agg_ty = self.cx.val_ty(*agg);
            match types[agg_ty] {
                OxType::Struct { .. } => {
                    let agg_dst = self.precompiled_result(inst);
                    self.codegen_insertvalue_struct(agg_dst, *agg, *elt, *idx)
                }
                ref ty => unimplemented!("Insert into ty {:?}", ty),
            }
        } else {
            bug!("expected OxInstruction::Unreachable, found {:?}", inst);
        }
    }

    fn codegen_structgep(&mut self, inst: &OxInstruction) -> CompiledInst {
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

    fn codegen_gep(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Gep { ptr: agg, indices, .. } = inst {
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
                OxType::Struct { .. } if indices.len() > 1 => {
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

    fn codegen_invoke(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Invoke { .. } = inst {
            // Emit the call
            self.codegen_call(inst)
            // FIXME: skip the resume for now
        } else {
            bug!("expected OxInstruction::Invoke, found {:?}", inst);
        }
    }

    fn codegen_resume(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Resume(_) = inst {
            // FIXME: crash for now.
            CompiledInst::with_instructions(vec![MachineInst::Ud2])
        } else {
            bug!("expected OxInstruction::Resume, found {:?}", inst);
        }
    }

    fn codegen_landingpad(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::LandingPad { .. } = inst {
            // FIXME: crash for now.
            let landing_pad = self.precompiled_result(inst);
            CompiledInst::new(vec![MachineInst::Ud2], landing_pad)
        } else {
            bug!("expected OxInstruction::LandingPad, found {:?}", inst);
        }
    }

    fn codegen_mul(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Mul { lhs, rhs, signed } = inst {
            let mut asm = vec![];
            let mut comp_inst1 = self.codegen_value(*lhs);
            asm.append(&mut comp_inst1.asm);
            let mut comp_inst2 = self.codegen_value(*rhs);
            asm.append(&mut comp_inst2.asm);
            // The size in bits.
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg1 = Register::direct(SubRegister::new(RAX, access_mode(size)));
            let reg2 = Register::direct(SubRegister::new(RCX, access_mode(size)));
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
                MachineInst::mov(comp_inst1.result.unwrap(), reg1),
                MachineInst::mov(comp_inst2.result.unwrap(), reg2),
                mul_inst,
                MachineInst::mov(reg1, result.clone()),
            ]);
            CompiledInst::new(asm, result)

        } else {
            bug!("expected OxInstruction::Mul, found {:?}", inst);
        }
    }

    fn codegen_division(&mut self, inst: &OxInstruction) -> CompiledInst {
        match inst {
            OxInstruction::Div { lhs, rhs, signed } |
            OxInstruction::Rem { lhs, rhs, signed } => {
                let mut asm = vec![];
                let mut comp_lhs = self.codegen_value(*lhs);
                asm.append(&mut comp_lhs.asm);
                let mut comp_rhs = self.codegen_value(*rhs);
                asm.append(&mut comp_rhs.asm);
                // The size in bits.
                let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
                let rax = Register::direct(SubRegister::new(RAX, access_mode(size)));
                let rsi = Register::direct(SubRegister::new(RSI, access_mode(size)));
                let rdx = Register::direct(SubRegister::new(RDX, access_mode(size)));
                asm.push(MachineInst::xor(rdx, rdx));
                // The quotient is stored in %rax, and the remainder in %rdx.
                let div_inst = if *signed {
                    unimplemented!("Signed division");
                } else {
                    MachineInst::div(rsi)
                };
                let result = self.precompiled_result(inst);
                asm.extend(vec![
                    MachineInst::mov(comp_lhs.result.unwrap(), rax),
                    MachineInst::mov(comp_rhs.result.unwrap(), rsi),
                    div_inst
                ]);
                if let OxInstruction::Div { .. } = inst {
                    asm.push(MachineInst::mov(rax, result.clone()));
                } else {
                    asm.push(MachineInst::mov(rdx, result.clone()));
                }
                CompiledInst::new(asm, result)
            }
            _ => bug!("expected division instruction, found {:?}", inst),
        }
    }

    fn codegen_switch(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Switch { value, default, cases } = inst {
            let compiled_v = self.codegen_value(*value);
            let v_stack_loc = compiled_v.result.clone().unwrap();
            // Load the value and compare it with each of the cases
            let acc_mode = access_mode(
                self.cx.val_ty(*value).size(&self.cx.types.borrow()));
            let reg = Register::direct(SubRegister::new(RAX, acc_mode));
            let mut asm = Vec::with_capacity(5 * cases.len() + 1);
            let module = &self.cx.module.borrow();
            for (v, bb) in cases {
                let compiled_v = self.codegen_value(*v);
                asm.extend(compiled_v.asm);
                let compiled_v = compiled_v.result.clone().unwrap();
                let acc_mode = access_mode(
                    self.cx.val_ty(*v).size(&self.cx.types.borrow()));
                let v_reg = Register::direct(SubRegister::new(RCX, acc_mode));
                let bb = &module.functions[bb.0].basic_blocks[bb.1];
                asm.extend(vec![
                    MachineInst::mov(compiled_v, v_reg),
                    MachineInst::mov(v_stack_loc.clone(), reg),
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

    fn codegen_select(&mut self, inst: &OxInstruction) -> CompiledInst {
        if let OxInstruction::Select { cond, then_val, else_val } = inst {
            match cond {
                // FIXME: assert that the condition is either an i1 or a vector
                // of i1.
                Value::Instruction { .. }=> {
                    let mut asm = Vec::new();
                    let compiled_cond = self.codegen_instruction(*cond).result.unwrap();
                    let true_lbl = self.cx.get_sym_name("select_true");
                    let done_lbl = self.cx.get_sym_name("select_done");
                    // then_val and else_val have the same type/access mode.
                    let acc_mode = access_mode(
                        self.cx.val_ty(*then_val).size(&self.cx.types.borrow()));

                    let (then_val, else_val) = {
                        let mut add_asm_and_get_result =
                            |v: Value, asm: &mut Vec<MachineInst>| {
                                let v = self.codegen_value(v);
                                asm.extend(v.asm);
                                v.result.unwrap()
                            };
                        (add_asm_and_get_result(*then_val, &mut asm),
                         add_asm_and_get_result(*else_val, &mut asm))
                    };
                    let reg = Register::direct(SubRegister::new(RAX, acc_mode));
                    // The stack location where the result of this instruction
                    // will be stored.
                    let stack_loc = self.precompiled_result(inst);
                    let cond_reg = Register::direct(
                        SubRegister::new(RCX, AccessMode::Low8));
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

    /// Codegen instruction `inst_v`, and return the resulting `CompiledInst`.
    ///
    /// If this instruction has already been compiled, this returns the cached
    /// `CompiledInst`.
    /// Note: `inst_v` *has* to be a Value::Instruction.
    fn codegen_instruction(&mut self, inst_v: Value) -> CompiledInst {
        let module = self.cx.module.borrow();
        let inst = if let Value::Instruction { fn_idx, bb_idx, idx } = inst_v {
            &module.functions[fn_idx].basic_blocks[bb_idx].instrs[idx]
        } else {
            bug!("can only compile a Value::Instruction; found {:?}", inst_v);
        };
        if let Some(comp_inst) = self.compiled_insts.get(&inst_v) {
            return comp_inst.clone();
        }
        let comp_inst = match inst {
            OxInstruction::Br(..) => self.codegen_br(inst),
            OxInstruction::Ret(..) => self.codegen_ret(inst),
            OxInstruction::Store { .. } => self.codegen_store(inst),
            OxInstruction::Add { .. } => self.codegen_add(inst),
            OxInstruction::Sub { .. } => self.codegen_sub(inst),
            OxInstruction::Mul { .. } => self.codegen_mul(inst),
            OxInstruction::Div { .. } |
            OxInstruction::Rem { .. } => self.codegen_division(inst),
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
            OxInstruction::Neg(..) => self.codegen_neg(inst),
            OxInstruction::And { .. } => self.codegen_and(inst),
            OxInstruction::Invoke {..} => self.codegen_invoke(inst),
            OxInstruction::Resume(..)=> self.codegen_resume(inst),
            OxInstruction::LandingPad { .. } => self.codegen_landingpad(inst),
            OxInstruction::Select { .. } => self.codegen_select(inst),
            OxInstruction::Switch { .. } => self.codegen_switch(inst),
            OxInstruction::Gep { .. } => self.codegen_gep(inst),
        };
        // Cache the instruction, and return it on subsequent calls.
        self.compiled_insts.insert(inst_v, comp_inst.clone());
        comp_inst
    }

    /// Return the location where the result of `inst` is supposed to go.
    ///
    /// The `Operand` returned is always a location on the stack. Since each
    /// instruction has its own stack location for the result, instructions
    /// cannot clobber each other's results.
    fn precompiled_result(&self, inst: &OxInstruction) -> Operand {
        self.inst_loc[inst].clone()
    }

    /// Codegen a basic block.
    ///
    /// This returns the machine instructions that correspond to each `OxInstruction`'
    /// in the basic block. The first `MachineInst` in the vector returned a
    /// `MachineInst::Label`, which is not actually an instruction, but the label
    /// of the basic block.
    pub fn codegen_block(&mut self, bb: &OxBasicBlock) -> Vec<MachineInst> {
        let mut asm = vec![
            MachineInst::Label(bb.label.clone())
        ];
        for (idx, _) in bb.instrs.iter().enumerate() {
            let val_inst = Value::Instruction {
                fn_idx: bb.parent,
                bb_idx: bb.idx,
                idx
            };
            let mut comp_inst = self.codegen_instruction(val_inst);
            asm.append(&mut comp_inst.asm);
        }
        asm
    }

    /// Return the machine code for the prologue of this function.
    fn emit_prologue(&self) -> Vec<MachineInst> {
        vec![
            MachineInst::push(Register::direct(RBP)),
            MachineInst::mov(Register::direct(RSP), Register::direct(RBP)),
            MachineInst::sub(Operand::Immediate(self.stack_size as isize, AccessMode::Full),
                             Register::direct(RSP)),
        ]
    }

    /// Return the machine code for the epilogue of this function.
    fn emit_epilogue() -> Vec<MachineInst> {
        vec![
            MachineInst::Leave,
            MachineInst::Ret,
        ]
    }

    /// Map each `OxInstruction` in this function to the location on the stack
    /// where its result should be stored, and return all the machine instructions
    /// needed to make this work.
    ///
    /// In particular, `Alloca` instructions require additional instructions to be
    /// added to the function.
    fn map_insts_to_stack_locs(&mut self, f: &OxFunction) -> Vec<MachineInst> {
        let mut allocas = vec![];
        for bb in &f.basic_blocks {
            for inst in &bb.instrs {
                let instr_loc = match inst {
                    OxInstruction::Add { .. } |
                    OxInstruction::Sub { .. } |
                    OxInstruction::Mul { .. } |
                    OxInstruction::Div { .. } |
                    OxInstruction::Rem { .. } |
                    OxInstruction::And { .. } |
                    OxInstruction::Load { .. } |
                    OxInstruction::Cast { .. } |
                    OxInstruction::Icmp { .. } |
                    OxInstruction::CheckOverflow { .. } |
                    OxInstruction::LandingPad { .. } |
                    OxInstruction::StructGep { .. } |
                    OxInstruction::Gep { .. } |
                    OxInstruction::Select { .. } |
                    OxInstruction::InsertValue { .. } |
                    OxInstruction::ExtractValue { .. } |
                    OxInstruction::Not(_) |
                    OxInstruction::Neg(_) => {
                        let inst_size =
                            inst.val_ty(self.cx).size(&self.cx.types.borrow());
                        let acc_mode = access_mode(inst_size as u64);
                        self.stack_size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-self.stack_size, acc_mode);
                        Operand::from(result)
                    }
                    OxInstruction::Call { .. } | OxInstruction::Invoke { .. } => {
                        let ret_ty = inst.val_ty(self.cx);
                        // If the function doesn't return anything, carry on.
                        if let OxType::Void = self.cx.types.borrow()[ret_ty] {
                            continue;
                        }
                        let inst_size = ret_ty.size(&self.cx.types.borrow());
                        if inst_size > 128 {
                            unimplemented!("return value size: {}", inst_size);
                        }
                        let acc_mode = access_mode(inst_size as u64);
                        self.stack_size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-self.stack_size, acc_mode);
                        Operand::from(result)
                    }
                    OxInstruction::Alloca { ty, .. } => {
                        let types = &self.cx.types.borrow();
                        let inst_size = self.cx.ty_size(ty.pointee_ty(types));
                        let acc_mode = access_mode(inst_size);
                        self.stack_size += inst_size as isize / 8;
                        let result = Location::RbpOffset(-self.stack_size, acc_mode);
                        self.stack_size += std::mem::size_of::<usize>() as isize;
                        let result_addr =
                            Location::RbpOffset(-self.stack_size, AccessMode::Full);
                        let asm = vec![
                            MachineInst::lea(result, Register::direct(RAX)),
                            MachineInst::mov(Register::direct(RAX),
                                             result_addr.clone()),
                        ];
                        allocas.extend(asm.clone());
                        Operand::from(result_addr)
                    }
                    _ => continue,
                };
                self.inst_loc.insert((*inst).clone(), instr_loc);
            }
        }
        allocas
    }

    fn map_args_to_stack_locs(&mut self, f: &OxFunction) -> Vec<MachineInst> {
        let mut param_movs = vec![];
        for (idx, param) in f.params.iter().take(6).enumerate() {
            let param_size = self.cx.val_size(*param);
            self.stack_size += param_size as isize / 8;
            let acc_mode = access_mode(param_size);
            let reg = FunctionPrinter::fn_arg_reg(idx);
            let reg = Register::direct(SubRegister::new(reg, acc_mode));
            let result = Operand::from(
                Location::RbpOffset(-self.stack_size, acc_mode));
            param_movs.push(MachineInst::mov(reg, result.clone()));
            self.arg_loc.insert(*param, result);
        }
        let remaining_params: Vec<&Value> = f.params.iter().skip(6).collect();
        // The remaining arguments are pushed to the stack in reverse order.
        for (i, param) in remaining_params.iter().enumerate() {
            let real_param_size = self.cx.val_size(**param);
            let acc_mode = access_mode(real_param_size);
            // The first argument is at rbp + 16. All the remaining arguments of
            // the function are on the stack, aligned on an 8-byte boundary.
            let result = Operand::from(
                Location::RbpOffset(16 + i as isize * 8, acc_mode));
            self.arg_loc.insert(**param, result);
        }
        param_movs
    }

    /// Make sure the stack is 16-bytes aligned. This is necessary because the
    /// stack must be aligned to 16-byte boundary before each function call,
    /// according to the C calling convention
    fn align_stack(&mut self) {
        if self.stack_size % 16 != 0 {
            self.stack_size += 16 - self.stack_size % 16;
        }
    }
}

