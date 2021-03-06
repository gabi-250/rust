use super::ModuleIronOx;

use context::CodegenCx;
use ir::instruction::Instruction;
use ir::value::Value;

use ir::basic_block::OxBasicBlock;
use ir::function::OxFunction;
use ir::type_::{OxType, ScalarType, Type, TypeSize};
use rustc::mir::mono::Stats;
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::traits::BaseTypeMethods;
use rustc_codegen_ssa::traits::MiscMethods;
use std::cell::{Cell, RefCell};
use x86_64_instruction::MachineInst;
use x86_64_register::GeneralPurposeReg::{self, *};
use x86_64_register::{Location, Operand, Register, SubRegister, AccessMode,
                      operand_access_mode, access_mode};

macro_rules! asm {
    (
        $m:ident,
        $(
            $fmt:expr; [ $($args:expr),* ]
        ),*
    ) => {
        let mut asm_lines = vec![$(format!($fmt, $($args),*)),*];
        asm_lines = asm_lines.iter().map(|x| format!("\t{}\n", x)).collect();
        let asm_lines = asm_lines.join("");
        $m.push_str(&asm_lines);
    };
    ($m:ident, $new_asm:expr) => {
        $m.push_str($new_asm);
    }
}

fn label(sym: &str) -> String {
    format!("{}:\n", sym)
}

/// The result of evaluating an `Instruction`.
#[derive(Clone, Debug)]
pub struct CompiledInst {
    /// The sequence of assembly instructions the instruction is compiled to.
    asm: Vec<MachineInst>,
    /// The register/memory address that contains the result of the instruction.
    result: Option<Operand>,
}

impl CompiledInst {
    pub fn new(asm: Vec<MachineInst>) -> CompiledInst {
        CompiledInst { asm, result: None }
    }

    pub fn with_result(mut self, op: Operand) -> CompiledInst {
        self.result = Some(op);
        self
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
    stack_size: RefCell<Vec<usize>>,
    inst_stack_loc: RefCell<FxHashMap<Instruction, CompiledInst>>,
}

impl FunctionPrinter<'a, 'll, 'tcx> {
    fn new(cx: &'a CodegenCx<'ll, 'tcx>) -> FunctionPrinter<'a, 'll, 'tcx> {
        FunctionPrinter {
            cx,
            compiled_insts: Default::default(),
            compiled_params: Default::default(),
            stack_size: Default::default(),
            inst_stack_loc: Default::default(),
        }
    }

    fn codegen_function(&self, f: &OxFunction) -> (String, Vec<MachineInst>) {
        let mut asm = vec![];
        let (mut param_movs, stack_size) = self.compute_stack_size(&f);
        asm.append(&mut FunctionPrinter::emit_prologue(stack_size));
        for param in &f.params {
            let mut inst_asm = self.compile_value(*param);
            asm.append(&mut inst_asm.asm);
        }
        asm.append(&mut param_movs);
        for bb in &f.basic_blocks {
            asm.push(MachineInst::Label(bb.label.clone()));
            asm.append(&mut self.compile_block(&bb));
        }
        (label(&f.name), asm)
    }

    fn get_func_arg_str(idx: usize) -> GeneralPurposeReg {
        let regs = vec![RDI, RSI, RDX, RCX, R8, R9];
        if idx < 6 {
            return regs[idx].clone();
        } else {
            unimplemented!("function arg no {}", idx);
        }
    }

    fn compile_value(&self, value: Value) -> CompiledInst {
        let mut asm = vec![];
        let module = self.cx.module.borrow();
        match value {
            Value::ConstUint(idx) => {
                let value = self.cx.u_consts.borrow()[idx].value;
                let result = Operand::Immediate(value as isize);
                CompiledInst::new(asm).with_result(result)
            }
            Value::Param(_, _) => {
                if let Some(instr_asm) = self.compiled_params.borrow().get(&value) {
                    let result = instr_asm.result.clone();
                    CompiledInst::new(asm).with_result(result.unwrap())
                } else {
                    bug!("param should've already been compiled!");
                }
            }
            Value::Instruction(fn_idx, bb_idx, idx) => {
                let inst = &module.functions[fn_idx].basic_blocks[bb_idx].instrs[idx];
                if let Some(inst_asm) = self.compiled_insts.borrow().get(&value) {
                    // If the instruction is in `compiled_insts`, it means it has
                    // already been compiled.
                    if inst.is_branch() {
                        // If the value is an `Instruction::Br`, return a blank
                        // `CompiledInst` (at this point, `asm` == vec![]).
                        // Branching instructions have no result.
                        CompiledInst::new(asm)
                    } else {
                        // If this is any other kind of instruction, return its
                        // result (no need to emit the instruction again: since it
                        // was found in `compiled_insts`, it means the assembly file
                        // already contains the machine code of this instruction.
                        // This simply retrieves its result from the cache.
                        CompiledInst::new(asm).with_result(
                            inst_asm.result.clone().unwrap())
                    }
                } else {
                    self.compile_instruction(value)
                }
            }
            Value::Global(idx) => {
                let global = self.cx.globals.borrow()[idx].name.clone();
                let result = Operand::Loc(Location::RipOffset(global));
                CompiledInst::new(asm).with_result(result)
            }
            Value::Cast(idx) => {
                self.compile_value(self.cx.const_casts.borrow()[idx].value)
            }
            Value::Function(idx) => {
                // Move the result to the stack, and assume its size is 8.
                let function_name = if module.functions[idx].is_declaration() {
                    format!("{}@PLT", &module.functions[idx].name)
                } else {
                    module.functions[idx].name.clone()
                };
                // %rax
                let result = Location::from(Register::direct(RAX));
                asm.push(MachineInst::lea(Operand::Sym(function_name), result.clone()));
                CompiledInst::new(asm).with_result(Operand::from(result))
            }
            _ => {
                unimplemented!("compile_value({:?})", value);
            }
        }
    }

    fn compile_add(&self, inst: &Instruction) -> CompiledInst {
        if let Instruction::Add(lhs, rhs) = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.compile_value(*lhs);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.compile_value(*rhs);
            asm.append(&mut instr_asm2.asm);
            // The size in bits.
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(instr_asm1.result.unwrap(), reg),
                MachineInst::add(instr_asm2.result.unwrap(), reg),
            ]);
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(reg, result.clone()));
            CompiledInst::new(asm).with_result(result)
        } else {
            bug!("expected Instruction::Add");
        }
    }

    fn compile_sub(&self, inst: &Instruction) -> CompiledInst {
        if let Instruction::Sub(lhs, rhs) = inst {
            let mut asm = vec![];
            let mut instr_asm1 = self.compile_value(*lhs);
            asm.append(&mut instr_asm1.asm);
            let mut instr_asm2 = self.compile_value(*rhs);
            asm.append(&mut instr_asm2.asm);
            let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
            let reg = Register::direct(SubRegister::reg(RAX, access_mode(size)));
            asm.extend(vec![
                MachineInst::mov(instr_asm1.result.unwrap(), reg),
                MachineInst::sub(instr_asm2.result.unwrap(), reg),
            ]);
            let result = self.precompiled_result(inst);
            asm.push(MachineInst::mov(reg, result.clone()));
            CompiledInst::new(asm).with_result(result)
        } else {
            bug!("expected Instruction::Add");
        }
    }

    fn compile_instruction(&self, inst_v: Value) -> CompiledInst {
        let module = self.cx.module.borrow();
        let inst = if let Value::Instruction(fn_idx, bb_idx, idx) = inst_v {
            &module.functions[fn_idx].basic_blocks[bb_idx].instrs[idx]
        } else {
            bug!("can only compile a Value::Instruction; found {:?}", inst_v);
        };
        if let Some(instr_asm) = self.compiled_insts.borrow().get(&inst_v) {
            return instr_asm.clone();
        }
        let mut asm = vec![];
        let instr_asm = match inst {
            Instruction::Br(target) => {
                asm.push(MachineInst::jmp(target.to_string()));
                CompiledInst::new(asm)
            }
            Instruction::Ret(Some(val)) => {
                asm.push(
                    MachineInst::xor(Register::direct(RAX), Register::direct(RAX)));
                let mut instr_asm = self.compile_value(*val);
                asm.append(&mut instr_asm.asm);
                let result = instr_asm.result.unwrap();

                let size = inst.val_ty(self.cx).size(&self.cx.types.borrow());
                let reg = Register::direct(SubRegister::reg(RAX, access_mode(size)));
                asm.push(MachineInst::mov(result, reg));
                asm.append(&mut FunctionPrinter::emit_epilogue());
                CompiledInst::new(asm)
            }
            Instruction::Ret(None) => {
                asm.append(&mut FunctionPrinter::emit_epilogue());
                CompiledInst::new(asm)
            }
            Instruction::Store(dest, src) => {
                let mut instr_asm1 = self.compile_value(*dest);
                asm.append(&mut instr_asm1.asm);
                let mut instr_asm2 = self.compile_value(*src);
                asm.append(&mut instr_asm2.asm);
                let dest_result = instr_asm1.result.clone().unwrap();
                let src_result = instr_asm2.result.unwrap();

                let reg =
                    Register::direct(SubRegister::reg(RBX, src_result.access_mode()));
                if self.cx.val_ty(*dest).is_ptr(&self.cx.types.borrow()) {
                    asm.extend(vec![
                        MachineInst::mov(dest_result, Register::direct(RAX)),
                        MachineInst::mov(src_result, reg),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                    ]);
                } else {
                    asm.extend(vec![
                        MachineInst::lea(dest_result, Register::direct(RAX)),
                        MachineInst::mov(src_result, reg),
                        MachineInst::mov(reg, Register::indirect(RAX)),
                    ]);
                }
                CompiledInst::new(asm).with_result(instr_asm1.result.unwrap())
            }
            Instruction::Add(v1, v2) => self.compile_add(inst),
            Instruction::Sub(v1, v2) => self.compile_sub(inst),
            Instruction::Call(value, ref args) => {
                // Prepare the function arguments.
                for (idx, arg) in args.iter().enumerate() {
                    let mut instr_asm = self.compile_value(*arg);
                    asm.append(&mut instr_asm.asm);
                    let param = FunctionPrinter::get_func_arg_str(idx);
                    if arg.is_global() {
                        // Globals are always treated as pointers.
                        asm.push(MachineInst::lea(instr_asm.result.unwrap(),
                                                  Register::direct(param)));
                    } else {
                        asm.push(MachineInst::mov(instr_asm.result.unwrap(),
                                                  Register::direct(param)));
                    }
                }

                match value {
                    Value::Function(idx) => {
                        // Move the result to the stack, and assume its size is 8.
                        let fn_name = if module.functions[*idx].is_declaration() {
                            format!("{}@PLT", &module.functions[*idx].name)
                        } else {
                            module.functions[*idx].name.clone()
                        };
                        asm.push(MachineInst::call(fn_name.clone()));

                        let ret_ty = inst.val_ty(self.cx);
                        // If the function doesn't return anything, carry on.
                        if let OxType::Void = self.cx.types.borrow()[*ret_ty] {
                            CompiledInst::new(asm)
                        } else {
                            let result = self.precompiled_result(inst);
                            let acc_mode = result.access_mode();
                            asm.push(MachineInst::mov(
                                Register::direct(SubRegister::reg(RAX, acc_mode)),
                                result.clone()));
                            CompiledInst::new(asm).with_result(result)
                        }
                    }
                    Value::Instruction(_, _, _) => {
                        let ptr = self.compile_value(*value);
                        let result = ptr.result.clone().unwrap();
                        let acc_mode = result.access_mode();
                        let reg =
                            Register::direct(SubRegister::reg(RAX, acc_mode));
                        asm.extend(vec![
                            MachineInst::call(result.clone()),
                            MachineInst::mov(reg, result.clone()),
                        ]);
                        CompiledInst::new(asm).with_result(result)
                    }
                    _ => unimplemented!("call to {:?}", value),
                }
            }
            Instruction::Alloca(_, _, _) => {
                let result = self.precompiled_result(inst);
                CompiledInst::new(asm).with_result(result)
            }
            Instruction::Eq(v1, v2) | Instruction::Lt(v1, v2) => {
                let mut instr_asm1 = self.compile_value(*v1);
                asm.append(&mut instr_asm1.asm);
                let mut instr_asm2 = self.compile_value(*v2);
                asm.append(&mut instr_asm2.asm);
                let result1 = instr_asm1.result.unwrap();
                let result2 = instr_asm2.result.unwrap();
                // Move the value into a register and compare it with the second value.
                let reg = Register::direct(
                    SubRegister::reg(RAX, operand_access_mode(&result1, &result2)));
                asm.extend(vec![
                    MachineInst::mov(result2, reg),
                    MachineInst::cmp(reg, result1),
                ]);
                CompiledInst::new(asm)
            }
            Instruction::CondBr(cond, bb1, bb2) => match cond {
                Value::Instruction(fn_idx, bb_idx, idx) => {
                    let cond_inst =
                        &module.functions[*fn_idx].basic_blocks[*bb_idx].instrs[*idx];
                    match cond_inst {
                        Instruction::Eq(_, _) => {
                            let _ = self.compile_instruction(*cond);
                            asm.extend(vec![
                                MachineInst::je(bb1.to_string()),
                                MachineInst::jmp(bb2.to_string()),
                            ]);
                            CompiledInst::new(asm)
                        }
                        Instruction::Lt(_, _) => {
                            let _ = self.compile_instruction(*cond);
                            asm.extend(vec![
                                MachineInst::jl(bb1.to_string()),
                                MachineInst::jmp(bb2.to_string()),
                            ]);
                            CompiledInst::new(asm)
                        }
                        x => {
                            unimplemented!("cond br: {:?}", cond);
                        }
                    }
                }
                _ => {
                    bug!("cond must be a Value::Instruction, not {:?}", cond);
                }
            },
            Instruction::Load(ptr, _) => {
                let mut instr_asm = self.compile_value(*ptr);
                asm.append(&mut instr_asm.asm);
                let inst_size =
                    inst.val_ty(self.cx).size(&self.cx.types.borrow());
                let acc_mode = access_mode(inst_size as u64);
                let reg = Register::direct(SubRegister::reg(RAX, acc_mode));
                asm.push(MachineInst::mov(instr_asm.result.unwrap(), reg));
                if self.cx.val_ty(*ptr).is_ptr(&self.cx.types.borrow()) {
                    asm.push(MachineInst::mov(
                        Register::indirect(RAX),
                        Register::direct(SubRegister::reg(RAX, acc_mode))));
                }

                let result = self.precompiled_result(inst);
                asm.push(MachineInst::mov(
                    Register::direct(SubRegister::reg(RAX, acc_mode)), result.clone()));
                CompiledInst::new(asm).with_result(result)
            }
            Instruction::CheckOverflow(inst, ty, signed) => {
                let instr_asm = self.compile_instruction(*inst);
                // Get register %dl.
                let reg = Register::direct(SubRegister::reg(RDX, AccessMode::Low8));
                if *signed {
                    asm.push(MachineInst::seto(reg));
                } else {
                    asm.push(MachineInst::setb(reg));
                }
                CompiledInst::new(asm).with_result(Operand::from(reg))
            }
            Instruction::Cast(inst, ty) => {
                // FIXME: Disregard the type for now.
                self.compile_value(*inst)
            }
            Instruction::Unreachable => {
                asm.push(MachineInst::UD2);
                CompiledInst::new(asm)
            }
            _ => {
                unimplemented!("instruction {:?}", inst);
            }
        };
        let compiled_inst = if let Some(ref result) = instr_asm.result {
            CompiledInst::new(instr_asm.asm.clone()).with_result(result.clone())
        } else {
            CompiledInst::new(instr_asm.asm.clone())
        };
        self.compiled_insts.borrow_mut().insert(inst_v, compiled_inst);
        instr_asm
    }

    fn precompiled_result(&self, inst: &Instruction) -> Operand {
        self.inst_stack_loc.borrow()[inst].result.clone().unwrap().clone()
    }

    fn already_compiled(&self, value: Value) -> bool {
        self.compiled_insts.borrow().get(&value).is_some()
    }

    pub fn compile_block(&self, bb: &OxBasicBlock) -> Vec<MachineInst> {
        let mut asm = vec![];
        for (inst_idx, inst) in bb.instrs.iter().enumerate() {
            let inst = &bb.instrs[inst_idx];
            if self.already_compiled(Value::Instruction(bb.parent, bb.idx, inst_idx)) {
                // Always compile branching instructions.
                if inst.is_branch() {
                    let mut instr_asm = self.compile_instruction(
                        Value::Instruction(bb.parent, bb.idx, inst_idx));
                    asm.append(&mut instr_asm.asm);
                }
            } else {
                let mut instr_asm = self.compile_instruction(
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
            MachineInst::sub(Operand::Immediate(stack_size as isize),
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

        for (idx, param) in f.params.iter().enumerate() {
            let param_size =
                self.cx.val_ty(*param).size(&self.cx.types.borrow()) as isize;
            size += param_size;
            let acc_mode = access_mode(param_size as u64);
            let offset = -size / 8;
            let result = Location::RbpOffset(offset, acc_mode);
            let reg = FunctionPrinter::get_func_arg_str(idx);
            let reg = Register::direct(SubRegister::reg(reg, acc_mode));
            param_movs.push(MachineInst::mov(reg, result.clone()));
            let instr_asm =
                CompiledInst::new(param_movs.clone())
                    .with_result(Operand::from(result));
            self.compiled_params.borrow_mut().insert(*param, instr_asm);
        }
        for bb in &f.basic_blocks {
            for inst in &bb.instrs {
                let instr_asm = match inst {
                    Instruction::Add(v1, v2) | Instruction::Sub(v1, v2) => {
                        let inst_size =
                            inst.val_ty(self.cx).size(&self.cx.types.borrow());
                        let acc_mode = access_mode(inst_size as u64);
                        size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-size, acc_mode);
                        CompiledInst::new(vec![]).with_result(Operand::from(result))
                    }
                    Instruction::Call(_, _) => {
                        let ret_ty = inst.val_ty(self.cx);
                        // If the function doesn't return anything, carry on.
                        if let OxType::Void = self.cx.types.borrow()[*ret_ty] {
                            continue;
                        }
                        let inst_size = ret_ty.size(&self.cx.types.borrow());
                        let acc_mode = access_mode(inst_size as u64);
                        // FIXME: check non void ret.
                        size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-size, acc_mode);
                        CompiledInst::new(vec![]).with_result(Operand::from(result))
                    }
                    Instruction::Alloca(_, ty, align) => {
                        let inst_size =
                            inst.val_ty(self.cx).size(&self.cx.types.borrow());
                        let acc_mode = access_mode(inst_size as u64);
                        size += (inst_size / 8) as isize;
                        let result = Location::RbpOffset(-size, acc_mode);
                        CompiledInst::new(vec![]).with_result(Operand::from(result))
                    }
                    Instruction::Load(ptr, _) => {
                        let inst_size =
                            inst.val_ty(self.cx).size(&self.cx.types.borrow());
                        let acc_mode = access_mode(inst_size as u64);
                        size += (inst_size / 8) as isize;
                        let result = Operand::Loc(Location::RbpOffset(-size, acc_mode));
                        CompiledInst::new(vec![]).with_result(Operand::from(result))
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
}

impl AsmPrinter<'ll, 'tcx> {
    pub fn new(cx: CodegenCx<'ll, 'tcx>) -> AsmPrinter<'ll, 'tcx> {
        AsmPrinter { cx }
    }

    fn constant_value(&self, v: Value) -> String {
        match v {
            Value::ConstCstr(idx) => self.cx.const_cstrs.borrow()[idx].name.clone(),
            Value::Global(idx) => {
                self.constant_value(self.cx.globals.borrow()[idx].initializer.unwrap())
            }
            _ => unimplemented!("value of {:?}", v),
        }
    }

    fn compile_const_global(&self, c: Value) -> String {
        let mut asm = String::new();
        match c {
            Value::ConstFatPtr(idx) => {
                let (v1, v2) = self.cx.const_fat_ptrs.borrow()[idx];
                asm!(asm, &self.compile_const_global(v1));
                asm!(asm, &self.compile_const_global(v2));
            }
            Value::ConstUint(idx) => {
                let u_const = self.cx.u_consts.borrow()[idx];
                let directive = self.get_decl_directive(u_const.ty);
                asm!(asm, "{}\t{}"; [directive, u_const.value]);
            }
            Value::Cast(idx) => {
                let cast_inst = &self.cx.const_casts.borrow()[idx];
                let directive = self.get_decl_directive(cast_inst.ty);
                let v = self.constant_value(cast_inst.value);
                asm!(asm, "{}\t{}"; [directive, v]);
            }
            Value::Function(idx) => {}
            Value::Cast(idx) => {}
            _ => unimplemented!("compile_const_global({:?})", c),
        };
        asm.to_string()
    }

    fn declare_globals(&self) -> String {
        let mut asm = ".section .rodata\n".to_string();
        for global in self.cx.globals.borrow().iter() {
            match global.initializer {
                Some(Value::ConstCstr(idx)) => {
                    let c_str = &self.cx.const_cstrs.borrow()[idx];
                    let const_str = self.get_str(c_str.ptr, c_str.len);
                    asm!(asm, ".type {},@object"; [c_str.name],
                              ".size {},{}"; [c_str.name, c_str.len]);
                    asm.push_str(&label(&c_str.name));
                    asm!(asm, ".ascii \"{}\""; [const_str]);
                }
                Some(Value::ConstStruct(idx)) => {
                    let const_struct = &self.cx.const_structs.borrow()[idx];
                    asm.push_str(&label(&global.name));
                    for c in &const_struct.components {
                        asm!(asm, &self.compile_const_global(*c));
                    }
                }
                None => bug!("no initializer found for {:?}", global),
                _ => unimplemented!("declare_globals({:?})", global),
            }
        }
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

    fn get_decl_directive(&self, ty: Type) -> String {
        match self.cx.types.borrow()[*ty] {
            OxType::Scalar(ScalarType::I32) => ".long",
            OxType::Scalar(ScalarType::I64) => ".quad",
            OxType::PtrTo { .. } => ".quad",
            _ => unimplemented!("type of {:?}", ty),
        }.to_string()
    }


    fn declare_functions(&self) -> String {
        let mut asm = ".text\n".to_string();
        let module = self.cx.module.borrow();
        for f in &module.functions {
            asm!(asm, ".globl {}"; [f.name],
                      ".type {},@function"; [f.name]);
        }
        asm
    }

    /// Consume the printer, and return the stats and the codegen result.
    ///
    /// The codegen result is a string that contains the x86-64 program that
    /// corresponds to the module from the `CodegenCx` of this printer.
    pub fn codegen(self) -> (Stats, String) {
        let mut asm = String::new();
        // Define the globals.
        asm!(asm, &self.declare_globals());
        // Declare the functions.
        asm!(asm, &self.declare_functions());
        for f in &self.cx.module.borrow().functions {
            if !f.is_declaration() {
                let mut fn_asm = String::new();
                let (label, instructions) =
                    FunctionPrinter::new(&self.cx).codegen_function(&f);
                fn_asm.push_str(&label);
                for inst in instructions {
                    fn_asm.push_str(&format!("{}", inst));
                }
                asm.push_str(&fn_asm);
            }
        }
        (self.cx.consume_stats().into_inner(), asm)
    }
}
