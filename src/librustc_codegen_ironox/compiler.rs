use super::ModuleIronOx;
use value::{Value, Instruction};

use rustc::util::nodemap::FxHashMap;

#[derive(Debug)]
pub struct InstrAsm {
    asm: String,
    result: String,
}

pub struct ModuleAsm<'a> {
    module: &'a ModuleIronOx,
    compiled_insts: FxHashMap<&'a Instruction, InstrAsm>,
    compiled_params: FxHashMap<Value, InstrAsm>,
    stack_size: usize,
}

impl ModuleAsm<'a> {
    pub fn new(module: &ModuleIronOx) -> ModuleAsm {
        ModuleAsm {
            module,
            compiled_insts: Default::default(),
            compiled_params: Default::default(),
            stack_size: 0,
        }
    }

    pub fn get_func_arg_str(idx: usize) -> String {
        let regs = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        if idx < 6 {
            return regs[idx].to_string()
        } else {
            unimplemented!("function arg no {}", idx);
        }
    }

    pub fn compile_add(&mut self, inst: &'a Instruction) -> InstrAsm {
        if let Instruction::Add(lhs, rhs) = inst {
            let mut asm = "".to_string();
            let (new_asm, arg1) = self.compile_value(*lhs);
            asm.push_str(&new_asm);
            let (new_asm, arg2) = self.compile_value(*rhs);
            asm.push_str(&new_asm);
            asm.push_str("\txor %rax, %rax\n");
            asm.push_str(&format!("\tadd {}, %rax\n", arg1));
            asm.push_str(&format!("\tadd {}, %rax\n", arg2));
            // FIXME:
            let param_size = 8;
            asm.push_str(&format!("\tsub ${}, %rsp\n", param_size));
            self.stack_size += param_size;
            //
            let result = format!("-{}(%rbp)", self.stack_size);
            asm.push_str(&format!("\tmov %rax, {}\n", result));
            InstrAsm { asm, result }
        } else {
            bug!("expected Instruction::Add");
        }
    }

    pub fn compile_sub(&mut self, inst: &'a Instruction) -> InstrAsm {
        if let Instruction::Sub(lhs, rhs) = inst {
            let mut asm = "".to_string();
            let (new_asm, arg1) = self.compile_value(*lhs);
            asm.push_str(&new_asm);
            let (new_asm, arg2) = self.compile_value(*rhs);
            asm.push_str(&new_asm);
            asm.push_str("\txor %rax, %rax\n");
            asm.push_str(&format!("\tadd {}, %rax\n", arg1));
            asm.push_str(&format!("\tsub {}, %rax\n", arg2));
            // FIXME:
            let param_size = 8;
            asm.push_str(&format!("\tsub ${}, %rsp\n", param_size));
            self.stack_size += param_size;
            //
            let result = format!("-{}(%rbp)", self.stack_size);
            asm.push_str(&format!("\tmov %rax, {}\n", result));
            InstrAsm { asm, result }
        } else {
            bug!("expected Instruction::Add");
        }
    }

    pub fn compile_value(&mut self, value: Value) -> (String, String) {
        match value {
            Value::ConstUint(idx) => {
                let value = self.module.u_consts[idx].value;
                ("".to_string(), format!("${}", value))
            },
            Value::Param(idx, _) => {

                if let Some(instr_asm) = self.compiled_params.get(&value) {
                    let ret = ("".to_string(), instr_asm.result.clone());
                    return ret;
                }
                // FIXME:
                let param_size = 8;
                let reg = ModuleAsm::get_func_arg_str(idx);
                let mut asm = format!("\tsub ${}, %rsp\n", param_size);
                self.stack_size += param_size;
                let result = format!("-{}(%rbp)", self.stack_size);
                asm.push_str(&format!("\tmov {}, {}\n", reg, result));
                let instr_asm = InstrAsm{ asm: asm.clone(), result: result.clone() };
                self.compiled_params.insert(value, instr_asm);
                (asm, result)
            },
            Value::Instruction(fn_idx, bb_idx, idx) => {
                let inst = &self.module.functions[fn_idx].
                    basic_blocks[bb_idx].instrs[idx];
                if let Some(inst_asm) = self.compiled_insts.get(inst) {
                    if let Instruction::Br(..) = inst {
                        // nope
                    } else {
                        return ("".to_string(), inst_asm.result.clone());
                    }
                }
                self.compile_instruction(value)
            },
            _ => {
                unimplemented!("compile_value({:?})", value);
            }
        }
    }

    pub fn compile_instruction(&mut self, inst_v: Value) -> (String, String) {
        let mut inst;
        if let Value::Instruction(fn_idx, bb_idx, idx) = inst_v {
            inst = &self.module.functions[fn_idx].basic_blocks[bb_idx].instrs[idx];
        } else {
            bug!("can only compile a Value::Instruction");
        }

        if let Some(instr_asm) = self.compiled_insts.get(inst) {
            let ret = (instr_asm.asm.clone(), instr_asm.result.clone());
            return ret;
        }

        let mut asm = "".to_string();
        let instr_asm = match inst {
            Instruction::Br(target) => {
                asm.push_str(&format!("\tjmp {}\n", target));
                (asm, "".to_string())
            },
            Instruction::Ret(Some(val)) => {
                let (new_asm, res) = self.compile_value(*val);
                asm.push_str(&new_asm);
                asm.push_str(&format!("\tmov {}, %rax\n", res));
                asm.push_str("\tleave\n");
                asm.push_str("\tret\n");
                (asm, "".to_string())
            },
            Instruction::Ret(None) => {
                asm.push_str("\tleave\n");
                asm.push_str("\tret\n");
                (asm, "".to_string())
            },
            Instruction::Store(v1, v2) => {
                let (new_asm, dest) = self.compile_value(*v1);
                asm.push_str(&new_asm);
                let (new_asm, source) = self.compile_value(*v2);
                asm.push_str(&new_asm);
                asm.push_str(&format!("\tlea {}, %rax\n", dest));
                // if dest is a pointer:
                //asm.push_str(&format!("\tmov {}, %rax\n", dest));
                asm.push_str(&format!("\tmovq {}, %rbx\n", source));
                asm.push_str("\tmovq %rbx, (%rax)\n");
                (asm, dest)
            },
            Instruction::Add(v1, v2) => {
                let instr_asm = self.compile_add(inst);
                (instr_asm.asm, instr_asm.result.clone())
            },
            Instruction::Sub(v1, v2) => {
                let instr_asm = self.compile_sub(inst);
                (instr_asm.asm, instr_asm.result.clone())
            },
            Instruction::Call(ref fn_idx, ref args) => {
                for (idx, arg) in args.iter().enumerate() {
                    let (new_asm, value) = self.compile_value(*arg);
                    asm.push_str(&new_asm);
                    let param = ModuleAsm::get_func_arg_str(idx);
                    asm.push_str(&format!("\tmov {}, {}\n", value, param));
                }
                asm.push_str(
                    &format!("\tcall {}\n", self.module.functions[*fn_idx].name));

                // move the result to the stack, and assume its size is 8
                let param_size = 8;
                asm.push_str(&format!("\tsub ${}, %rsp\n", param_size));
                self.stack_size += param_size;
                let result = format!("-{}(%rbp)", self.stack_size);
                asm.push_str(&format!("\tmov %rax, {}\n", result));
                (asm, result)
            },

            Instruction::Alloca(_, ty, ty_size, align) => {
                asm.push_str(&format!("\tsub ${}, %rsp\n", ty_size));
                self.stack_size += *ty_size as usize;
                let result = format!("-{}(%rbp)", self.stack_size);
                (asm, result)
            },
            Instruction::Eq(v1, v2) | Instruction::Lt(v1, v2) => {
                let (new_asm, dest) = self.compile_value(*v1);
                asm.push_str(&new_asm);
                let (new_asm, source) = self.compile_value(*v2);
                asm.push_str(&new_asm);
                asm.push_str(&format!("\tcmpq {}, {}\n", source, dest));
                (asm, "".to_string())
            },
            Instruction::CondBr(cond, bb1, bb2) => {
                match cond {
                    Value::Instruction(fn_idx, bb_idx, idx) => {
                        let cond_inst =
                            &self.module.functions[*fn_idx]
                                .basic_blocks[*bb_idx].instrs[*idx];
                        match cond_inst {
                            Instruction::Eq(_, _) => {
                                let _ = self.compile_instruction(*cond);
                                asm.push_str(&format!("\tje {}\n", bb1));
                                asm.push_str(&format!("\tjmp {}\n", bb2));
                                (asm, "".to_string())
                            },
                            Instruction::Lt(_, _) => {
                                let _ = self.compile_instruction(*cond);
                                asm.push_str(&format!("\tjl {}\n", bb1));
                                asm.push_str(&format!("\tjmp {}\n", bb2));
                                (asm, "".to_string())
                            },
                            x => {
                                unimplemented!("cond br: {:?}", cond);
                            }
                        }
                    },
                    _ => {
                        bug!("cond must be a Value::Instruction, not {:?}", cond);
                    }
                }
            },
            _ => {
                unimplemented!("instruction {:?}", inst);
            },
        };
        let ret = instr_asm.clone();
        self.compiled_insts.insert(inst,
                                   InstrAsm { asm: instr_asm.0, result: instr_asm.1 });
        ret
    }

    pub fn already_compiled(&self, inst: &Instruction) -> bool {
        self.compiled_insts.get(inst).is_some()
    }

    pub fn compile(&mut self) -> String {
        let mut asm = ".text\n".to_string();
        let module = self.module;
        for f in &module.functions {
            asm.push_str(&format!("\t.globl {}\n", f.name));
            asm.push_str(&format!("\t.type {},@function\n", f.name));
        }
        for (fn_idx, f) in module.functions.iter().enumerate() {
            self.compiled_params.clear();
            let mut ret = None;
            let mut ret_bb = "".to_string();
            self.stack_size = 0;
            asm.push_str(&format!("{}:\n", f.name));
            asm.push_str("\tpush %rbp\n");
            asm.push_str("\tmov %rsp, %rbp\n");
            for param in &f.params {
                asm.push_str(&self.compile_value(*param).0);
            }
            for (bb_idx, bb) in f.basic_blocks.iter().enumerate() {
                for (inst_idx, inst) in bb.instrs.iter().enumerate() {
                    // Make sure the ret is emitted at the end
                    if let Instruction::Ret(_) = inst {
                        if inst_idx == 0 {
                            ret_bb.push_str(&format!("\t{}:\n", bb.label));
                        }
                        ret = Some(Value::Instruction(fn_idx, bb_idx, inst_idx));
                        continue;
                    }
                    if inst_idx == 0 {
                        asm.push_str(&format!("\t{}:\n", bb.label));
                    }
                    let inst = &self.module.functions[fn_idx].
                        basic_blocks[bb_idx].instrs[inst_idx];

                    if self.already_compiled(inst) {
                        // always compile branching instructions
                        if inst.is_branch() {
                            let (new_asm, _) = self.compile_instruction(
                                Value::Instruction(fn_idx, bb_idx, inst_idx));
                            asm.push_str(&new_asm);
                        }
                    } else {
                        let (new_asm, _) = self.compile_instruction(
                            Value::Instruction(fn_idx, bb_idx, inst_idx));
                        asm.push_str(&new_asm);
                    }
                }
            }
            // emit the ret:
            if let Some(inst) = ret {
                asm.push_str(&ret_bb);
                let (new_asm, _) = self.compile_instruction(inst);
                asm.push_str(&new_asm);
            } else {
                bug!("No return instruction for {}", f.name);
            }
        }
        asm
    }
}
