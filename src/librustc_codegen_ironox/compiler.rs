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
    stack_size: usize,
}

impl ModuleAsm<'a> {
    pub fn new(module: &ModuleIronOx) -> ModuleAsm {
        ModuleAsm {
            module,
            compiled_insts: Default::default(),
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

    pub fn compile_value(&mut self, value: Value) -> (String, String) {
        match value {
            Value::ConstUint(idx) => {
                let value = self.module.u_consts[idx].value;
                ("".to_string(), format!("${}", value))
            },
            Value::Param(idx, _) => {
                // FIXME:
                let param_size = 8;
                let reg = ModuleAsm::get_func_arg_str(idx);
                let mut asm = format!("\tsub ${}, %rsp\n", param_size);
                self.stack_size += param_size;
                asm.push_str(&format!("\tmov {}, -{}(%rbp)\n", reg, self.stack_size));
                (asm, format!("-{}(%rbp)", self.stack_size))
            },
            Value::Instruction(fn_idx, bb_idx, idx) => {
                let inst = &self.module.functions[fn_idx].
                    basic_blocks[bb_idx].instrs[idx];
                if let Some(inst_asm) = self.compiled_insts.get(inst) {
                    return ("".to_string(), inst_asm.result.clone());
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
                asm.push_str(&format!("\tmov {}, %rax\n", dest));
                asm.push_str(&format!("\tmovq {}, (%rax)\n", source));
                (asm, dest)
            },
            Instruction::Add(v1, v2) => {
                let instr_asm = self.compile_add(inst);
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
            _ => {
                unimplemented!("instruction");
            },
        };
        let ret = instr_asm.clone();
        self.compiled_insts.insert(inst,
                                   InstrAsm { asm: instr_asm.0, result: instr_asm.1 });
        ret
    }

    pub fn compile(&mut self) -> String {
        let mut asm = ".text\n".to_string();
        let module = self.module;
        for f in &module.functions {
            asm.push_str(&format!("\t.globl {}\n", f.name));
            asm.push_str(&format!("\t.type {},@function\n", f.name));
        }
        for (fn_idx, f) in module.functions.iter().enumerate() {
            self.stack_size = 0;
            asm.push_str(&format!("{}:\n", f.name));
            asm.push_str("\tpush %rbp\n");
            asm.push_str("\tmov %rsp, %rbp\n");
            for (bb_idx, bb) in f.basic_blocks.iter().enumerate() {
                asm.push_str(&format!("\t{}:\n", bb.label));
                for (inst_idx, inst) in bb.instrs.iter().enumerate() {
                    let inst = &self.module.functions[fn_idx].
                        basic_blocks[bb_idx].instrs[inst_idx];
                    if !self.compiled_insts.get(inst).is_some() {
                        let (new_asm, _) = self.compile_instruction(
                            Value::Instruction(fn_idx, bb_idx, inst_idx));
                        asm.push_str(&new_asm);
                    }
                }
            }
        }
        asm
    }
}
