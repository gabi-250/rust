use super::ModuleIronOx;

use context::CodegenCx;
use ir::value::Value;
use ir::instruction::Instruction;

use rustc::mir::mono::Stats;
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::traits::MiscMethods;
use rustc_codegen_ssa::traits::BaseTypeMethods;
use std::cell::{Cell, RefCell};
use ir::type_::{Type, TypeSize, OxType, ScalarType};
use ir::basic_block::OxBasicBlock;
use ir::function::OxFunction;
use x86_register::Register;

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

macro_rules! label {
    ($m:ident, $lbl:expr) => {
        $m.push_str(&format!("{}:\n", $lbl));
    }
}

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

macro_rules! label {
    ($m:ident, $lbl:expr) => {
        $m.push_str(&format!("{}:\n", $lbl));
    }
}

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
    stack_size: RefCell<Vec<usize>>,
    inst_stack_loc: RefCell<FxHashMap<Instruction, InstrAsm>>,
}

impl ModuleAsm<'ll, 'tcx> {
    pub fn new(cx: CodegenCx<'ll, 'tcx>) -> ModuleAsm<'ll, 'tcx> {
        ModuleAsm {
            cx,
            compiled_insts: Default::default(),
            compiled_params: Default::default(),
            stack_size: Default::default(),
            inst_stack_loc: Default::default(),
        }
    }

    pub fn get_func_arg_str(idx: usize) -> Register {
        let regs = vec![Register::RDI, Register::RSI, Register::RDX,
                        Register::RCX, Register::R8, Register::R9];
        if idx < 6 {
            return regs[idx]
        } else {
            unimplemented!("function arg no {}", idx);
        }
    }

    pub fn compile_add(&self, inst: &Instruction) -> InstrAsm {
        if let Instruction::Add(lhs, rhs) = inst {
            let mut asm = "".to_string();
            let (new_asm, arg1) = self.compile_value(*lhs);
            asm!(asm, &new_asm);
            let (new_asm, arg2) = self.compile_value(*rhs);
            asm!(asm, &new_asm);
            if self.cx.val_ty(*lhs).is_ptr(&self.cx.types.borrow()) {
                asm!(asm, "mov {}, %rbx"; [arg1],
                          "mov (%rbx), %rax"; [],
                          "add {}, %rax"; [arg2]);
            } else {
                asm!(asm, "mov {}, %rax"; [arg1],
                          "add {}, %rax"; [arg2]);
            }
            // FIXME:
            let result =
                self.inst_stack_loc.borrow().get(inst).unwrap().result.clone();
            asm!(asm, "mov %rax, {}"; [result]);
            InstrAsm { asm, result }
        } else {
            bug!("expected Instruction::Add");
        }
    }

    pub fn compile_sub(&self, inst: &Instruction) -> InstrAsm {
        if let Instruction::Sub(lhs, rhs) = inst {
            let mut asm = "".to_string();
            let (new_asm, arg1) = self.compile_value(*lhs);
            asm!(asm, &new_asm);
            let (new_asm, arg2) = self.compile_value(*rhs);
            asm!(asm, &new_asm);
            asm!(asm, "xor %rax, %rax"; [],
                      "add {}, %rax"; [arg1],
                      "sub {}, %rax"; [arg2]);
            // FIXME:
            let result =
                self.inst_stack_loc.borrow().get(inst).unwrap().result.clone();
            asm!(asm, "mov %rax, {}"; [result]);
            InstrAsm { asm, result }
        } else {
            bug!("expected Instruction::Add");
        }
    }

    pub fn compile_value(&self, value: Value) -> (String, String) {
        let module = self.cx.module.borrow();
        match value {
            Value::ConstUint(idx) => {
                let value = self.cx.u_consts.borrow()[idx].value;
                ("".to_string(), format!("${}", value))
            },
            Value::Param(_, _) => {
                if let Some(instr_asm) = self.compiled_params.borrow().get(&value) {
                    return ("".to_string(), instr_asm.result.clone());
                } else {
                    bug!("param should've already been compiled!");
                }
            },
            Value::Instruction(fn_idx, bb_idx, idx) => {
                let inst = &module.functions[fn_idx].
                    basic_blocks[bb_idx].instrs[idx];
                if let Some(inst_asm) = self.compiled_insts.borrow().get(&value) {
                    if let Instruction::Br(..) = inst {
                        // Do nothing.
                    } else {
                        return ("".to_string(), inst_asm.result.clone());
                    }
                }
                self.compile_instruction(value)
            },
            Value::Global(idx) => {
                let global = format!("{}(%rip)",
                                     self.cx.globals.borrow()[idx].name.clone());
                ("".to_string(), global)
            }
            Value::Cast(idx) => {
                self.compile_value(self.cx.const_casts.borrow()[idx].value)
            }
            Value::Function(idx) => {
                let mut asm = String::new();
                // Move the result to the stack, and assume its size is 8.
                let function_name = if module.functions[idx].is_declaration() {
                    format!("{}@PLT", &module.functions[idx].name)
                } else {
                    module.functions[idx].name.clone()
                };
                let result = "%rax".to_string();
                asm!(asm, "lea {}, %rax"; [function_name]);
                (asm, result)
            }
            _ => {
                unimplemented!("compile_value({:?})", value);
            }
        }
    }

    pub fn compile_instruction(&self, inst_v: Value) -> (String, String) {
        let module = self.cx.module.borrow();
        let inst = if let Value::Instruction(fn_idx, bb_idx, idx) = inst_v {
            &module.functions[fn_idx].basic_blocks[bb_idx].instrs[idx]
        } else {
            bug!("can only compile a Value::Instruction; found {:?}", inst_v);
        };
        if let Some(instr_asm) = self.compiled_insts.borrow().get(&inst_v) {
            return (instr_asm.asm.clone(), instr_asm.result.clone());
        }
        let mut asm = "".to_string();
        let instr_asm = match inst {
            Instruction::Br(target) => {
                asm!(asm, "jmp {}"; [target]);
                (asm, "".to_string())
            }
            Instruction::Ret(Some(val)) => {
                let (new_asm, res) = self.compile_value(*val);
                asm!(asm, &new_asm);
                asm!(asm, "mov {}, %rax"; [res],
                          "leave"; [],
                          "ret"; []);
                (asm, "".to_string())
            }
            Instruction::Ret(None) => {
                asm!(asm, "leave"; [],
                          "ret"; []);
                (asm, "".to_string())
            }
            Instruction::Store(v1, v2) => {
                let (new_asm, dest) = self.compile_value(*v1);
                asm!(asm, &new_asm);
                let (new_asm, source) = self.compile_value(*v2);
                asm!(asm, &new_asm);
                if self.cx.val_ty(*v1).is_ptr(&self.cx.types.borrow()) {
                    asm!(asm, "mov {}, %rax"; [dest],
                              "movq {}, %rbx"; [source],
                              "movq %rbx, (%rax)"; []);
                } else {
                    asm!(asm, "lea {}, %rax"; [dest],
                              "movq {}, %rbx"; [source],
                              "movq %rbx, (%rax)"; []);
                }
                (asm, dest)
            }
            Instruction::Add(v1, v2) => {
                let instr_asm = self.compile_add(inst);
                (instr_asm.asm, instr_asm.result.clone())
            }
            Instruction::Sub(v1, v2) => {
                let instr_asm = self.compile_sub(inst);
                (instr_asm.asm, instr_asm.result.clone())
            }
            Instruction::Call(value, ref args) => {
                let result = match value {
                    Value::Function(idx) => {
                        // Move the result to the stack, and assume its size is 8.
                        let function_name = if module.functions[*idx].is_declaration() {
                            format!("{}@PLT", &module.functions[*idx].name)
                        } else {
                            module.functions[*idx].name.clone()
                        };
                        let result =
                            self.inst_stack_loc.borrow().get(inst).unwrap().result.clone();
                        asm!(asm, "call {}"; [function_name],
                                  "mov %rax, {}"; [result]);
                        function_name
                    }
                    Value::Instruction(_, _, _) => {
                        eprintln!("value is {:?}", *value);
                        let ptr = self.compile_value(*value);

                        let result =
                            self.inst_stack_loc.borrow().get(inst).unwrap().result.clone();
                        asm!(asm, "call *{}"; [ptr.1],
                                  "mov %rax, {}"; [result]);
                        result
                    }
                    _ => unimplemented!("call to {:?}", value)
                };
                for (idx, arg) in args.iter().enumerate() {
                    let (new_asm, value) = self.compile_value(*arg);
                    asm!(asm, &new_asm);
                    let param = ModuleAsm::get_func_arg_str(idx);
                    if arg.is_global() {
                        // globals are always treated as addresses
                        asm!(asm, "lea {}, {}"; [value, param]);
                    } else {
                        asm!(asm, "mov {}, {}"; [value, param]);
                    }
                }
                (asm, result)
            }
            Instruction::Alloca(_, ty, align) => {
                // This should be ty.size(&self.cx.types.borrow()), not 8.
                let ty_size = 8;

                let result = self.inst_stack_loc.borrow().get(inst).unwrap().result.clone();
                (asm, result)
            }
            Instruction::Eq(v1, v2) | Instruction::Lt(v1, v2) => {
                let (new_asm, dest) = self.compile_value(*v1);
                asm!(asm, &new_asm);
                let (new_asm, source) = self.compile_value(*v2);
                asm!(asm, &new_asm);
                asm!(asm, "mov {}, %rax"; [source],
                          "cmp %rax, {}"; [dest]);
                (asm, "".to_string())
            }
            Instruction::CondBr(cond, bb1, bb2) => {
                match cond {
                    Value::Instruction(fn_idx, bb_idx, idx) => {
                        let cond_inst =
                            &module.functions[*fn_idx]
                                .basic_blocks[*bb_idx].instrs[*idx];
                        match cond_inst {
                            Instruction::Eq(_, _) => {
                                let _ = self.compile_instruction(*cond);
                                asm!(asm, "je {}"; [bb1],
                                          "jmp {}"; [bb2]);
                                (asm, "".to_string())
                            },
                            Instruction::Lt(_, _) => {
                                let _ = self.compile_instruction(*cond);
                                asm!(asm, "jl {}"; [bb1],
                                          "jmp {}"; [bb2]);
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
            }
            Instruction::Load(ptr, _) => {
                let (new_asm, dest) = self.compile_value(*ptr);
                // FIXME:
                asm.push_str(&new_asm);
                asm!(asm, "mov {}, %rax"; [dest]);
                // Allocas are an exception. They don't need to be dereferenced.
                if self.cx.val_ty(*ptr).is_ptr(&self.cx.types.borrow()) {
                    // Dereference the pointer.
                    asm!(asm,"mov (%rax), %rax"; []);
                }

                let result =
                    self.inst_stack_loc.borrow().get(inst).unwrap().result.clone();
                asm!(asm, "mov %rax, {}"; [result]);
                (asm, result)
            }
            Instruction::CheckOverflow(inst, ty) => {
                let (new_asm, result) = self.compile_instruction(*inst);
                asm!(asm, "setb %dl"; []);
                (asm, "%dl".to_string())
            }
            Instruction::Cast(inst, ty) => {
                // FIXME: Disregard the type for now.
                self.compile_value(*inst)
            }
            Instruction::Unreachable => {
                ("\tud2\n".to_string(), "".to_string())
            }
            _ => {
                unimplemented!("instruction {:?}", inst);
            }
        };
        let ret = instr_asm.clone();
        self.compiled_insts.borrow_mut().insert(
            inst_v,
            InstrAsm { asm: instr_asm.0, result: instr_asm.1 });
        ret
    }

    pub fn already_compiled(&self, value: Value) -> bool {
        self.compiled_insts.borrow().get(&value).is_some()
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
        eprintln!("types {:?}", self.cx.types.borrow());
        match self.cx.types.borrow()[*ty] {
            OxType::Scalar(ScalarType::I32) => ".long",
            OxType::Scalar(ScalarType::I64) => ".quad",
            OxType::PtrTo{ .. } => ".quad",
            _ => unimplemented!("type of {:?}", ty),
        }.to_string()
    }

    fn constant_value(&self, v: Value) -> String {
        match v {
            Value::ConstCstr(idx) => {
                self.cx.const_cstrs.borrow()[idx].name.clone()
            },
            Value::Global(idx) => {
                self.constant_value(self.cx.globals.borrow()[idx].initializer.unwrap())
            },
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
            Value::Function(idx) => {

            }
            Value::Cast(idx) => {

            }
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
                    label!(asm, c_str.name);
                    asm!(asm, ".ascii \"{}\""; [const_str]);
                },
                Some(Value::ConstStruct(idx)) => {
                    let const_struct = &self.cx.const_structs.borrow()[idx];
                    label!(asm, global.name);
                    for c in &const_struct.components {
                        asm!(asm, &self.compile_const_global(*c));
                    }
                },
                None => bug!("no initializer found for {:?}", global),
                _ => unimplemented!("declare_globals({:?})", global),
            }
        }
        asm
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

    pub fn compile_block(&self, bb: &OxBasicBlock) -> String {
        let mut asm = String::new();
        label!(asm, format!("{}", bb.label));
        eprintln!("{}, {}: {:?}", bb.parent, bb.idx, bb.instrs);
        for (inst_idx, inst) in bb.instrs.iter().enumerate() {
            let inst = &bb.instrs[inst_idx];
            if self.already_compiled(
                Value::Instruction(bb.parent, bb.idx, inst_idx)) {
                // Always compile branching instructions.
                if inst.is_branch() {
                    let (new_asm, _) = self.compile_instruction(
                        Value::Instruction(bb.parent, bb.idx, inst_idx));
                    asm!(asm, &new_asm);
                }
            } else {
                let (new_asm, _) = self.compile_instruction(
                    Value::Instruction(bb.parent, bb.idx, inst_idx));
                asm!(asm, &new_asm);
            }
        }
        asm
    }

    pub fn compile_function(&self, f: &OxFunction) -> String {
        eprintln!("compiling function {} {}", f.idx, f.name);
        let mut asm = String::new();
        self.compiled_params.borrow_mut().clear();
        self.inst_stack_loc.borrow_mut().clear();
        label!(asm, f.name);
        let (param_movs, stack_size) = self.compute_stack_size(&f);
        asm!(asm, "push %rbp"; [],
                  "mov %rsp, %rbp"; [],
                  "sub ${}, %rsp"; [stack_size]);
        asm!(asm, &param_movs);
        for param in &f.params {
            asm.push_str(&self.compile_value(*param).0);
        }
        for bb in &f.basic_blocks {
            asm!(asm, &self.compile_block(&bb));
        }
        asm
    }

    fn compute_stack_size(&self, f: &OxFunction) -> (String, usize) {
        let mut size = 0;
        let mut param_movs = String::new();

        for (idx, param) in f.params.iter().enumerate() {
            // FIXME:
            let param_size = 8;
            size += param_size;
            let result = format!("-{}(%rbp)", size);
            let reg = ModuleAsm::get_func_arg_str(idx);
            asm!(param_movs, "mov {}, {}"; [reg, result]);
            let instr_asm = InstrAsm { asm: param_movs.clone(),
                                       result: result.clone() };
            self.compiled_params.borrow_mut().insert(*param, instr_asm);
        }
        for bb in &f.basic_blocks {
            for inst in &bb.instrs {
                match inst {
                    Instruction::Add(_, _) | Instruction::Sub(_, _) => {
                        size += 8;
                        let result = format!("-{}(%rbp)", size);
                        let instr_asm = InstrAsm { asm: String::new(),
                                                   result: result.clone() };
                        self.inst_stack_loc.borrow_mut().insert((*inst).clone(),
                                                                instr_asm);
                    },
                    Instruction::Call(_, _) => {
                        // FIXME: check non void ret
                        size += 8;
                        let result = format!("-{}(%rbp)", size);
                        let instr_asm = InstrAsm { asm: String::new(),
                                                   result: result.clone() };
                        self.inst_stack_loc.borrow_mut().insert((*inst).clone(),
                                                                instr_asm);
                    },
                    Instruction::Alloca(_, ty, align) => {
                        size += 8;
                        let result = format!("-{}(%rbp)", size);
                        let instr_asm = InstrAsm { asm: String::new(),
                                                   result: result.clone() };
                        self.inst_stack_loc.borrow_mut().insert((*inst).clone(),
                                                                instr_asm);
                    },
                    Instruction::Load(ptr, _) => {
                        size += 8;
                        let result = format!("-{}(%rbp)", size);
                        let instr_asm = InstrAsm { asm: String::new(),
                                                   result: result.clone() };
                        self.inst_stack_loc.borrow_mut().insert((*inst).clone(),
                                                                instr_asm);
                    },
                    _ => {

                    },
                };
            }
        }
        (param_movs, size)
    }

    /// Return the x86-64 instructions that correspond to this module.
    pub fn compile(&self) -> String {
        let module = self.cx.module.borrow();
        let mut asm = String::new();
        // Define the constant strings.
        asm!(asm, &self.declare_globals());
        // Declare the functions
        asm!(asm, &self.declare_functions());
        eprintln!("{}", asm);
        eprintln!("globals {:?}", self.cx.globals.borrow());

        for f in &module.functions {
            if !f.is_declaration() {
                asm!(asm, &self.compile_function(&f));
            }
        }
        asm
    }
}

pub fn compile(mut module: ModuleAsm) -> (Stats, String) {
    let asm = module.compile();
    (module.cx.consume_stats().into_inner(), asm)
}
