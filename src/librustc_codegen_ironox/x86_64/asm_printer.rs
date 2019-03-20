use context::CodegenCx;
use ir::type_::{OxType, ScalarType, Type};
use ir::value::Value;

use rustc::mir::mono::Visibility;
use std::cell::RefCell;

use x86_64::fn_printer::FunctionPrinter;
use x86_64::gas_directive::{BigNum, GasDirective, GasType};
use x86_64::instruction::MachineInst;

pub struct AsmPrinter<'a, 'll, 'tcx> {
    /// The codegen context, which also contains the `ModuleIronOx` to be compiled.
    cx: &'a CodegenCx<'ll, 'tcx>,
    codegenned_consts: RefCell<Vec<Value>>,
}

impl AsmPrinter<'a, 'll, 'tcx> {
    pub fn new(cx: &'a CodegenCx<'ll, 'tcx>) -> AsmPrinter<'a, 'll, 'tcx> {
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
            Value::ConstUint(idx) => {
                let u_const = self.cx.u_consts.borrow()[idx];
                let directive = self.declare_scalar(u_const.ty, u_const.value);
                asm.push(MachineInst::Directive(directive));
            }
            Value::ConstCast(idx) => {
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
            // Functions are handled somewhere else.
            if global.ty.is_fn(&self.cx.types.borrow()) {
                continue;
            }
            asm.push(MachineInst::Label(global.name.clone()));
            if !global.private {
                // Mark the symbol as global.
                asm.push(
                    MachineInst::Directive(GasDirective::Global(global.name.clone())));
            }
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
    pub fn codegen(self) -> String {
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
        asm
    }
}
