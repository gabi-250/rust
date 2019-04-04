use context::CodegenCx;
use ir::global::OxGlobal;
use ir::type_::{OxType, ScalarType, Type};
use ir::value::Value;

use rustc::mir::mono::Linkage;
use std::cell::RefCell;

use x86_64::fn_printer::FunctionPrinter;
use x86_64::gas_directive::{BigNum, GasDirective};
use x86_64::instruction::MachineInst;

pub struct AsmPrinter<'a, 'll, 'tcx> {
    /// The codegen context, which also contains the `ModuleIronOx` to be compiled.
    cx: &'a CodegenCx<'ll, 'tcx>,
    /// The global constants that have already been emitted to the object file.
    codegenned_consts: RefCell<Vec<Value>>,
}

impl AsmPrinter<'a, 'll, 'tcx> {
    pub fn new(cx: &'a CodegenCx<'ll, 'tcx>) -> AsmPrinter<'a, 'll, 'tcx> {
        AsmPrinter { cx, codegenned_consts: Default::default() }
    }

    /// Get the name of value `v`.
    ///
    /// `v` may be a static operation, such as a ConstGep, or a constant/global
    /// value.
    fn global_name(&self, v: Value) -> String {
        match v {
            Value::ConstCstr(idx) => self.cx.const_cstrs.borrow()[idx].name.clone(),
            Value::Global(idx) => {
                // Find the name of the initialiser of the global.
                self.global_name(self.cx.globals.borrow()[idx].initializer.unwrap())
            },
            Value::ConstGep { ptr_idx, offset } => {
                // FIXME: handle non-zero offsets statically.
                assert_eq!(offset, 0);
                let name = self.cx.const_structs.borrow()[ptr_idx].name.clone();
                name
            }
            _ => unimplemented!("value of {:?}", v),
        }
    }

    /// Emit the GNU Assembler directives to codegen the constant value `c`.
    fn codegen_const_global(&self, c: Value) -> Vec<MachineInst> {
        let mut asm = vec![];
        match c {
            Value::ConstUint(idx) => {
                // Get the actual value of the constant.
                let u_const = self.cx.u_consts.borrow()[idx];
                // Get the declaration of the constant.
                let directive = self.declare_scalar(u_const.ty, u_const.value);
                asm.push(MachineInst::Directive(directive));
            }
            Value::ConstCast(idx) => {
                // Find the instruction.
                let cast_inst = &self.cx.const_casts.borrow()[idx];
                // Get the name of the value being cast.
                let name = self.global_name(cast_inst.value);
                asm.push(
                    MachineInst::Directive(
                        GasDirective::Quad(vec![BigNum::Sym(name)])));
            }
            Value::ConstStruct(idx) => {
                let const_struct = &self.cx.const_structs.borrow()[idx];
                // Make sure this is not emitted again.
                self.codegenned_consts.borrow_mut().push(c);
                // Emit the label of the struct.
                asm.push(MachineInst::Label(const_struct.name.clone()));
                // Codegen each of its components.
                for c in &const_struct.components {
                    asm.extend(self.codegen_const_global(*c));
                }
            }
            Value::Function(idx) => {
                // If the value is a function, it can be represented as
                // .quad <fn_name>
                // This essentially behaves like a function pointer.
                let name = self.cx.module.borrow().functions[idx].name.clone();
                asm.push(MachineInst::Directive(GasDirective::Quad(
                            vec![BigNum::Sym(name)])));
            }
            Value::ConstCstr(idx) => {
                let c_str = &self.cx.const_cstrs.borrow()[idx];
                // Get the content of the string.
                let const_str = self.get_str(c_str.ptr, c_str.len);
                // Specify the type of the symbol (object), its size, and finally
                // the contents.
                asm.extend(vec![
                    MachineInst::Label(c_str.name.clone()),
                    MachineInst::Directive(GasDirective::Ascii(vec![const_str])),
                ]);
            }
            Value::ConstBytes(idx) => {
                let const_bytes = &self.cx.bytes.borrow()[idx];
                // Emit the label of the allocation, the bytes, and its length.
                asm.extend(vec![
                    MachineInst::Label(const_bytes.name.clone()),
                    MachineInst::Directive(
                        GasDirective::Byte(const_bytes.bytes.clone())),
                ]);
            },
            _ => unimplemented!("codegen_const_global({:?})", c),
        };
        asm
    }

    /// Return the string representation of the sequence of bytes.
    ///
    /// Creates a `String` out of the byte sequence of length `len` pointed to
    /// by `ptr`.
    fn get_str(&self, ptr: *const u8, len: usize) -> String {
        let mut c_str = String::with_capacity(len);
        for i in 0..len {
            unsafe {
                c_str.push(*ptr.offset(i as isize) as char);
            }
        }
        c_str
    }

    /// Emit the GNU Assembler directives that add the bytes of a scalar value.
    ///
    /// The size of the type of the scalar determines the directive used. The value
    /// is simply added after the directive.
    fn declare_scalar(&self, ty: Type, value: u128) -> GasDirective {
        match self.cx.types.borrow()[ty] {
            OxType::Scalar(ScalarType::I32) => GasDirective::Long(vec![value as u32]),
            OxType::Scalar(ScalarType::I64) => GasDirective::Quad(
                vec![BigNum::Immediate(value as u64)]),
            _ => unimplemented!("type of {:?}", ty),
        }
    }

    /// Codegen a single global.
    fn codegen_global(&self, global: &OxGlobal) -> Vec<MachineInst> {
        let mut asm = vec![];
        // The data of the global comes after its label
        asm.push(MachineInst::Label(global.name.clone()));
        if !global.private {
            // Mark the symbol as global.
            asm.push(
                MachineInst::Directive(GasDirective::Global(global.name.clone())));
        }
        match global.initializer {
            Some(v) => asm.extend(self.codegen_const_global(v)),
            None => bug!("no initializer found for {:?}", global),
        };
        asm
    }

    /// Emit the GNU Assembler directives that declare the globals of the module.
    fn declare_globals(&self) -> Vec<MachineInst> {
        let mut asm = vec![
            MachineInst::Directive(GasDirective::Section(".rodata".to_string()))];
        for global in self.cx.globals.borrow().iter().rev() {
            // Functions are handled somewhere else.
            if global.ty.is_fn(&self.cx.types.borrow()) {
                continue;
            }
            asm.extend(self.codegen_global(&global));
        }
        // Emit code for all the constant structs. Constant GEP instructions operate
        // on constant structs.
        for (i, _c_struct) in self.cx.const_structs.borrow().iter().rev().enumerate() {
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

    /// Declare all the functions in the `ModuleIronOx`.
    fn declare_functions(&self) -> Vec<MachineInst> {
        // Start writing in the text section.
        let mut asm: Vec<MachineInst> = vec![
            MachineInst::Directive(GasDirective::Text)
        ];
        let module = self.cx.module.borrow();
        for f in &module.functions {
            match f.linkage {
                Linkage::External => {
                    asm.extend(vec![
                        MachineInst::Directive(GasDirective::Global(f.name.clone())),
                    ]);
                },
                Linkage::Internal => { /* Nothing to do for now. */ },
                linkage => unimplemented!("Function linkage {:?}", linkage),
            }
        }
        asm
    }

    /// Consume the printer, and return the codegen result.
    ///
    /// The codegen result is a string that contains the x86-64 program that
    /// corresponds to the module from the `CodegenCx` of this printer.
    pub fn codegen(self) -> String {
        // Define the globals.
        let mut asm = self.declare_globals();
        // Declare the functions.
        asm.extend(self.declare_functions());
        for f in &self.cx.module.borrow().functions {
            // Skip the external functions.
            if !f.is_declaration() {
                // Generate the instructions of this function.
                let asm_insts = FunctionPrinter::new(&self.cx).codegen_function(&f);
                asm.extend(asm_insts);
            }
        }
        let asm: Vec<String> = asm.iter().map(|x| x.to_string()).collect();
        asm.join("")
    }
}
