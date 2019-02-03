use context::CodegenCx;
use ir::type_::Type;
use ir::value::Value;
use super::super::ModuleIronOx;

use rustc::ty::layout::Align;
use rustc_codegen_ssa::traits::BaseTypeMethods;

/// An IronOx instruction.
#[derive(PartialEq, Debug, Eq, Hash)]
pub enum Instruction {
    /// Call(fn_idx, args). Emit a call to the function found at index `fn_idx`
    /// in the `functions` vector of the module.
    Call(usize, Vec<Value>),
    /// Allocate space on the stack for a variable of a particular type and
    /// alignment.
    Alloca(String, Type, Align),
    /// Cast a value to a type.
    Cast(Value, Type),
    /// Add two values and return the result.
    Add(Value, Value),
}

impl Instruction {
    /// Return the `Type` of the value that would result from evaluating this
    /// instruction.
    pub fn val_ty(&self, cx: &CodegenCx, module: &ModuleIronOx) -> Type {
        match *self {
            Instruction::Alloca(_, ty, _) => ty,
            Instruction::Cast(_, ty) => ty,
            Instruction::Add(v1, v2) => {
                let ty = cx.val_ty(v1);
                assert_eq!(ty, cx.val_ty(v2));
                ty
            },
            Instruction::Call(fn_idx, _) => cx.module.borrow().functions[fn_idx].ret,
        }
    }
}
