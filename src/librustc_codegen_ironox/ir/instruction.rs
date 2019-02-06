use context::CodegenCx;
use ir::type_::{OxType, Type};
use ir::value::Value;
use super::super::ModuleIronOx;

use rustc::ty::layout::Align;
use rustc_codegen_ssa::traits::BaseTypeMethods;

/// An IronOx instruction.
#[derive(PartialEq, Clone, Debug, Eq, Hash)]
pub enum Instruction {
    /// Store (ptr, value).
    Store(Value, Value),
    /// Load the value of a pointer.
    Load(Value, Align),
    /// An unconditional branch to a label.
    Br(String),
    CondBr(Value, String, String),
    /// Return instruction.
    Ret(Option<Value>),
    /// Call(fn_idx, args). Emit a call to the function found at index `fn_idx`
    /// in the `functions` vector of the module.
    Call(Value, Vec<Value>),
    /// Allocate space on the stack for a variable of a particular type and
    /// alignment.
    Alloca(String, Type, Align),
    /// Cast a value to a type.
    Cast(Value, Type),
    /// Add two values and return the result.
    Add(Value, Value),
    Sub(Value, Value),
    Eq(Value, Value),
    Lt(Value, Value),
    CheckOverflow(Value, Type),
    StructGep(Value, u64),
    Unreachable,
}

pub struct ConstCast {
    pub value: Value,
    pub ty: Type,
}

impl ConstCast {
    pub fn new(value: Value, ty: Type) -> ConstCast {
        ConstCast { value, ty }
    }
}

impl Instruction {
    /// Return the `Type` of the value that would result from evaluating this
    /// instruction.
    pub fn val_ty(&self, cx: &CodegenCx, module: &ModuleIronOx) -> Type {
        match *self {
            Instruction::Alloca(_, ty, _) => ty,
            Instruction::Cast(_, ty) => ty,
            Instruction::Add(v1, v2) => {
                let ty1 = cx.val_ty(v1);
                let ty2 = cx.val_ty(v2);
                assert_eq!(ty1, ty2);
                ty1
            },
            Instruction::Call(value, _) => cx.val_ty(value),
            Instruction::Load(ptr, _) => {
                match ptr {
                    Value::Instruction(fn_idx, bb_idx, idx) => {
                        let inst =
                            &module.functions[fn_idx].basic_blocks[bb_idx]
                                .instrs[idx];
                        match inst {
                            Instruction::Alloca(_, ty, _) => {
                                *ty
                            },
                            _ => inst.val_ty(cx, module)
                        }
                    },
                    Value::Param(_, ty) => {
                        if let OxType::PtrTo{ pointee } = cx.types.borrow()[*ty] {
                            pointee
                        } else {
                            bug!("Cannot load from non-pointer parameter {:?}", ty);
                        }
                    },
                    _ => {
                        bug!("Cannot load from value {:?}", ptr);
                    }
                }
            },
            Instruction::StructGep(val, idx) => {
                let struct_ty = cx.val_ty(val);
                if let OxType::StructType { ref members, .. } =
                    cx.types.borrow()[*struct_ty] {
                    members[idx as usize]
                } else {
                    bug!("expected OxType::StructType, found {:?}", struct_ty);
                }
            },
            _ => unimplemented!("instruction {:?}", *self),
        }
    }

    pub fn is_branch(&self) -> bool {
        match *self {
            Instruction::Br(..) | Instruction::CondBr(..) => true,
            _ => false,
        }
    }
}
