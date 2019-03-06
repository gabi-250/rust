use context::CodegenCx;
use ir::basic_block::BasicBlock;
use ir::type_::{OxType, Type};
use ir::value::Value;
use super::super::ModuleIronOx;

use rustc::ty::layout::Align;
use rustc_codegen_ssa::traits::{BaseTypeMethods, DerivedTypeMethods};

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
    /// (cond, then_val, else_val)
    Select(Value, Value, Value),
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
    /// Multiply two values and return the result.
    Mul(Value, Value, bool),
    Sub(Value, Value),
    Icmp(Value, Value, CompOp),
    Not(Value),
    /// Check overflow: (instruction, type, signed).
    CheckOverflow(Value, Type, bool),
    /// (agg_val, elt, idx)
    InsertValue(Value, Value, u64),
    ExtractValue(Value, u64),
    /// Get an element from an aggregate value, as indicated by the indices:
    /// (agg_val, [indices], inbounds).
    Gep(Value, Vec<Value>, bool),
    StructGep(Value, u64),
    /// (type, pers_fn, num_clauses)
    LandingPad { ty: Type, pers_fn: Value, num_clauses: usize, cleanup: bool },
    Resume(Value),
    Switch { value: Value, default: BasicBlock, cases: Vec<(Value, BasicBlock)>},
    // FIXME: add the funclet?
    Invoke { callee: Value, args: Vec<Value>, then: BasicBlock, catch: BasicBlock },
    Unreachable,
}

#[derive(PartialEq, Clone, Copy, Debug, Eq, Hash)]
pub enum CompOp {
    Eq,
    Ne,
    Ugt,
    Sgt,
    Uge,
    Sge,
    Ult,
    Slt,
    Ule,
    Sle,
}

#[derive(PartialEq, Clone, Debug, Eq, Hash)]
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
    fn load_ty(val: Value, cx: &CodegenCx) -> Type {
        if let Value::Instruction(fn_idx, bb_idx, inst_idx) = val {
            let inst = &cx.module.borrow().functions[fn_idx].
                basic_blocks[bb_idx].instrs[inst_idx];
            match inst {
                Instruction::Alloca(_, ty, _) |
                Instruction::Cast(_, ty) => ty.pointee_ty(&cx.types.borrow()),
                Instruction::StructGep(_, _) => {
                    let ty = inst.val_ty(cx);
                    match cx.types.borrow()[*ty] {
                        OxType::PtrTo { pointee } => pointee,
                        _ => unimplemented!("Load from non-pointer ty {:?}", ty),
                    }
                },
                Instruction::Load(ptr, _) => {
                    let ty = Instruction::load_ty(*ptr, cx);
                    ty.pointee_ty(&cx.types.borrow())
                },
                Instruction::Call(llfn, ..) => {
                    if let Value::Function(idx) = llfn {
                        let fn_ty = cx.module.borrow().functions[*idx].ret;
                        fn_ty.pointee_ty(&cx.types.borrow())
                    } else {
                        unimplemented!("Call to {:?}", llfn);
                    }
                },
                Instruction::Gep(..) => {
                    let ty = cx.val_ty(val);
                    let ty = ty.pointee_ty(&cx.types.borrow());
                    eprintln!("The load ty of this gep is {:?}", ty);
                    ty
                }
                _ => unimplemented!("Load from instruction {:?}", inst)
            }
        } else if let Value::Param(_, _, ty) = val {
            if let OxType::PtrTo { ref pointee } = cx.types.borrow()[*ty] {
                *pointee
            } else {
                unimplemented!("Load from non-pointer param {:?}", val);
            }
        } else {
            bug!("cannot load from {:?}", val);
        }
    }

    /// Return the `Type` of the value that would result from evaluating this
    /// instruction.
    pub fn val_ty(&self, cx: &CodegenCx) -> Type {
        match *self {
            Instruction::Alloca(_, ty, _) => ty,
            Instruction::Cast(_, ty) => ty,
            Instruction::Ret(Some(v)) => cx.val_ty(v),
            Instruction::Add(v1, v2) | Instruction::Sub(v1, v2) |
            Instruction::Mul(v1, v2, _) => {
                let ty1 = cx.val_ty(v1);
                let ty2 = cx.val_ty(v2);
                assert_eq!(ty1, ty2);
                ty1
            },
            Instruction::Call(value, _) => {
                let fn_ty = cx.val_ty(value);
                match cx.types.borrow()[*fn_ty] {
                    OxType::FnType { ref ret, .. } => *ret,
                    OxType::PtrTo { ref pointee } => {
                        if let OxType::FnType { ref ret, .. } = cx.types.borrow()[**pointee] {
                            *ret
                        } else {
                            bug!("Cannot call value {:?}", value);
                        }
                    },
                    _ => {
                        unimplemented!("val_ty({:?})\n{:?}", fn_ty, cx.types.borrow());
                    }
                }
            }
            Instruction::Load(ptr, _) => Instruction::load_ty(ptr, cx),
            Instruction::StructGep(ptr, idx) => {
                let member_ty = {
                    let types = cx.types.borrow();
                    let struct_ptr = cx.val_ty(ptr);
                    let struct_ptr = &types[*struct_ptr];
                    if let OxType::PtrTo { pointee } = struct_ptr {
                        let struct_ty = &types[**pointee];
                        if let OxType::StructType { ref members, .. } = struct_ty {
                            members[idx as usize]
                        } else {
                            bug!("expected OxType::StructType, found {:?}", struct_ty);
                        }
                    } else {
                        bug!("expected OxType::PtrTo, found {:?}", struct_ptr);
                    }
                };
                cx.type_ptr_to(member_ty)
            },
            Instruction::ExtractValue(ptr, idx) => {
                let agg_ty = cx.val_ty(ptr);
                agg_ty.ty_at_idx(idx, &cx.types.borrow())
            },
            // FIXME: is that right?
            Instruction::LandingPad { ty, .. } => ty,
            // FIXME: is that right?
            Instruction::Invoke { callee, .. } => {
                match cx.types.borrow()[*cx.val_ty(callee)] {
                    OxType::FnType { ref ret, .. } => *ret,
                    OxType::PtrTo { ref pointee } => {
                        if let OxType::FnType { ref ret, .. } = cx.types.borrow()[**pointee] {
                            *ret
                        } else {
                            bug!("Cannot call value {:?}", callee);
                        }
                    },
                    _ => {
                        unimplemented!("val_ty({:?})\n{:?}", callee, cx.types.borrow());
                    }
                }
            },
            Instruction::Not(v) => cx.val_ty(v),
            Instruction::InsertValue(agg, v, idx) => cx.val_ty(agg),
            Instruction::Select(_, v1, v2) => {
                let ty = cx.val_ty(v1);
                assert_eq!(ty, cx.val_ty(v2));
                ty
            },
            Instruction::Icmp(..) => cx.type_bool(),
            Instruction::Gep(agg, ref indices, inbounds) => {
                let ty = cx.val_ty(agg);
                let ty = ty.pointee_ty(&cx.types.borrow());
                let arr_ty = match cx.types.borrow()[*ty] {
                    OxType::Array { len, ty } => {
                        assert_eq!(indices.len(), 2);
                        ty
                    },
                    OxType::Scalar(..) => ty,
                    OxType::StructType { .. } => {
                        // The type of a getelementptr on a struct* is a struct*
                        // if the getelementptr uses a single index.
                        // FIXME: explain this better.
                        assert_eq!(indices.len(), 1);
                        ty
                    }
                    _ => unimplemented!("GEP({:?}) {:?}", ty, cx.types.borrow()),
                };
                cx.type_ptr_to(ty)
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
