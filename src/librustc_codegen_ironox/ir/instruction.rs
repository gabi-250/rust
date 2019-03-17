use context::CodegenCx;
use ir::basic_block::BasicBlock;
use ir::type_::{OxType, Type};
use ir::value::Value;

use rustc::ty::layout::Align;
use rustc_codegen_ssa::traits::{BaseTypeMethods, DerivedTypeMethods};

/// An IronOx instruction.
#[derive(PartialEq, Clone, Debug, Eq, Hash)]
pub enum OxInstruction {
    /// Store value `val` at the address pointed to by `ptr`.
    Store { ptr: Value, val: Value },
    /// Return the value `ptr` points to.
    Load { ptr: Value, align: Align },
    /// An unconditional branch to a basic block.
    Br(BasicBlock),
    /// Branch to `then_bb` if `cond` is true, otherwise branch to `else_bb`.
    CondBr { cond: Value, then_bb: BasicBlock, else_bb: BasicBlock },
    /// If `cond` is true, return `then_val` else return `else_val`.
    Select { cond: Value, then_val: Value, else_val: Value },
    /// Return from the current function.
    Ret(Option<Value>),
    /// Call `callee` with the arguments in `args`.
    Call { callee: Value, args: Vec<Value> },
    /// Allocate space on the stack for a variable named `name` of type `ty`, and
    /// make sure the location on the stack has the alignment `align`.
    Alloca { name: String, ty: Type, align: Align},
    /// Cast value `val` to a type `ty`.
    Cast { val: Value, ty: Type },
    /// Add `lhs` to `rhs` and return the result.
    Add { lhs: Value, rhs: Value },
    /// Multiply `lhs` by `rhs` and return the result.
    Mul { lhs: Value, rhs: Value, signed: bool },
    /// Subtract `rhs` from `lhs` and return the result.
    Sub { lhs: Value, rhs: Value},
    /// Compare `lhs` with `rhs` using the comparison operator `op`. Returns a bool.
    Icmp { lhs: Value, rhs: Value, op: CompOp },
    /// Negate a boolean value.
    Not(Value),
    /// Compute the bitwise and of `lhs` and `rhs` and return the result.
    And { lhs: Value, rhs: Value },
    /// Check overflow: (instruction, type, signed).
    CheckOverflow { inst: Value, ty: Type, signed: bool },
    /// Insert value `elt` at position `idx` into the aggregate `agg`.
    InsertValue { agg: Value, elt: Value, idx: u64 },
    /// Extract the value at position `idx` from the aggregate `agg`.
    ExtractValue { agg: Value, idx: u64 },
    /// Get a pointer to an element from the aggregate pointed to by `ptr`, as
    /// indicated by `indices`.
    Gep { ptr: Value, indices: Vec<Value>, inbounds: bool },
    /// Get a pointer to the element at position `idx` from the struct `ptr` points to.
    StructGep { ptr: Value, idx: u64 },
    /// **Not currently implemented**: crashes the program.
    LandingPad { ty: Type, pers_fn: Value, num_clauses: usize, cleanup: bool },
    /// **Not currently implemented**: crashes the program.
    Resume(Value),
    /// Compare `value` with each of the values in `cases`. If a value in `cases`
    /// matches `value`, branch to the respective basic block; otherwise, branch
    /// to `default`.
    Switch { value: Value, default: BasicBlock, cases: Vec<(Value, BasicBlock)>},
    /// **Not currently implemented**: this is simply a call.
    Invoke { callee: Value, args: Vec<Value>, then: BasicBlock, catch: BasicBlock },
    /// Emits an instruction that stops the execution.
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

impl OxInstruction {
    fn load_ty(cx: &CodegenCx, val: Value) -> Type {
        if let Value::Instruction(fn_idx, bb_idx, inst_idx) = val {
            let inst = &cx.module.borrow().functions[fn_idx].
                basic_blocks[bb_idx].instrs[inst_idx];
            match inst {
                OxInstruction::Alloca { ty, .. } |
                OxInstruction::Cast { ty, .. } => ty.pointee_ty(&cx.types.borrow()),
                OxInstruction::StructGep { .. } => {
                    let ty = inst.val_ty(cx);
                    match cx.types.borrow()[ty] {
                        OxType::PtrTo { pointee } => pointee,
                        _ => unimplemented!("Load from non-pointer ty {:?}", ty),
                    }
                },
                OxInstruction::Load { ptr, .. } => {
                    let ty = OxInstruction::load_ty(cx, *ptr);
                    ty.pointee_ty(&cx.types.borrow())
                },
                OxInstruction::Call { callee, .. } => {
                    if let Value::Function(idx) = callee {
                        let fn_ty = cx.module.borrow().functions[*idx].ret;
                        fn_ty.pointee_ty(&cx.types.borrow())
                    } else {
                        unimplemented!("Call to {:?}", callee);
                    }
                },
                OxInstruction::Gep { .. } => {
                    let ty = cx.val_ty(val);
                    ty.pointee_ty(&cx.types.borrow())
                }
                _ => unimplemented!("Load from instruction {:?}", inst)
            }
        } else if let Value::Param(_, _, ty) = val {
            if let OxType::PtrTo { ref pointee } = cx.types.borrow()[ty] {
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
            OxInstruction::Alloca { ty, .. } => ty,
            OxInstruction::Cast { ty, .. } => ty,
            OxInstruction::Ret(Some(v)) => cx.val_ty(v),
            OxInstruction::Add { lhs, rhs } | OxInstruction::Sub { lhs, rhs } |
            OxInstruction::Mul { lhs, rhs, .. } | OxInstruction::And { lhs, rhs } => {
                let ty1 = cx.val_ty(lhs);
                let ty2 = cx.val_ty(rhs);
                assert_eq!(ty1, ty2);
                ty1
            },
            OxInstruction::Call { callee, .. } => {
                let fn_ty = cx.val_ty(callee);
                match cx.types.borrow()[fn_ty] {
                    OxType::FnType { ref ret, .. } => *ret,
                    OxType::PtrTo { ref pointee } => {
                        if let OxType::FnType { ref ret, .. } = cx.types.borrow()[*pointee] {
                            *ret
                        } else {
                            bug!("Cannot call callee {:?}", callee);
                        }
                    },
                    _ => {
                        unimplemented!("val_ty({:?})\n{:?}", fn_ty, cx.types.borrow());
                    }
                }
            }
            OxInstruction::Load { ptr, .. } => OxInstruction::load_ty(cx, ptr),
            OxInstruction::StructGep { ptr, idx } => {
                let member_ty = {
                    let types = cx.types.borrow();
                    let struct_ptr = cx.val_ty(ptr);
                    let struct_ptr = &types[struct_ptr];
                    if let OxType::PtrTo { pointee } = struct_ptr {
                        let struct_ty = &types[*pointee];
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
            OxInstruction::ExtractValue { agg: ptr, idx } => {
                let agg_ty = cx.val_ty(ptr);
                agg_ty.ty_at_idx(idx, &cx.types.borrow())
            },
            // FIXME: is that right?
            OxInstruction::LandingPad { ty, .. } => ty,
            // FIXME: is that right?
            OxInstruction::Invoke { callee, .. } => {
                match cx.types.borrow()[cx.val_ty(callee)] {
                    OxType::FnType { ref ret, .. } => *ret,
                    OxType::PtrTo { ref pointee } => {
                        if let OxType::FnType { ref ret, .. } = cx.types.borrow()[*pointee] {
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
            OxInstruction::Not(v) => cx.val_ty(v),
            OxInstruction::InsertValue { agg, .. } => cx.val_ty(agg),
            OxInstruction::Select { then_val, else_val, .. } => {
                let then_ty = cx.val_ty(then_val);
                assert_eq!(then_ty, cx.val_ty(else_val));
                then_ty
            },
            OxInstruction::Icmp { .. }=> cx.type_bool(),
            OxInstruction::Gep { ptr, ref indices, .. }=> {
                let ty = cx.val_ty(ptr);
                let ty = ty.pointee_ty(&cx.types.borrow());
                let arr_ty = match cx.types.borrow()[ty] {
                    OxType::Array { ty, .. } => {
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
                // FIXME:
                cx.type_ptr_to(arr_ty)
            },
            _ => unimplemented!("instruction {:?}", *self),
        }
    }

    pub fn is_branch(&self) -> bool {
        match *self {
            OxInstruction::Br(..) | OxInstruction::CondBr { .. } => true,
            _ => false,
        }
    }
}
