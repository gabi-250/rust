use rustc_codegen_ssa::common::{IntPredicate, RealPredicate, AtomicOrdering,
    SynchronizationScope, AtomicRmwBinOp};
use rustc_codegen_ssa::MemFlags;
use rustc_codegen_ssa::base::to_immediate;
use rustc_codegen_ssa::mir::operand::{OperandValue, OperandRef};
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc::hir::def_id::DefId;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{self, Align, Size, TyLayout};
use rustc_codegen_ssa::traits::*;
use std::borrow::Cow;
use std::ffi::CStr;
use std::ops::{Deref, Range};
use syntax;

use context::CodegenCx;
use ir::basic_block::BasicBlock;
use ir::instruction::{CompOp, OxInstruction};
use ir::type_::Type;
use ir::value::Value;
use type_of::LayoutIronOxExt;

impl BackendTypes for Builder<'_, 'll, 'tcx> {
    type Value = <CodegenCx<'ll, 'tcx> as BackendTypes>::Value;
    type BasicBlock = <CodegenCx<'ll, 'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'ll, 'tcx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'ll, 'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'ll, 'tcx> as BackendTypes>::DIScope;
}


/// Identifies the current insertion point.
pub struct BuilderPosition {
    /// The index of the function.
    fn_idx: usize,
    /// The index of the basic block to which to add instructions.
    bb_idx: usize,
    /// The position at which to add the next instruction in the current basic block.
    inst_idx: usize,
}

impl BuilderPosition {
    fn new(fn_idx: usize, bb_idx: usize, inst_idx: usize) -> BuilderPosition {
        BuilderPosition {
            fn_idx,
            bb_idx,
            inst_idx,
        }
    }
}

pub struct Builder<'a, 'll: 'a, 'tcx: 'll> {
    pub cx: &'a CodegenCx<'ll, 'tcx>,
    pub build_pos: BuilderPosition,
}

impl ty::layout::LayoutOf for Builder<'_, '_, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyLayout = TyLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyLayout {
        self.cx.layout_of(ty)
    }
}

impl ty::layout::HasDataLayout for Builder<'_, '_, '_> {
    fn data_layout(&self) -> &ty::layout::TargetDataLayout {
        self.cx.data_layout()
    }
}

impl ty::layout::HasTyCtxt<'tcx> for Builder<'_, '_, 'tcx> {
    fn tcx<'a>(&'a self) -> TyCtxt<'a, 'tcx, 'tcx> {
        self.cx.tcx
    }
}

impl Deref for Builder<'_, 'll, 'tcx> {
    type Target = CodegenCx<'ll, 'tcx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

impl HasCodegen<'tcx> for Builder<'a, 'll, 'tcx> {
    type CodegenCx = CodegenCx<'ll, 'tcx>;
}

impl StaticBuilderMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn get_static(&self, _def_id: DefId) -> Value {
        unimplemented!("get_static");
    }
}

impl Builder<'a, 'll, 'tcx> {
    /// Add the instruction at the current `BuilderPosition`.
    fn emit_instr(&mut self, inst: OxInstruction) -> Value {
        let mut module = self.cx.module.borrow_mut();
        module.get_function(self.build_pos.fn_idx)
            .insert_inst(self.build_pos.bb_idx, self.build_pos.inst_idx, inst);
        let inst = Value::Instruction {
            fn_idx: self.build_pos.fn_idx,
            bb_idx: self.build_pos.bb_idx,
            idx: self.build_pos.inst_idx
        };
        // move to the next instruction
        self.build_pos.inst_idx += 1;
        inst
    }
}

impl BuilderMethods<'a, 'tcx> for Builder<'a, 'll, 'tcx> {
    fn checked_binop(
        &mut self,
        oop: OverflowOp,
        ty: Ty,
        lhs: Value,
        rhs: Value,
    ) -> (Value, Value) {
        use syntax::ast::IntTy::*;
        use syntax::ast::UintTy::*;
        use rustc::ty::{Int, Uint};

        let new_sty = match ty.sty {
            Int(Isize) => Int(self.tcx.sess.target.isize_ty),
            Uint(Usize) => Uint(self.tcx.sess.target.usize_ty),
            ref t @ Uint(_) | ref t @ Int(_) => t.clone(),
            _ => panic!("tried to get overflow intrinsic for op applied to non-int type")
        };
        let (ty, signed) = match new_sty {
            Uint(U8) => (self.type_i8(), false),
            Uint(U16) => (self.type_i16(), false),
            Uint(U32) => (self.type_i32(), false),
            Uint(U64) => (self.type_i64(), false),
            Uint(U128) => (self.type_i128(), false),

            Int(I8) => (self.type_i8(), true),
            Int(I16) => (self.type_i16(), true),
            Int(I32) => (self.type_i32(), true),
            Int(I64) => (self.type_i64(), true),
            Int(I128) => (self.type_i128(), true),

            _ => unreachable!()
        };
        let inst = match oop {
            OverflowOp::Add => OxInstruction::Add { lhs, rhs },
            OverflowOp::Sub => OxInstruction::Sub { lhs, rhs },
            OverflowOp::Mul => OxInstruction::Mul { lhs, rhs, signed },
        };
        let inst = self.emit_instr(inst);
        (inst, self.emit_instr(OxInstruction::CheckOverflow { inst, ty, signed }))
    }

    fn new_block<'b>(
        cx: &'a Self::CodegenCx,
        llfn: Value,
        name: &'b str
    )-> Self {
        let mut bx = Builder::with_cx(cx);
        let fn_idx = match llfn {
            Value::Function(f) => f,
            _ => bug!("The parent of a basic block has to be a function")
        };
        let bb = {
            let mut f = &mut cx.module.borrow_mut().functions[fn_idx];
            f.add_bb(cx, name)
        };
        bx.position_at_end(bb);
        bx
    }

    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        // FIXME? this is an invalid position and must be overwritten
        Builder {
            cx,
            build_pos: BuilderPosition::new(0, 0, 0)
        }
    }

    fn build_sibling_block<'b>(&self, name: &'b str) -> Self {
        Builder::new_block(self.cx, self.llfn(), name)
    }

    fn cx(&self) -> &CodegenCx<'ll, 'tcx> {
        &self.cx
    }

    fn llfn(&self) -> Value {
        // The function this build_pos is adding instructions to.
        Value::Function(self.build_pos.fn_idx)
    }

    fn llbb(&self) -> BasicBlock {
        // The BasicBlock this builder is adding instructions to.
        BasicBlock(self.build_pos.fn_idx,
                   self.build_pos.bb_idx)
    }

    fn count_insn(&self, _category: &str) {
        unimplemented!("count_insn");
    }

    fn set_value_name(&mut self, _value: Value, _name: &str) {
        // Do nothing. Value names don't matter.
    }

    fn position_at_end(&mut self, llbb: BasicBlock) {
        let llfn = llbb.0;
        let llbb = llbb.1;
        // The next instruction will be inserted as the last instruction in
        // the basic block.
        let instr =
            self.cx.module.borrow().functions[llfn].basic_blocks[llbb].instrs.len();
        self.build_pos = {
            BuilderPosition::new(llfn, llbb, instr)
        };
    }

    fn position_at_start(&mut self, _llbb: BasicBlock) {
        unimplemented!("position_at_start");
    }

    fn ret_void(&mut self) {
        let _ = self.emit_instr(OxInstruction::Ret(None));
    }

    fn ret(&mut self, v: Value) {
        let _ = self.emit_instr(OxInstruction::Ret(Some(v)));
    }

    fn br(&mut self, dest: BasicBlock) {
        let _ = self.emit_instr(OxInstruction::Br(dest));
    }

    fn cond_br(
        &mut self,
        cond: Value,
        then_llbb: BasicBlock,
        else_llbb: BasicBlock,
    ) {
        self.emit_instr(
            OxInstruction::CondBr { cond, then_bb: then_llbb, else_bb: else_llbb });
    }

    fn switch(
        &mut self,
        v: Value,
        else_llbb: BasicBlock,
        num_cases: usize,
    )-> Value {
        self.emit_instr(OxInstruction::Switch {
            value: v,
            default: else_llbb,
            cases: Vec::with_capacity(num_cases)
        })
    }

    fn invoke(
        &mut self,
        llfn: Value,
        args: &[Value],
        then: BasicBlock,
        catch: BasicBlock,
        _funclet: Option<&Self::Funclet>,
    )-> Value {
        let invoke = self.emit_instr(OxInstruction::Invoke {
            callee: llfn, args: args.to_vec(), then, catch
        });
        // Branch to 'then' if the invoke returns
        let _ = self.emit_instr(OxInstruction::Br(then));
        invoke
    }

    fn unreachable(&mut self) {
        self.emit_instr(OxInstruction::Unreachable);
    }

    fn add(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        self.emit_instr(OxInstruction::Add { lhs, rhs })
    }

    fn fadd(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fadd");
    }

    fn fadd_fast(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fadd_fast");
    }

    fn sub(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        self.emit_instr(OxInstruction::Sub { lhs, rhs })
    }

    fn fsub(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fsub");
    }

    fn fsub_fast(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fsub_fast");
    }

    fn mul(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        // Unsigned mul.
        self.emit_instr(OxInstruction::Mul { lhs, rhs, signed: false })
    }

    fn fmul(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fmul");
    }

    fn fmul_fast(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fmul_fast");
    }

    fn udiv(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("udiv");
    }

    fn exactudiv(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("exactudiv");
    }

    fn sdiv(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("sdiv");
    }

    fn exactsdiv(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("exactsdiv");
    }

    fn fdiv(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fdiv");
    }

    fn fdiv_fast(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fdiv_fast");
    }

    fn urem(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("urem");
    }

    fn srem(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("srem");
    }

    fn frem(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("frem");
    }

    fn frem_fast(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("frem_fast");
    }

    fn shl(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("shl");
    }

    fn lshr(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("lshr");
    }

    fn ashr(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("ashr");
    }

    fn and(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        self.emit_instr(OxInstruction::And { lhs, rhs })
    }

    fn or(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("or");
    }

    fn xor(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("xor");
    }

    fn neg(
        &mut self,
        v: Value
    )-> Value {
        self.emit_instr(OxInstruction::Neg(v))
    }

    fn fneg(
        &mut self,
        _v: Value
    )-> Value {
        unimplemented!("fneg");
    }

    fn not(
        &mut self,
        v: Value
    )-> Value {
        self.emit_instr(OxInstruction::Not(v))
    }

    fn alloca(
        &mut self,
        ty: Type,
        name: &str,
        align: Align
    )-> Value {
        let ty = self.type_ptr_to(ty);
        self.emit_instr(OxInstruction::Alloca { name: name.to_string(), ty, align })
    }

    fn dynamic_alloca(
        &mut self,
        _ty: Type,
        _name: &str,
        _align: Align
    )-> Value {
        unimplemented!("dynamic_alloca");
    }

    fn array_alloca(
        &mut self,
        _ty: Type,
        _len: Value,
        _name: &str,
        _align: Align
    )-> Value {
        unimplemented!("array_alloca");
    }

    fn load(
        &mut self,
        ptr: Value,
        align: Align
    )-> Value {
        self.emit_instr(OxInstruction::Load { ptr, align })
    }

    fn volatile_load(
        &mut self,
        _ptr: Value
    )-> Value {
        unimplemented!("volatile_load");
    }

    fn atomic_load(
        &mut self,
        _ptr: Value,
        _order: AtomicOrdering,
        _size: Size
    )-> Value {
        unimplemented!("atomic_load");
    }

    fn range_metadata(
        &mut self,
        _load: Value,
        _range: Range<u128>
    ) {
        unimplemented!("range_metadata");
    }

    fn nonnull_metadata(&mut self, _load: Value) {
        unimplemented!("nonnull_metadata");
    }

    fn store(
        &mut self,
        val: Value,
        ptr: Value,
        align: Align
    )-> Value {
        self.store_with_flags(val, ptr, align, MemFlags::empty())
    }

    fn atomic_store(
        &mut self,
        _val: Value,
        _ptr: Value,
        _order: AtomicOrdering,
        _size: Size
    ) {
        unimplemented!("atomic_store");
    }

    fn store_with_flags(
        &mut self,
        val: Value,
        ptr: Value,
        _align: Align,
        _flags: MemFlags,
    )-> Value {
        // FIXME: ignore the flags for now
        self.emit_instr(OxInstruction::Store { ptr, val })
    }

    fn gep(
        &mut self,
        ptr: Value,
        indices: &[Value]
    )-> Value {
        self.emit_instr(
            OxInstruction::Gep { ptr, indices: indices.to_vec(), inbounds: false })
    }

    fn inbounds_gep(
        &mut self,
        ptr: Value,
        indices: &[Value]
    )-> Value {
        self.emit_instr(
            OxInstruction::Gep { ptr, indices: indices.to_vec(), inbounds: true })
    }

    fn struct_gep(
        &mut self,
        ptr: Value,
        idx: u64
    )-> Value {
        self.emit_instr(OxInstruction::StructGep { ptr, idx })
    }

    fn trunc(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    fn sext(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("sext");
    }

    fn fptoui(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("fptoui");
    }

    fn fptosi(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("fptosi");
    }

    fn uitofp(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("uitofp");
    }

    fn sitofp(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("sitofp");
    }

    fn fptrunc(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("fptrunc");
    }

    fn fpext(
        &mut self,
        _val: Value,
        _dest_ty: Type
    )-> Value {
        unimplemented!("fpext");
    }

    fn ptrtoint(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    fn inttoptr(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    fn bitcast(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    fn intcast(
        &mut self,
        val: Value,
        dest_ty: Type,
        _is_signed: bool
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    fn pointercast(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    fn icmp(
        &mut self,
        op: IntPredicate,
        lhs: Value, rhs: Value
    )-> Value {
        let op = match op {
            IntPredicate::IntEQ => CompOp::Eq,
            IntPredicate::IntNE => CompOp::Ne,
            IntPredicate::IntUGT => CompOp::Ugt,
            IntPredicate::IntSGT => CompOp::Sgt,
            IntPredicate::IntUGE => CompOp::Uge,
            IntPredicate::IntSGE => CompOp::Sge,
            IntPredicate::IntULT => CompOp::Ult,
            IntPredicate::IntSLT => CompOp::Slt,
            IntPredicate::IntULE => CompOp::Ule,
            IntPredicate::IntSLE => CompOp::Sle,
        };
        self.emit_instr(OxInstruction::Icmp { lhs, rhs, op })
    }

    fn fcmp(
        &mut self,
        _op: RealPredicate,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("fcmp");
    }

    fn empty_phi(
        &mut self,
        _ty: Type
    )-> Value {
        unimplemented!("empty_phi");
    }

    fn phi(
        &mut self,
        _ty: Type,
        _vals: &[Value],
        _bbs: &[BasicBlock]
    )-> Value {
        unimplemented!("phi");
    }

    fn inline_asm_call(
        &mut self,
        _asm: &CStr,
        _cons: &CStr,
        _inputs: &[Value],
        _output: Type,
        _volatile: bool,
        _alignstack: bool,
        _dia: syntax::ast::AsmDialect
    ) -> Option<Value> {
        unimplemented!("inline_asm_call");
    }

    fn minnum(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("minnum");
    }

    fn maxnum(
        &mut self,
        _lhs: Value,
        _rhs: Value
    )-> Value {
        unimplemented!("maxnum");
    }

    fn select(
        &mut self, cond: Value,
        then_val: Value,
        else_val: Value,
    )-> Value {
        self.emit_instr(OxInstruction::Select { cond, then_val, else_val })
    }

    fn va_arg(
        &mut self,
        _list: Value,
        _ty: Type
    )-> Value {
        unimplemented!("va_arg");
    }

    fn extract_element(&mut self,
        _vec: Value,
        _idx: Value
    )-> Value {
        unimplemented!("extract_element");
    }

    fn insert_element(
        &mut self,
        _vec: Value,
        _elt: Value,
        _idx: Value,
    )-> Value {
        unimplemented!("insert_element");
    }

    fn shuffle_vector(
        &mut self,
        _v1: Value,
        _v2: Value,
        _mask: Value
    )-> Value {
        unimplemented!("shuffle_vector");
    }

    fn vector_splat(
        &mut self,
        _num_elts: usize,
        _elt: Value
    )-> Value {
        unimplemented!("vector_splat");
    }

    fn vector_reduce_fadd_fast(
        &mut self,
        _acc: Value,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_fadd_fast");
    }

    fn vector_reduce_fmul_fast(
        &mut self,
        _acc: Value,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmul_fast");
    }

    fn vector_reduce_add(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_add");
    }

    fn vector_reduce_mul(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_mul");
    }

    fn vector_reduce_and(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_and");
    }

    fn vector_reduce_or(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_or");
    }

    fn vector_reduce_xor(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_xor");
    }

    fn vector_reduce_fmin(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmin");
    }

    fn vector_reduce_fmax(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmax");
    }

    fn vector_reduce_fmin_fast(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmin_fast");
    }

    fn vector_reduce_fmax_fast(
        &mut self,
        _src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmax_fast");
    }

    fn vector_reduce_min(
        &mut self,
        _src: Value,
        _is_signed: bool
    )-> Value {
        unimplemented!("vector_reduce_min");
    }

    fn vector_reduce_max(
        &mut self,
        _src: Value,
        _is_signed: bool
    )-> Value {
        unimplemented!("vector_reduce_max");
    }

    fn extract_value(
        &mut self,
        agg_val: Value,
        idx: u64
    )-> Value {
        // Extract the value at position `idx` in aggregate `agg_val`.
        self.emit_instr(OxInstruction::ExtractValue { agg: agg_val, idx })
    }

    fn insert_value(
        &mut self,
        agg_val: Value,
        elt: Value,
        idx: u64
    )-> Value {
        // Insert `elt` into aggregate`agg_val` at `idx`.
        self.emit_instr(OxInstruction::InsertValue { agg: agg_val, elt, idx })
    }

    fn landing_pad(
        &mut self,
        ty: Type,
        pers_fn: Value,
        num_clauses: usize
    )-> Value {
        self.emit_instr(OxInstruction::LandingPad {
            ty,
            pers_fn,
            num_clauses,
            cleanup: false,
        })
    }

    fn add_clause(
        &mut self,
        _landing_pad: Value,
        _clause: Value
    ) {
        unimplemented!("add_clause");
    }

    fn set_cleanup(
        &mut self,
        landing_pad: Value
    ) {
        match landing_pad {
            Value::Instruction { fn_idx, bb_idx, idx } => {
                let mut inst = &mut self.module.borrow_mut().functions[fn_idx].
                    basic_blocks[bb_idx].instrs[idx];
                match inst {
                    OxInstruction::LandingPad{ ref mut cleanup, .. } => {
                        *cleanup = true;
                    },
                    _ => bug!("Expected OxInstruction::LandingPad, found {:?}", inst),
                }
            },
            _ => bug!("Expected Value::Instruction, found {:?}", landing_pad),
        }
    }

    fn resume(
        &mut self,
        exn: Value
    )-> Value {
        self.emit_instr(OxInstruction::Resume(exn))
    }

    fn cleanup_pad(
        &mut self,
        _parent: Option<Value>,
        _args: &[Value]
    ) {
        unimplemented!("cleanup_pad");
    }

    fn cleanup_ret(
        &mut self,
        _cleanup: &<Self::CodegenCx as BackendTypes>::Funclet,
        _unwind: Option<BasicBlock>,
    ) -> Value {
        unimplemented!("cleanup_ret");
    }

    fn catch_pad(
        &mut self,
        _parent: Value,
        _args: &[Value]
    ) {
        unimplemented!("catch_pad");
    }

    fn catch_ret(
        &mut self,
        _pad: &<Self::CodegenCx as BackendTypes>::Funclet,
        _unwind: BasicBlock
    ) -> Value {
        unimplemented!("catch_ret");
    }

    fn catch_switch(
        &mut self,
        _parent: Option<Value>,
        _unwind: Option<BasicBlock>,
        _num_handlers: usize,
    )-> Value {
        unimplemented!("catch_switch");
    }

    fn add_handler(
        &mut self,
        _catch_switch: Value,
        _handler: BasicBlock
    ) {
        unimplemented!("add_handler");
    }

    fn set_personality_fn(&mut self, _personality: Value) {
        // FIXME? IronOx doesn't handle personality functions.
    }

    fn atomic_cmpxchg(
        &mut self,
        _dst: Value,
        _cmp: Value,
        _src: Value,
        _order: AtomicOrdering,
        _failure_order: AtomicOrdering,
        _weak: bool,
    )-> Value {
        unimplemented!("");
    }

    fn atomic_rmw(
        &mut self,
        _op: AtomicRmwBinOp,
        _dst: Value,
        _src: Value,
        _order: AtomicOrdering,
    )-> Value {
        unimplemented!("atomic_rmw");
    }

    fn atomic_fence(
        &mut self,
        _order: AtomicOrdering,
        _scope: SynchronizationScope
    ) {
        unimplemented!("atomic_fence");
    }

    fn add_case(
        &mut self,
        s: Value,
        on_val: Value,
        dest: BasicBlock
    ) {
        match s {
            Value::Instruction { fn_idx, bb_idx, idx } => {
                let mut inst = &mut self.module.borrow_mut().functions[fn_idx].
                    basic_blocks[bb_idx].instrs[idx];
                match inst {
                    OxInstruction::Switch { ref mut cases, .. } => {
                        cases.push((on_val, dest));
                    },
                    _ => bug!("Expected OxInstruction::Switch, found {:?}", inst),
                }
            },
            _ => bug!("Expected Value::Instruction, found {:?}", s)
        }
    }

    fn add_incoming_to_phi(
        &mut self,
        _phi: Value,
        _val: Value,
        _bb: BasicBlock
    ) {
        unimplemented!("add_incoming_to_phi");
    }

    fn set_invariant_load(&mut self, _load: Value) {
        unimplemented!("set_invariant_load");
    }

    /// Returns the ptr value that should be used for storing `val`.
    fn check_store(
        &mut self,
        _val: Value,
        _ptr: Value
    )-> Value {
        unimplemented!("check_store");
    }

    /// Returns the args that should be used for a call to `llfn`.
    fn check_call<'b>(
        &mut self,
        _typ: &str,
        _llfn: Value,
        _args: &'b [Value]
    ) -> Cow<'b, [Value]> {
        unimplemented!("check_call");
    }

    fn lifetime_start(&mut self, _ptr: Value, _size: Size) {
        // FIXME? nothing to do for now
    }

    fn lifetime_end(&mut self, _ptr: Value, _size: Size) {
        // FIXME? nothing to do for now
    }

    fn call(
        &mut self,
        llfn: Value,
        args: &[Value],
        funclet: Option<&Self::Funclet>,
    )-> Value {
        if funclet.is_some() {
            unimplemented!("call funclet: {:?}", funclet);
        }
        self.emit_instr(OxInstruction::Call { callee: llfn, args: args.to_vec() })
    }

    fn zext(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        self.emit_instr(OxInstruction::Cast { val, ty: dest_ty })
    }

    unsafe fn delete_basic_block(&mut self, _bb: BasicBlock) {
        unimplemented!("delete_basic_block");
    }

    fn do_not_inline(&mut self, _llret: Value) {
        // No need to do anything. IronOx never inlines.
    }

    /// This loops `size` times, copying each byte at `src` to `dst`.
    ///
    /// It is roughly equivalent to:
    /// while size > 0 {
    ///     *dst = *src;
    ///     dst = dst + 1;
    ///     src = src + 1;
    ///     size = size - 1;
    /// }
    fn memcpy(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        _flags: MemFlags,
    ) {
        // The basic block that checks how many more bytes need to be copied.
        let mut memcpy_cond = self.build_sibling_block("memcpy_cond");
        // The body of the loop.
        let mut memcpy_start = self.build_sibling_block("memcpy_start");
        // The block to jump to once the `memcpy` is done.
        let memcpy_end = self.build_sibling_block("memcpy_end");

        // Helper variables:
        let i8p = self.type_i8p();
        let i64_ty = self.type_i64();
        let const_one = memcpy_start.cx.const_uint(
            memcpy_start.cx.type_i64(), 1);
        let const_zero = memcpy_start.cx.const_uint(
            memcpy_start.cx.type_i64(), 0);
        let size_align = Align::from_bytes(8).unwrap();

        // Store the source and destination pointers, and the size on the stack.
        let src = self.pointercast(src, i8p);
        let dst = self.pointercast(dst, i8p);
        let src_loc = self.alloca(i8p, "memcpy_src", src_align);
        let dst_loc = self.alloca(i8p, "memcpy_dst", dst_align);
        let size_loc = self.alloca(i64_ty, "memcpy_size", size_align);
        self.store(src, src_loc, src_align);
        self.store(dst, dst_loc, dst_align);
        self.store(size, size_loc, size_align);

        // Branch to the block which evaluates the condition of the memcpy.
        self.br(memcpy_cond.llbb());
        let size = memcpy_cond.load(size_loc, size_align);
        let cond = memcpy_cond.icmp(IntPredicate::IntUGT, size, const_zero);
        let memcpy_start_bb = memcpy_start.llbb();
        let memcpy_end_bb = memcpy_end.llbb();
        memcpy_cond.cond_br(cond, memcpy_start_bb, memcpy_end_bb);

        // Load the pointers from the stack.
        let src = memcpy_start.load(src_loc, src_align);
        let dst = memcpy_start.load(dst_loc, dst_align);

        // Load the first byte pointed to by `src`, and store it at `dst`.
        let src_val = memcpy_start.load(src, src_align);
        memcpy_start.store(src_val, dst, dst_align);

        // Advance the two pointers to the next byte.
        let src = memcpy_start.pointercast(src, self.cx.type_i64());
        let dst = memcpy_start.pointercast(dst, self.cx.type_i64());

        // Update the size: there is one less byte to copy.
        let size = memcpy_start.load(size_loc, size_align);
        let new_src = memcpy_start.add(src, const_one);
        let new_dst = memcpy_start.add(dst, const_one);
        let new_size = memcpy_start.sub(size, const_one);

        // Store the pointers and the size back to the stack.
        memcpy_start.store(new_src, src_loc, src_align);
        memcpy_start.store(new_dst, dst_loc, dst_align);
        memcpy_start.store(new_size, size_loc, size_align);

        // Jump back, and re-evaluate the condition.
        memcpy_start.br(memcpy_cond.llbb());
        *self = memcpy_end;
    }

    fn memmove(
        &mut self,
        _dst: Self::Value,
        _dst_align: Align,
        _src: Self::Value,
        _src_align: Align,
        _size: Self::Value,
        _flags: MemFlags,
    ) {
        unimplemented!("memmove");
    }

    /// This loops `size` times, setting each byte of `ptr` to `fill_byte`.
    ///
    /// It is roughly equivalent to:
    /// while size > 0 {
    ///     *ptr = fill_byte;
    ///     ptr = ptr + 1;
    ///     size = size - 1;
    /// }
    fn memset(
        &mut self,
        ptr: Value,
        fill_byte: Value,
        size: Value,
        align: Align,
        _flags: MemFlags,
    ) {
        // The basic block that checks how many more bytes need to be copied.
        let mut memset_cond = self.build_sibling_block("memset_cond");
        // The body of the loop.
        let mut memset_start = self.build_sibling_block("memset_start");
        // The block to jump to once the `memset` is done.
        let memset_end = self.build_sibling_block("memset_end");
        // Helper variables:
        let i64_ty = self.type_i64();
        let i8p = self.type_i8p();
        let const_one = memset_start.cx.const_uint(
            memset_start.cx.type_i64(), 1);
        let const_zero = memset_start.cx.const_uint(
            memset_start.cx.type_i64(), 0);
        let size_align = Align::from_bytes(8).unwrap();

        // The address where the pointer is stored on the stack.
        let ptr_loc = self.alloca(i8p, "memset_ptr", align);
        // The address where the size is stored on the stack.
        let size_loc = self.alloca(i64_ty, "memset_size", size_align);
        let ptr = self.pointercast(ptr, i8p);
        self.store(ptr, ptr_loc, align);
        self.store(size, size_loc, size_align);

        // Branch to the block which evaluates the condition of the memset.
        self.br(memset_cond.llbb());
        // Load the value of the size, and check if it is greater than zero.
        let size = memset_cond.load(size_loc, size_align);
        let cond = memset_cond.icmp(IntPredicate::IntUGT, size, const_zero);
        let memset_start_bb = memset_start.llbb();
        let memset_end_bb = memset_end.llbb();

        // Begin memsetting if size > 0.
        memset_cond.cond_br(cond, memset_start_bb, memset_end_bb);

        // Load the pointer from the stack.
        let ptr = memset_start.load(ptr_loc, align);
        // Store `fill_byte` into the pointer.
        memset_start.store(fill_byte, ptr, align);

        // Advance the pointer to the next byte.
        let ptr = memset_start.pointercast(ptr, self.cx.type_i64());

        // Subtract 1 from the total size, as there is one less byte to copy.
        let size = memset_start.load(size_loc, size_align);
        let new_size = memset_start.sub(size, const_one);

        // Store the pointers and the size back to the stack.
        memset_start.store(ptr, ptr_loc, align);
        memset_start.store(new_size, size_loc, size_align);

        // Jump back, and re-evaluate the condition.
        memset_start.br(memset_cond.llbb());
        *self = memset_end;
    }

    fn load_operand(&mut self, place: PlaceRef<'tcx, Value>)
        -> OperandRef<'tcx, Value> {
        // FIXME?
        let val = if let Some(llextra) = place.llextra {
            OperandValue::Ref(place.llval, Some(llextra), place.align)
        } else if place.layout.is_ironox_immediate() {
            let mut const_llval = if let Value::Global(idx) = place.llval {
                self.cx.globals.borrow()[idx].get_initializer()
            } else {
                None
            };
            // FIXME: If this is a constant global, get its initializer.
            let llval = const_llval.unwrap_or_else(|| {
                self.load(place.llval, place.align)
            });
            OperandValue::Immediate(to_immediate(self, llval, place.layout))
        } else if let layout::Abi::ScalarPair(ref a, ref b) = place.layout.abi {
            // FIXME: also handle the metadata
            let b_offset = a.value.size(self).align_to(b.value.align(self).abi);
            // Load and return both elements of the pair.
            let a_ptr = self.struct_gep(place.llval, 0);
            let a = self.load(a_ptr, place.align);
            let b_ptr = self.struct_gep(place.llval, 1);
            let b = self.load(b_ptr, place.align.restrict_for_offset(b_offset));
            OperandValue::Pair(a, b)
        } else {
            OperandValue::Ref(place.llval, None, place.align)
        };
        OperandRef { val, layout: place.layout }
    }
}
