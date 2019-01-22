// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use rustc_codegen_ssa::common::{IntPredicate, RealPredicate, AtomicOrdering,
    SynchronizationScope, AtomicRmwBinOp};
use rustc_codegen_ssa::MemFlags;
use libc::c_char;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::base::to_immediate;
use rustc_codegen_ssa::mir::operand::{OperandValue, OperandRef};
use rustc_codegen_ssa::mir::place::PlaceRef;
use context::CodegenCx;
use rustc::hir::def_id::DefId;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{Align, Size, TyLayout};
use rustc_codegen_ssa::traits::*;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ffi::CStr;
use std::ops::{Deref, Range};
use syntax;

use ir::basic_block::{BasicBlock, BasicBlockData};
use type_::Type;
use value::{Instruction, Value};

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
    pub builder: BuilderPosition,
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
    fn get_static(&self, def_id: DefId) -> Value {
        unimplemented!("get_static");
    }
}

impl Builder<'a, 'll, 'tcx> {
    /// Add the instruction at the current `BuilderPosition`.
    fn emit_instr(&mut self, inst: Instruction) -> Value {
        let mut module = self.cx.module.borrow_mut();
        module.get_function(self.builder.fn_idx)
            .insert_inst(self.builder.bb_idx, self.builder.inst_idx, inst);
        let ret = Value::Instruction(self.builder.fn_idx,
                                     self.builder.bb_idx,
                                     self.builder.inst_idx);
        // move to the next instruction
        self.builder.inst_idx += 1;
        ret
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
        unimplemented!("checked_binop");
    }

    fn new_block<'b>(
        cx: &'a Self::CodegenCx,
        llfn: Value,
        name: &'b str
    )-> Self {
        let mut bx = Builder::with_cx(cx);
        let bb = BasicBlockData::new(cx, name, llfn);
        bx.position_at_end(bb);
        bx
    }

    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        // FIXME? this is an invalid position and must be overwritten
        Builder {
            cx,
            builder: BuilderPosition::new(0, 0, 0)
        }
    }

    fn build_sibling_block<'b>(&self, name: &'b str) -> Self {
        Builder::new_block(self.cx, self.llfn(), name)
    }

    fn cx(&self) -> &CodegenCx<'ll, 'tcx> {
        &self.cx
    }

    fn llfn(&self) -> Value {
        // The function this builder is adding instructions to.
        Value::Function(self.builder.fn_idx)
    }

    fn llbb(&self) -> BasicBlock {
        // The BasicBlock this builder is adding instructions to.
        BasicBlock(self.builder.fn_idx,
                   self.builder.bb_idx)
    }

    fn count_insn(&self, category: &str) {
        unimplemented!("count_insn");
    }

    fn set_value_name(&mut self, value: Value, name: &str) {
        let mut module = self.cx.module.borrow_mut();
        // FIXME: rename the value
        match value {
            Value::Param(idx, ty) => {
                eprintln!("rename {} {:?} to {}", idx,
                          self.types.borrow()[ty], name);
                module.functions[idx].rename_local(value, name.to_string());
            }
            _ => {
                unimplemented!("set_value_name: {:?}", value);
            }
        }
    }

    fn position_at_end(&mut self, llbb: BasicBlock) {
        let llfn = llbb.0;
        let llbb = llbb.1;
        // The next instruction will be inserted as the last instruction in
        // the basic block.
        let instr =
            self.cx.module.borrow().functions[llfn].basic_blocks[llbb].instrs.len();
        self.builder = BuilderPosition::new(llfn, llbb, instr);
    }

    fn position_at_start(&mut self, llbb: BasicBlock) {
        unimplemented!("position_at_start");
    }

    fn ret_void(&mut self) {
        let _ = self.emit_instr(Instruction::Ret(None));
    }

    fn ret(&mut self, v: Value) {
        let _ = self.emit_instr(Instruction::Ret(Some(v)));
    }

    fn br(&mut self, dest: BasicBlock) {
        let mut label;
        {
            let module = self.cx.module.borrow();
            label = module.functions[dest.0].basic_blocks[dest.1].label.to_string();
        }
        let _ = self.emit_instr(Instruction::Br(label));
    }

    fn cond_br(
        &mut self,
        cond: Value,
        then_llbb: BasicBlock,
        else_llbb: BasicBlock,
    ) {
        let mut true_label;
        let mut false_label;
        {
            let module = self.cx.module.borrow();
            true_label =
                module.functions[then_llbb.0].basic_blocks[then_llbb.1
                ].label.to_string();
            false_label =
                module.functions[else_llbb.0].basic_blocks[else_llbb.1]
                .label.to_string();
        }
        self.emit_instr(Instruction::CondBr(cond, true_label, false_label));
    }

    fn switch(
        &mut self,
        v: Value,
        else_llbb: BasicBlock,
        num_cases: usize,
    )-> Value {
        unimplemented!("switch");
    }

    fn invoke(
        &mut self,
        llfn: Value,
        args: &[Value],
        then: BasicBlock,
        catch: BasicBlock,
        funclet: Option<&Self::Funclet>,
    )-> Value {
        unimplemented!("invoke");
    }

    fn unreachable(&mut self) {
        unimplemented!("unreachable");
    }

    fn add(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        eprintln!("{:?} + {:?}", lhs, rhs);
        self.emit_instr(Instruction::Add(lhs, rhs))
    }

    fn fadd(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fadd");
    }

    fn fadd_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fadd_fast");
    }

    fn sub(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("sub");
    }

    fn fsub(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fsub");
    }

    fn fsub_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fsub_fast");
    }

    fn mul(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("mul");
    }

    fn fmul(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fmul");
    }

    fn fmul_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fmul_fast");
    }

    fn udiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("udiv");
    }

    fn exactudiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("exactudiv");
    }

    fn sdiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("sdiv");
    }

    fn exactsdiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("exactsdiv");
    }

    fn fdiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fdiv");
    }

    fn fdiv_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("fdiv_fast");
    }

    fn urem(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("urem");
    }

    fn srem(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("srem");
    }

    fn frem(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("frem");
    }

    fn frem_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("frem_fast");
    }

    fn shl(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("shl");
    }

    fn lshr(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("lshr");
    }

    fn ashr(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("ashr");
    }

    fn and(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("and");
    }

    fn or(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("or");
    }

    fn xor(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("xor");
    }

    fn neg(
        &mut self,
        v: Value
    )-> Value {
        unimplemented!("neg");
    }

    fn fneg(
        &mut self,
        v: Value
    )-> Value {
        unimplemented!("fneg");
    }

    fn not(
        &mut self,
        v: Value
    )-> Value {
        unimplemented!("not");
    }

    fn alloca(
        &mut self,
        ty: Type,
        name: &str, align: Align
    )-> Value {
        self.emit_instr(Instruction::Alloca(name.to_string(), ty, align))
    }

    fn dynamic_alloca(
        &mut self,
        ty: Type,
        name: &str, align: Align
    )-> Value {
        unimplemented!("dynamic_alloca");
    }

    fn array_alloca(
        &mut self,
        ty: Type,
        len: Value,
        name: &str,
        align: Align
    )-> Value {
        unimplemented!("array_alloca");
    }

    fn load(
        &mut self,
        ptr: Value,
        align: Align
    )-> Value {
        unimplemented!("load");
    }

    fn volatile_load(
        &mut self,
        ptr: Value
    )-> Value {
        unimplemented!("volatile_load");
    }

    fn atomic_load(
        &mut self,
        ptr: Value,
        order: AtomicOrdering, size: Size
    )-> Value {
        unimplemented!("atomic_load");
    }

    fn range_metadata(
        &mut self,
        load: Value,
        range: Range<u128>
    ) {
        unimplemented!("range_metadata");
    }

    fn nonnull_metadata(&mut self, load: Value) {
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
        val: Value,
        ptr: Value,
        order: AtomicOrdering,
        size: Size
    ) {
        unimplemented!("atomic_store");
    }

    fn store_with_flags(
        &mut self,
        val: Value,
        ptr: Value,
        align: Align,
        flags: MemFlags,
    )-> Value {
        // FIXME: ignore the flags for now
        self.emit_instr(Instruction::Store(ptr, val))
    }

    fn gep(
        &mut self,
        ptr: Value,
        indices: &[Value]
    )-> Value {
        unimplemented!("gep");
    }

    fn inbounds_gep(
        &mut self,
        ptr: Value,
        indices: &[Value]
    )-> Value {
        unimplemented!("inbounds_gep");
    }

    fn struct_gep(
        &mut self,
        ptr: Value,
        idx: u64
    )-> Value {
        unimplemented!("struct_gep");
    }

    fn trunc(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        // XXX truncate
        val
    }

    fn sext(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("sext");
    }

    fn fptoui(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("fptoui");
    }

    fn fptosi(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("fptosi");
    }

    fn uitofp(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("uitofp");
    }

    fn sitofp(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("sitofp");
    }

    fn fptrunc(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("fptrunc");
    }

    fn fpext(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("fpext");
    }

    fn ptrtoint(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("ptrtoint");
    }

    fn inttoptr(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("inttoptr");
    }

    fn bitcast(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        // XXX cast
        //unimplemented!("bitcast {:?} to {:?}", val, self.types.borrow()[dest_ty]);
        val
    }

    fn intcast(
        &mut self,
        val: Value,
        dest_ty: Type,
        is_signed: bool
    )-> Value {
        val
        //self.emit_instr(Instruction::Cast(val, dest_ty))
    }

    fn pointercast(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        val
        //self.emit_instr(Instruction::Cast(val, dest_ty))
    }

    fn icmp(
        &mut self,
        op: IntPredicate,
        lhs: Value, rhs: Value
    )-> Value {
        match op {
            IntPredicate::IntEQ => {
                self.emit_instr(Instruction::Eq(lhs, rhs))
            },
            _ => {
                unimplemented!("icmp");
            }
        }
    }

    fn fcmp(
        &mut self,
        op: RealPredicate,
        lhs: Value, rhs: Value
    )-> Value {
        unimplemented!("fcmp");
    }

    fn empty_phi(
        &mut self,
        ty: Type)-> Value {
        unimplemented!("empty_phi");
    }

    fn phi(
        &mut self,
        ty: Type,
        vals: &[Value],
        bbs: &[BasicBlock]
    )-> Value {
        unimplemented!("phi");
    }

    fn inline_asm_call(
        &mut self,
        asm: &CStr,
        cons: &CStr,
        inputs: &[Value], output: Type,
        volatile: bool, alignstack: bool,
        dia: syntax::ast::AsmDialect) -> Option<Value> {
        unimplemented!("inline_asm_call");
    }

    fn minnum(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("minnum");
    }

    fn maxnum(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("maxnum");
    }

    fn select(
        &mut self, cond: Value,
        then_val: Value,
        else_val: Value,
    )-> Value {
        unimplemented!("select");
    }

    fn va_arg(
        &mut self,
        list: Value,
        ty: Type
    )-> Value {
        unimplemented!("va_arg");
    }

    fn extract_element(&mut self,
        vec: Value,
        idx: Value
    )-> Value {
        unimplemented!("extract_element");
    }

    fn insert_element(
        &mut self, vec: Value,
        elt: Value,
        idx: Value,
    )-> Value {
        unimplemented!("insert_element");
    }

    fn shuffle_vector(
        &mut self,
        v1: Value,
        v2: Value,
        mask: Value
    )-> Value {
        unimplemented!("shuffle_vector");
    }

    fn vector_splat(
        &mut self,
        num_elts: usize,
        elt: Value
    )-> Value {
        unimplemented!("vector_splat");
    }

    fn vector_reduce_fadd_fast(
        &mut self,
        acc: Value,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_fadd_fast");
    }

    fn vector_reduce_fmul_fast(
        &mut self,
        acc: Value,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmul_fast");
    }

    fn vector_reduce_add(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_add");
    }

    fn vector_reduce_mul(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_mul");
    }

    fn vector_reduce_and(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_and");
    }

    fn vector_reduce_or(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_or");
    }

    fn vector_reduce_xor(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_xor");
    }

    fn vector_reduce_fmin(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmin");
    }

    fn vector_reduce_fmax(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmax");
    }

    fn vector_reduce_fmin_fast(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmin_fast");
    }

    fn vector_reduce_fmax_fast(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("vector_reduce_fmax_fast");
    }

    fn vector_reduce_min(
        &mut self,
        src: Value,
        is_signed: bool
    )-> Value {
        unimplemented!("vector_reduce_min");
    }

    fn vector_reduce_max(
        &mut self,
        src: Value,
        is_signed: bool
    )-> Value {
        unimplemented!("vector_reduce_max");
    }

    fn extract_value(
        &mut self,
        agg_val: Value,
        idx: u64
    )-> Value {
        unimplemented!("extract_value");
    }

    fn insert_value(
        &mut self,
        agg_val: Value,
        elt: Value,
        idx: u64
    )-> Value {
        // FIXME: insert elt into agg_val at idx
        eprintln!("insert {:?} into {:?} at index {}", elt, agg_val, idx);
        let types = self.types.borrow();
        for i in 0..types.len() {
            eprintln!("{}: {:?}", i, types[i]);
        }
        elt
    }

    fn landing_pad(
        &mut self,
        ty: Type,
        pers_fn: Value,
        num_clauses: usize
    )-> Value {
        unimplemented!("landing_pad");
    }

    fn add_clause(
        &mut self,
        landing_pad: Value,
        clause: Value
    ) {
        unimplemented!("add_clause");
    }

    fn set_cleanup(
        &mut self,
        landing_pad: Value
    ) {
        unimplemented!("set_cleanup");
    }

    fn resume(
        &mut self,
        exn: Value
    )-> Value {
        unimplemented!("resume");
    }

    fn cleanup_pad(
        &mut self,
        parent: Option<Value>,
        args: &[Value]
    ) {
        unimplemented!("cleanup_pad");
    }

    fn cleanup_ret(
        &mut self,
        cleanup: &<Self::CodegenCx as BackendTypes>::Funclet,
        unwind: Option<BasicBlock>,
    ) -> Value {
        unimplemented!("cleanup_ret");
    }

    fn catch_pad(
        &mut self,
        parent: Value,
        args: &[Value]
    ) {
        unimplemented!("catch_pad");
    }

    fn catch_ret(
        &mut self,
        pad: &<Self::CodegenCx as BackendTypes>::Funclet,
        unwind: BasicBlock
    ) -> Value {
        unimplemented!("catch_ret");
    }

    fn catch_switch(
        &mut self,
        parent: Option<Value>,
        unwind: Option<BasicBlock>,
        num_handlers: usize,
    )-> Value {
        unimplemented!("catch_switch");
    }

    fn add_handler(
        &mut self,
        catch_switch: Value,
        handler: BasicBlock
    ) {
        unimplemented!("add_handler");
    }

    fn set_personality_fn(&mut self, personality: Value) {
        unimplemented!("set_personality_fn");
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: Value,
        cmp: Value,
        src: Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        weak: bool,
    )-> Value {
        unimplemented!("");
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: Value,
        src: Value,
        order: AtomicOrdering,
    )-> Value {
        unimplemented!("atomic_rmw");
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, scope: SynchronizationScope) {
        unimplemented!("atomic_fence");
    }

    fn add_case(
        &mut self,
        s: Value,
        on_val: Value,
        dest: BasicBlock
    ) {
        unimplemented!("add_case(");
    }

    fn add_incoming_to_phi(
        &mut self,
        phi: Value,
        val: Value,
        bb: BasicBlock
    ) {
        unimplemented!("add_incoming_to_phi");
    }

    fn set_invariant_load(&mut self, load: Value) {
        unimplemented!("set_invariant_load");
    }

    /// Returns the ptr value that should be used for storing `val`.
    fn check_store(
        &mut self,
        val: Value,
        ptr: Value
    )-> Value {
        unimplemented!("check_store");
    }

    /// Returns the args that should be used for a call to `llfn`.
    fn check_call<'b>(
        &mut self,
        typ: &str,
        llfn: Value,
        args: &'b [Value]
    ) -> Cow<'b, [Value]> {
        unimplemented!("check_call");
    }

    fn lifetime_start(&mut self, ptr: Value, size: Size) {
        // FIXME? nothing to do for now
    }

    fn lifetime_end(&mut self, ptr: Value, size: Size) {
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
        match llfn {
            Value::Function(idx) => {
                self.emit_instr(Instruction::Call(idx, args.to_vec()))
            },
            Value::Param(idx, ty) => {
                eprintln!("expected Value::Function, found  {:?}", self.pretty_ty(ty));
                self.emit_instr(Instruction::Call(0, args.to_vec()))
            },
            _ => {
                eprintln!("expected Value::Function, found  {:?} {:?}",
                          llfn, self.types.borrow());

                self.emit_instr(Instruction::Call(0, args.to_vec()))
            }
        }
    }

    fn zext(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        unimplemented!("zext");
    }

    unsafe fn delete_basic_block(&mut self, bb: BasicBlock) {
        unimplemented!("delete_basic_block");
    }

    fn do_not_inline(&mut self, llret: Value) {
        unimplemented!("do_not_inline");
    }

    fn memcpy(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        unimplemented!("memcpy");
    }

    fn memmove(
        &mut self,
        dst: Self::Value,
        dst_align: Align,
        src: Self::Value,
        src_align: Align,
        size: Self::Value,
        flags: MemFlags,
    ) {
        unimplemented!("memmove");
    }

    fn memset(
        &mut self,
        ptr: Self::Value,
        fill_byte: Self::Value,
        size: Self::Value,
        align: Align,
        flags: MemFlags,
    ) {
        unimplemented!("memset");
    }

    fn load_operand(&mut self, place: PlaceRef<'tcx, Value>)
        -> OperandRef<'tcx, Value> {
        // FIXME?
        let imm = to_immediate(self, place.llval, place.layout);
        OperandRef {
            val: OperandValue::Immediate(imm),
            layout: place.layout,
        }
    }
}
