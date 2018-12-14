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
use value::Value;
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
use registers::GPR;

impl BackendTypes for Builder<'_, 'll, 'tcx> {
    type Value = <CodegenCx<'ll, 'tcx> as BackendTypes>::Value;
    type BasicBlock = <CodegenCx<'ll, 'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'ll, 'tcx> as BackendTypes>::Type;
    type Funclet = <CodegenCx<'ll, 'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'ll, 'tcx> as BackendTypes>::DIScope;
}


/// Identifies the current insertion point: function index, basic block index,
/// instruction index.
pub struct BuilderPosition(usize, usize, usize);

pub struct Builder<'a, 'll: 'a, 'tcx: 'll> {
    pub cx: &'a CodegenCx<'ll, 'tcx>,
    pub builder: RefCell<BuilderPosition>,
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

    fn emit_instr(&mut self, asm: String) {
        let mut builder = self.builder.borrow_mut();
        let llfn = builder.0;
        let llbb = builder.1;
        let instr = builder.2;
        let mut module = self.cx.module.borrow_mut();
        module.functions[llfn].basic_blocks[llbb].instrs
            .insert(instr, asm);
        // move to the next instruction
        builder.2 += 1;
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
        eprintln!("Create a new block in llfn {:?} named {}", llfn, name);
        eprintln!("Function count {}", cx.module.borrow().functions.len());
        bx.position_at_end(bb);
        bx
    }

    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        // FIXME? this is an invalid position and must be overwritten
        Builder {
            cx,
            builder: RefCell::new(BuilderPosition(0, 0, 0))
        }
    }

    fn build_sibling_block<'b>(&self, name: &'b str) -> Self {
        Builder::new_block(self.cx, self.llfn(), name)
    }

    fn cx(&self) -> &CodegenCx<'ll, 'tcx> {
        &self.cx
    }

    fn llfn(&self) -> Value {
        let bb = self.builder.borrow();
        // the parent of the current basic block
        let fn_index = bb.0;
        Value::Function(fn_index)
    }

    fn llbb(&self) -> BasicBlock {
        BasicBlock(self.builder.borrow().0,
                   self.builder.borrow().1)
    }

    fn count_insn(&self, category: &str) {
        unimplemented!("count_insn");
    }

    fn set_value_name(&mut self, value: Value, name: &str) {
        eprintln!("rename value {:?} to {:?}", value, name);
    }

    fn position_at_end(&mut self, llbb: BasicBlock) {
        let llfn = llbb.0;
        let llbb = llbb.1;
        let instr =
            self.cx.module.borrow().functions[llfn].basic_blocks[llbb].instrs.len();
        self.builder.replace(BuilderPosition(llfn, llbb, instr));
    }

    fn position_at_start(&mut self, llbb: BasicBlock) {
        unimplemented!("position_at_start");
    }

    fn ret_void(&mut self) {
        self.emit_instr("ret".to_string());
    }

    fn ret(&mut self, v: Value) {
         eprintln!("ret {:?}", v);
    }

    fn br(&mut self, dest: BasicBlock) {
        let br_instr = {
            let module = self.cx.module.borrow();
            format!(
                "jmp {}",
                module.functions[dest.0].basic_blocks[dest.1].label)
        };
        self.emit_instr(br_instr);
    }

    fn cond_br(
        &mut self,
        cond: Value,
        then_llbb: BasicBlock,
        else_llbb: BasicBlock,
    ) {
        unimplemented!("cond_br");
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
        // FIXME?
    }

    fn add(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("add");
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
        let cur_fn = self.builder.borrow().0;
        let mut module = self.cx.module.borrow_mut();
        let local_idx = module.functions[cur_fn].alloca(self.cx, ty, name, align);
        Value::Local(cur_fn, local_idx)
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
        unimplemented!("store");
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
        unimplemented!("store_with_flags");
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
        unimplemented!("trunc");
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
        unimplemented!("bitcast");
    }

    fn intcast(
        &mut self,
        val: Value,
        dest_ty: Type,
        is_signed: bool
    )-> Value {
        val
    }

    fn pointercast(
        &mut self,
        val: Value,
        dest_ty: Type
    )-> Value {
        // FIXME? nothing to do
        eprintln!("pointercast");
        val
    }

    fn icmp(
        &mut self,
        op: IntPredicate,
        lhs: Value, rhs: Value
    )-> Value {
        unimplemented!("icmp");
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
        eprintln!("Insert {:?} into {:?} at idx {}", agg_val, elt, idx);
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
    ) -> Cow<'b, [Value]>
        where [Value] : ToOwned {
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
        //unimplemented!("call to {:?} with args {:?}", llfn, args);
        Value::None
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
        eprintln!("load ref {:?}", place);
        let imm = to_immediate(self, place.llval, place.layout);
        eprintln!("imm is {:?}", imm);
        eprintln!("layout is {:?}", place.layout);
        OperandRef {
            val: OperandValue::Immediate(imm),
            layout: place.layout,
        }
    }
}
