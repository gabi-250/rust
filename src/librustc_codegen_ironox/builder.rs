use rustc_codegen_ssa::common::{IntPredicate, RealPredicate, AtomicOrdering,
    OperandBundleDef, SynchronizationScope, AtomicRmwBinOp};
use rustc_codegen_ssa::MemFlags;
use context::CodegenCx;
use value::Value;
use libc::c_char;
use rustc::ty::TyCtxt;
use rustc::ty::layout::{Align, Size};
//use rustc_data_structures::small_c_str::SmallCStr;
use rustc_codegen_ssa::interfaces::*;
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use std::borrow::Cow;
use std::ops::Range;
use syntax::ast::AsmDialect;

use basic_block::BasicBlock;

pub struct Builder<'a, 'll: 'a, 'tcx: 'll, V : 'll> {
    pub cx: &'a CodegenCx<'ll, 'tcx, V>,
}

impl<'a, 'll, 'tcx> HasCodegen<'a, 'll, 'tcx> for Builder<'a, 'll, 'tcx, &'ll Value> {
    type CodegenCx = CodegenCx<'ll, 'tcx, &'ll Value>;
}

impl<'a, 'll, 'tcx> BuilderMethods<'a, 'll, 'tcx>
    for Builder<'a, 'll, 'tcx, &'ll Value> {
    fn new_block<'b>(
        cx: &'a Self::CodegenCx,
        llfn: <Self::CodegenCx as Backend<'ll>>::Value,
        name: &'b str
    )-> Self {
        let bx = Builder::with_cx(cx);
        // add a basic block
        bx
    }

    fn with_cx(cx: &'a Self::CodegenCx) -> Self {
        Builder {
            cx,
        }
    }

    fn build_sibling_block<'b>(&self, name: &'b str) -> Self {
        Builder::new_block(self.cx, self.llfn(), name)
    }
    fn cx(&self) -> &'a Self::CodegenCx {
        &self.cx
    }
    fn tcx(&self) -> TyCtxt<'a, 'tcx, 'tcx> {
        self.cx.tcx
    }
    fn llfn(&self) -> &'ll Value {
        &Value {}
    }
    fn llbb(&self) -> &'ll BasicBlock {
        &BasicBlock {}
    }
    fn count_insn(&self, category: &str) {
        unimplemented!("count_insn(&self, category: &str)");
    }

    fn set_value_name(&mut self, value: <Self::CodegenCx as Backend<'ll>>::Value, name: &str) {
        unimplemented!("set_value_name(&mut self, value: <Self::CodegenCx as Backend<'ll>>::Value, name: &str)");
    }
    fn position_at_end(&mut self, llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock) {
        unimplemented!("position_at_end(&mut self, llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock)");
    }
    fn position_at_start(&mut self, llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock) {
        unimplemented!("position_at_start(&mut self, llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock)");
    }
    fn ret_void(&mut self) {
        unimplemented!("ret_void(&mut self)");
    }
    fn ret(&mut self, v: <Self::CodegenCx as Backend<'ll>>::Value) {
        unimplemented!("ret(&mut self, v: <Self::CodegenCx as Backend<'ll>>::Value)");
    }
    fn br(&mut self, dest: <Self::CodegenCx as Backend<'ll>>::BasicBlock) {
        unimplemented!("br(&mut self, dest: <Self::CodegenCx as Backend<'ll>>::BasicBlock)");
    }
    fn cond_br(
        &mut self,
        cond: <Self::CodegenCx as Backend<'ll>>::Value,
        then_llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock,
        else_llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock,
    ) {
        unimplemented!("");
    }
    fn switch(
        &mut self,
        v: <Self::CodegenCx as Backend<'ll>>::Value,
        else_llbb: <Self::CodegenCx as Backend<'ll>>::BasicBlock,
        num_cases: usize,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn invoke(
        &mut self,
        llfn: <Self::CodegenCx as Backend<'ll>>::Value,
        args: &[<Self::CodegenCx as Backend<'ll>>::Value],
        then: <Self::CodegenCx as Backend<'ll>>::BasicBlock,
        catch: <Self::CodegenCx as Backend<'ll>>::BasicBlock,
        bundle: Option<&OperandBundleDef<'ll, <Self::CodegenCx as Backend<'ll>>::Value>>
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn unreachable(&mut self) {
        unimplemented!();
    }
    fn add(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fadd(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fadd_fast(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn sub(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fsub(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fsub_fast(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn mul(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fmul(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fmul_fast(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn udiv(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn exactudiv(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn sdiv(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn exactsdiv(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fdiv(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fdiv_fast(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn urem(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn srem(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn frem(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn frem_fast(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn shl(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn lshr(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn ashr(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn and(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn or(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn xor(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn neg(
        &mut self,
        v: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fneg(
        &mut self,
        v: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn not(
        &mut self,
        v: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn alloca(
        &mut self,
        ty: <Self::CodegenCx as Backend<'ll>>::Type,
        name: &str, align: Align
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn dynamic_alloca(
        &mut self,
        ty: <Self::CodegenCx as Backend<'ll>>::Type,
        name: &str, align: Align
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn array_alloca(
        &mut self,
        ty: <Self::CodegenCx as Backend<'ll>>::Type,
        len: <Self::CodegenCx as Backend<'ll>>::Value,
        name: &str,
        align: Align
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn load(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        align: Align
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn volatile_load(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn atomic_load(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        order: AtomicOrdering, align: Align
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn load_ref(
        &mut self,
        ptr: &PlaceRef<'tcx,<Self::CodegenCx as Backend<'ll>>::Value>
    )-> OperandRef<'tcx, <Self::CodegenCx as Backend<'ll>>::Value> {
        unimplemented!("");
    }

    fn range_metadata(
        &mut self,
        load: <Self::CodegenCx as Backend<'ll>>::Value,
        range: Range<u128>
    ) {
        unimplemented!("");
    }
    fn nonnull_metadata(&mut self, load: <Self::CodegenCx as Backend<'ll>>::Value) {
        unimplemented!("");
    }

    fn store(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        align: Align
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn atomic_store(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        order: AtomicOrdering,
        align: Align
    ) {
        unimplemented!("");
    }
    fn store_with_flags(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        align: Align,
        flags: MemFlags,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn gep(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        indices: &[<Self::CodegenCx as Backend<'ll>>::Value]
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn inbounds_gep(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        indices: &[<Self::CodegenCx as Backend<'ll>>::Value]
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn struct_gep(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        idx: u64
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn trunc(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn sext(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fptoui(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fptosi(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn uitofp(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn sitofp(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fptrunc(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fpext(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn ptrtoint(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn inttoptr(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn bitcast(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn intcast(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type, is_signed: bool
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn pointercast(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn icmp(
        &mut self,
        op: IntPredicate,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value, rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn fcmp(
        &mut self,
        op: RealPredicate,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value, rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn empty_phi(
        &mut self,
        ty: <Self::CodegenCx as Backend<'ll>>::Type)-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn phi(
        &mut self,
        ty: <Self::CodegenCx as Backend<'ll>>::Type,
        vals: &[<Self::CodegenCx as Backend<'ll>>::Value],
        bbs: &[<Self::CodegenCx as Backend<'ll>>::BasicBlock]
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn inline_asm_call(
        &mut self,
        asm: *const c_char,
        cons: *const c_char,
        inputs: &[<Self::CodegenCx as Backend<'ll>>::Value],
        output: <Self::CodegenCx as Backend<'ll>>::Type,
        volatile: bool,
        alignstack: bool,
        dia: AsmDialect
    )-> Option<<Self::CodegenCx as Backend<'ll>>::Value> {
        unimplemented!("");
    }

    fn minnum(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn maxnum(
        &mut self,
        lhs: <Self::CodegenCx as Backend<'ll>>::Value,
        rhs: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn select(
        &mut self, cond: <Self::CodegenCx as Backend<'ll>>::Value,
        then_val: <Self::CodegenCx as Backend<'ll>>::Value,
        else_val: <Self::CodegenCx as Backend<'ll>>::Value,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn va_arg(
        &mut self,
        list: <Self::CodegenCx as Backend<'ll>>::Value,
        ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn extract_element(&mut self,
        vec: <Self::CodegenCx as Backend<'ll>>::Value,
        idx: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn insert_element(
        &mut self, vec: <Self::CodegenCx as Backend<'ll>>::Value,
        elt: <Self::CodegenCx as Backend<'ll>>::Value,
        idx: <Self::CodegenCx as Backend<'ll>>::Value,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn shuffle_vector(
        &mut self,
        v1: <Self::CodegenCx as Backend<'ll>>::Value,
        v2: <Self::CodegenCx as Backend<'ll>>::Value,
        mask: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_splat(
        &mut self,
        num_elts: usize,
        elt: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_fadd_fast(
        &mut self,
        acc: <Self::CodegenCx as Backend<'ll>>::Value,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_fmul_fast(
        &mut self,
        acc: <Self::CodegenCx as Backend<'ll>>::Value,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_add(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_mul(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_and(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_or(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_xor(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_fmin(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_fmax(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_fmin_fast(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_fmax_fast(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_min(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value,
        is_signed: bool
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn vector_reduce_max(
        &mut self,
        src: <Self::CodegenCx as Backend<'ll>>::Value,
        is_signed: bool
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn extract_value(
        &mut self,
        agg_val: <Self::CodegenCx as Backend<'ll>>::Value,
        idx: u64
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn insert_value(
        &mut self,
        agg_val: <Self::CodegenCx as Backend<'ll>>::Value,
        elt: <Self::CodegenCx as Backend<'ll>>::Value,
        idx: u64
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn landing_pad(
        &mut self,
        ty: <Self::CodegenCx as Backend<'ll>>::Type,
        pers_fn: <Self::CodegenCx as Backend<'ll>>::Value,
        num_clauses: usize
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn add_clause(
        &mut self,
        landing_pad: <Self::CodegenCx as Backend<'ll>>::Value,
        clause: <Self::CodegenCx as Backend<'ll>>::Value
    ) {
        unimplemented!("");
    }
    fn set_cleanup(
        &mut self,
        landing_pad: <Self::CodegenCx as Backend<'ll>>::Value
    ) {
        unimplemented!("");
    }
    fn resume(
        &mut self,
        exn: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn cleanup_pad(
        &mut self,
        parent: Option<<Self::CodegenCx as Backend<'ll>>::Value>,
        args: &[<Self::CodegenCx as Backend<'ll>>::Value]
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn cleanup_ret(
        &mut self, cleanup: <Self::CodegenCx as Backend<'ll>>::Value,
        unwind: Option<<Self::CodegenCx as Backend<'ll>>::BasicBlock>,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn catch_pad(
        &mut self,
        parent: <Self::CodegenCx as Backend<'ll>>::Value,
        args: &[<Self::CodegenCx as Backend<'ll>>::Value]
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn catch_ret(
        &mut self,
        pad: <Self::CodegenCx as Backend<'ll>>::Value,
        unwind: <Self::CodegenCx as Backend<'ll>>::BasicBlock
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn catch_switch(
        &mut self,
        parent: Option<<Self::CodegenCx as Backend<'ll>>::Value>,
        unwind: Option<<Self::CodegenCx as Backend<'ll>>::BasicBlock>,
        num_handlers: usize,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn add_handler(
        &mut self,
        catch_switch: <Self::CodegenCx as Backend<'ll>>::Value,
        handler: <Self::CodegenCx as Backend<'ll>>::BasicBlock
    ) {
        unimplemented!("");
    }
    fn set_personality_fn(&mut self, personality: <Self::CodegenCx as Backend<'ll>>::Value) {
        unimplemented!("");
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: <Self::CodegenCx as Backend<'ll>>::Value,
        cmp: <Self::CodegenCx as Backend<'ll>>::Value,
        src: <Self::CodegenCx as Backend<'ll>>::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        weak: bool,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: <Self::CodegenCx as Backend<'ll>>::Value,
        src: <Self::CodegenCx as Backend<'ll>>::Value,
        order: AtomicOrdering,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }
    fn atomic_fence(&mut self, order: AtomicOrdering, scope: SynchronizationScope) {
        unimplemented!("");
    }
    fn add_case(
        &mut self,
        s: <Self::CodegenCx as Backend<'ll>>::Value,
        on_val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest: <Self::CodegenCx as Backend<'ll>>::BasicBlock
    ) {
        unimplemented!("");
    }
    fn add_incoming_to_phi(
        &mut self,
        phi: <Self::CodegenCx as Backend<'ll>>::Value,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        bb: <Self::CodegenCx as Backend<'ll>>::BasicBlock
    ) {
        unimplemented!("");
    }
    fn set_invariant_load(&mut self, load: <Self::CodegenCx as Backend<'ll>>::Value) {
        unimplemented!("");
    }

    /// Returns the ptr value that should be used for storing `val`.
    fn check_store(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    /// Returns the args that should be used for a call to `llfn`.
    fn check_call<'b>(
        &mut self,
        typ: &str,
        llfn: <Self::CodegenCx as Backend<'ll>>::Value,
        args: &'b [<Self::CodegenCx as Backend<'ll>>::Value]
    ) -> Cow<'b, [<Self::CodegenCx as Backend<'ll>>::Value]>
        where [<Self::CodegenCx as Backend<'ll>>::Value] : ToOwned {
        unimplemented!("");
    }

    fn lifetime_start(&mut self, ptr: <Self::CodegenCx as Backend<'ll>>::Value, size: Size) {
        unimplemented!("");
    }
    fn lifetime_end(&mut self, ptr: <Self::CodegenCx as Backend<'ll>>::Value, size: Size) {
        unimplemented!("");
    }

    /// If LLVM lifetime intrinsic support is enabled (i.e. optimizations
    /// on), and `ptr` is nonzero-sized, then extracts the size of `ptr`
    /// and the intrinsic for `lt` and passes them to `emit`, which is in
    /// charge of generating code to call the passed intrinsic on whatever
    /// block of generated code is targeted for the intrinsic.
    ///
    /// If LLVM lifetime intrinsic support is disabled (i.e.  optimizations
    /// off) or `ptr` is zero-sized, then no-op (does not call `emit`).
    fn call_lifetime_intrinsic(
        &mut self,
        intrinsic: &str,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value, size: Size
    ) {
        unimplemented!("");
    }

    fn call(
        &mut self,
        llfn: <Self::CodegenCx as Backend<'ll>>::Value,
        args: &[<Self::CodegenCx as Backend<'ll>>::Value],
        bundle: Option<&OperandBundleDef<'ll, <Self::CodegenCx as Backend<'ll>>::Value>>
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn call_memcpy(
        &mut self,
        dst: <Self::CodegenCx as Backend<'ll>>::Value,
        src: <Self::CodegenCx as Backend<'ll>>::Value,
        n_bytes: <Self::CodegenCx as Backend<'ll>>::Value,
        align: Align,
        flags: MemFlags,
    ) {
        unimplemented!("");
    }

    fn call_memset(
        &mut self,
        ptr: <Self::CodegenCx as Backend<'ll>>::Value,
        fill_byte: <Self::CodegenCx as Backend<'ll>>::Value,
        size: <Self::CodegenCx as Backend<'ll>>::Value,
        align: <Self::CodegenCx as Backend<'ll>>::Value,
        volatile: bool,
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn zext(
        &mut self,
        val: <Self::CodegenCx as Backend<'ll>>::Value,
        dest_ty: <Self::CodegenCx as Backend<'ll>>::Type
    )-> <Self::CodegenCx as Backend<'ll>>::Value {
        unimplemented!("");
    }

    fn delete_basic_block(&mut self, bb: <Self::CodegenCx as Backend<'ll>>::BasicBlock) {
        unimplemented!("delete_basic_block(&mut self, bb: <Self::CodegenCx as Backend<'ll>>::BasicBlock)");
    }
    fn do_not_inline(&mut self, llret: <Self::CodegenCx as Backend<'ll>>::Value) {
        unimplemented!("do_not_inline(&mut self, llret: <Self::CodegenCx as Backend<'ll>>::Value)");
    }
}
