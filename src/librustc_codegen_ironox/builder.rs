use rustc_codegen_ssa::common::{IntPredicate, RealPredicate, AtomicOrdering,
    SynchronizationScope, AtomicRmwBinOp};
use rustc_codegen_ssa::MemFlags;
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use context::CodegenCx;
use value::Value;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{Align, Size, TyLayout};
use rustc_codegen_ssa::traits::*;
use std::borrow::Cow;
use std::ffi::CStr;
use std::ops::Range;
use syntax;

use basic_block::BasicBlock;
use ironox_type::Type;

impl BackendTypes for Builder<'_, 'll, 'tcx> {
    type Value = <CodegenCx<'ll, 'tcx> as BackendTypes>::Value;
    type BasicBlock = <CodegenCx<'ll, 'tcx> as BackendTypes>::BasicBlock;
    type Type = <CodegenCx<'ll, 'tcx> as BackendTypes>::Type;
    type Context = <CodegenCx<'ll, 'tcx> as BackendTypes>::Context;
    type Funclet = <CodegenCx<'ll, 'tcx> as BackendTypes>::Funclet;

    type DIScope = <CodegenCx<'ll, 'tcx> as BackendTypes>::DIScope;
}

pub struct Builder<'a, 'll: 'a, 'tcx: 'll> {
    pub cx: &'a CodegenCx<'ll, 'tcx>,
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

impl HasCodegen<'tcx> for Builder<'a, 'll, 'tcx> {
    type CodegenCx = CodegenCx<'ll, 'tcx>;
}

impl BuilderMethods<'a, 'tcx> for Builder<'a, 'll, 'tcx> {
    fn new_block<'b>(
        cx: &'a Self::CodegenCx,
        llfn: <Self::CodegenCx as BackendTypes>::Value,
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
    fn cx(&self) -> &CodegenCx<'ll, 'tcx> {
        &self.cx
    }

    fn llfn(&self) -> &'ll Value {
        &Value {}
    }

    fn llbb(&self) -> &'ll BasicBlock {
        &BasicBlock {}
    }

    fn count_insn(&self, category: &str) {
        unimplemented!("count_insn");
    }

    fn set_value_name(&mut self, value: <Self::CodegenCx as BackendTypes>::Value, name: &str) {
        unimplemented!("set_value_name");
    }

    fn position_at_end(&mut self, llbb: <Self::CodegenCx as BackendTypes>::BasicBlock) {
        unimplemented!("position_at_end");
    }

    fn position_at_start(&mut self, llbb: <Self::CodegenCx as BackendTypes>::BasicBlock) {
        unimplemented!("position_at_start");
    }

    fn ret_void(&mut self) {
        unimplemented!("ret_void");
    }

    fn ret(&mut self, v: <Self::CodegenCx as BackendTypes>::Value) {
        unimplemented!("ret");
    }

    fn br(&mut self, dest: <Self::CodegenCx as BackendTypes>::BasicBlock) {
        unimplemented!("br");
    }

    fn cond_br(
        &mut self,
        cond: <Self::CodegenCx as BackendTypes>::Value,
        then_llbb: <Self::CodegenCx as BackendTypes>::BasicBlock,
        else_llbb: <Self::CodegenCx as BackendTypes>::BasicBlock,
    ) {
        unimplemented!("cond_br");
    }

    fn switch(
        &mut self,
        v: <Self::CodegenCx as BackendTypes>::Value,
        else_llbb: <Self::CodegenCx as BackendTypes>::BasicBlock,
        num_cases: usize,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("switch");
    }

    fn invoke(
        &mut self,
        llfn: <Self::CodegenCx as BackendTypes>::Value,
        args: &[<Self::CodegenCx as BackendTypes>::Value],
        then: <Self::CodegenCx as BackendTypes>::BasicBlock,
        catch: <Self::CodegenCx as BackendTypes>::BasicBlock,
        funclet: Option<&Self::Funclet>,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("invoke");
    }

    fn unreachable(&mut self) {
        unimplemented!("unreachable");
    }

    fn add(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("add");
    }

    fn fadd(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fadd");
    }

    fn fadd_fast(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fadd_fast");
    }

    fn sub(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("sub");
    }

    fn fsub(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fsub");
    }

    fn fsub_fast(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fsub_fast");
    }

    fn mul(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("mul");
    }

    fn fmul(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fmul");
    }

    fn fmul_fast(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fmul_fast");
    }

    fn udiv(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("udiv");
    }

    fn exactudiv(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("exactudiv");
    }

    fn sdiv(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("sdiv");
    }

    fn exactsdiv(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("exactsdiv");
    }

    fn fdiv(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fdiv");
    }

    fn fdiv_fast(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fdiv_fast");
    }

    fn urem(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("urem");
    }

    fn srem(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("srem");
    }

    fn frem(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("frem");
    }

    fn frem_fast(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("frem_fast");
    }

    fn shl(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("shl");
    }

    fn lshr(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("lshr");
    }

    fn ashr(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("ashr");
    }

    fn and(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("and");
    }

    fn or(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("or");
    }

    fn xor(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("xor");
    }

    fn neg(
        &mut self,
        v: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("neg");
    }

    fn fneg(
        &mut self,
        v: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fneg");
    }

    fn not(
        &mut self,
        v: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("not");
    }

    fn alloca(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        name: &str, align: Align
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("alloca");
    }

    fn dynamic_alloca(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        name: &str, align: Align
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("dynamic_alloca");
    }

    fn array_alloca(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        len: <Self::CodegenCx as BackendTypes>::Value,
        name: &str,
        align: Align
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("array_alloca");
    }

    fn load(
        &mut self,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        align: Align
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("load");
    }

    fn volatile_load(
        &mut self,
        ptr: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("volatile_load");
    }

    fn atomic_load(
        &mut self,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        order: AtomicOrdering, size: Size
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("atomic_load");
    }

    fn range_metadata(
        &mut self,
        load: <Self::CodegenCx as BackendTypes>::Value,
        range: Range<u128>
    ) {
        unimplemented!("range_metadata");
    }

    fn nonnull_metadata(&mut self, load: <Self::CodegenCx as BackendTypes>::Value) {
        unimplemented!("nonnull_metadata");
    }

    fn store(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        align: Align
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("store");
    }

    fn atomic_store(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        order: AtomicOrdering,
        size: Size
    ) {
        unimplemented!("atomic_store");
    }

    fn store_with_flags(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        align: Align,
        flags: MemFlags,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("store_with_flags");
    }

    fn gep(
        &mut self,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        indices: &[<Self::CodegenCx as BackendTypes>::Value]
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("gep");
    }

    fn inbounds_gep(
        &mut self,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        indices: &[<Self::CodegenCx as BackendTypes>::Value]
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("inbounds_gep");
    }

    fn struct_gep(
        &mut self,
        ptr: <Self::CodegenCx as BackendTypes>::Value,
        idx: u64
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("struct_gep");
    }

    fn trunc(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("trunc");
    }

    fn sext(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("sext");
    }

    fn fptoui(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fptoui");
    }

    fn fptosi(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fptosi");
    }

    fn uitofp(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("uitofp");
    }

    fn sitofp(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("sitofp");
    }

    fn fptrunc(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fptrunc");
    }

    fn fpext(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fpext");
    }

    fn ptrtoint(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("ptrtoint");
    }

    fn inttoptr(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("inttoptr");
    }

    fn bitcast(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("bitcast");
    }

    fn intcast(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type, is_signed: bool
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("intcast");
    }

    fn pointercast(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("pointercast");
    }

    fn icmp(
        &mut self,
        op: IntPredicate,
        lhs: <Self::CodegenCx as BackendTypes>::Value, rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("icmp");
    }

    fn fcmp(
        &mut self,
        op: RealPredicate,
        lhs: <Self::CodegenCx as BackendTypes>::Value, rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("fcmp");
    }

    fn empty_phi(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type)-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("empty_phi");
    }

    fn phi(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        vals: &[<Self::CodegenCx as BackendTypes>::Value],
        bbs: &[<Self::CodegenCx as BackendTypes>::BasicBlock]
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("phi");
    }

    fn inline_asm_call(
        &mut self,
        asm: &CStr,
        cons: &CStr,
        inputs: &[&'ll Value], output: &'ll Type,
        volatile: bool, alignstack: bool,
        dia: syntax::ast::AsmDialect) -> Option<&'ll Value> {
        unimplemented!("inline_asm_call");
    }

    fn minnum(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("minnum");
    }

    fn maxnum(
        &mut self,
        lhs: <Self::CodegenCx as BackendTypes>::Value,
        rhs: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("maxnum");
    }

    fn select(
        &mut self, cond: <Self::CodegenCx as BackendTypes>::Value,
        then_val: <Self::CodegenCx as BackendTypes>::Value,
        else_val: <Self::CodegenCx as BackendTypes>::Value,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("select");
    }

    fn va_arg(
        &mut self,
        list: <Self::CodegenCx as BackendTypes>::Value,
        ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("va_arg");
    }

    fn extract_element(&mut self,
        vec: <Self::CodegenCx as BackendTypes>::Value,
        idx: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("extract_element");
    }

    fn insert_element(
        &mut self, vec: <Self::CodegenCx as BackendTypes>::Value,
        elt: <Self::CodegenCx as BackendTypes>::Value,
        idx: <Self::CodegenCx as BackendTypes>::Value,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("insert_element");
    }

    fn shuffle_vector(
        &mut self,
        v1: <Self::CodegenCx as BackendTypes>::Value,
        v2: <Self::CodegenCx as BackendTypes>::Value,
        mask: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("shuffle_vector");
    }

    fn vector_splat(
        &mut self,
        num_elts: usize,
        elt: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_splat");
    }

    fn vector_reduce_fadd_fast(
        &mut self,
        acc: <Self::CodegenCx as BackendTypes>::Value,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_fadd_fast");
    }

    fn vector_reduce_fmul_fast(
        &mut self,
        acc: <Self::CodegenCx as BackendTypes>::Value,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_fmul_fast");
    }

    fn vector_reduce_add(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_add");
    }

    fn vector_reduce_mul(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_mul");
    }

    fn vector_reduce_and(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_and");
    }

    fn vector_reduce_or(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_or");
    }

    fn vector_reduce_xor(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_xor");
    }

    fn vector_reduce_fmin(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_fmin");
    }

    fn vector_reduce_fmax(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_fmax");
    }

    fn vector_reduce_fmin_fast(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_fmin_fast");
    }

    fn vector_reduce_fmax_fast(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_fmax_fast");
    }

    fn vector_reduce_min(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value,
        is_signed: bool
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_min");
    }

    fn vector_reduce_max(
        &mut self,
        src: <Self::CodegenCx as BackendTypes>::Value,
        is_signed: bool
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("vector_reduce_max");
    }

    fn extract_value(
        &mut self,
        agg_val: <Self::CodegenCx as BackendTypes>::Value,
        idx: u64
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("extract_value");
    }

    fn insert_value(
        &mut self,
        agg_val: <Self::CodegenCx as BackendTypes>::Value,
        elt: <Self::CodegenCx as BackendTypes>::Value,
        idx: u64
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("insert_value");
    }

    fn landing_pad(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        pers_fn: <Self::CodegenCx as BackendTypes>::Value,
        num_clauses: usize
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("landing_pad");
    }

    fn add_clause(
        &mut self,
        landing_pad: <Self::CodegenCx as BackendTypes>::Value,
        clause: <Self::CodegenCx as BackendTypes>::Value
    ) {
        unimplemented!("add_clause");
    }

    fn set_cleanup(
        &mut self,
        landing_pad: <Self::CodegenCx as BackendTypes>::Value
    ) {
        unimplemented!("set_cleanup");
    }

    fn resume(
        &mut self,
        exn: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("resume");
    }

    fn cleanup_pad(
        &mut self,
        parent: Option<&'ll Value>,
        args: &[&'ll Value]
    ) {
        unimplemented!("cleanup_pad");
    }

    fn cleanup_ret(
        &mut self,
        cleanup: &<Self::CodegenCx as BackendTypes>::Funclet,
        unwind: Option<<Self::CodegenCx as BackendTypes>::BasicBlock>,
    ) -> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("cleanup_ret");
    }

    fn catch_pad(
        &mut self,
        parent: <Self::CodegenCx as BackendTypes>::Value,
        args: &[<Self::CodegenCx as BackendTypes>::Value]
    ) {
        unimplemented!("catch_pad");
    }

    fn catch_ret(
        &mut self,
        pad: &<Self::CodegenCx as BackendTypes>::Funclet,
        unwind: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) -> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("catch_ret");
    }

    fn catch_switch(
        &mut self,
        parent: Option<<Self::CodegenCx as BackendTypes>::Value>,
        unwind: Option<<Self::CodegenCx as BackendTypes>::BasicBlock>,
        num_handlers: usize,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("catch_switch");
    }

    fn add_handler(
        &mut self,
        catch_switch: <Self::CodegenCx as BackendTypes>::Value,
        handler: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) {
        unimplemented!("add_handler");
    }

    fn set_personality_fn(&mut self, personality: <Self::CodegenCx as BackendTypes>::Value) {
        unimplemented!("set_personality_fn");
    }

    fn atomic_cmpxchg(
        &mut self,
        dst: <Self::CodegenCx as BackendTypes>::Value,
        cmp: <Self::CodegenCx as BackendTypes>::Value,
        src: <Self::CodegenCx as BackendTypes>::Value,
        order: AtomicOrdering,
        failure_order: AtomicOrdering,
        weak: bool,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("atomic_cmpxchg");
    }

    fn atomic_rmw(
        &mut self,
        op: AtomicRmwBinOp,
        dst: <Self::CodegenCx as BackendTypes>::Value,
        src: <Self::CodegenCx as BackendTypes>::Value,
        order: AtomicOrdering,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("atomic_rmw");
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, scope: SynchronizationScope) {
        unimplemented!("atomic_fence");
    }

    fn add_case(
        &mut self,
        s: <Self::CodegenCx as BackendTypes>::Value,
        on_val: <Self::CodegenCx as BackendTypes>::Value,
        dest: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) {
        unimplemented!("add_case(");
    }

    fn add_incoming_to_phi(
        &mut self,
        phi: <Self::CodegenCx as BackendTypes>::Value,
        val: <Self::CodegenCx as BackendTypes>::Value,
        bb: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) {
        unimplemented!("add_incoming_to_phi");
    }

    fn set_invariant_load(&mut self, load: <Self::CodegenCx as BackendTypes>::Value) {
        unimplemented!("set_invariant_load");
    }

    /// Returns the ptr value that should be used for storing `val`.
    fn check_store(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        ptr: <Self::CodegenCx as BackendTypes>::Value
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("check_store");
    }

    /// Returns the args that should be used for a call to `llfn`.
    fn check_call<'b>(
        &mut self,
        typ: &str,
        llfn: <Self::CodegenCx as BackendTypes>::Value,
        args: &'b [<Self::CodegenCx as BackendTypes>::Value]
    ) -> Cow<'b, [<Self::CodegenCx as BackendTypes>::Value]>
        where [<Self::CodegenCx as BackendTypes>::Value] : ToOwned {
        unimplemented!("check_call");
    }

    fn lifetime_start(&mut self, ptr: <Self::CodegenCx as BackendTypes>::Value, size: Size) {
        unimplemented!("lifetime_start");
    }

    fn lifetime_end(&mut self, ptr: <Self::CodegenCx as BackendTypes>::Value, size: Size) {
        unimplemented!("lifetime_end");
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
        ptr: <Self::CodegenCx as BackendTypes>::Value, size: Size
    ) {
        unimplemented!("call_lifetime_intrinsic");
    }

    fn call(
        &mut self,
        llfn: <Self::CodegenCx as BackendTypes>::Value,
        args: &[<Self::CodegenCx as BackendTypes>::Value],
        funclet: Option<&Self::Funclet>,
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("call");
    }

    fn zext(
        &mut self,
        val: <Self::CodegenCx as BackendTypes>::Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> <Self::CodegenCx as BackendTypes>::Value {
        unimplemented!("zext");
    }

    unsafe fn delete_basic_block(&mut self, bb: <Self::CodegenCx as BackendTypes>::BasicBlock) {
        unimplemented!("delete_basic_block)");
    }

    fn do_not_inline(&mut self, llret: <Self::CodegenCx as BackendTypes>::Value) {
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

    fn load_operand(&mut self, place: PlaceRef<'tcx, Self::Value>)
        -> OperandRef<'tcx, Self::Value> {
        unimplemented!("load_operand");
    }
}
