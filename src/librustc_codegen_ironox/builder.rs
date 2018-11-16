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
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{Align, Size, TyLayout};
use rustc_codegen_ssa::traits::*;
use std::borrow::Cow;
use std::ffi::CStr;
use std::ops::Range;
use syntax;

use basic_block::BasicBlock;
use ironox_type::Type;
use registers::GPR;

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
        llfn: Value,
        name: &'b str
    )-> Self {
        let bb = BasicBlock::new(cx, name, llfn);
        eprintln!("Create a new block in llfn {:?} named {}", llfn, name);
        eprintln!("Function count {}", cx.module.borrow().functions.len());
        let bx = Builder::with_cx(cx);
        let (llfn, bb) = match bb {
            Value::BasicBlock(f, b) => (f, b),
            _ => bug!("Invalid basic block or function")
        };

        cx.current_bb.replace(Some(Value::BasicBlock(llfn, bb)));
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

    fn llfn(&self) -> Value {
        // the parent of the current basic block
        let bb = self.cx.current_bb.borrow().expect("Invalid insertion point");
        let fn_index = match bb {
            Value::BasicBlock(fn_index, _) => fn_index,
            _ => bug!("expected basic block, found {:?}", bb)
        };
        Value::Function(fn_index)
    }

    fn llbb(&self) -> Value {
        self.cx.current_bb.borrow().expect("Invalid insertion point")
    }

    fn count_insn(&self, category: &str) {
        unimplemented!("count_insn");
    }


    fn set_value_name(&mut self, value: Value, name: &str) {
        eprintln!("rename value {:?} to {:?}", value, name);
    }

    fn position_at_end(&mut self, llbb: Value) {
        match llbb {
            Value::BasicBlock(f, bb) => self.cx.current_bb.replace(Some(llbb)),
            _ => bug!("Not a basic block!")
        };
    }

    fn position_at_start(&mut self, llbb: <Self::CodegenCx as BackendTypes>::BasicBlock) {
        unimplemented!("position_at_start");
    }

    fn ret_void(&mut self) {
        unimplemented!("ret_void(&mut self)");
    }

    fn ret(&mut self, v: Value) {
         eprintln!("ret {:?}", v);
    }

    fn br(&mut self, dest: <Self::CodegenCx as BackendTypes>::BasicBlock) {
         eprintln!("br to {:?}", dest);
    }

    fn cond_br(
        &mut self,
        cond: Value,
        then_llbb: <Self::CodegenCx as BackendTypes>::BasicBlock,
        else_llbb: <Self::CodegenCx as BackendTypes>::BasicBlock,
    ) {
        unimplemented!("");
    }

    fn switch(
        &mut self,
        v: Value,
        else_llbb: <Self::CodegenCx as BackendTypes>::BasicBlock,
        num_cases: usize,
    )-> Value {
        unimplemented!("");
    }

    fn invoke(
        &mut self,
        llfn: Value,
        args: &[Value],
        then: <Self::CodegenCx as BackendTypes>::BasicBlock,
        catch: <Self::CodegenCx as BackendTypes>::BasicBlock,
        funclet: Option<&Self::Funclet>,
    )-> Value {
        unimplemented!("");
    }

    fn unreachable(&mut self) {
        unimplemented!();
    }

    fn add(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fadd(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fadd_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn sub(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fsub(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fsub_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn mul(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fmul(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fmul_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn udiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn exactudiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn sdiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn exactsdiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fdiv(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fdiv_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn urem(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn srem(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn frem(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn frem_fast(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn shl(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn lshr(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn ashr(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn and(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn or(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn xor(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn neg(
        &mut self,
        v: Value
    )-> Value {
        unimplemented!("");
    }

    fn fneg(
        &mut self,
        v: Value
    )-> Value {
        unimplemented!("");
    }

    fn not(
        &mut self,
        v: Value
    )-> Value {
        unimplemented!("");
    }


    fn alloca(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        name: &str, align: Align
    )-> Value {
        let (cur_fn, cur_bb)  = match *self.cx.current_bb.borrow() {
            Some(Value::BasicBlock(f, bb)) => (f, bb),
            _ => bug!("Invalid insertion point")
        }
;
        let mut module = self.cx.module.borrow_mut();
        let local_idx = module.functions[cur_fn].alloca(ty, name, align);
        Value::Local(cur_fn, local_idx)
    }

    fn dynamic_alloca(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        name: &str, align: Align
    )-> Value {
        unimplemented!("");
    }

    fn array_alloca(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        len: Value,
        name: &str,
        align: Align
    )-> Value {
        unimplemented!("");
    }


    fn load(
        &mut self,
        ptr: Value,
        align: Align
    )-> Value {
        unimplemented!("");
    }

    fn volatile_load(
        &mut self,
        ptr: Value
    )-> Value {
        unimplemented!("");
    }

    fn atomic_load(
        &mut self,
        ptr: Value,
        order: AtomicOrdering, size: Size
    )-> Value {
        unimplemented!("");
    }

    fn range_metadata(
        &mut self,
        load: Value,
        range: Range<u128>
    ) {
        unimplemented!("");
    }

    fn nonnull_metadata(&mut self, load: Value) {
        unimplemented!("");
    }


    fn store(
        &mut self,
        val: Value,
        ptr: Value,
        align: Align
    )-> Value {
        unimplemented!("");
    }

    fn atomic_store(
        &mut self,
        val: Value,
        ptr: Value,
        order: AtomicOrdering,
        size: Size
    ) {
        unimplemented!("");
    }

    fn store_with_flags(
        &mut self,
        val: Value,
        ptr: Value,
        align: Align,
        flags: MemFlags,
    )-> Value {
        eprintln!("Store {:?} in {:?}", val, ptr);
        ptr
    }


    fn gep(
        &mut self,
        ptr: Value,
        indices: &[Value]
    )-> Value {
        unimplemented!("");
    }

    fn inbounds_gep(
        &mut self,
        ptr: Value,
        indices: &[Value]
    )-> Value {
        unimplemented!("");
    }

    fn struct_gep(
        &mut self,
        ptr: Value,
        idx: u64
    )-> Value {
        unimplemented!("");
    }


    fn trunc(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn sext(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn fptoui(
        &mut self,
        val: Value,
        dest_ty: &'ll Type
    )-> Value {
        unimplemented!("");
    }

    fn fptosi(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn uitofp(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn sitofp(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn fptrunc(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn fpext(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn ptrtoint(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn inttoptr(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn bitcast(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn intcast(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type, is_signed: bool
    )-> Value {
        unimplemented!("");
    }

    fn pointercast(
        &mut self,
        val: Value,
        dest_ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        // XXX nothing to do
        eprintln!("pointercast");
        val
    }


    fn icmp(
        &mut self,
        op: IntPredicate,
        lhs: Value, rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn fcmp(
        &mut self,
        op: RealPredicate,
        lhs: Value, rhs: Value
    )-> Value {
        unimplemented!("");
    }


    fn empty_phi(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type)-> Value {
        unimplemented!("");
    }

    fn phi(
        &mut self,
        ty: <Self::CodegenCx as BackendTypes>::Type,
        vals: &[Value],
        bbs: &[<Self::CodegenCx as BackendTypes>::BasicBlock]
    )-> Value {
        unimplemented!("");
    }

    fn inline_asm_call(
        &mut self,
        asm: &CStr,
        cons: &CStr,
        inputs: &[Value], output: &'ll Type,
        volatile: bool, alignstack: bool,
        dia: syntax::ast::AsmDialect) -> Option<Value> {
        unimplemented!("");
    }


    fn minnum(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn maxnum(
        &mut self,
        lhs: Value,
        rhs: Value
    )-> Value {
        unimplemented!("");
    }

    fn select(
        &mut self, cond: Value,
        then_val: Value,
        else_val: Value,
    )-> Value {
        unimplemented!("");
    }


    fn va_arg(
        &mut self,
        list: Value,
        ty: <Self::CodegenCx as BackendTypes>::Type
    )-> Value {
        unimplemented!("");
    }

    fn extract_element(&mut self,
        vec: Value,
        idx: Value
    )-> Value {
        unimplemented!("");
    }

    fn insert_element(
        &mut self, vec: Value,
        elt: Value,
        idx: Value,
    )-> Value {
        unimplemented!("");
    }

    fn shuffle_vector(
        &mut self,
        v1: Value,
        v2: Value,
        mask: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_splat(
        &mut self,
        num_elts: usize,
        elt: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_fadd_fast(
        &mut self,
        acc: Value,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_fmul_fast(
        &mut self,
        acc: Value,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_add(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_mul(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_and(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_or(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_xor(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_fmin(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_fmax(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_fmin_fast(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_fmax_fast(
        &mut self,
        src: Value
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_min(
        &mut self,
        src: Value,
        is_signed: bool
    )-> Value {
        unimplemented!("");
    }

    fn vector_reduce_max(
        &mut self,
        src: Value,
        is_signed: bool
    )-> Value {
        unimplemented!("");
    }

    fn extract_value(
        &mut self,
        agg_val: Value,
        idx: u64
    )-> Value {
        unimplemented!("");
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
        ty: <Self::CodegenCx as BackendTypes>::Type,
        pers_fn: Value,
        num_clauses: usize
    )-> Value {
        unimplemented!("");
    }

    fn add_clause(
        &mut self,
        landing_pad: Value,
        clause: Value
    ) {
        unimplemented!("");
    }

    fn set_cleanup(
        &mut self,
        landing_pad: Value
    ) {
        unimplemented!("");
    }

    fn resume(
        &mut self,
        exn: Value
    )-> Value {
        unimplemented!("");
    }

    fn cleanup_pad(
        &mut self,
        parent: Option<Value>,
        args: &[Value]
    ) {
        unimplemented!("");
    }

    fn cleanup_ret(
        &mut self,
        cleanup: &<Self::CodegenCx as BackendTypes>::Funclet,
        unwind: Option<<Self::CodegenCx as BackendTypes>::BasicBlock>,
    ) -> Value {
        unimplemented!("");
    }

    fn catch_pad(
        &mut self,
        parent: Value,
        args: &[Value]
    ) {
        unimplemented!("");
    }

    fn catch_ret(
        &mut self,
        pad: &<Self::CodegenCx as BackendTypes>::Funclet,
        unwind: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) -> Value {
        unimplemented!("");
    }

    fn catch_switch(
        &mut self,
        parent: Option<Value>,
        unwind: Option<<Self::CodegenCx as BackendTypes>::BasicBlock>,
        num_handlers: usize,
    )-> Value {
        unimplemented!("");
    }

    fn add_handler(
        &mut self,
        catch_switch: Value,
        handler: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) {
        unimplemented!("");
    }

    fn set_personality_fn(&mut self, personality: Value) {
        unimplemented!("");
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
        unimplemented!("");
    }

    fn atomic_fence(&mut self, order: AtomicOrdering, scope: SynchronizationScope) {
        unimplemented!("");
    }

    fn add_case(
        &mut self,
        s: Value,
        on_val: Value,
        dest: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) {
        unimplemented!("");
    }

    fn add_incoming_to_phi(
        &mut self,
        phi: Value,
        val: Value,
        bb: <Self::CodegenCx as BackendTypes>::BasicBlock
    ) {
        unimplemented!("");
    }

    fn set_invariant_load(&mut self, load: Value) {
        unimplemented!("");
    }


    /// Returns the ptr value that should be used for storing `val`.
    fn check_store(
        &mut self,
        val: Value,
        ptr: Value
    )-> Value {
        unimplemented!("");
    }


    /// Returns the args that should be used for a call to `llfn`.
    fn check_call<'b>(
        &mut self,
        typ: &str,
        llfn: Value,
        args: &'b [Value]
    ) -> Cow<'b, [Value]>
        where [Value] : ToOwned {
        unimplemented!("");
    }


    fn lifetime_start(&mut self, ptr: Value, size: Size) {
        // XXX nothing to do for now
    }

    fn lifetime_end(&mut self, ptr: Value, size: Size) {
        // XXX nothing to do for now
    }


    fn call_lifetime_intrinsic(
        &mut self,
        intrinsic: &str,
        ptr: Value, size: Size
    ) {
        unimplemented!("");
    }


    fn call(
        &mut self,
        llfn: Value,
        args: &[Value],
        funclet: Option<&Self::Funclet>,
    )-> Value {
        eprintln!("call to {:?} with args {:?}", llfn, args);
        let (cur_fn, cur_bb)  = match *self.cx.current_bb.borrow() {
            Some(Value::BasicBlock(f, bb)) => (f, bb),
            _ => bug!("Invalid insertion point")
        };
        let module = self.cx.module.borrow_mut();
        // XXX store the function return type in IronOxFunction
        Value::Register(GPR::RAX)
    }


    fn zext(
        &mut self,
        val: Value,
        dest_ty: &'ll Type
    )-> Value {
        eprintln!("zero extend {:?} as type {:?}", val, dest_ty);
        let (cur_fn, cur_bb)  = match *self.cx.current_bb.borrow() {
            Some(Value::BasicBlock(f, bb)) => (f, bb),
            _ => bug!("Invalid insertion point")
        };
        let mut module = self.cx.module.borrow_mut();
        let local_idx = module.functions[cur_fn].zext_local(val, dest_ty);
        Value::Local(cur_fn, local_idx)
    }


    unsafe fn delete_basic_block(&mut self, bb: <Self::CodegenCx as BackendTypes>::BasicBlock) {
        unimplemented!("delete_basic_block(&mut self, bb: <Self::CodegenCx as BackendTypes>::BasicBlock)");
    }

    fn do_not_inline(&mut self, llret: Value) {
        unimplemented!("do_not_inline(&mut self, llret: Value)");
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
        // XXX
        let imm = to_immediate(self, place.llval, place.layout);
        OperandRef {
            val: OperandValue::Immediate(imm),
            layout: place.layout,
        }
    }
}
