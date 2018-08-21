// Copyright 2012-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(non_camel_case_types, non_snake_case)]

//! Code that is useful in various codegen modules.

use llvm::{self, TypeKind};
use llvm::{True, False, Bool};
use rustc::hir::def_id::DefId;
use rustc::middle::lang_items::LangItem;
use abi;
use base;
use builder::Builder;
use consts;
use declare;
use type_::Type;
use type_of::LayoutLlvmExt;
use value::{Value, ValueTrait};

use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{HasDataLayout, LayoutOf};
use rustc::hir;
use traits::{BuilderMethods, OperandBundleDef};

use libc::{c_uint, c_char};

use syntax::symbol::LocalInternedString;
use syntax_pos::{Span, DUMMY_SP};

pub use context::CodegenCx;

pub fn type_needs_drop<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, ty: Ty<'tcx>) -> bool {
    ty.needs_drop(tcx, ty::ParamEnv::reveal_all())
}

pub fn type_is_sized<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, ty: Ty<'tcx>) -> bool {
    ty.is_sized(tcx.at(DUMMY_SP), ty::ParamEnv::reveal_all())
}

pub fn type_is_freeze<'a, 'tcx>(tcx: TyCtxt<'a, 'tcx, 'tcx>, ty: Ty<'tcx>) -> bool {
    ty.is_freeze(tcx, ty::ParamEnv::reveal_all(), DUMMY_SP)
}

/*
* A note on nomenclature of linking: "extern", "foreign", and "upcall".
*
* An "extern" is an LLVM symbol we wind up emitting an undefined external
* reference to. This means "we don't have the thing in this compilation unit,
* please make sure you link it in at runtime". This could be a reference to
* C code found in a C library, or rust code found in a rust crate.
*
* Most "externs" are implicitly declared (automatically) as a result of a
* user declaring an extern _module_ dependency; this causes the rust driver
* to locate an extern crate, scan its compilation metadata, and emit extern
* declarations for any symbols used by the declaring crate.
*
* A "foreign" is an extern that references C (or other non-rust ABI) code.
* There is no metadata to scan for extern references so in these cases either
* a header-digester like bindgen, or manual function prototypes, have to
* serve as declarators. So these are usually given explicitly as prototype
* declarations, in rust code, with ABI attributes on them noting which ABI to
* link via.
*
* An "upcall" is a foreign call generated by the compiler (not corresponding
* to any user-written call in the code) into the runtime library, to perform
* some helper task such as bringing a task to life, allocating memory, etc.
*
*/

/// A structure representing an active landing pad for the duration of a basic
/// block.
///
/// Each `Block` may contain an instance of this, indicating whether the block
/// is part of a landing pad or not. This is used to make decision about whether
/// to emit `invoke` instructions (e.g. in a landing pad we don't continue to
/// use `invoke`) and also about various function call metadata.
///
/// For GNU exceptions (`landingpad` + `resume` instructions) this structure is
/// just a bunch of `None` instances (not too interesting), but for MSVC
/// exceptions (`cleanuppad` + `cleanupret` instructions) this contains data.
/// When inside of a landing pad, each function call in LLVM IR needs to be
/// annotated with which landing pad it's a part of. This is accomplished via
/// the `OperandBundleDef` value created for MSVC landing pads.
pub struct Funclet<'ll> {
    cleanuppad: &'ll Value,
    operand: OperandBundleDef<'ll, &'ll Value>,
}

impl Funclet<'ll> {
    pub fn new(cleanuppad: &'ll Value) -> Self {
        Funclet {
            cleanuppad,
            operand: OperandBundleDef::new("funclet", cleanuppad),
        }
    }

    pub fn cleanuppad(&self) -> &'ll Value {
        self.cleanuppad
    }

    pub fn bundle(&self) -> &OperandBundleDef<'ll, &'ll Value> {
        &self.operand
    }
}

pub fn val_ty<Value : ?Sized>(v: &'ll Value) -> &'ll Type where Value : ValueTrait {
    unsafe {
        llvm::LLVMTypeOf(v.to_llvm())
    }
}

// LLVM constant constructors.
pub fn C_null(t: &'ll Type) -> &'ll Value {
    unsafe {
        llvm::LLVMConstNull(t)
    }
}

pub fn C_undef<Value : ?Sized>(t: &'ll Type) -> &'ll Value where Value : ValueTrait {
    unsafe {
        Value::of_llvm(llvm::LLVMGetUndef(t))
    }
}

pub fn C_int<Value : ?Sized>(t: &'ll Type, i: i64) -> &'ll Value where Value : ValueTrait {
    unsafe {
        Value::of_llvm(llvm::LLVMConstInt(t, i as u64, True))
    }
}

pub fn C_uint<Value : ?Sized>(t: &'ll Type, i: u64) -> &'ll Value where Value : ValueTrait {
    unsafe {
        Value::of_llvm(llvm::LLVMConstInt(t, i, False))
    }
}

pub fn C_uint_big(t: &'ll Type, u: u128) -> &'ll Value {
    unsafe {
        let words = [u as u64, (u >> 64) as u64];
        llvm::LLVMConstIntOfArbitraryPrecision(t, 2, words.as_ptr())
    }
}

pub fn C_bool<Value : ?Sized>(
    cx: &CodegenCx<'ll, '_, &'ll Value>,
    val: bool
) -> &'ll Value where Value : ValueTrait {
    C_uint(Type::i1(cx), val as u64)
}

pub fn C_i32<Value : ?Sized>(
    cx: &CodegenCx<'ll, '_, &'ll Value>,
    i: i32
) -> &'ll Value where Value : ValueTrait {
    C_int(Type::i32(cx), i as i64)
}

pub fn C_u32(cx: &CodegenCx<'ll, '_, &'ll Value>, i: u32) -> &'ll Value {
    C_uint(Type::i32(cx), i as u64)
}

pub fn C_u64(cx: &CodegenCx<'ll, '_, &'ll Value>, i: u64) -> &'ll Value {
    C_uint(Type::i64(cx), i)
}

pub fn C_usize<Value : ?Sized>(
    cx: &CodegenCx<'ll, '_, &'ll Value>,
    i: u64
) -> &'ll Value where Value : ValueTrait {
    let bit_size = cx.data_layout().pointer_size.bits();
    if bit_size < 64 {
        // make sure it doesn't overflow
        assert!(i < (1<<bit_size));
    }

    C_uint(cx.isize_ty, i)
}

pub fn C_u8(cx: &CodegenCx<'ll, '_, &'ll Value>, i: u8) -> &'ll Value {
    C_uint(Type::i8(cx), i as u64)
}


// This is a 'c-like' raw string, which differs from
// our boxed-and-length-annotated strings.
pub fn C_cstr(
    cx: &CodegenCx<'ll, '_, &'ll Value>,
    s: LocalInternedString,
    null_terminated: bool,
) -> &'ll Value {
    unsafe {
        if let Some(&llval) = cx.const_cstr_cache.borrow().get(&s) {
            return llval;
        }

        let sc = llvm::LLVMConstStringInContext(cx.llcx,
                                                s.as_ptr() as *const c_char,
                                                s.len() as c_uint,
                                                !null_terminated as Bool);
        let sym = cx.generate_local_symbol_name("str");
        let g = declare::define_global(cx, &sym[..], val_ty(sc)).unwrap_or_else(||{
            bug!("symbol `{}` is already defined", sym);
        });
        llvm::LLVMSetInitializer(g, sc);
        llvm::LLVMSetGlobalConstant(g, True);
        llvm::LLVMRustSetLinkage(g, llvm::Linkage::InternalLinkage);

        cx.const_cstr_cache.borrow_mut().insert(s, g);
        g
    }
}

// NB: Do not use `do_spill_noroot` to make this into a constant string, or
// you will be kicked off fast isel. See issue #4352 for an example of this.
pub fn C_str_slice(cx: &CodegenCx<'ll, '_, &'ll Value>, s: LocalInternedString) -> &'ll Value {
    let len = s.len();
    let cs = consts::ptrcast(C_cstr(cx, s, false),
        cx.layout_of(cx.tcx.mk_str()).llvm_type(cx).ptr_to());
    C_fat_ptr(cx, cs, C_usize(cx, len as u64))
}

pub fn C_fat_ptr(
    cx: &CodegenCx<'ll, '_, &'ll Value>,
    ptr: &'ll Value,
    meta: &'ll Value
) -> &'ll Value {
    assert_eq!(abi::FAT_PTR_ADDR, 0);
    assert_eq!(abi::FAT_PTR_EXTRA, 1);
    C_struct(cx, &[ptr, meta], false)
}

pub fn C_struct(
    cx: &CodegenCx<'ll, '_, &'ll Value>,
    elts: &[&'ll Value],
    packed: bool
) -> &'ll Value {
    C_struct_in_context(cx.llcx, elts, packed)
}

pub fn C_struct_in_context(
    llcx: &'ll llvm::Context,
    elts: &[&'ll Value],
    packed: bool,
) -> &'ll Value {
    unsafe {
        llvm::LLVMConstStructInContext(llcx,
                                       elts.as_ptr(), elts.len() as c_uint,
                                       packed as Bool)
    }
}

pub fn C_array(ty: &'ll Type, elts: &[&'ll Value]) -> &'ll Value {
    unsafe {
        return llvm::LLVMConstArray(ty, elts.as_ptr(), elts.len() as c_uint);
    }
}

pub fn C_vector(elts: &[&'ll Value]) -> &'ll Value {
    unsafe {
        return llvm::LLVMConstVector(elts.as_ptr(), elts.len() as c_uint);
    }
}

pub fn C_bytes(cx: &CodegenCx<'ll, '_, &'ll Value>, bytes: &[u8]) -> &'ll Value {
    C_bytes_in_context(cx.llcx, bytes)
}

pub fn C_bytes_in_context(llcx: &'ll llvm::Context, bytes: &[u8]) -> &'ll Value {
    unsafe {
        let ptr = bytes.as_ptr() as *const c_char;
        return llvm::LLVMConstStringInContext(llcx, ptr, bytes.len() as c_uint, True);
    }
}

pub fn const_get_elt(v: &'ll Value, idx: u64) -> &'ll Value {
    unsafe {
        assert_eq!(idx as c_uint as u64, idx);
        let us = &[idx as c_uint];
        let r = llvm::LLVMConstExtractValue(v, us.as_ptr(), us.len() as c_uint);

        debug!("const_get_elt(v={:?}, idx={}, r={:?})",
               v, idx, r);

        r
    }
}

pub fn const_get_real(v: &'ll Value) -> Option<(f64, bool)> {
    unsafe {
        if is_const_real(v) {
            let mut loses_info: llvm::Bool = ::std::mem::uninitialized();
            let r = llvm::LLVMConstRealGetDouble(v, &mut loses_info);
            let loses_info = if loses_info == 1 { true } else { false };
            Some((r, loses_info))
        } else {
            None
        }
    }
}

pub fn const_to_uint(v: &'ll Value) -> u64 {
    unsafe {
        llvm::LLVMConstIntGetZExtValue(v)
    }
}

pub fn is_const_integral(v: &'ll Value) -> bool {
    unsafe {
        llvm::LLVMIsAConstantInt(v).is_some()
    }
}

pub fn is_const_real(v: &'ll Value) -> bool {
    unsafe {
        llvm::LLVMIsAConstantFP(v).is_some()
    }
}


#[inline]
fn hi_lo_to_u128(lo: u64, hi: u64) -> u128 {
    ((hi as u128) << 64) | (lo as u128)
}

pub fn const_to_opt_u128(v: &'ll Value, sign_ext: bool) -> Option<u128> {
    unsafe {
        if is_const_integral(v) {
            let (mut lo, mut hi) = (0u64, 0u64);
            let success = llvm::LLVMRustConstInt128Get(v, sign_ext,
                                                       &mut hi, &mut lo);
            if success {
                Some(hi_lo_to_u128(lo, hi))
            } else {
                None
            }
        } else {
            None
        }
    }
}

pub fn langcall(tcx: TyCtxt,
                span: Option<Span>,
                msg: &str,
                li: LangItem)
                -> DefId {
    tcx.lang_items().require(li).unwrap_or_else(|s| {
        let msg = format!("{} {}", msg, s);
        match span {
            Some(span) => tcx.sess.span_fatal(span, &msg[..]),
            None => tcx.sess.fatal(&msg[..]),
        }
    })
}

// To avoid UB from LLVM, these two functions mask RHS with an
// appropriate mask unconditionally (i.e. the fallback behavior for
// all shifts). For 32- and 64-bit types, this matches the semantics
// of Java. (See related discussion on #1877 and #10183.)

pub fn build_unchecked_lshift(
    bx: &Builder<'a, 'll, 'tcx, &'ll Value>,
    lhs: &'ll Value,
    rhs: &'ll Value
) -> &'ll Value {
    let rhs = base::cast_shift_expr_rhs(bx, hir::BinOpKind::Shl, lhs, rhs);
    // #1877, #10183: Ensure that input is always valid
    let rhs = shift_mask_rhs(bx, rhs);
    bx.shl(lhs, rhs)
}

pub fn build_unchecked_rshift(
    bx: &Builder<'a, 'll, 'tcx, &'ll Value>, lhs_t: Ty<'tcx>, lhs: &'ll Value, rhs: &'ll Value
) -> &'ll Value {
    let rhs = base::cast_shift_expr_rhs(bx, hir::BinOpKind::Shr, lhs, rhs);
    // #1877, #10183: Ensure that input is always valid
    let rhs = shift_mask_rhs(bx, rhs);
    let is_signed = lhs_t.is_signed();
    if is_signed {
        bx.ashr(lhs, rhs)
    } else {
        bx.lshr(lhs, rhs)
    }
}

fn shift_mask_rhs(bx: &Builder<'a, 'll, 'tcx, &'ll Value>, rhs: &'ll Value) -> &'ll Value {
    let rhs_llty = val_ty(rhs);
    bx.and(rhs, shift_mask_val(bx, rhs_llty, rhs_llty, false))
}

pub fn shift_mask_val(
    bx: &Builder<'a, 'll, 'tcx, &'ll Value>,
    llty: &'ll Type,
    mask_llty: &'ll Type,
    invert: bool
) -> &'ll Value {
    let kind = llty.kind();
    match kind {
        TypeKind::Integer => {
            // i8/u8 can shift by at most 7, i16/u16 by at most 15, etc.
            let val = llty.int_width() - 1;
            if invert {
                C_int(mask_llty, !val as i64)
            } else {
                C_uint(mask_llty, val)
            }
        },
        TypeKind::Vector => {
            let mask = shift_mask_val(bx, llty.element_type(), mask_llty.element_type(), invert);
            bx.vector_splat(mask_llty.vector_length(), mask)
        },
        _ => bug!("shift_mask_val: expected Integer or Vector, found {:?}", kind),
    }
}
