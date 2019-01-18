// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use builder::Builder;
use context::CodegenCx;
use value::Value;
use type_::{LLType, Type};
use type_of::LayoutIronOxExt;

use libc::c_uint;

use rustc::ty::{self, Ty, Instance};
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::mir::operand::OperandValue;
use rustc_target::abi::LayoutOf;

pub use rustc_target::spec::abi::Abi;
pub use rustc_target::abi::call::*;

impl AbiBuilderMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn apply_attrs_callsite(
        &mut self,
        ty: &FnType<'tcx, Ty<'tcx>>,
        callsite: Value
    ) {
        // FIXME?
    }
}

impl ArgTypeMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn store_fn_arg(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Value>
    ) {
        match ty.mode {
            PassMode::Ignore => {},
            PassMode::Pair(..) => {
                unimplemented!("PassMode::Pair");
            }
            PassMode::Indirect(_, Some(_)) => {
                unimplemented!("PassMode::Indirect");
            }
            PassMode::Direct(_) | PassMode::Indirect(_, None) | PassMode::Cast(_) => {
                if ty.is_ignore() {
                    return;
                }
                let val = self.cx.get_param(self.llfn(), *idx as c_uint);
                *idx += 1;
                if ty.is_sized_indirect() {
                    unimplemented!("sized indirect");
                } else if ty.is_unsized_indirect() {
                    bug!("unsized ArgType must be handled through store_fn_arg");
                } else if let PassMode::Cast(cast) = ty.mode {
                    unimplemented!("PassMode::Cast");
                } else {
                    OperandValue::Immediate(val).store(self, dst);
                }
            }
        }
    }

    fn store_arg_ty(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        val: Value,
        dst: PlaceRef<'tcx, Value>
    ) {
        unimplemented!("store_arg_ty");
    }

    fn memory_ty(&self, ty: &ArgType<'tcx, Ty<'tcx>>) -> Type {
        unimplemented!("memory_ty");
    }
}

pub trait FnTypeExt<'tcx> {
    fn new(cx: &CodegenCx<'_, 'tcx>, sig: ty::FnSig<'tcx>,
           extra_args: &[Ty<'tcx>]) -> Self;
    fn ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type;
    fn ptr_to_ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type;
}

impl FnTypeExt<'tcx> for FnType<'tcx, Ty<'tcx>> {
    fn new(
        cx: &CodegenCx<'_, 'tcx>,
        sig: ty::FnSig<'tcx>,
        extra_args: &[Ty<'tcx>]) -> Self {
        use self::Abi::*;

        let conv = match cx.tcx.sess.target.target.adjust_abi(sig.abi) {
            RustIntrinsic | PlatformIntrinsic |
            Rust | RustCall => Conv::C,
            System => bug!("system abi should be selected elsewhere"),
            C => Conv::C,
            Unadjusted => Conv::C,
            Cdecl => Conv::C,
            _ => unimplemented!("Unknown calling convention")
        };

        // Create an ArgType for a function argument of type `ty`.
        // `arg_idx` is None if `ty` is the return type, and Some(index),
        // if `ty` is the type of an argument.
        let arg_of = |ty: Ty<'tcx>, arg_idx: Option<usize>| {
            let is_return = arg_idx.is_none();
            // The ArgType of the specified ty.
            let mut arg = ArgType::new(cx.layout_of(ty));
            // Does the function adhere to the Rust ABI?
            let rust_abi = match sig.abi {
                RustIntrinsic | PlatformIntrinsic | Rust | RustCall => true,
                _ => false
            };
            // Is this a zero-sized type?
            if arg.layout.is_zst() {
                if is_return || rust_abi {
                    arg.mode = PassMode::Ignore;
                }
            }
            arg
        };
        // Return the FnType of this function.
        let mut fn_ty = FnType {
            ret: arg_of(sig.output(), None),
            args: sig.inputs().iter().chain(extra_args).enumerate().map(|(i, ty)| {
                arg_of(ty, Some(i))
            }).collect(),
            variadic: sig.variadic,
            conv
        };
        fn_ty
    }

    /// Return the IronOx `Type` of this `FnType`.
    fn ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type {
        // Create the return type.
        let ret_ty = match self.ret.mode {
            PassMode::Ignore => cx.type_void(),
            PassMode::Direct(_) => {
                self.ret.layout.immediate_ironox_type(cx)
            },
            mode => unimplemented!("{:?}", mode)
        };
        // Create the types of the arguments.
        let mut arg_tys = Vec::with_capacity(self.args.len());

        for arg in &self.args {
            let arg_ty = match arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct(_) => {
                    arg.layout.immediate_ironox_type(cx)
                },
                mode => unimplemented!("{:?}", mode),
            };
            arg_tys.push(arg_ty);
        }
        if self.variadic {
            cx.type_variadic_func(&arg_tys, ret_ty)
        } else {
            cx.type_func(&arg_tys, ret_ty)
        }
    }

    fn ptr_to_ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type {
        let fn_ty = self.ironox_type(cx);
        cx.add_type(LLType::PtrTo { pointee: fn_ty })
    }
}

impl AbiMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn new_fn_type(&self, sig: ty::FnSig<'tcx>, extra_args: &[Ty<'tcx>])
        -> FnType<'tcx, Ty<'tcx>> {
        FnType::new(&self, sig, extra_args)
    }

    fn new_vtable(
        &self,
        sig: ty::FnSig<'tcx>,
        extra_args: &[Ty<'tcx>]
    ) -> FnType<'tcx, Ty<'tcx>> {
        unimplemented!("new_vtable");
    }

    fn fn_type_of_instance(&self, instance: &Instance<'tcx>) -> FnType<'tcx, Ty<'tcx>> {
        unimplemented!("fn_type_of_instance");
    }
}

