use builder::Builder;
use context::CodegenCx;
use ir::value::Value;
use ir::type_::{OxType, Type};
use type_of::LayoutIronOxExt;

use libc::c_uint;

use rustc::ty::{self, Ty, Instance};
use rustc::ty::layout;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::mir::operand::OperandValue;
use rustc_target::abi::LayoutOf;
use rustc_target::spec::HasTargetSpec;

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

fn store(
    ty: &ArgType<'tcx, Ty<'tcx>>,
    bx: &mut Builder<'_, 'll, 'tcx>,
    val: Value,
    dst: PlaceRef<'tcx, Value>) {
    if ty.is_ignore() {
        return;
    }
    if ty.is_sized_indirect() {
        unimplemented!("sized indirect");
    } else if ty.is_unsized_indirect() {
        bug!("unsized ArgType must be handled through store_fn_arg");
    } else if let PassMode::Cast(cast) = ty.mode {
        unimplemented!("PassMode::Cast");
    } else {
        OperandValue::Immediate(val).store(bx, dst);
    }
}

// FIXME: copied from LLVM:
pub trait IronOxType {
    fn ironox_type(&self, cx: &CodegenCx<'ll, '_>) -> Type;
}

impl IronOxType for Reg {
    fn ironox_type(&self, cx: &CodegenCx<'ll, '_>) -> Type {
        match self.kind {
            RegKind::Integer => cx.type_ix(self.size.bits()),
            RegKind::Float => {
                match self.size.bits() {
                    32 => cx.type_f32(),
                    64 => cx.type_f64(),
                    _ => bug!("unsupported float: {:?}", self)
                }
            }
            RegKind::Vector => {
                cx.type_vector(cx.type_i8(), self.size.bytes())
            }
        }
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
                let arg1 = self.get_param(self.llfn(), *idx as u32);
                *idx += 1;
                let arg2 = self.get_param(self.llfn(), *idx as u32);
                *idx += 1;
                OperandValue::Pair(arg1, arg2).store(self, dst);
            },
            PassMode::Indirect(_, Some(_)) => unimplemented!("PassMode::Indirect"),
            PassMode::Direct(_) | PassMode::Indirect(_, None) | PassMode::Cast(_) => {
                let val = self.get_param(self.llfn(), *idx as u32);
                *idx += 1;
                store(ty, self, val, dst);
            }
        }
    }

    fn store_arg_ty(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        val: Value,
        dst: PlaceRef<'tcx, Value>
    ) {
        store(ty, self, val, dst)
    }

    fn memory_ty(&self, ty: &ArgType<'tcx, Ty<'tcx>>) -> Type {
        ty.layout.ironox_type(&self.cx)
    }
}

pub trait FnTypeExt<'tcx> {
    fn of_instance(cx: &CodegenCx<'ll, 'tcx>, instance: &ty::Instance<'tcx>) -> Self;
    fn new(cx: &CodegenCx<'_, 'tcx>, sig: ty::FnSig<'tcx>,
           extra_args: &[Ty<'tcx>]) -> Self;
    /// Return the IronOx `Type` that is equivalent to this type.
    fn ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type;
    /// Return the IronOx `Type` that is equivalent to this pointer type.
    fn ptr_to_ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type;
    fn adjust_for_abi(&mut self, cx: &CodegenCx<'ll, 'tcx>, abi: Abi);
}

impl FnTypeExt<'tcx> for FnType<'tcx, Ty<'tcx>> {
    fn of_instance(cx: &CodegenCx<'ll, 'tcx>, instance: &ty::Instance<'tcx>) -> Self {
        let sig = instance.fn_sig(cx.tcx);
        let sig = cx.tcx.normalize_erasing_late_bound_regions(ty::ParamEnv::reveal_all(), &sig);
        FnType::new(cx, sig, &[])
    }

    fn new(
        cx: &CodegenCx<'_, 'tcx>,
        sig: ty::FnSig<'tcx>,
        extra_args: &[Ty<'tcx>]) -> Self {
        use self::Abi::*;
        let conv = match cx.tcx.sess.target.target.adjust_abi(sig.abi) {
            RustIntrinsic |
            Rust | RustCall => Conv::C,
            System => bug!("system abi should be selected elsewhere"),
            C => Conv::C,
            Cdecl => Conv::C,
            conv => unimplemented!("unsupported calling convention: {:?}", conv)
        };

        let mut inputs = sig.inputs();
        let extra_args = if sig.abi == RustCall {
            assert!(!sig.variadic && extra_args.is_empty());
            match sig.inputs().last().unwrap().sty {
                ty::Tuple(ref tupled_arguments) => {
                    inputs = &sig.inputs()[0..sig.inputs().len() - 1];
                    tupled_arguments
                }
                _ => {
                    bug!("argument to function with \"rust-call\" ABI \
                          is not a tuple");
                }
            }
        } else {
            assert!(sig.variadic || extra_args.is_empty());
            extra_args
        };

        // Create an ArgType for a function argument of type `ty`.
        // `arg_idx` is None if `ty` is the return type, and Some(index),
        // if `ty` is the type of an argument.
        let arg_of = |ty: Ty<'tcx>, arg_idx: Option<usize>| {
            let is_return = arg_idx.is_none();
            // The ArgType of the specified ty.
            let mut arg = ArgType::new(cx.layout_of(ty));
            if let layout::Abi::ScalarPair(ref a, ref b) = arg.layout.abi {
                let mut a_attrs = ArgAttributes::new();
                let mut b_attrs = ArgAttributes::new();
                arg.mode = PassMode::Pair(a_attrs, b_attrs);
            }
            // Does the function adhere to the Rust ABI?
            let rust_abi = match sig.abi {
                RustIntrinsic | Rust | RustCall => true,
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
        // The arguments/return type of the function may not fit in registers,
        // so adjust the way they are passed:
        fn_ty.adjust_for_abi(cx, sig.abi);
        fn_ty
    }

    fn adjust_for_abi(&mut self, cx: &CodegenCx<'ll, 'tcx>, abi: Abi) {
        if abi == Abi::Rust || abi == Abi::RustCall ||
           abi == Abi::RustIntrinsic || abi == Abi::PlatformIntrinsic {
            let fixup = |arg: &mut ArgType<'tcx, Ty<'tcx>>| {
                if arg.is_ignore() { return; }

                match arg.layout.abi {
                    layout::Abi::Aggregate { .. } => {}

                    // This is a fun case! The gist of what this is doing is
                    // that we want callers and callees to always agree on the
                    // ABI of how they pass SIMD arguments. If we were to *not*
                    // make these arguments indirect then they'd be immediates
                    // in LLVM, which means that they'd used whatever the
                    // appropriate ABI is for the callee and the caller. That
                    // means, for example, if the caller doesn't have AVX
                    // enabled but the callee does, then passing an AVX argument
                    // across this boundary would cause corrupt data to show up.
                    //
                    // This problem is fixed by unconditionally passing SIMD
                    // arguments through memory between callers and callees
                    // which should get them all to agree on ABI regardless of
                    // target feature sets. Some more information about this
                    // issue can be found in #44367.
                    //
                    // Note that the platform intrinsic ABI is exempt here as
                    // that's how we connect up to LLVM and it's unstable
                    // anyway, we control all calls to it in libstd.
                    layout::Abi::Vector { .. }
                        if cx.sess().target.target.options.simd_types_indirect =>
                    {
                        unimplemented!("vector layout for arg {:?}", arg);
                        //arg.make_indirect();
                        return
                    }

                    _ => return
                }
                let size = arg.layout.size;
                if arg.layout.is_unsized() || size > layout::Pointer.size(cx) {
                    arg.make_indirect();
                } else {

                    // We want to pass small aggregates as immediates, but using
                    // a LLVM aggregate type for this leads to bad optimizations,
                    // so we pick an appropriately sized integer type instead.
                    arg.cast_to(Reg {
                        kind: RegKind::Integer,
                        size
                    });
                }
            };
            fixup(&mut self.ret);
            for arg in &mut self.args {
                fixup(arg);
            }
            return;
        }
        if let Err(msg) = self.adjust_for_cabi(cx, abi) {
            cx.sess().fatal(&msg);
        }
    }

    /// Return the IronOx `Type` of this `FnType`.
    fn ironox_type(&self, cx: &CodegenCx<'a, 'tcx>) -> Type {
        let return_val_as_param = {
            if let PassMode::Indirect(..) = self.ret.mode { 1 } else { 0 }
        };
        let args_capacity: usize = self.args.iter().map(|arg|
            //if arg.pad.is_some() { 1 } else { 0 } +
            if let PassMode::Pair(_, _) = arg.mode { 2 } else { 1 }
        ).sum();
        let mut arg_tys = Vec::with_capacity(return_val_as_param + args_capacity);
        // Create the return type.
        let ret_ty = match self.ret.mode {
            PassMode::Ignore => cx.type_void(),
            PassMode::Direct(_) | PassMode::Pair(..) => {
                self.ret.layout.immediate_ironox_type(cx)
            },
            PassMode::Indirect(..) => {
                // The first argument acts as the return value:
                arg_tys.push(cx.type_ptr_to(self.ret.layout.ironox_type(cx)));
                // The function's return type is now void, because the 'return
                // value' is actually one of its arguments, which is a pointer
                // it modifies
                cx.type_void()
            },
            _ => unimplemented!("mode: {:?}", self.ret.mode),
        };

        let mut idx = 0;
        for arg in &self.args {
            idx += 1;
            let arg_ty = match arg.mode {
                PassMode::Ignore => continue,
                PassMode::Direct(_) => {
                    arg.layout.immediate_ironox_type(cx)
                },
                PassMode::Pair(..) => {
                    arg_tys.push(arg.layout.scalar_pair_element_ironox_type(cx, 0, true));
                    arg_tys.push(arg.layout.scalar_pair_element_ironox_type(cx, 1, true));
                    continue;
                }
                PassMode::Indirect(_, None) => cx.type_ptr_to(arg.layout.ironox_type(cx)),
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
        cx.add_type(OxType::PtrTo { pointee: fn_ty })
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
        FnType::of_instance(&self, instance)
    }
}

