use builder::Builder;
use context::CodegenCx;
use value::Value;
use ironox_type::Type;

use rustc::ty::{self, Ty, Instance};
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_target::abi::call::*;
use rustc_target::abi::LayoutOf;
use rustc_target::spec::abi::Abi;

impl AbiBuilderMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn apply_attrs_callsite(
        &mut self,
        ty: &FnType<'tcx, Ty<'tcx>>,
        callsite: <Self::CodegenCx as BackendTypes>::Value
    ) {
        // XXX Do nothing for now
        //ty.apply_attrs_callsite(self, callsite)
        //unimplemented!("apply_attrs_callsite");
    }
}

impl ArgTypeMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn store_fn_arg(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, Value>
    ) {
        unimplemented!("store_fn_arg");
    }

    fn store_arg_ty(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        val: Value,
        dst: PlaceRef<'tcx, Value>
    ) {
        unimplemented!("store_arg_ty");
    }

    fn memory_ty(&self, ty: &ArgType<'tcx, Ty<'tcx>>) -> &'ll Type {
        unimplemented!("memory_ty");
    }
}

impl AbiMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn new_fn_type(&self, sig: ty::FnSig<'tcx>, extra_args: &[Ty<'tcx>])
        -> FnType<'tcx, Ty<'tcx>> {
        use self::Abi::*;
        let conv = match self.tcx.sess.target.target.adjust_abi(sig.abi) {
            RustIntrinsic | PlatformIntrinsic |
            Rust | RustCall => Conv::C,
            // It's the ABI's job to select this, not ours.
            System => bug!("system abi should be selected elsewhere"),
            C => Conv::C,
            Unadjusted => Conv::C,
            // These API constants ought to be more specific...
            Cdecl => Conv::C,
            _ => unimplemented!("Unknown calling convention")
        };
        let cx = self;
        let mk_arg_type = |ty, _| {
            ArgType::new(cx.layout_of(ty))
        };

        let target = &cx.sess().target.target;
        let win_x64_gnu = target.target_os == "windows"
                       && target.arch == "x86_64"
                       && target.target_env == "gnu";
        let linux_s390x = target.target_os == "linux"
                       && target.arch == "s390x"
                       && target.target_env == "gnu";
        let rust_abi = match sig.abi {
            RustIntrinsic | PlatformIntrinsic | Rust | RustCall => true,
            _ => false
        };

        let arg_of = |ty: Ty<'tcx>, arg_idx: Option<usize>| {
            let is_return = arg_idx.is_none();
            let mut arg = mk_arg_type(ty, arg_idx);
            if arg.layout.is_zst() {
                // For some forsaken reason, x86_64-pc-windows-gnu
                // doesn't ignore zero-sized struct arguments.
                // The same is true for s390x-unknown-linux-gnu.
                if is_return || rust_abi || (!win_x64_gnu && !linux_s390x) {
                    arg.mode = PassMode::Ignore;
                }
            }

            arg
        };


        // return the FnType
        let mut fn_ty = FnType {
            ret: arg_of(sig.output(), None),
            // XXX
            args: sig.inputs().iter().chain(extra_args).enumerate().map(|(i, ty)| {
                arg_of(ty, Some(i))
            }).collect(),
            variadic: sig.variadic,
            conv
        };

        fn_ty
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

