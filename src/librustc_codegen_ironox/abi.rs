use builder::Builder;
use context::CodegenCx;
use value::Value;
use ironox_type::Type;

use rustc::ty::{self, Ty, Instance};
use rustc_codegen_ssa::traits::{AbiMethods, AbiBuilderMethods, ArgTypeMethods,
    BackendTypes};
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
        //ty.apply_attrs_callsite(self, callsite)
        unimplemented!("apply_attrs_callsite");
    }
}

impl ArgTypeMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn store_fn_arg(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        idx: &mut usize,
        dst: PlaceRef<'tcx, &'ll Value>
    ) {
        unimplemented!("store_fn_arg");
    }

    fn store_arg_ty(
        &mut self,
        ty: &ArgType<'tcx, Ty<'tcx>>,
        val: &'ll Value,
        dst: PlaceRef<'tcx, &'ll Value>
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
        // return the FnType
        //
        FnType {
            ret: ArgType::new(self.layout_of(sig.output())),
            args: vec![], //sig.inputs().iter().collect(),
            variadic: sig.variadic,
            conv
        }
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

