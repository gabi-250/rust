use context::CodegenCx;
use value::Value;

use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods};
use rustc_codegen_ssa::common::TypeKind;
use rustc::util::nodemap::FxHashMap;
use rustc::ty::{layout, Ty};
use rustc::ty::layout::TyLayout;
use std::cell::RefCell;
use rustc_target::abi::call::{CastTarget, FnType, Reg};

#[derive(PartialEq, Debug)]
pub struct Type {
    size: u64
}

impl BaseTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn type_void(&self) -> &'ll Type {
        unimplemented!("type_void");
    }

    fn type_metadata(&self) -> &'ll Type {
        unimplemented!("type_metadata");
    }

    fn type_i1(&self) -> &'ll Type {
        unimplemented!("type_i1");
    }

    fn type_i8(&self) -> &'ll Type {
        unimplemented!("type_i8");
    }


    fn type_i16(&self) -> &'ll Type {
        unimplemented!("type_i16");
    }

    fn type_i32(&self) -> &'ll Type {
        unimplemented!("type_i32");
    }

    fn type_i64(&self) -> &'ll Type {
        unimplemented!("type_i64");
    }

    fn type_i128(&self) -> &'ll Type {
        unimplemented!("type_i128");
    }

    fn type_ix(&self, num_bits: u64) -> &'ll Type {
        unimplemented!("type_ix");
    }

    fn type_isize(&self) -> &'ll Type {
        unimplemented!("type_isize");
    }

    fn type_f32(&self) -> &'ll Type {
        unimplemented!("type_f32");
    }

    fn type_f64(&self) -> &'ll Type {
        unimplemented!("type_f64");
    }

    fn type_x86_mmx(&self) -> &'ll Type {
        unimplemented!("type_x86_mmx");
    }

    fn type_func(
        &self,
        args: &[&'ll Type],
        ret: &'ll Type
    ) -> &'ll Type {
        unimplemented!("type_func");
    }

    fn type_variadic_func(
        &self,
        args: &[&'ll Type],
        ret: &'ll Type
    ) -> &'ll Type {
        unimplemented!("type_variadic_func");
    }

    fn type_struct(
        &self,
        els: &[&'ll Type],
        packed: bool
    ) -> &'ll Type {
        unimplemented!("type_struct");
    }

    fn type_named_struct(&self, name: &str) -> &'ll Type {
        unimplemented!("type_named_struct");
    }

    fn type_array(&self, ty: &'ll Type, len: u64) -> &'ll Type {
        unimplemented!("type_array");
    }

    fn type_vector(&self, ty: &'ll Type, len: u64) -> &'ll Type {
        unimplemented!("type_vector");
    }

    fn type_kind(&self, ty: &'ll Type) -> TypeKind {
        unimplemented!("type_kind");
    }

    fn set_struct_body(&self, ty: &'ll Type, els: &[&'ll Type], packed: bool) {
        unimplemented!("set_struct_body");
    }

    fn type_ptr_to(&self, ty: &'ll Type) -> &'ll Type {
        unimplemented!("type_ptr_to");
    }

    fn element_type(&self, ty: &'ll Type) -> &'ll Type {
        unimplemented!("element_type");
    }

    fn vector_length(&self, ty: &'ll Type) -> usize {
        unimplemented!("vector_length");
    }

    fn func_params_types(&self, ty: &'ll Type) -> Vec<&'ll Type> {
        unimplemented!("func_params_types");
    }

    fn float_width(&self, ty : &'ll Type) -> usize {
        unimplemented!("float_width");
    }

    fn int_width(&self, ty: &'ll Type) -> u64 {
        unimplemented!("int_width");
    }

    fn val_ty(&self, v: &'ll Value) -> &'ll Type {
        unimplemented!("val_ty");
    }

    fn scalar_lltypes(&self) -> &RefCell<FxHashMap<Ty<'tcx>, Self::Type>> {
        unimplemented!("scalar_lltypes");
    }
}

impl LayoutTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn backend_type(&self, ty: TyLayout<'tcx>) -> &'ll Type {
        unimplemented!("backend_type");
    }
    fn immediate_backend_type(&self, ty: TyLayout<'tcx>) -> &'ll Type {
        unimplemented!("immediate_backend_type");
    }
    fn is_backend_immediate(&self, ty: TyLayout<'tcx>) -> bool {
        match ty.abi {
            layout::Abi::Scalar(_) |
            layout::Abi::Vector { .. } => true,
            layout::Abi::ScalarPair(..) => false,
            layout::Abi::Uninhabited |
            layout::Abi::Aggregate { .. } => ty.is_zst()
        }
    }
    fn is_backend_scalar_pair(&self, ty: TyLayout<'tcx>) -> bool {
        match ty.abi {
            layout::Abi::ScalarPair(..) => true,
            layout::Abi::Uninhabited |
            layout::Abi::Scalar(_) |
            layout::Abi::Vector { .. } |
            layout::Abi::Aggregate { .. } => false
        }
    }
    fn backend_field_index(&self, ty: TyLayout<'tcx>, index: usize) -> u64 {
        unimplemented!("backend_field_index");
    }
    fn scalar_pair_element_backend_type<'a>(
        &self,
        ty: TyLayout<'tcx>,
        index: usize,
        immediate: bool
    ) -> &'ll Type {
        unimplemented!("scalar_pair_element_backend_type");
    }
    fn cast_backend_type(&self, ty: &CastTarget) -> &'ll Type {
        unimplemented!("cast_backend_type");
    }
    fn fn_backend_type(&self, ty: &FnType<'tcx, Ty<'tcx>>) -> &'ll Type {
        unimplemented!("fn_backend_type");
    }
    fn reg_backend_type(&self, ty: &Reg) -> &'ll Type {
        unimplemented!("reg_backend_type");
    }
    fn fn_ptr_backend_type(&self, ty: &FnType<'tcx, Ty<'tcx>>) -> &'ll Type {
        unimplemented!("fn_ptr_backend_type");
    }
}
