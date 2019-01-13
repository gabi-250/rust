// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use context::CodegenCx;
use value::Value;

use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods};
use rustc_codegen_ssa::common::TypeKind;
use rustc::util::nodemap::FxHashMap;
use rustc::ty::{self, layout, Ty, TyCtxt};
use rustc::ty::layout::TyLayout;
use std::cell::RefCell;
use rustc_target::abi::LayoutOf;
use rustc_target::abi::call::{CastTarget, FnType, Reg};

/// The type of a scalar.
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ScalarType {
    /// A 1-bit integer value.
    I1,
    /// An 8-bit integer value.
    I8,
    /// A 32-bit integer value.
    I32,
    /// An integer value which has a target-dependent size.
    ISize,
    /// An integer value of a custom size.
    Ix(u64),
}

/// A `Type` is an index into the vector of types in the codegen context.
pub type Type = usize;

/// The actual types.
#[derive(PartialEq, Debug)]
pub enum LLType {
    /// A function type.
    FnType {
        args: Vec<Type>,
        ret: Type
    },
    /// A scalar type.
    Scalar(ScalarType),
    /// A placeholder for types. This will be removed later.
    None,
}

impl CodegenCx<'ll, 'tcx> {
    crate fn type_named_struct(&self, name: &str) -> &Type {
        unimplemented!("type_named_struct");
    }

    crate fn set_struct_body(&self, ty: &Type, els: &[&Type], packed: bool) {
        unimplemented!("set_struct_body");
    }
}

impl BaseTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn type_void(&self) -> Type {
        unimplemented!("type_void");
    }

    fn type_metadata(&self) -> Type {
        unimplemented!("type_metadata");
    }

    fn type_i1(&self) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::Scalar(ScalarType::I1));
        borrowed_types.len() - 1
    }

    fn type_i8(&self) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::Scalar(ScalarType::I8));
        borrowed_types.len() - 1
    }

    fn type_i16(&self) -> Type {
        unimplemented!("type_i16");
    }

    fn type_i32(&self) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::Scalar(ScalarType::I32));
        borrowed_types.len() - 1
    }

    fn type_i64(&self) -> Type {
        unimplemented!("type_i64");
    }

    fn type_i128(&self) -> Type {
        unimplemented!("type_i128");
    }

    fn type_ix(&self, num_bits: u64) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::Scalar(ScalarType::Ix(num_bits)));
        borrowed_types.len() - 1
    }

    fn type_isize(&self) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::Scalar(ScalarType::ISize));
        borrowed_types.len() - 1
    }

    fn type_f32(&self) -> Type {
        unimplemented!("type_f32");
    }

    fn type_f64(&self) -> Type {
        unimplemented!("type_f64");
    }

    fn type_x86_mmx(&self) -> Type {
        unimplemented!("type_x86_mmx");
    }

    fn type_func(
        &self,
        args: &[Type],
        ret: Type
    ) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        // define the types of the arguments of the function
        let mut ll_args = vec![];
        for arg in args {
            ll_args.push(arg.clone());
        }
        // return a FnType that can be used to declare a function
        let fn_type = LLType::FnType {
            args: ll_args,
            ret: ret.clone(),
        };
        borrowed_types.push(fn_type);
        borrowed_types.len() - 1
    }

    fn type_variadic_func(
        &self,
        args: &[Type],
        ret: Type
    ) -> Type {
        unimplemented!("type_variadic_func");
    }

    fn type_struct(
        &self,
        els: &[Type],
        packed: bool
    ) -> Type {
        unimplemented!("type_struct");
    }

    fn type_array(&self, ty: Type, len: u64) -> Type {
        unimplemented!("type_array");
    }

    fn type_vector(&self, ty: Type, len: u64) -> Type {
        unimplemented!("type_vector");
    }

    fn type_kind(&self, ty: Type) -> TypeKind {
        unimplemented!("type_kind");
    }

    fn type_ptr_to(&self, ty: Type) -> Type {
        unimplemented!("type_ptr_to");
    }

    fn element_type(&self, ty: Type) -> Type {
        unimplemented!("element_type");
    }

    fn vector_length(&self, ty: Type) -> usize {
        unimplemented!("vector_length");
    }

    fn func_params_types(&self, ty: Type) -> Vec<Type> {
        unimplemented!("func_params_types");
    }

    fn float_width(&self, ty : Type) -> usize {
        unimplemented!("float_width");
    }

    fn int_width(&self, ty: Type) -> u64 {
        unimplemented!("int_width");
    }

    fn val_ty(&self, v: Value) -> Type {
        // FIXME: return the real type
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::None);
        borrowed_types.len() - 1
    }

    fn scalar_lltypes(&self) -> &RefCell<FxHashMap<Ty<'tcx>, Self::Type>> {
        unimplemented!("scalar_lltypes");
    }
}

impl LayoutTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn backend_type(&self, ty: TyLayout<'tcx>) -> Type {
        let ironox_ty = match ty.ty.sty {
            ty::Ref(_, ty, _) |
            ty::RawPtr(ty::TypeAndMut { ty, .. }) => {
                self.type_ptr_to(self.backend_type(self.layout_of(ty)))
            }
            ty::Adt(def, _) if def.is_box() => {
                let mut borrowed_types = self.types.borrow_mut();
                // FIXME: return the real type
                borrowed_types.push(LLType::None);
                borrowed_types.len() - 1
            }
            ty::FnPtr(sig) => {
                let mut borrowed_types = self.types.borrow_mut();
                // FIXME: return the real type
                borrowed_types.push(LLType::None);
                borrowed_types.len() - 1
            }
            _ => {
                let mut borrowed_types = self.types.borrow_mut();
                // FIXME: return the real type
                borrowed_types.push(LLType::None);
                borrowed_types.len() - 1
            }
        };
        ironox_ty
    }

    fn immediate_backend_type(&self, ty: TyLayout<'tcx>) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        // FIXME: return the real type
        borrowed_types.push(LLType::None);
        borrowed_types.len() - 1
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
    ) -> Type {
        unimplemented!("scalar_pair_element_backend_type");
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Type {
        unimplemented!("cast_backend_type");
    }

    fn fn_backend_type(&self, ty: &FnType<'tcx, Ty<'tcx>>) -> Type {
        unimplemented!("fn_backend_type");
    }

    fn reg_backend_type(&self, ty: &Reg) -> Type {
        unimplemented!("reg_backend_type");
    }

    fn fn_ptr_backend_type(&self, ty: &FnType<'tcx, Ty<'tcx>>) -> Type {
        unimplemented!("fn_ptr_backend_type");
    }
}
