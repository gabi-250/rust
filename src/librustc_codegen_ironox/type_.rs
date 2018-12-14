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
use type_of::LayoutIronOxExt;
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
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LLType {
    /// A function type.
    FnType {
        args: Vec<Type>,
        ret: Type
    },
    Array {
        length: u64,
    },
    PtrTo {
        pointee: Type,
    },
    Scalar(ScalarType),
    StructType {
        name: Option<String>,
        members: Vec<Type>
    },
    Void,
}

impl CodegenCx<'ll, 'tcx> {
    /// Add the specified `LLType` to the vector of `Type`s for this context.
    ///
    /// If the type already exists in the type cache, its index in the type
    /// vector is returned. Otherwise, it is added to the type vector, and to
    /// the type cache.
    fn add_type(&self, ll_type: LLType) -> Type {
        let mut types = self.types.borrow_mut();
        // If ll_type is not in types, it will be inserted at the end of the
        // types vector.
        let ty_idx = types.len();
        // FIXME: don't always clone ll_type.
        match self.type_cache.borrow_mut().insert(ll_type.clone(), ty_idx) {
            Some(ty) => ty,
            None => {
                types.push(ll_type);
                ty_idx
            }
        }
    }

    crate fn type_named_struct(&self, name: &str) -> Type {
        let struct_type = LLType::StructType {
            name: Some(name.to_string()),
            members: vec![],
        };
        self.add_type(struct_type)
    }

    crate fn set_struct_body(&self, ty: Type, els: &[Type], packed: bool) {
        let mut types = self.types.borrow_mut();
        if let LLType::StructType{ ref name, ref mut members } = types[ty] {
            *members = els.to_vec();
            return;
        }
        bug!("expected LLType::StructType, found {:?}", types[ty]);
    }
}

pub trait IxLlcx {
    fn ix_llcx(cx: &CodegenCx, num_bits: u64) -> Type;
}

impl IxLlcx for Type {
    fn ix_llcx(cx: &CodegenCx, num_bits: u64) -> Type {
        cx.add_type(LLType::Scalar(ScalarType::Ix(num_bits)))
    }
}

impl BaseTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn type_void(&self) -> Type {
        self.add_type(LLType::Void)
    }

    fn type_metadata(&self) -> Type {
        unimplemented!("type_metadata");
    }

    fn type_i1(&self) -> Type {
        self.add_type(LLType::Scalar(ScalarType::I1))
    }

    fn type_i8(&self) -> Type {
        self.add_type(LLType::Scalar(ScalarType::I8))
    }

    fn type_i16(&self) -> Type {
        unimplemented!("type_i16");
    }

    fn type_i32(&self) -> Type {
        self.add_type(LLType::Scalar(ScalarType::I32))
    }

    fn type_i64(&self) -> Type {
        unimplemented!("type_i64");
    }

    fn type_i128(&self) -> Type {
        unimplemented!("type_i128");
    }

    fn type_ix(&self, num_bits: u64) -> Type {
        Type::ix_llcx(self, num_bits)
    }

    fn type_isize(&self) -> Type {
        self.add_type(LLType::Scalar(ScalarType::ISize))
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
        self.add_type(fn_type)
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
        let mut members = els.to_vec();
        self.add_type(LLType::StructType {
            name: None,
            members,
        })
    }

    fn type_array(&self, ty: Type, len: u64) -> Type {
        self.add_type(LLType::Array { length: len })
    }

    fn type_vector(&self, ty: Type, len: u64) -> Type {
        unimplemented!("type_vector");
    }

    fn type_kind(&self, ty: Type) -> TypeKind {
        unimplemented!("type_kind");
    }

    fn type_ptr_to(&self, ty: Type) -> Type {
        self.add_type(LLType::PtrTo { pointee: ty })
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
        let mut borrowed_types = self.types.borrow_mut();
        match v {
            Value::Local(fn_index, local_index) => {
                self.module.borrow().functions[fn_index].local_ty(local_index)
            },
            Value::ConstUint(const_idx) => {
                self.u_consts.borrow()[const_idx].ty
            },
            Value::ConstInt(const_idx) => {
                self.i_consts.borrow()[const_idx].ty
            },
            x => {
                unimplemented!("type of {:?}", x);
            }
        }
    }

    fn scalar_lltypes(&self) -> &RefCell<FxHashMap<Ty<'tcx>, Self::Type>> {
        unimplemented!("scalar_lltypes");
    }
}

impl LayoutTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn backend_type(&self, ty: TyLayout<'tcx>) -> Type {
        ty.ironox_type(self)
    }

    fn immediate_backend_type(&self, ty: TyLayout<'tcx>) -> Type {
        ty.immediate_ironox_type(self)
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
