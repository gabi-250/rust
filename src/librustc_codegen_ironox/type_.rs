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

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ScalarType {
    I1,
    I8,
    I32,
    ISize,
    Ix(u64),
}

pub type Type = usize;

#[derive(PartialEq, Debug)]
pub enum LLType {
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
    crate fn type_named_struct(&self, name: &str) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        let struct_type = LLType::StructType {
            name: Some(name.to_string()),
            members: vec![],
        };
        borrowed_types.push(struct_type);
        borrowed_types.len() - 1
    }

    crate fn set_struct_body(&self, ty: Type, els: &[Type], packed: bool) {
        let mut borrowed_types = self.types.borrow_mut();
        eprintln!("borrowed types was {:?}", borrowed_types[ty]);
        if let LLType::StructType{ ref name, ref mut members } = borrowed_types[ty] {
            *members = els.to_vec();
            return;
            //eprintln!("borrowed types now is {:?}", borrowed_types[ty].clone());
        }
        bug!("expected LLType::StructType, found {:?}", borrowed_types[ty]);
    }
}

impl BaseTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn type_void(&self) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::Void);
        borrowed_types.len() - 1
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
        let mut ll_args = vec![];
        for arg in args {
            ll_args.push(arg.clone());
        }
        // return a FnType that can be used to declare a function
        let fn_type = LLType::FnType {
            args: ll_args,
            ret: ret.clone(),
        };
        eprintln!("Create type func: {:?}", fn_type);
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
        let mut borrowed_types = self.types.borrow_mut();
        let mut members = els.to_vec();
        let struct_type = LLType::StructType {
            name: None,
            members,
        };
        borrowed_types.push(struct_type);
        borrowed_types.len() - 1
    }

    fn type_array(&self, ty: Type, len: u64) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        let array_type = LLType::Array {
            length: len,
        };
        borrowed_types.push(array_type);
        borrowed_types.len() - 1
    }

    fn type_vector(&self, ty: Type, len: u64) -> Type {
        unimplemented!("type_vector");
    }

    fn type_kind(&self, ty: Type) -> TypeKind {
        unimplemented!("type_kind");
    }

    fn type_ptr_to(&self, ty: Type) -> Type {
        let mut borrowed_types = self.types.borrow_mut();
        borrowed_types.push(LLType::PtrTo { pointee: ty });
        borrowed_types.len() - 1
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
        eprintln!("Get the type of v!!!!! {:?}", v);
        // XXX return the real type
        let mut borrowed_types = self.types.borrow_mut();
        match v {
            Value::ConstStruct(_) => {
                eprintln!("*** struct (a global)");
            },
            _ => {}
        }

        borrowed_types.len() - 1
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
