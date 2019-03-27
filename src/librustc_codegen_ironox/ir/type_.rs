use abi::{FnTypeExt, IronOxType};
use context::CodegenCx;
use ir::value::Value;
use type_of::LayoutIronOxExt;

use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods};
use rustc_codegen_ssa::common::TypeKind;
use rustc::util::nodemap::FxHashMap;
use rustc_target::abi::call::{CastTarget, FnType, Reg};
use rustc::ty::{layout, Ty};
use rustc::ty::layout::TyLayout;
use rustc_data_structures::indexed_vec::{Idx, IndexVec};

use std::cell::RefCell;

/// The type of a scalar.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ScalarType {
    /// A 1-bit integer value.
    I1,
    /// An 8-bit integer value.
    I8,
    /// A 16-bit integer value.
    I16,
    /// A 32-bit integer value.
    I32,
    /// A 64-bit integer value.
    I64,
    /// An integer value which has a target-dependent size.
    ISize,
    /// An integer value of a custom size.
    Ix(u64),
}

impl ScalarType {
    /// The size in bits.
    pub fn size(&self) -> u64 {
        match *self {
            ScalarType::I1 => 8,
            ScalarType::I8 => 8,
            ScalarType::I16 => 16,
            ScalarType::I32 => 32,
            ScalarType::I64 => 64,
            ScalarType::ISize => (std::mem::size_of::<isize>() * 8) as u64,
            ScalarType::Ix(bits) => bits,
        }
    }
}

/// A `Type` is an index into the vector of types in the codegen context.
newtype_index! {
    pub struct Type { .. }
}

/// The type of an IronOx value.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum OxType {
    /// The type of a function.
    Function {
        args: Vec<Type>,
        ret: Type
    },
    /// The type of an array with `len` elements.
    Array {
        len: u64,
        ty: Type,
    },
    /// The type of a pointer that points to a value of type `pointee`.
    PtrTo {
        pointee: Type,
    },
    /// The type of a scalar value.
    Scalar(ScalarType),
    /// The type of a named struct.
    Struct {
        name: Option<String>,
        members: Vec<Type>
    },
    /// The type of a fat pointer: it contains the pointer itself, and the metadata.
    FatPtr {
        ptr: Type,
        meta: Type,
    },
    /// The void type.
    Void,
}

impl OxType {
    pub fn offset(&self, idx: u64, types: &IndexVec<Type, OxType>) -> u64 {
        // FIXME:?
        match *self {
            OxType::Struct { ref members, .. } => {
                let idx = idx as usize;
                let mut offset = 0;
                for i in 0..idx {
                    offset += members[i].size(types);
                }
                offset
            },
            OxType::Array { ty, .. } => {
                idx * ty.size(types)
            }
            _ => unimplemented!("{:?}.offset({})", *self, idx)
        }
    }
}

impl Type {
    pub fn size(&self, types: &IndexVec<Type, OxType>) -> u64 {
        match types[*self] {
            OxType::Scalar(sty) => sty.size(),
            OxType::PtrTo { .. } | OxType::Function { .. } => 64,
            OxType::FatPtr { ptr, meta } => {
                ptr.size(types) + meta.size(types)
            },
            OxType::Struct { ref members, .. } => {
                let mut struct_size = 0;
                for mem in members {
                    let size = mem.size(types);
                    struct_size += size;
                }
                struct_size
            },
            OxType::Array { len, ty } => len * ty.size(types),
            ref ty => unimplemented!("size of {:?}", ty),
        }
    }

    pub fn is_ptr(&self, types: &IndexVec<Type, OxType>) -> bool {
        match types[*self] {
            OxType::Function {..} => true,
            OxType::PtrTo {..} => true,
            _ => false,
        }
    }

    pub fn is_fn(&self, types: &IndexVec<Type, OxType>) -> bool {
        match types[*self] {
            OxType::Function {..} => true,
            _ => false,
        }
    }

    pub fn pointee_ty(&self, types: &IndexVec<Type, OxType>) -> Type {
        assert!(self.is_ptr(types));
        if let OxType::PtrTo { ref pointee } = types[*self] {
            *pointee
        } else {
            bug!("cannot get pointee of non-pointer type: {:?}", *self);
        }
    }

    pub fn ty_at_idx(&self, idx: u64, types: &IndexVec<Type, OxType>) -> Type {
        match types[*self] {
            OxType::Struct { ref members, .. } => members[idx as usize],
            OxType::Function { ref ret, .. } => ret.ty_at_idx(idx, types),
            OxType::PtrTo { ref pointee } => *pointee,
            OxType::Array { ref ty, .. } => *ty,
            ref ty => unimplemented!("Type at index {} for {:?}", idx, ty),
        }
    }
}

pub trait IxLlcx {
    fn ix_llcx(cx: &CodegenCx, num_bits: u64) -> Type;
}

impl IxLlcx for Type {
    fn ix_llcx(cx: &CodegenCx, num_bits: u64) -> Type {
        cx.type_ix(num_bits)
    }
}

impl CodegenCx<'ll, 'tcx> {
    /// Add the specified `OxType` to the vector of `Type`s for this context.
    ///
    /// If the type already exists in the type cache, its index in the type
    /// vector is returned. Otherwise, it is added to the type vector, and to
    /// the type cache.
    pub fn add_type(&self, ll_type: OxType) -> Type {
        let mut types = self.types.borrow_mut();
        let mut type_cache = self.type_cache.borrow_mut();
        // If ll_type is not in types, it will be inserted at the end of the
        // types vector.
        let ty_idx = types.len();
        *type_cache.entry(ll_type.clone()).or_insert_with(|| {
            // FIXME: don't always clone ll_type.
            types.push(ll_type);
            Type::new(ty_idx)
        })
    }

    crate fn type_named_struct(
        &self,
        name: &str,
        els: &[Type],
        _packed: bool
    ) -> Type {
        // FIXME: do something with packed.
        let struct_type = OxType::Struct {
            name: Some(name.to_string()),
            members: els.to_vec(),
        };
        self.add_type(struct_type)
    }

    /// If two structs have the same Type `ty`, this will modify the type of both
    /// structs.
    crate fn set_struct_body(
        &self,
        ty: Type,
        els: &[Type],
        _packed: bool
    ) {
        let mut types = self.types.borrow_mut();
        if let OxType::Struct{ ref mut members, .. } = types[ty] {
            *members = els.to_vec();
            return;
        }
        bug!("expected OxType::Struct, found {:?}", types[ty]);
    }
}

impl BaseTypeMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn type_void(&self) -> Type {
        self.add_type(OxType::Void)
    }

    fn type_metadata(&self) -> Type {
        unimplemented!("type_metadata");
    }

    fn type_i1(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::I1))
    }

    fn type_i8(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::I8))
    }

    fn type_i16(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::I16))
    }

    fn type_i32(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::I32))
    }

    fn type_i64(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::I64))
    }

    fn type_i128(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::Ix(128)))
    }

    fn type_ix(&self, num_bits: u64) -> Type {
        // This could just as well return
        // `self.add_type(OxType::Scalar(ScalarType::Ix(num_bits)))`. However,
        // that would make it harder to determine if two types are equal. For
        // instance, a OxType::Scalar(ScalarType::I64) is equal to a
        // OxType::Scalar(ScalarType::Ix(64)), but their `Types` are different
        // (because they are two distinct `OxType::Scalar`s).
        match num_bits {
            1 => self.type_i1(),
            8 => self.type_i8(),
            16 => self.type_i16(),
            32 => self.type_i32(),
            64 => self.type_i64(),
            128 => self.type_i128(),
            _ => self.add_type(OxType::Scalar(ScalarType::Ix(num_bits))),
        }
    }

    fn type_isize(&self) -> Type {
        self.add_type(OxType::Scalar(ScalarType::ISize))
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
        let mut ll_args = Vec::with_capacity(args.len());
        for arg in args {
            ll_args.push(arg.clone());
        }
        // return a Function that can be used to declare a function
        let fn_type = OxType::Function {
            args: ll_args,
            ret: ret.clone(),
        };
        self.add_type(fn_type)
    }

    fn type_variadic_func(
        &self,
        _args: &[Type],
        _ret: Type
    ) -> Type {
        unimplemented!("type_variadic_func");
    }

    fn type_struct(
        &self,
        els: &[Type],
        _packed: bool
    ) -> Type {
        self.add_type(OxType::Struct {
            name: None,
            members: els.to_vec(),
        })
    }

    fn type_array(&self, ty: Type, len: u64) -> Type {
        self.add_type(OxType::Array { len, ty })
    }

    fn type_vector(&self, _ty: Type, _len: u64) -> Type {
        unimplemented!("type_vector");
    }

    fn type_kind(&self, _ty: Type) -> TypeKind {
        unimplemented!("type_kind");
    }

    fn type_ptr_to(&self, ty: Type) -> Type {
        self.add_type(OxType::PtrTo { pointee: ty })
    }

    fn element_type(&self, _ty: Type) -> Type {
        unimplemented!("element_type");
    }

    fn vector_length(&self, _ty: Type) -> usize {
        unimplemented!("vector_length");
    }

    fn func_params_types(&self, _ty: Type) -> Vec<Type> {
        unimplemented!("func_params_types");
    }

    fn float_width(&self, _ty : Type) -> usize {
        unimplemented!("float_width");
    }

    fn int_width(&self, _ty: Type) -> u64 {
        unimplemented!("int_width");
    }

    fn val_ty(&self, v: Value) -> Type {
        let module = &self.module.borrow();
        match v {
            Value::ConstUint(const_idx) => self.u_consts.borrow()[const_idx].ty,
            Value::ConstInt(const_idx) => self.i_consts.borrow()[const_idx].ty,
            Value::Param { ty, .. } => ty,
            Value::Function(fn_idx) => module.functions[fn_idx].ironox_type,
            Value::Instruction { fn_idx, bb_idx, idx } => {
                let inst = &module
                    .functions[fn_idx].basic_blocks[bb_idx].instrs[idx];
                inst.val_ty(self)
            },
            Value::ConstStruct(idx) => self.const_structs.borrow()[idx].ty,
            Value::ConstCstr(idx) => self.const_cstrs.borrow()[idx].ty,
            Value::ConstCast(idx) => self.const_casts.borrow()[idx].ty,
            Value::ConstUndef(ty) => ty,
            Value::ConstBytes(_) | Value::ConstGep { .. } => {
                let i8_ty = {
                    self.type_i8()
                };
                self.type_ptr_to(i8_ty)
            },
            Value::Global(idx) => self.globals.borrow()[idx].ty,
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
        ty.ironox_field_index(index)
    }

    fn scalar_pair_element_backend_type<'a>(
        &self,
        ty: TyLayout<'tcx>,
        index: usize,
        immediate: bool
    ) -> Type {
        ty.scalar_pair_element_ironox_type(self, index, immediate)
    }

    fn cast_backend_type(&self, ty: &CastTarget) -> Type {
        ty.ironox_type(self)
    }

    fn fn_backend_type(&self, _ty: &FnType<'tcx, Ty<'tcx>>) -> Type {
        unimplemented!("fn_backend_type");
    }

    fn reg_backend_type(&self, _ty: &Reg) -> Type {
        unimplemented!("reg_backend_type");
    }

    fn fn_ptr_backend_type(&self, ty: &FnType<'tcx, Ty<'tcx>>) -> Type {
        ty.ptr_to_ironox_type(self)
        //let arg_tys: Vec<Type> = vec![];
        //let ret_ty = self.type_void();
        //let fn_ty = self.type_func(&arg_tys, ret_ty);
        //self.add_type(OxType::PtrTo { pointee: fn_ty })
    }
}
