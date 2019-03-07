use abi::{FnType, FnTypeExt};
use context::CodegenCx;
use ir::type_::Type;

use rustc::ty::{self, TypeFoldable};
use rustc::ty::layout::{self, Size, TyLayout};
use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods, DerivedTypeMethods};
use rustc_mir::monomorphize::item::DefPathBasedNames;
use rustc_target::abi::{FloatTy, LayoutOf};

use std::fmt::Write;

pub trait LayoutIronOxExt<'tcx> {
    fn is_ironox_immediate(&self) -> bool;
    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn immediate_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar,
                             size: Size) -> Type;
    fn scalar_pair_element_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>,
                                       index: usize, immediate: bool) -> Type;
    fn ironox_field_index(&self, index: usize) -> u64;
}

/// Return the IronOx `Type`s of the fields of the specified `layout`.
fn struct_field_types(
    cx: &CodegenCx<'_, 'tcx>,
    layout: TyLayout<'tcx>) -> Vec<Type> {
    let field_count = layout.fields.count();

    let mut offset = Size::ZERO;
    let mut prev_effective_align = layout.align.abi;
    let mut result: Vec<_> = Vec::with_capacity(1 + field_count * 2);
    for i in layout.fields.index_by_increasing_offset() {
        let target_offset = layout.fields.offset(i as usize);
        let field = layout.field(cx, i);
        let effective_field_align = layout.align.abi
            .min(field.align.abi)
            .restrict_for_offset(target_offset);

        assert!(target_offset >= offset);
        let padding = target_offset - offset;
        let padding_align = prev_effective_align.min(effective_field_align);
        assert_eq!(offset.align_to(padding_align) + padding, target_offset);
        result.push(cx.type_padding_filler( padding, padding_align));

        result.push(field.ironox_type(cx));
        offset = target_offset + field.size;
        prev_effective_align = effective_field_align;
    }
    if !layout.is_unsized() && field_count > 0 {
        if offset > layout.size {
            bug!("layout: {:#?} stride: {:?} offset: {:?}",
                 layout, layout.size, offset);
        }
        let padding = layout.size - offset;
        let padding_align = prev_effective_align;
        assert_eq!(offset.align_to(padding_align) + padding, layout.size);
        result.push(cx.type_padding_filler(padding, padding_align));
        assert_eq!(result.len(), 1 + field_count * 2);
    }

    result
}

impl LayoutIronOxExt<'tcx> for TyLayout<'tcx> {
    fn is_ironox_immediate(&self) -> bool {
        match self.abi {
            layout::Abi::Scalar(_) |
            layout::Abi::Vector { .. } => true,
            layout::Abi::ScalarPair(..) => false,
            layout::Abi::Uninhabited |
            layout::Abi::Aggregate { .. } => self.is_zst()
        }
    }

    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type {
        if let layout::Abi::Scalar(ref scalar) = self.abi {
            if let Some(&llty) = cx.scalar_lltypes.borrow().get(&self.ty) {
                return llty;
            }
            let llty = match self.ty.sty {
                ty::Ref(_, ty, _) |
                ty::RawPtr(ty::TypeAndMut { ty, .. }) => {
                    cx.type_ptr_to(cx.layout_of(ty).ironox_type(cx))
                }
                ty::Adt(def, _) if def.is_box() => {
                    cx.type_ptr_to(cx.layout_of(self.ty.boxed_ty()).ironox_type(cx))
                }
                ty::FnPtr(sig) => {
                    let sig = cx.tcx.normalize_erasing_late_bound_regions(
                        ty::ParamEnv::reveal_all(),
                        &sig,
                    );
                    cx.fn_ptr_backend_type(&FnType::new(cx, sig, &[]))
                }
                _ => {
                    self.scalar_ironox_type_at(cx, scalar, Size::ZERO)
                }
            };
            cx.scalar_lltypes.borrow_mut().insert(self.ty, llty);
            return llty;
        }

        let variant_index = match self.variants {
            layout::Variants::Single { index } => Some(index),
            _ => None
        };
        if let Some(&llty) = cx.lltypes.borrow().get(&(self.ty, variant_index)) {
            return llty;
        }
        assert!(!self.ty.has_escaping_bound_vars(),
                "{:?} has escaping bound vars", self.ty);

        // FIXME? extracted from llvm's type_of:
        //
        // Make sure lifetimes are erased, to avoid generating distinct LLVM
        // types for Rust types that only differ in the choice of lifetimes.
        let normal_ty = cx.tcx.erase_regions(&self.ty);

        // Leave the llfields for later.
        let mut defer = None;
        let llty = if self.ty != normal_ty {
            let mut layout = cx.layout_of(normal_ty);
            if let Some(v) = variant_index {
                layout = layout.for_variant(cx, v);
            }
            layout.ironox_type(cx)
        } else {
            match self.abi {
                layout::Abi::Scalar(_) => bug!("handled elsewhere"),
                layout::Abi::Vector { .. } => {
                    unimplemented!("Vector");
                }
                layout::Abi::ScalarPair(_, _) => {
                    // This may or may not be a fat pointer. It could be
                    // another type that must be represented as a pair of scalars.
                    // Create a struct that contains the types of the two elements
                    // of the pair.
                    return cx.type_struct(&[
                        self.scalar_pair_element_ironox_type(cx, 0, false),
                        self.scalar_pair_element_ironox_type(cx, 1, false),
                    ], false);
                }
                layout::Abi::Uninhabited |
                layout::Abi::Aggregate { .. } => {}
            }
            // Construct the name of the type.
            let name = match self.ty.sty {
                ty::Closure(..) |
                ty::Generator(..) |
                ty::Adt(..) |
                ty::Foreign(..) |
                ty::Str => {
                    let mut name = String::with_capacity(32);
                    let printer = DefPathBasedNames::new(cx.tcx, true, true);
                    // Add the name of the type.
                    printer.push_type_name(self.ty, &mut name);
                    if let (&ty::Adt(def, _), &layout::Variants::Single { index })
                         = (&self.ty.sty, &self.variants)
                    {
                        // If the type is an enum, also append the variant.
                        if def.is_enum() && !def.variants.is_empty() {
                            write!(&mut name, "::{}", def.variants[index].ident).unwrap();
                        }
                    }
                    Some(name)
                }
                _ => None
            };
            // FIXME: Packed is always true. Structs are always packed.
            let packed = true;
            match self.fields {
                layout::FieldPlacement::Union(_) => {
                    let fill = cx.type_padding_filler(self.size, self.align.abi);
                    let packed = true;
                    match name {
                        None => {
                            cx.type_struct(&[fill], packed)
                        }
                        Some(ref name) => {
                            let llty = cx.type_named_struct(name, &[fill], packed);
                            llty
                        }
                    }
                }
                layout::FieldPlacement::Array { count, .. } => {
                    cx.type_array(self.field(cx, 0).ironox_type(cx), count)
                }
                layout::FieldPlacement::Arbitrary { .. } => {
                    match name {
                        None => {
                            let fields = struct_field_types(cx, *self);
                            cx.type_struct(&fields, packed)
                        }
                        Some(ref name) => {
                            // Initialize the fields of the struct later.
                            let llty = cx.type_named_struct(name, &[], packed);
                            defer = Some((llty, *self));
                            llty
                        }
                    }
                }
            }
        };
        cx.lltypes.borrow_mut().insert((self.ty, variant_index), llty);
        if let Some((llty, layout)) = defer {
            let (llfields, packed) = (struct_field_types(cx, layout), true);
            cx.set_struct_body(llty, &llfields, packed)
        }
        return llty;
    }

    fn immediate_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type {
        if let layout::Abi::Scalar(ref scalar) = self.abi {
            // This is a special case because type_from_integer from
            // scalar_ironox_type_at only handles integers of sizes 8, 16, 32,
            // 64, and 128.
            if scalar.is_bool() {
                return cx.type_i1();
            }
        }
        self.ironox_type(cx)
    }

    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar,
                             _offset: Size) -> Type {
        match scalar.value {
            layout::Int(i, _) => cx.type_from_integer(i),
            layout::Float(FloatTy::F32) => cx.type_f32(),
            layout::Float(FloatTy::F64) => cx.type_f64(),
            layout::Pointer => {
                // FIXME: this is always a ptr to an i8
                let pointee = cx.type_i8();
                cx.type_ptr_to(pointee)
            }
        }
    }

    fn scalar_pair_element_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>,
                                       index: usize, immediate: bool) -> Type {
        // HACK(eddyb) special-case fat pointers until LLVM removes
        // pointee types, to avoid bitcasting every `OperandRef::deref`.
        match self.ty.sty {
            ty::Ref(..) |
            ty::RawPtr(_) => {
                return self.field(cx, index).ironox_type(cx);
            }
            ty::Adt(def, _) if def.is_box() => {
                let ptr_ty = cx.tcx.mk_mut_ptr(self.ty.boxed_ty());
                return cx.layout_of(ptr_ty).scalar_pair_element_ironox_type(cx, index, immediate);
            }
            _ => {}
        }

        let (a, b) = match self.abi {
            layout::Abi::ScalarPair(ref a, ref b) => (a, b),
            _ => bug!("TyLayout::scalar_pair_element_llty({:?}): not applicable", self)
        };
        let scalar = [a, b][index];
        // Make sure to return the same type `immediate_llvm_type` would when
        // dealing with an immediate pair.  This means that `(bool, bool)` is
        // effectively represented as `{i8, i8}` in memory and two `i1`s as an
        // immediate, just like `bool` is typically `i8` in memory and only `i1`
        // when immediate.  We need to load/store `bool` as `i8` to avoid
        // crippling LLVM optimizations or triggering other LLVM bugs with `i1`.
        if immediate && scalar.is_bool() {
            return cx.type_i1();
        }

        let offset = if index == 0 {
            Size::ZERO
        } else {
            a.value.size(cx).align_to(b.value.align(cx).abi)
        };
        self.scalar_ironox_type_at(cx, scalar, offset)
    }

    fn ironox_field_index(&self, index: usize) -> u64 {
        match self.abi {
            layout::Abi::Scalar(_) |
            layout::Abi::ScalarPair(..) => {
                bug!("TyLayout::llvm_field_index({:?}): not applicable", self)
            }
            _ => {}
        }
        match self.fields {
            layout::FieldPlacement::Union(_) => {
                bug!("TyLayout::llvm_field_index({:?}): not applicable", self)
            }
            layout::FieldPlacement::Array { .. } => {
                index as u64
            }
            layout::FieldPlacement::Arbitrary { .. } => {
                // FIXME:
                1 + (self.fields.memory_index(index) as u64) * 2
            }
        }
    }
}
