use abi::{FnType, FnTypeExt};
use context::CodegenCx;
use ir::type_::Type;

use rustc::ty::{self, TypeFoldable};
use rustc::ty::layout::{self, TyLayout};
use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods, DerivedTypeMethods};
use rustc_mir::monomorphize::item::DefPathBasedNames;
use rustc_target::abi::{FloatTy, LayoutOf};

use std::fmt::Write;

pub trait LayoutIronOxExt<'tcx> {
    fn is_ironox_immediate(&self) -> bool;
    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn immediate_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar) -> Type;
    fn scalar_pair_element_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>,
                                       index: usize, immediate: bool) -> Type;
}

/// Return the IronOx `Type`s of the fields of the specified `layout`.
fn struct_field_types(
    cx: &CodegenCx<'_, 'tcx>,
    layout: TyLayout<'tcx>) -> Vec<Type> {
    let mut fields = vec![];
    for i in layout.fields.index_by_increasing_offset() {
        let field = layout.field(cx, i);
        // FIXME: handle field alignment
        fields.push(field.ironox_type(cx));
    }
    fields
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
                    self.scalar_ironox_type_at(cx, scalar)
                }
            };
            return llty;
        }

        assert!(!self.ty.has_escaping_bound_vars(),
                "{:?} has escaping bound vars", self.ty);

        // FIXME? extracted from llvm's type_of:
        //
        // Make sure lifetimes are erased, to avoid generating distinct LLVM
        // types for Rust types that only differ in the choice of lifetimes.
        let normal_ty = cx.tcx.erase_regions(&self.ty);

        let llty = if self.ty != normal_ty {
            let mut layout = cx.layout_of(normal_ty);
            layout.ironox_type(cx)
        } else {
            match self.abi {
                layout::Abi::Scalar(_) => bug!("handled elsewhere"),
                layout::Abi::Vector { ref element, count } => {
                    unimplemented!("Vector");
                }
                layout::Abi::ScalarPair(..) => {
                    unimplemented!("ScalarPair");
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
            // FIXME: Packed is always false. Structs are never packed.
            let packed = false;
            match self.fields {
                layout::FieldPlacement::Union(_) => {
                    unimplemented!("Union");
                }
                layout::FieldPlacement::Array { count, .. } => {
                    unimplemented!("Array");
                }
                layout::FieldPlacement::Arbitrary { .. } => {
                    match name {
                        None => {
                            let fields = struct_field_types(cx, *self);
                            cx.type_struct(&fields, packed)
                        }
                        Some(ref name) => {
                            let llty = cx.type_named_struct(name);
                            let fields = struct_field_types(cx, *self);
                            cx.set_struct_body(llty, &fields, packed);
                            llty
                        }
                    }
                }
            }
        };
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
                             scalar: &layout::Scalar) -> Type {
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
        self.scalar_ironox_type_at(cx, scalar)
    }
}
