use abi::{FnType, FnTypeExt};
use context::CodegenCx;
use type_::Type;

use rustc::ty::{self, TypeFoldable};
use rustc::ty::layout::{self, TyLayout};
use rustc_codegen_ssa::traits::{BaseTypeMethods, LayoutTypeMethods, DerivedTypeMethods};
use rustc_mir::monomorphize::item::DefPathBasedNames;
use rustc_target::abi::{FloatTy, LayoutOf};

use std::fmt::Write;

pub trait LayoutIronOxExt<'tcx> {
    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn immediate_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar) -> Type;
}

fn struct_field_types(
    cx: &CodegenCx<'_, 'tcx>,
    layout: TyLayout<'tcx>) -> Vec<Type> {
    let fields = vec![];
    for i in layout.fields.index_by_increasing_offset() {
        let target_offset = layout.fields.offset(i as usize);
        let field = layout.field(cx, i);
        let effective_field_align = layout.align.abi
            .min(field.align.abi)
            .restrict_for_offset(target_offset);
        //packed |= effective_field_align.abi() < field.align.abi();
    }
    fields
}

impl LayoutIronOxExt<'tcx> for TyLayout<'tcx> {
    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type {
        if let layout::Abi::Scalar(ref scalar) = self.abi {
            let llty = match self.ty.sty {
                ty::Ref(_, ty, _) |
                ty::RawPtr(ty::TypeAndMut { ty, .. }) => {
                    cx.type_ptr_to(cx.layout_of(ty).ironox_type(cx))
                }
                ty::Adt(def, _) if def.is_box() => {
                    //unimplemented!("Def: {:?}", def);
                    cx.type_ptr_to(cx.layout_of(self.ty.boxed_ty()).ironox_type(cx))
                }
                ty::FnPtr(sig) => {
                    //unimplemented!("Fnptr: {:?}", sig);
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

        assert!(!self.ty.has_escaping_bound_vars(), "{:?} has escaping bound vars", self.ty);

        // Make sure lifetimes are erased, to avoid generating distinct LLVM
        // types for Rust types that only differ in the choice of lifetimes.
        let normal_ty = cx.tcx.erase_regions(&self.ty);

        //let mut defer = None;
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

            let name = match self.ty.sty {
                ty::Closure(..) |
                ty::Generator(..) |
                ty::Adt(..) |
                // FIXME(eddyb) producing readable type names for trait objects can result
                // in problematically distinct types due to HRTB and subtyping (see #47638).
                // ty::Dynamic(..) |
                ty::Foreign(..) |
                ty::Str => {
                    // FIXME from llvm's type_of
                    let mut name = String::with_capacity(32);
                    let printer = DefPathBasedNames::new(cx.tcx, true, true);
                    printer.push_type_name(self.ty, &mut name);
                    if let (&ty::Adt(def, _), &layout::Variants::Single { index })
                         = (&self.ty.sty, &self.variants)
                    {
                        if def.is_enum() && !def.variants.is_empty() {
                            write!(&mut name, "::{}", def.variants[index].ident).unwrap();
                        }
                    }
                    Some(name)
                }
                _ => None
            };

            match self.fields {
                layout::FieldPlacement::Union(_) => {
                    let padding = cx.type_padding_filler(self.size, self.align.abi);
                    match name {
                        None => {
                            cx.type_struct(&[padding], false)
                        }
                        Some(ref name) => {
                            let struct_ty = cx.type_named_struct(name);
                            cx.set_struct_body(struct_ty, &[padding], false);
                            struct_ty
                        }
                    }
                }
                layout::FieldPlacement::Array { count, .. } => {
                    unimplemented!("Array");
                }
                layout::FieldPlacement::Arbitrary { .. } => {
                    match name {
                        None => {
                            let fields = struct_field_types(cx, *self);
                            // FIXME: packed = false
                            cx.type_struct(&fields, false)
                        }
                        Some(ref name) => {
                            let llty = cx.type_named_struct(name);
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
            if scalar.is_bool() {
                return cx.type_i1();
            }
        }
        self.ironox_type(cx)
    }

    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar) -> Type {
        match scalar.value {
            layout::Int(i, _) => cx.type_from_integer( i),
            layout::Float(FloatTy::F32) => cx.type_f32(),
            layout::Float(FloatTy::F64) => cx.type_f64(),
            layout::Pointer => {
                // FIXME
                let pointee = cx.type_i8();
                cx.type_ptr_to(pointee)
            }
        }
    }
}
