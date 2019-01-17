use context::CodegenCx;
use type_::Type;

use rustc::ty::layout::{self, TyLayout};

pub trait LayoutIronOxExt<'tcx> {
    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn immediate_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type;
    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar) -> Type;
}

impl LayoutIronOxExt<'tcx> for TyLayout<'tcx> {
    fn ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type {
        unimplemented!("ironox_type");
    }

    fn immediate_ironox_type(&self, cx: &CodegenCx<'_, 'tcx>) -> Type {
        unimplemented!("immediate_ironox_type");
    }

    fn scalar_ironox_type_at(&self, cx: &CodegenCx<'_, 'tcx>,
                             scalar: &layout::Scalar) -> Type {
        unimplemented!("scalar_ironox_type_at");
    }
}
