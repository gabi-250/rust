use context::CodegenCx;
use value::Value;
use ironox_type::Type;

use rustc_codegen_ssa::traits::*;
use rustc::hir::def_id::DefId;
use rustc::ty::layout::Align;


impl StaticMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn static_ptrcast(&self, val: &'ll Value, ty: &'ll Type) -> &'ll Value {
        unimplemented!("static_ptrcast");
    }

    fn static_bitcast(&self, val: &'ll Value, ty: &'ll Type) -> &'ll Value {
        unimplemented!("static_bitcast");
    }

    fn static_addr_of_mut(
        &self,
        cv: &'ll Value,
        align: Align,
        kind: Option<&str>,
    ) -> &'ll Value {
        unimplemented!("static_addr_of_mut");
    }

    fn static_addr_of(
        &self,
        cv: &'ll Value,
        align: Align,
        kind: Option<&str>,
    ) -> &'ll Value {
        unimplemented!("static_addr_of");
    }

    fn get_static(&self, def_id: DefId) -> &'ll Value {
        unimplemented!("get_static");
    }

    fn codegen_static(
        &self,
        def_id: DefId,
        is_mutable: bool,
    ) {
        unimplemented!("codegen_static");
    }

    unsafe fn static_replace_all_uses(&self, old_g: &'ll Value, new_g: &'ll Value) {
        unimplemented!("fn static_replace_all_uses");
    }
}
