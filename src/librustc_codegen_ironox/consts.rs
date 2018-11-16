use context::CodegenCx;
use value::Value;
use ironox_type::Type;

use rustc_codegen_ssa::interfaces::*;
use rustc::hir::def_id::DefId;
use rustc::ty::layout::Align;

impl StaticMethods<'ll> for CodegenCx<'ll, 'tcx, Value> {
    fn static_ptrcast(&self, val: Value, ty: &'ll Type) -> Value {
        // XXX
        val
    }

    fn static_bitcast(&self, val: Value, ty: &'ll Type) -> Value {
        unimplemented!("");
    }

    fn static_addr_of_mut(
        &self,
        cv: Value,
        align: Align,
        kind: Option<&str>,
    ) -> Value {
        unimplemented!("");
    }

    fn static_addr_of(
        &self,
        cv: Value,
        align: Align,
        kind: Option<&str>,
    ) -> Value {
        // XXX implement this
        Value::Local(112, 112)
    }

    fn get_static(&self, def_id: DefId) -> Value {
        unimplemented!("");
    }

    fn codegen_static(
        &self,
        def_id: DefId,
        is_mutable: bool,
    ) {
        unimplemented!("");
    }

    fn static_replace_all_uses(&self, old_g: Value, new_g: Value) {
        unimplemented!("");
    }
}
