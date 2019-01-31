use context::CodegenCx;
use ir::instruction::Instruction;
use ir::value::Value;
use ir::type_::Type;

use rustc_codegen_ssa::traits::*;
use rustc::hir::def_id::DefId;
use rustc::ty::layout::Align;

pub fn ptrcast(cx: &CodegenCx, value: Value, ty: Type) -> Value {
    let mut const_casts = cx.const_casts.borrow_mut();
    let cast_idx = const_casts.len();
    const_casts.push(Instruction::Cast(value, ty));
    Value::Cast(cast_idx)
}


impl StaticMethods for CodegenCx<'ll, 'tcx> {
    fn static_addr_of(
        &self,
        cv: Value,
        align: Align,
        kind: Option<&str>,
    ) -> Value {
        if let Some(&gv) = self.const_globals_cache.borrow().get(&cv) {
            // FIXME: update the alignment
            return gv;
        }
        let gv = match kind {
            Some(kind) if !self.tcx.sess.fewer_names() => {
                // FIXME: generate name
                let name = "my_global".to_string();
                let gv = self.define_global(&name[..],
                    self.val_ty(cv)).unwrap_or_else(||{
                        bug!("symbol `{}` is already defined", name);
                });
                gv
            },
            _ => self.define_private_global(self.val_ty(cv)),
        };
        let mut const_globals = self.const_globals.borrow_mut();
        match gv {
            Value::Global(idx) => {
                const_globals[idx].set_initializer(cv);
                self.const_globals_cache.borrow_mut().insert(cv, gv);
            },
            Value::PrivGlobal(idx) => {
                // FIXME
            },
            _ => bug!("expected global, found {:?}", gv),
        }

        gv
    }

    fn codegen_static(
        &self,
        def_id: DefId,
        is_mutable: bool,
    ) {
        unimplemented!("codegen_static");
    }
}
