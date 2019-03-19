use context::CodegenCx;
use ir::instruction::ConstCast;
use ir::value::Value;
use ir::type_::Type;

use rustc_codegen_ssa::traits::*;
use rustc::hir::def_id::DefId;
use rustc::ty::layout::Align;

pub fn ptrcast(cx: &CodegenCx, value: Value, ty: Type) -> Value {
    let mut const_casts = cx.const_casts.borrow_mut();
    let cast_idx = const_casts.len();
    const_casts.push(ConstCast::new(value, ty));
    Value::ConstCast(cast_idx)
}

impl StaticMethods for CodegenCx<'ll, 'tcx> {
    fn static_addr_of(
        &self,
        cv: Value,
        _align: Align,
        kind: Option<&str>,
    ) -> Value {
        let mut const_globals_cache = self.const_globals_cache.borrow_mut();
        if let Some(&gv) = const_globals_cache.get(&cv) {
            // FIXME: update the alignment
            return Value::Global(gv);
        }
        let gv = match kind {
            Some(_kind) if !self.tcx.sess.fewer_names() => {
                let suffix = kind.unwrap_or("global");
                let name = self.get_sym_name(suffix);
                let gv = self.define_global(
                    &name[..], self.val_ty(cv)).unwrap_or_else(|| {
                        bug!("symbol `{}` is already defined", name);
                });
                if let Value::Global(idx) = gv {
                    // Set the linkage to private:
                    self.globals.borrow_mut()[idx].private = true;
                } else {
                    bug!("Expected Value::Global, found {:?}", gv);
                }
                gv
            },
            _ => self.define_private_global(self.val_ty(cv)),
        };
        if let Value::Global(gv) = gv {
            self.globals.borrow_mut()[gv].set_initializer(cv);
            const_globals_cache.insert(cv, gv);
        } else {
            bug!("expected global, found {:?}", gv);
        }
        gv
    }

    fn codegen_static(
        &self,
        _def_id: DefId,
        _is_mutable: bool,
    ) {
        unimplemented!("codegen_static");
    }
}
