use context::CodegenCx;
use rustc::hir::def_id::DefId;
use rustc::mir::mono::{Linkage, Visibility};
use rustc_mir::monomorphize::Instance;
use value::Value;
use rustc_codegen_ssa::traits::*;

pub use rustc::mir::mono::MonoItem;

pub use rustc_mir::monomorphize::item::MonoItemExt as BaseMonoItemExt;

impl PreDefineMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn predefine_static(&self,
                        def_id: DefId,
                        linkage: Linkage,
                        visibility: Visibility,
                        symbol_name: &str) {
        unimplemented!("predefine_static");
    }

    fn predefine_fn(&self,
                    instance: Instance<'tcx>,
                    linkage: Linkage,
                    visibility: Visibility,
                    symbol_name: &str) {
        let mono_ty = instance.ty(self.tcx);
        eprintln!("\nInstance {:?}\n", instance);
        eprintln!("fn ty is {:?} {:?}", instance, mono_ty);
        let mono_sig = instance.fn_sig(self.tcx);

        let attrs = self.tcx.codegen_fn_attrs(instance.def_id());
        let fn_decl = self.declare_fn(symbol_name, mono_sig);
        self.instances.borrow_mut().insert(instance, fn_decl);
    }
}
