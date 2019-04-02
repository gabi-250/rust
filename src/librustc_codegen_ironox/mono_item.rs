use context::CodegenCx;
use rustc::hir::def_id::DefId;
use rustc::mir::mono::{Linkage, Visibility};
use rustc_codegen_ssa::traits::*;
use rustc_mir::monomorphize::Instance;

pub use rustc::mir::mono::MonoItem;

pub use rustc_mir::monomorphize::item::MonoItemExt as BaseMonoItemExt;

impl PreDefineMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn predefine_static(&self,
                        _def_id: DefId,
                        _linkage: Linkage,
                        _visibility: Visibility,
                        _symbol_name: &str) {
        unimplemented!("predefine_static");
    }

    fn predefine_fn(&self,
                    instance: Instance<'tcx>,
                    linkage: Linkage,
                    visibility: Visibility,
                    symbol_name: &str) {
        let mono_sig = instance.fn_sig(self.tcx);
        // Create an IronOx function for this instance.
        let fn_decl = self.declare_fn(symbol_name, mono_sig);
        self.module.borrow_mut().functions[fn_decl.fn_idx()].set_visibility(visibility);
        self.module.borrow_mut().functions[fn_decl.fn_idx()].set_linkage(linkage);
        self.module.borrow_mut().functions[fn_decl.fn_idx()].set_is_codegenned(true);
        // Map the instance to the IronOx function it corresponds to.
        self.instances.borrow_mut().insert(instance, fn_decl);
    }
}
