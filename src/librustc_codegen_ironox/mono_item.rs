use context::CodegenCx;
use rustc::hir::def_id::DefId;
use rustc::mir::mono::{Linkage, Visibility};
use rustc_mir::monomorphize::Instance;
use ir::value::Value;
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
        let mono_sig = instance.fn_sig(self.tcx);
        // Create an IronOx function for this instance.
        let fn_decl = self.declare_fn(symbol_name, mono_sig);
        // Map the instance to the IronOx function it corresponds to.
        self.instances.borrow_mut().insert(instance, fn_decl);
    }
}
