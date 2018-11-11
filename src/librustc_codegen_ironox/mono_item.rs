use context::CodegenCx;
use rustc::hir::def_id::DefId;
use rustc::mir::mono::{Linkage, Visibility};
use rustc_mir::monomorphize::Instance;
use value::Value;
use rustc_codegen_ssa::interfaces::*;

pub use rustc::mir::mono::MonoItem;

pub use rustc_mir::monomorphize::item::MonoItemExt as BaseMonoItemExt;

impl<'ll, 'tcx: 'll> PreDefineMethods<'ll, 'tcx> for CodegenCx<'ll, 'tcx, &'ll Value> {
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

        // insert an empty value for now
        self.instances.borrow_mut().insert(instance, &Value {});
        //unimplemented!("predefine_fn");
    }
}
