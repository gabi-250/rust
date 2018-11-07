use rustc::ty::{self, Ty};
use rustc::ty::layout::LayoutOf;
use rustc::session::config::Sanitizer;
use rustc_data_structures::small_c_str::SmallCStr;
use rustc_target::spec::PanicStrategy;
use abi::{Abi, FnType, FnTypeExt};
use attributes;
use context::CodegenCx;
use rustc_codegen_ssa::common;
use type_::Type;
use rustc_codegen_ssa::interfaces::*;
use value::Value;

impl DeclareMethods<'ll, 'tcx> for CodegenCx<'ll, 'tcx, &'ll Value> {

    fn declare_global(
        &self,
        name: &str, ty: &'ll Type
    ) -> &'ll Value {
    }

    fn declare_cfn(
        &self,
        name: &str,
        fn_type: &'ll Type
    ) -> &'ll Value {
    }

    fn declare_fn(
        &self,
        name: &str,
        fn_type: Ty<'tcx>,
    ) -> &'ll Value {
        debug!("declare_rust_fn(name={:?}, fn_type={:?})", name, fn_type);
        let sig = common::ty_fn_sig(self, fn_type);
        let sig = self.tcx.normalize_erasing_late_bound_regions(ty::ParamEnv::reveal_all(), &sig);
        debug!("declare_rust_fn (after region erasure) sig={:?}", sig);

        let fty = FnType::new(self, sig, &[]);
        let llfn = declare_raw_fn(self, name, fty.llvm_cconv(), fty.llvm_type(self));

        if self.layout_of(sig.output()).abi.is_uninhabited() {
            llvm::Attribute::NoReturn.apply_llfn(Function, llfn);
        }

        if sig.abi != Abi::Rust && sig.abi != Abi::RustCall {
            attributes::unwind(llfn, false);
        }

        fty.apply_attrs_llfn(llfn);

        llfn
    }

    fn define_global(
        &self,
        name: &str,
        ty: &'ll Type
    ) -> Option<&'ll Value> {
        if self.get_defined_value(name).is_some() {
            None
        } else {
            Some(self.declare_global(name, ty))
        }
    }

    fn define_private_global(&self, ty: &'ll Type) -> &'ll Value {
        unsafe {
            llvm::LLVMRustInsertPrivateGlobal(self.llmod, ty)
        }
    }

    fn define_fn(
        &self,
        name: &str,
        fn_type: Ty<'tcx>,
    ) -> &'ll Value {
        if self.get_defined_value(name).is_some() {
            self.sess().fatal(&format!("symbol `{}` already defined", name))
        } else {
            self.declare_fn(name, fn_type)
        }
    }

    fn define_internal_fn(
        &self,
        name: &str,
        fn_type: Ty<'tcx>,
    ) -> &'ll Value {
        let llfn = self.define_fn(name, fn_type);
        unsafe { llvm::LLVMRustSetLinkage(llfn, llvm::Linkage::InternalLinkage) };
        llfn
    }

    fn get_declared_value(&self, name: &str) -> Option<&'ll Value> {
        debug!("get_declared_value(name={:?})", name);
        let namebuf = SmallCStr::new(name);
        unsafe { llvm::LLVMRustGetNamedValue(self.llmod, namebuf.as_ptr()) }
    }

    fn get_defined_value(&self, name: &str) -> Option<&'ll Value> {
        self.get_declared_value(name).and_then(|val|{
            let declaration = unsafe {
                llvm::LLVMIsDeclaration(val) != 0
            };
            if !declaration {
                Some(val)
            } else {
                None
            }
        })
    }
}
