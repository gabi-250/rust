use abi::FnTypeExt;
use context::CodegenCx;
use ir::global::OxGlobal;
use ir::type_::Type;
use ir::value::Value;

use rustc::ty::{self, PolyFnSig};
use rustc_codegen_ssa::traits::*;

impl CodegenCx<'ll, 'tcx> {
    /// Declare a global variable, which may have either private linkage, or default.
    fn declare_global_with_linkage(
        &self,
        name: &str,
        ty: Type,
        private: bool) -> Value {
        let gv = OxGlobal::new(ty, name.to_string(), private);
        let mut globals = self.globals.borrow_mut();
        let mut globals_cache = self.globals_cache.borrow_mut();

        if let Some(idx) = globals_cache.get(name) {
            return Value::Global(*idx);
        }
        let gv_idx = globals.len();
        let gv_idx = *globals_cache.entry(name.to_string()).or_insert_with(|| {
            globals.push(gv);
            gv_idx
        });
        Value::Global(gv_idx)
    }
}

impl DeclareMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn declare_global(
        &self,
        name: &str, ty: Type
    ) -> Value {
        self.declare_global_with_linkage(name, ty, false)
    }

    /// Declare a function of a particular `Type`.
    ///
    /// If a function with the specified named has already been declared, this simply
    /// returns its `Value`.
    ///
    /// FIXME: This is supposed to follow the C calling convention. IronOx currently
    /// only supports the Rust calling convention.
    fn declare_cfn(
        &self,
        name: &str,
        fn_type: Type
    ) -> Value {
        // Make sure the function ends up in the global cache.
        self.declare_global_with_linkage(name, fn_type, false);
        self.module.borrow_mut().add_function(self, name, fn_type)
    }

    /// Declare a function of a particular `PolyFnSig`.
    ///
    /// This calls declare_cfn, so it has similar problems.
    fn declare_fn(
        &self,
        name: &str,
        sig: PolyFnSig<'tcx>,
    ) -> Value {
        // Normalize the signature.
        let sig = self.tcx.normalize_erasing_late_bound_regions(
            ty::ParamEnv::reveal_all(),
            &sig);
        // Get the IronOx function type that corresponds to this signature.
        let fn_type = self.new_fn_type(sig, &[]).ironox_type(self);
        // Create a function of type fn_type.
        let fn_val = self.declare_cfn(name, fn_type);
        fn_val
    }

    /// Create a new global variable with the given name and type.
    fn define_global(
        &self,
        name: &str,
        ty: Type
    ) -> Option<Value> {
        if self.get_defined_value(name).is_some() {
            None
        } else {
            // `get_defined_value` returns None if this variable hasn't been declared
            // yet, or if it's initialised outside of the current translation unit.
            // As such, setting the linkage option private = false makes the symbol
            // global (its definition can be accessed).
            Some(self.declare_global_with_linkage(name, ty, false))
        }
    }

    /// Create a new global variable with private linkage.
    fn define_private_global(&self, ty: Type) -> Value {
        let name = self.get_sym_name("priv_glbl");
        self.declare_global_with_linkage(&name[..], ty, true)
    }

    fn define_fn(
        &self,
        _name: &str,
        _fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_fn");
    }

    fn define_internal_fn(
        &self,
        _name: &str,
        _fn_sig: PolyFnSig<'tcx>,
    ) -> Value {
        unimplemented!("define_internal_fn");
    }

    /// Find the global variable with the given name.
    fn get_declared_value(&self, name: &str) -> Option<Value> {
        if let Some(idx) = self.globals_cache.borrow().get(name) {
            Some(Value::Global(*idx))
        } else {
            None
        }
    }

    /// Find the value with the specified name, and return it if it's defined in
    /// the current translation unit.
    fn get_defined_value(&self, name: &str) -> Option<Value> {
        match self.get_declared_value(name) {
            Some(Value::Global(idx)) => {
                // If this is a declaration (if it is defined outside of the
                // current translation unit), return None:
                if self.globals.borrow()[idx].is_declaration() {
                    None
                } else {
                    Some(Value::Global(idx))
                }

            },
            Some(v) => bug!("Expected global, found {:?}", v),
            None => None,
        }
    }
}
