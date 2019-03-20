#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;
use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutError, TyLayout, VariantIdx};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::base::wants_msvc_seh;
use rustc_codegen_ssa::callee::resolve_and_get_fn;
use rustc_codegen_ssa::traits::*;
use rustc_mir::monomorphize::Instance;
use rustc_target::abi::LayoutOf;
use rustc_target::spec::{HasTargetSpec, Target};
use rustc_data_structures::indexed_vec::IndexVec;
use std::cell::{Cell, RefCell};
use std::sync::Arc;
use syntax::symbol::LocalInternedString;

use debuginfo::DIScope;
use ir::basic_block::BasicBlock;
use ir::bytes::OxConstBytes;
use ir::constant::{OxUnsignedConst, OxSignedConst};
use ir::const_cstr::OxConstStr;
use ir::global::OxGlobal;
use ir::instruction::ConstCast;
use ir::struct_::OxStruct;
use ir::type_::{OxType, Type};
use ir::value::Value;

use super::ModuleIronOx;

impl BackendTypes for CodegenCx<'ll, 'tcx> {
    type Value = Value;
    type BasicBlock = BasicBlock;
    type Type = Type;
    type Funclet = ();
    type DIScope = &'ll DIScope;
}

pub struct CodegenCx<'ll, 'tcx: 'll> {
    /// The typing context of this codegen context.
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    /// The codegen unit compiled using this context. This is None if this `CodegenCx`
    /// is used to compile an allocator module.
    pub codegen_unit: Option<Arc<CodegenUnit<'tcx>>>,
    /// Whether to emit overflow checks.
    pub check_overflow: bool,
    /// A cache of declared functions.
    pub instances: RefCell<FxHashMap<Instance<'tcx>, Value>>,
    /// The IronOx module that contains all the functions in this CGU.
    pub module: RefCell<&'ll mut ModuleIronOx>,
    /// V-tables cache.
    pub vtables: RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>,
    /// All the types defined in this context.
    pub types: RefCell<IndexVec<Type, OxType>>,
    /// The index of an `OxType` in `types`.
    pub type_cache: RefCell<FxHashMap<OxType, Type>>,
    /// The unsigned constants defined in this context.
    pub u_consts: RefCell<Vec<OxUnsignedConst>>,
    /// The signed constants defined in this context.
    pub i_consts: RefCell<Vec<OxSignedConst>>,
    /// All the globals defined so far, including functions.
    pub globals: RefCell<Vec<OxGlobal>>,
    /// Map all the names of the globals defined so far to their index in `globals`.
    pub globals_cache: RefCell<FxHashMap<String, usize>>,
    /// The constant globals (which have a static address).
    pub const_structs: RefCell<Vec<OxStruct>>,
    /// Maps `Value`s to indices in `globals`. This creates globals for the values
    /// for which static_addr_of is called.
    pub const_globals_cache: RefCell<FxHashMap<Value, usize>>,
    /// This ensures constant strings are not emitted multiple times.
    pub const_cstr_cache: RefCell<FxHashMap<LocalInternedString, Value>>,
    /// All the constant strings that need to be emitted.
    pub const_cstrs: RefCell<Vec<OxConstStr>>,
    /// All the constant casts.
    pub const_casts: RefCell<Vec<ConstCast>>,
    /// Map the Rust type of a scalar to its IronOx Type.
    pub scalar_lltypes: RefCell<FxHashMap<Ty<'tcx>, Type>>,
    pub lltypes: RefCell<FxHashMap<(Ty<'tcx>, Option<VariantIdx>), Type>>,
    /// All the constant allocations that need to be emitted.
    pub bytes: RefCell<Vec<OxConstBytes>>,
    /// The statistics collected while codegnning. IronOx doesn't collect statistics.
    pub stats: RefCell<Stats>,
    /// The number of symbols generated so far. This is used to produce unique
    /// symbol names for unnamed structures.
    sym_count: Cell<usize>,
    /// The error handler.
    eh_personality: Cell<Option<Value>>,
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn new(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
               codegen_unit: Option<Arc<CodegenUnit<'tcx>>>,
               module: &'ll mut ModuleIronOx)
                 -> CodegenCx<'ll, 'tcx> {
        CodegenCx {
            tcx,
            codegen_unit,
            check_overflow: tcx.sess.overflow_checks(),
            stats: RefCell::new(Stats::default()),
            instances: Default::default(),
            module: RefCell::new(module),
            vtables: Default::default(),
            eh_personality: Default::default(),
            types: Default::default(),
            type_cache: Default::default(),
            u_consts: Default::default(),
            i_consts: Default::default(),
            globals: Default::default(),
            globals_cache: Default::default(),
            const_structs: Default::default(),
            const_globals_cache: Default::default(),
            const_cstr_cache: Default::default(),
            const_cstrs: Default::default(),
            const_casts: Default::default(),
            sym_count: Cell::new(0),
            scalar_lltypes: Default::default(),
            lltypes: Default::default(),
            bytes: Default::default(),
        }
    }

    /// Add a constant string to the list of constant strings of the context.
    ///
    /// This returns a `Value::ConstCstr(idx)`, where `idx` is the position of
    /// the constant strings in the vector of constant strings used generated
    /// by this context.
    pub fn insert_cstr(&self,
                       name: &str,
                       c_str: *const u8,
                       len: usize,
                       null_terminated: bool) -> Value {
        let mut const_cstrs = self.const_cstrs.borrow_mut();
        let idx = const_cstrs.len();
        // A C string is a char*.
        let ty = self.type_ptr_to(self.type_i8());
        const_cstrs.push(OxConstStr::new(name.to_string(),
                                        ty,
                                        c_str,
                                        len,
                                        null_terminated));
        Value::ConstCstr(idx)
    }

    /// Generate a unique symbol name that starts with the specified prefix.
    pub fn get_sym_name(&self, prefix: &str) -> String {
        let count = self.sym_count.get();
        self.sym_count.set(count + 1);
        format!(".L{}.{}", prefix, count)
    }
}

impl ty::layout::HasTyCtxt<'tcx> for CodegenCx<'ll, 'tcx> {
    fn tcx<'b>(&'b self) -> TyCtxt<'b, 'tcx, 'tcx> {
        self.tcx
    }
}

impl LayoutOf for CodegenCx<'ll, 'tcx> {
    type Ty = Ty<'tcx>;
    type TyLayout = TyLayout<'tcx>;

    fn layout_of(&self, ty: Ty<'tcx>) -> Self::TyLayout {
        self.tcx.layout_of(ty::ParamEnv::reveal_all().and(ty))
            .unwrap_or_else(|e| if let LayoutError::SizeOverflow(_) = e {
                self.sess().fatal(&e.to_string())
            } else {
                bug!("failed to get layout for `{}`: {}", ty, e)
            })
    }
}

impl ty::layout::HasDataLayout for CodegenCx<'ll, 'tcx> {
    fn data_layout(&self) -> &ty::layout::TargetDataLayout {
        &self.tcx.data_layout
    }
}

impl HasTargetSpec for CodegenCx<'ll, 'tcx> {
    fn target_spec(&self) -> &Target {
        &self.tcx.sess.target.target
    }
}

impl MiscMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn vtables(&self) -> &RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>
    {
        &self.vtables
    }

    fn instances(&self) -> &RefCell<FxHashMap<Instance<'tcx>, Value>> {
        &self.instances
    }

    fn get_fn(&self, instance: Instance<'tcx>) -> Value {
        // FIXME: this is a simplified version of what needs to be done...
        let sym_name = self.tcx.symbol_name(instance).as_str();
        if let Some(ref llfn) = self.instances.borrow().get(&instance) {
            // The function has already been defined
            return **llfn;
        }
        let llfn = if let Some(llfn) = self.get_declared_value(&sym_name) {
            llfn
        } else {
            // Otherwise, it probably exists in an external lib...
            let fn_sig = instance.fn_sig(self.tcx);
            let llfn = self.declare_fn(&sym_name, fn_sig);
            let instance_def_id = instance.def_id();
            // This is a non-generic function
            self.module.borrow_mut().functions[llfn.fn_idx()]
                .set_is_codegenned(self.tcx.is_codegened_item(instance_def_id));
            // FIXME: visibility, linkage.
            llfn
        };
        self.instances.borrow_mut().insert(instance, llfn);
        llfn
    }

    fn get_param(&self, llfn: Value, index: c_uint) -> Value {
        let llfn_index = match llfn {
            Value::Function(i) => i,
            _ => bug!("Expected Value::Function, found {:?}", llfn),
        };
        self.module.borrow().functions[llfn_index].get_param(index as usize)
    }

    fn eh_personality(&self) -> Value {
        if let Some(llpersonality) = self.eh_personality.get() {
            return llpersonality
        }
        let tcx = self.tcx;
        let llfn = match tcx.lang_items().eh_personality() {
            Some(def_id) if !wants_msvc_seh(self.sess()) => {
                resolve_and_get_fn(self, def_id, tcx.intern_substs(&[]))
            }
            _ => {
                let name = if wants_msvc_seh(self.sess()) {
                    unimplemented!("Unsupported platform: MSVC")
                } else {
                    "rust_eh_personality"
                };
                let fty = self.type_variadic_func(&[], self.type_i32());
                self.declare_cfn(name, fty)
            }
        };
        self.eh_personality.set(Some(llfn));
        llfn
    }

    fn eh_unwind_resume(&self) -> Value {
        unimplemented!("eh_unwind_resume");
    }

    fn sess(&self) -> &Session {
        &self.tcx.sess
    }

    fn check_overflow(&self) -> bool {
        self.check_overflow
    }

    fn stats(&self) -> &RefCell<Stats> {
        &self.stats
    }

    fn consume_stats(self) -> RefCell<Stats> {
        self.stats
    }

    fn codegen_unit(&self) -> &Arc<CodegenUnit<'tcx>> {
        if let Some(ref codegen_unit) = self.codegen_unit {
            codegen_unit
        } else {
            panic!("The codegen context has no codegen unit!");
        }
    }

    fn used_statics(&self) -> &RefCell<Vec<Value>> {
        unimplemented!("used_statics");
    }

    fn set_frame_pointer_elimination(&self, _llfn: Value) {
        // FIXME
    }

    fn apply_target_cpu_attr(&self, _llfn: Value) {
        // FIXME
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }
}
