// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use libc::c_uint;
use rustc::mir::mono::Stats;

use rustc::session::Session;

use rustc::ty::{self, Ty, TyCtxt};
use rustc::ty::layout::{LayoutOf, LayoutError, self, TyLayout, Size};
use rustc::util::nodemap::FxHashMap;
use rustc_codegen_ssa::base::wants_msvc_seh;
use rustc_codegen_ssa::callee::resolve_and_get_fn;
use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_mir::monomorphize::Instance;
use rustc_target::abi::HasDataLayout;
use rustc::mir::interpret::{Scalar, Allocation};
use std::cell::{Cell, RefCell};
use std::sync::Arc;
use syntax::symbol::LocalInternedString;

use debuginfo::DIScope;
use ir::basic_block::{BasicBlock, BasicBlockData};
use ir::function::IronOxFunction;
use ir::struct_::IronOxStruct;
use constant::{UnsignedConst, SignedConst};
use value::Value;
use type_::{Type, LLType, IxLlcx};

use super::ModuleIronOx;

impl BackendTypes for CodegenCx<'ll, 'tcx> {
    type Value = Value;
    type BasicBlock = BasicBlock;
    type Type = Type;
    type Funclet = ();
    type DIScope = &'ll DIScope;
}

pub struct IronOxContext<'ll> {
    pub module: &'ll mut ::ModuleIronOx
}

pub struct CodegenCx<'ll, 'tcx: 'll> {
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    pub stats: RefCell<Stats>,
    pub codegen_unit: Arc<CodegenUnit<'tcx>>,
    pub instances: RefCell<FxHashMap<Instance<'tcx>, Value>>,
    pub module: RefCell<&'ll mut ModuleIronOx>,
    pub vtables: RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>,
    eh_personality: Cell<Option<Value>>,
    /// All the types defined in this context.
    pub types: RefCell<Vec<LLType>>,
    /// The index of an `LLType` in `types`.
    pub type_cache: RefCell<FxHashMap<LLType, Type>>,
    /// Maps symbols to Values (indices in either `functions` or `structs`).
    pub named_globals: RefCell<FxHashMap<String, Value>>,
    pub private_globals: RefCell<Vec<Type>>,
    pub global_cache: RefCell<FxHashMap<Value, Value>>,
    pub u_consts: RefCell<Vec<UnsignedConst>>,
    pub i_consts: RefCell<Vec<SignedConst>>,
    pub unnamed_structs: RefCell<Vec<IronOxStruct>>,
}

impl<'ll, 'tcx> CodegenCx<'ll, 'tcx> {
    pub fn new(tcx: TyCtxt<'ll, 'tcx, 'tcx>,
               codegen_unit: Arc<CodegenUnit<'tcx>>,
               module: &'ll mut ModuleIronOx)
                 -> CodegenCx<'ll, 'tcx> {
        CodegenCx {
            tcx,
            codegen_unit,
            stats: RefCell::new(Stats::default()),
            instances: Default::default(),
            module: RefCell::new(module),
            vtables: Default::default(),
            eh_personality: Default::default(),
            types: Default::default(),
            type_cache: Default::default(),
            named_globals: Default::default(),
            private_globals: Default::default(),
            global_cache: Default::default(),
            u_consts: Default::default(),
            i_consts: Default::default(),
            unnamed_structs: Default::default(),
        }
    }

    pub fn ty_size(&self, ty: Type) -> u64 {
        // FIXME: implement
        8
    }

    pub fn pretty_ty(&self, ty: Type) -> String {
        eprintln!("ty is {} {:?}", ty, self.types.borrow()[ty]);
        match self.types.borrow()[ty] {
            LLType::Void => "Void".to_string(),
            LLType::PtrTo { pointee } => {
                format!("PtrTo {}", self.pretty_ty(pointee)).to_string()
            },
            LLType::Array { .. } => "Array".to_string(),
            LLType::Scalar(scalar_ty) => scalar_ty.to_string(),
            LLType::FnType { ref args, ref ret } => {
                let mut str_args = Vec::with_capacity(args.len());
                for arg in args {
                    str_args.push(self.pretty_ty(*arg));
                }
                let ret_ty = self.pretty_ty(*ret);
                format!("FnType {{ args:{} ret:{} }}",
                        str_args.join(","), ret_ty).to_string()
            },
            LLType::StructType { ref name, ref members } => {
                let mut str_members = Vec::with_capacity(members.len());
                for mem in members {
                    str_members.push(self.pretty_ty(*mem));
                }
                format!("Struct {:?} {{ {} }}", name, str_members.join(","))
            }
        }
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

impl MiscMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn vtables(&self) -> &RefCell<
        FxHashMap<(Ty<'tcx>, Option<ty::PolyExistentialTraitRef<'tcx>>), Value>>
    {
        &self.vtables
    }

    fn instances(&self) -> &RefCell<FxHashMap<Instance<'tcx>, Value>> {
        &self.instances
    }

    fn get_fn(&self, instance: Instance<'tcx>) ->
        Value {
        eprintln!("get_fn {:?}", instance);
        if let Some(ref llfn) = self.instances.borrow().get(&instance) {
            // The function has already been defined
            return **llfn;
        }

        let sym_name = self.tcx.symbol_name(instance).as_str();
        let llfn = if let Some(llfn) = self.get_declared_value(&sym_name) {
            // FIXME:
            llfn
        } else {
            // Otherwise, it probably exists in an external lib...
            let fn_sig = instance.fn_sig(self.tcx);
            let llfn = self.declare_fn(&sym_name, fn_sig);
            // visbility, linkage......
            llfn
        };
        self.instances.borrow_mut().insert(instance, llfn);
        llfn
    }

    fn get_param(&self, llfn: Value, index: c_uint) -> Value {
        let llfn_index = match llfn {
            Value::Function(i) => i,
            _ => bug!("llfn must be a function! Found: {:?}", llfn),
        };
        let p = self.module.borrow().functions[llfn_index].get_param(index as usize);

        eprintln!("returning {:?}", p);
        p
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
        //attributes::apply_target_cpu_attr(self, llfn);
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
        unimplemented!("check_overflow");
    }

    fn stats(&self) -> &RefCell<Stats> {
        &self.stats
    }

    fn consume_stats(self) -> RefCell<Stats> {
        self.stats
    }

    fn codegen_unit(&self) -> &Arc<CodegenUnit<'tcx>> {
        &self.codegen_unit
    }

    fn used_statics(&self) -> &RefCell<Vec<Value>> {
        unimplemented!("used_statics");
    }

    fn set_frame_pointer_elimination(&self, llfn: Value) {
        // FIXME
    }

    fn apply_target_cpu_attr(&self, llfn: Value) {
        // FIXME
    }

    fn create_used_variable(&self) {
        unimplemented!("create_used_variable");
    }

}

impl CodegenCx<'ll, 'tcx> {
    fn const_signed(
        &self,
        ty: Type,
        value: i128,
        allowed_range: (i128, i128)) -> Value {
        let iconst = SignedConst {
            ty,
            value,
            allowed_range,
        };
        let mut borrowed_consts = self.i_consts.borrow_mut();
        borrowed_consts.push(iconst);
        Value::ConstInt(borrowed_consts.len() - 1)
    }

    fn const_unsigned(
        &self,
        ty: Type,
        value: u128,
        allowed_range: (u128, u128)) -> Value {
        let uconst = UnsignedConst {
            ty,
            value,
            allowed_range,
        };
        let mut borrowed_consts = self.u_consts.borrow_mut();
        borrowed_consts.push(uconst);
        Value::ConstUint(borrowed_consts.len() - 1)
    }
}

// common?
impl ConstMethods<'tcx> for CodegenCx<'ll, 'tcx> {

    fn const_null(&self, t: Type) -> Value {
        self.const_unsigned(t, 0u128, (u128::min_value(), u128::max_value()))
    }

    fn const_undef(&self, t: Type) -> Value {
        Value::ConstUndef(t)
    }

    fn const_int(&self, t: Type, i: i64) -> Value {
        unimplemented!("const_int");
    }

    fn const_uint(&self, t: Type, i: u64) -> Value {
        unimplemented!("const_uint");
    }

    fn const_uint_big(&self, t: Type, u: u128) -> Value {
        self.const_unsigned(t, u, (u128::min_value(), u128::max_value()))
    }

    fn const_bool(&self, val: bool) -> Value {
        unimplemented!("const_bool");
    }

    fn const_i32(&self, i: i32) -> Value {
        unimplemented!("const_i32");
    }

    fn const_u32(&self, i: u32) -> Value {
        unimplemented!("const_u32");
    }

    fn const_u64(&self, i: u64) -> Value {
        unimplemented!("const_u64");
    }

    fn const_usize(&self, i: u64) -> Value {
        let bit_size = self.data_layout().pointer_size.bits();
        if bit_size < 64 {
            // make sure it doesn't overflow
            assert!(i < (1<<bit_size));
        }
        // FIXME: isize_ty should be cached in CodegenCx
        let isize_ty = Type::ix_llcx(self, self.tcx.data_layout.pointer_size.bits());
        self.const_unsigned(isize_ty, i as u128,
                            (0u128, (1<<bit_size - 1) as u128))
    }

    fn const_u8(&self, i: u8) -> Value {
        unimplemented!("const_u8");
    }

    fn const_cstr(
        &self,
        s: LocalInternedString,
        null_terminated: bool,
    ) -> Value {
        unimplemented!("const_cstr");
    }

    fn const_str_slice(&self, s: LocalInternedString) -> Value {
        unimplemented!("const_str_slice");
    }

    fn const_fat_ptr(
        &self,
        ptr: Value,
        meta: Value
    ) -> Value {
        unimplemented!("const_fat_ptr");
    }

    fn const_struct(
        &self,
        elts: &[Value],
        packed: bool
    ) -> Value {
        let mut structs = self.unnamed_structs.borrow_mut();
        structs.push(IronOxStruct::new(elts));
        Value::ConstStruct(structs.len() - 1)
    }

    fn const_array(&self, ty: Type, elts: &[Value]) -> Value {
        unimplemented!("const_array");
    }

    fn const_vector(&self, elts: &[Value]) -> Value {
        unimplemented!("const_vector");
    }

    fn const_bytes(&self, bytes: &[u8]) -> Value {
        unimplemented!("const_bytes");
    }

    fn const_get_elt(&self, v: Value, idx: u64) -> Value {
        unimplemented!("const_get_elt");
    }

    fn const_get_real(&self, v: Value) -> Option<(f64, bool)> {
        unimplemented!("const_get_real");
    }

    fn const_to_uint(&self, v: Value) -> u64 {
        unimplemented!("const_to_uint");
    }

    fn is_const_integral(&self, v: Value) -> bool {
        unimplemented!("is_const_integral");
    }

    fn is_const_real(&self, v: Value) -> bool {
        unimplemented!("is_const_real");
    }

    fn const_to_opt_u128(&self, v: Value, sign_ext: bool) -> Option<u128> {
        unimplemented!("const_to_opt_u128");
    }

    fn const_ptrcast(&self, val: Value, ty: Type) -> Value {
        val
        //unimplemented!("const_ptrcast");
    }

    fn scalar_to_backend(
        &self,
        cv: Scalar,
        layout: &layout::Scalar,
        llty: Type,
    ) -> Value {
        let bitsize = if layout.is_bool() { 1 } else { layout.value.size(self).bits() };
        match cv {
            Scalar::Bits { size: 0, .. } => {
                assert_eq!(0, layout.value.size(self).bytes());
                self.const_undef(self.type_ix(0))
            },
            Scalar::Bits { bits, size } => {
                assert_eq!(size as u64, layout.value.size(self).bytes());
                let llval = self.const_uint_big(self.type_ix(bitsize), bits);
                // FIXME
                llval
            },
            Scalar::Ptr(ptr) => {
                unimplemented!("Scalar::Ptr");
            }
        }
    }

    fn from_const_alloc(
        &self,
        layout: TyLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, Value> {
        unimplemented!("from_const_alloc");
    }
}
