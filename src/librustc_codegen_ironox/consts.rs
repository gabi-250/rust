use context::CodegenCx;
use ir::bytes::OxConstBytes;
use ir::constant::{OxUnsignedConst, OxSignedConst};
use ir::instruction::ConstCast;
use ir::value::Value;
use ir::struct_::OxStruct;
use ir::type_::{Type, IxLlcx};
use type_of::LayoutIronOxExt;

use rustc_codegen_ssa::traits::*;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_target::abi::{HasDataLayout, LayoutOf};
use rustc::hir::def_id::DefId;
use rustc::mir::interpret::{Scalar, AllocKind, Allocation, Pointer, read_target_uint};
use rustc::ty::layout::{self, Align, Size, TyLayout};
use syntax::ast::Mutability;
use syntax::symbol::LocalInternedString;

impl CodegenCx<'ll, 'tcx> {
    pub fn const_cast(&self, value: Value, ty: Type) -> Value {
        let mut const_casts = self.const_casts.borrow_mut();
        let cast_idx = const_casts.len();
        const_casts.push(ConstCast::new(value, ty));
        Value::ConstCast(cast_idx)
    }

    fn const_gep(base_addr: Value, offset: u64) -> Value {
        if let Value::ConstStruct(idx) = base_addr {
            Value::ConstGep { ptr_idx: idx, offset }
        } else {
            bug!("Expected Value::ConstStruct, found {:?}", base_addr)
        }
    }

    #[allow(unused)]
    /// Return a signed constant which has the specified type, and value.
    fn const_signed(&self, ty: Type, value: i128) -> Value {
        let iconst = OxSignedConst {
            ty,
            value,
        };
        let mut consts = self.i_consts.borrow_mut();
        consts.push(iconst);
        Value::ConstUint(consts.len() - 1)
    }

    /// Return an unsigned constant which has the specified type, and value.
    fn const_unsigned(&self, ty: Type, value: u128) -> Value {
        let uconst = OxUnsignedConst {
            ty,
            value,
        };
        let mut consts = self.u_consts.borrow_mut();
        consts.push(uconst);
        Value::ConstUint(consts.len() - 1)
    }

    fn const_alloc_to_ironox(
        &self,
        alloc: &Allocation,
    ) -> Value {
        // The allocation is represented as a struct.
        let mut struct_elts = Vec::with_capacity(alloc.relocations.len() + 1);
        let dl = self.data_layout();
        let ptr_size = dl.pointer_size.bytes() as usize;
        // The offset into the allocation where we left off...
        let mut last_offset = 0;
        // alloc.relocations are the relocations (pointers) from the `Allocation`.
        // This iterates over the pointers in sorted order.
        for &(offset, ((), alloc_id)) in alloc.relocations.iter() {
            // The offset of the current pointer.
            let offset = offset.bytes();
            assert_eq!(offset as usize as u64, offset);
            let offset = offset as usize;
            // There are some bytes left in between the last pointer codegenned and
            // the current one. Generate the bytes in between:
            if offset > last_offset {
                struct_elts.push(self.const_bytes(&alloc.bytes[last_offset..offset]));
            }
            let ptr_offset = read_target_uint(
                dl.endian,
                &alloc.bytes[offset..(offset + ptr_size)],
            ).expect("const_alloc_to_ironox: could not read relocation pointer") as u64;
            struct_elts.push(self.scalar_to_backend(
                Pointer::new(alloc_id, Size::from_bytes(ptr_offset)).into(),
                &layout::Scalar {
                    value: layout::Primitive::Pointer,
                    valid_range: 0..=!0
                },
                self.type_i8p()
            ));
            // Move to the next byte that has not yet been added to the struct.
            last_offset = offset + ptr_size;
        }
        // If there are more bytes after the last emitted relocation, add them
        // to the struct:
        if alloc.bytes.len() > last_offset {
            struct_elts.push(self.const_bytes(&alloc.bytes[last_offset..]));
        }
        self.const_struct(&struct_elts, true)
    }
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

impl ConstMethods<'tcx> for CodegenCx<'ll, 'tcx> {
    fn const_null(&self, t: Type) -> Value {
        self.const_unsigned(t, 0u128)
    }

    fn const_undef(&self, t: Type) -> Value {
        Value::ConstUndef(t)
    }

    fn const_int(&self, _t: Type, _i: i64) -> Value {
        unimplemented!("const_int");
    }

    fn const_uint(&self, t: Type, i: u64) -> Value {
        self.const_unsigned(t, i as u128)
    }

    fn const_uint_big(&self, t: Type, u: u128) -> Value {
        self.const_unsigned(t, u)
    }

    fn const_bool(&self, val: bool) -> Value {
        self.const_unsigned(self.type_i1(), val as u128)
    }

    fn const_i32(&self, _i: i32) -> Value {
        unimplemented!("const_i32");
    }

    fn const_u32(&self, i: u32) -> Value {
        self.const_unsigned(self.type_i32(), i as u128)
    }

    fn const_u64(&self, _i: u64) -> Value {
        unimplemented!("const_u64");
    }

    fn const_usize(&self, i: u64) -> Value {
        let bit_size = self.data_layout().pointer_size.bits();
        let isize_ty = Type::ix_llcx(self,
                                     self.tcx.data_layout.pointer_size.bits());
        if bit_size < 64 {
            // make sure it doesn't overflow
            assert!(i < (1<<bit_size));
        }

        self.const_uint(isize_ty, i)
    }

    fn const_u8(&self, i: u8) -> Value {
        self.const_unsigned(self.type_i8(), i as u128)
    }

    fn const_cstr(
        &self,
        s: LocalInternedString,
        null_terminated: bool,
    ) -> Value {
        // If the cstr has already been defined, return its value from the cache.
        if let Some(val) = self.const_cstr_cache.borrow().get(&s) {
            return *val;
        }
        // Create a unique name for this symbol.
        let symbol_name = self.get_sym_name("str");
        let str_val = self.insert_cstr(&symbol_name,
                                       s.as_ptr() as *const u8,
                                       s.len(),
                                       null_terminated);
        let gv = self.define_global(&symbol_name[..],
                                    self.val_ty(str_val))
            .unwrap_or_else(|| {
                bug!("symbol `{}' is already defined", symbol_name);
            });
        match gv {
            Value::Global(idx) => {
                let mut global = &mut self.globals.borrow_mut()[idx];
                global.set_initializer(str_val);
                // A string always has private linkage.
                global.private = true;
            },
            _ => bug!("expected global, found {:?}", gv),
        };
        self.const_cstr_cache.borrow_mut().insert(s, gv);
        gv
    }

    fn const_str_slice(&self, s: LocalInternedString) -> Value {
        let len = s.len();
        let cs = self.const_cast(
            self.const_cstr(s, false),
            self.type_ptr_to(self.layout_of(self.tcx.mk_str()).ironox_type(self)));
        self.const_fat_ptr(cs, self.const_usize(len as u64))
    }

    fn const_fat_ptr(
        &self,
        ptr: Value,
        meta: Value
    ) -> Value {
        self.const_struct(&[ptr, meta], false)
    }

    fn const_struct(
        &self,
        elts: &[Value],
        packed: bool
    ) -> Value {
        let name = self.get_sym_name("struct");
        let elt_tys = {
            let mut tys = Vec::with_capacity(elts.len());
            for v in elts {
                tys.push(self.val_ty(*v))
            }
            tys
        };
        let mut structs = self.const_structs.borrow_mut();
        let ty = self.type_struct(&elt_tys[..], packed);
        structs.push(OxStruct::new(name.clone(), elts, ty));
        Value::ConstStruct(structs.len() - 1)
    }

    fn const_array(&self, _ty: Type, _elts: &[Value]) -> Value {
        unimplemented!("const_array");
    }

    fn const_vector(&self, _elts: &[Value]) -> Value {
        unimplemented!("const_vector");
    }

    fn const_bytes(&self, bytes: &[u8]) -> Value {
        let mut c_bytes = self.bytes.borrow_mut();
        let name = self.get_sym_name("const_bytes");
        c_bytes.push(OxConstBytes::new(name, bytes));
        Value::ConstBytes(c_bytes.len() - 1)
    }

    fn const_get_elt(&self, _v: Value, _idx: u64) -> Value {
        unimplemented!("const_get_elt");
    }

    fn const_get_real(&self, _v: Value) -> Option<(f64, bool)> {
        unimplemented!("const_get_real");
    }

    fn const_to_uint(&self, _v: Value) -> u64 {
        unimplemented!("const_to_uint");
    }

    fn is_const_integral(&self, v: Value) -> bool {
        match v {
            Value::ConstUint(_) | Value::ConstInt(_) => true,
            _ => false,
        }
    }

    fn is_const_real(&self, _v: Value) -> bool {
        unimplemented!("is_const_real");
    }

    fn const_to_opt_u128(&self, v: Value, _sign_ext: bool) -> Option<u128> {
        match v {
            Value::ConstUint(i) => Some(self.u_consts.borrow()[i].value),
            Value::ConstInt(_) => unimplemented!("signed integers"),
            _ =>  None
        }
    }

    fn const_ptrcast(&self, val: Value, ty: Type) -> Value {
        self.const_cast(val, ty)
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
                self.const_ptrcast(llval, llty)
            },
            Scalar::Ptr(ptr) => {
                let alloc_kind = self.tcx.alloc_map.lock().get(ptr.alloc_id);
                let base_addr = match alloc_kind {
                    Some(AllocKind::Memory(alloc)) => {
                        let init = self.const_alloc_to_ironox(alloc);
                        if alloc.mutability == Mutability::Mutable {
                            unimplemented!("Mutable allocation");
                        } else {
                            init
                        }
                    }
                    None => bug!("missing allocation {:?}", ptr.alloc_id),
                    _ => unimplemented!("scalar_to_backend({:?})", alloc_kind),
                };
                let val = CodegenCx::const_gep(base_addr, ptr.offset.bytes());
                self.const_ptrcast(val, llty)
            }
        }
    }

    fn from_const_alloc(
        &self,
        layout: TyLayout<'tcx>,
        alloc: &Allocation,
        offset: Size,
    ) -> PlaceRef<'tcx, Value> {
        let const_struct = self.const_alloc_to_ironox(alloc);
        let val = CodegenCx::const_gep(const_struct, offset.bytes());
        PlaceRef::new_sized(val, layout, alloc.align)
    }
}
