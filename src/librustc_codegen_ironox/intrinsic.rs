use abi::{IronOxType, PassMode};
use builder::Builder;
use context::CodegenCx;
use ir::value::Value;
use type_of::LayoutIronOxExt;

use rustc::ty::{self, Ty};
use rustc::ty::layout::LayoutOf;
use rustc_codegen_ssa::common::IntPredicate;
use rustc_codegen_ssa::traits::{BuilderMethods, ConstMethods, IntrinsicCallMethods,
                                BaseTypeMethods};
use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::MemFlags;
use rustc_target::abi::call::FnType;
use syntax_pos::Span;

impl IntrinsicCallMethods<'tcx> for Builder<'a, 'll, 'tcx> {
    fn codegen_intrinsic_call(
        &mut self,
        callee_ty: Ty<'tcx>,
        fn_ty: &FnType<'tcx, Ty<'tcx>>,
        args: &[OperandRef<'tcx, Value>],
        llresult: Value,
        span: Span,
    ) {


        let (def_id, substs) = match callee_ty.sty {
            ty::FnDef(def_id, substs) => (def_id, substs),
            _ => bug!("expected fn item type, found {}", callee_ty)
        };

        let name = &*self.tcx.item_name(def_id).as_str();

        let sig = callee_ty.fn_sig(self.tcx);
        let sig = self.tcx.normalize_erasing_late_bound_regions(
            ty::ParamEnv::reveal_all(), &sig);
        let ret_ty = sig.output();

        let llret_ty = self.layout_of(&ret_ty).ironox_type(self);
        let result = PlaceRef::new_sized(llresult,
                                         fn_ty.ret.layout,
                                         fn_ty.ret.layout.align.abi);
        let llval = match name {
            "size_of" => {
                let tp_ty = substs.type_at(0);
                let size = self.cx.layout_of(tp_ty).size;
                Some(self.const_usize(size.bytes()))
            },
            "offset" => {
                let ptr = args[0].immediate();
                let offset = args[1].immediate();
                eprintln!("offset is {:?}", args[1]);
                Some(self.inbounds_gep(ptr, &[offset]))
            },
            "arith_offset" => {
                let ptr = args[0].immediate();
                let offset = args[1].immediate();
                Some(self.gep(ptr, &[offset]))
            },
            "copy_nonoverlapping" => {

                let src = args[0].immediate();
                let dst = args[1].immediate();
                let count = args[2].immediate();
                let ty = substs.type_at(0);
                let layout = self.layout_of(ty);
                let (size, align) = (layout.size, layout.align.abi);
                let size_bytes = {
                    self.const_usize(size.bytes())
                };

                let size = self.mul(size_bytes, count);
                let flags = MemFlags::empty();
                self.memcpy(dst, align, src, align, size, flags);
                None
            }

            _ => {
                eprintln!("Intrinsic: {:?} {:?} {:?} {:?}", callee_ty,
                          fn_ty, args, llresult);
                // Do nothing.
                None
            }
        };
        if let Some(llval) = llval {
            if !fn_ty.ret.is_ignore() {
                if let PassMode::Cast(ty) = fn_ty.ret.mode {
                    let ptr_llty = self.type_ptr_to(ty.ironox_type(self));
                    let ptr = self.pointercast(result.llval, ptr_llty);
                    self.store(llval, ptr, result.align);
                } else {
                    OperandRef::from_immediate_or_packed_pair(
                        self, llval, result.layout).val.store(self, result);
                }
            }
        }
    }

    fn abort(&mut self) {
        unimplemented!("abort");
    }

    fn assume(&mut self, val: Value) {
        // Do nothing.
    }

    fn expect(&mut self, cond: Value, expected: bool) -> Value {
        let expected_val = {
            self.const_bool(!expected)
        };
        self.icmp(IntPredicate::IntEQ, cond, expected_val)
    }
}
