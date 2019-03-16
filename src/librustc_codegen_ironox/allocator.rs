use libc::c_uint;

use rustc::middle::allocator::AllocatorKind;
use rustc::ty::TyCtxt;
use rustc_allocator::{ALLOCATOR_METHODS, AllocatorTy};
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods, DeclareMethods,
                                MiscMethods};

use builder::Builder;
use context::CodegenCx;
use x86_64::asm_printer::AsmPrinter;
use ModuleIronOx;

pub(crate) fn codegen<'ll, 'tcx: 'll>(
    tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    module: &'ll mut ModuleIronOx,
    kind: AllocatorKind) {
    let asm = {
        let cx = CodegenCx::new(tcx, None, module);
        let target_usize = match &tcx.sess.target.target.target_pointer_width[..] {
            "64" => cx.type_i64(),
            tws => bug!("Unsupported target word size for int: {}", tws),
        };
        let i8p = cx.type_ptr_to(cx.type_i8());
        for method in ALLOCATOR_METHODS {
            let mut args = Vec::with_capacity(method.inputs.len());
            for ty in method.inputs.iter() {
                match *ty {
                    AllocatorTy::Layout => {
                        args.push(target_usize); // size
                        args.push(target_usize); // align
                    }
                    AllocatorTy::Ptr => args.push(i8p),
                    AllocatorTy::Usize => args.push(target_usize),

                    AllocatorTy::ResultPtr |
                    AllocatorTy::Unit => panic!("invalid allocator arg"),
                }
            }
            let output = match method.output {
                AllocatorTy::ResultPtr => Some(i8p),
                AllocatorTy::Unit => None,

                AllocatorTy::Layout |
                AllocatorTy::Usize |
                AllocatorTy::Ptr => panic!("invalid allocator output"),
            };
            let fn_ty = cx.type_func(&args[..], output.unwrap_or(cx.type_void()));
            let name = format!("__rust_{}", method.name);
            let llfn = cx.declare_cfn(&name, fn_ty);
            // FIXME: set the visibility
            let callee = kind.fn_name(method.name);
            let callee = cx.declare_cfn(&callee, fn_ty);
            let mut builder = Builder::new_block(&cx, llfn, "entry");
            let args = args.iter().enumerate().map(|(i, _)| {
                cx.get_param(llfn, i as c_uint)
            }).collect::<Vec<_>>();
            let ret = builder.call(callee, &args[..], None);
            if output.is_some() {
                builder.ret(ret)
            } else {
                builder.ret_void()
            }
        }
        AsmPrinter::new(&cx).codegen()
    };
    module.asm = Some(asm);
}
