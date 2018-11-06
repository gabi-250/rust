use rustc::ty::{self, TyCtxt, InstanceDef};
use super::super::ModuleIronOx;

pub fn emit_prologue(module: &mut ModuleIronOx, stack_size: u64) {
    module.asm.push_str(
        &format!("push %rbp\nmov %rsp, %rbp\nsub {}, %rsp\n", stack_size));
}

pub fn emit_epilogue(module: &mut ModuleIronOx, stack_size: u64) {
    module.asm.push_str(
        &format!("add {}, %rsp\nleave\n", stack_size));
}

pub fn stack_size<'ll, 'tcx> (tcx: TyCtxt <'ll, 'tcx, 'tcx>, inst_def: InstanceDef<'tcx>) -> u64 {
    let mir = tcx.instance_mir(inst_def);
    let mut stack_size = 0;
    let locals = &mir.local_decls;
    for local in locals.iter().skip(mir.arg_count + 1) {
        let ty = local.ty;
        eprintln!("Local is {:?} {:?}", local, ty.sty);
        let size = tcx.layout_of(ty::ParamEnv::reveal_all().and(ty))
            .ok().map_or(0, |x| x.size.bytes());
        stack_size += size;
    }
    stack_size
}
