#![allow(dead_code)]
use rustc_mir::monomorphize::partitioning::CodegenUnit;

use rustc::mir::mono::Stats;
use rustc::ty::TyCtxt;

use std::cell::RefCell;
use std::sync::Arc;

pub struct CodegenCx<'ll, 'tcx: 'll, V> {
    pub tcx: TyCtxt<'ll, 'tcx, 'tcx>,
    pub stats: RefCell<Stats>,
    pub codegen_unit: Arc<CodegenUnit<'tcx>>,
    pub value: V,
}
