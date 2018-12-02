use basic_block::BasicBlockData;
use ironox_type::{LLType, Type};
use value::Value;
use registers::GPR;
use context::CodegenCx;

use rustc::ty::FnSig;
use rustc::ty::layout::Align;

#[derive(PartialEq, Debug)]
pub struct IronOxFunction {
    pub name: String,
    pub basic_blocks: Vec<BasicBlockData>,
    pub stack_size: u64,
    pub locals: Vec<u64>,
    pub params: Vec<Value>,
}

impl IronOxFunction {
    pub fn new(name: &str, sig: FnSig) -> IronOxFunction {
        let mut params = vec![];
        for (index, input) in sig.inputs().iter().enumerate() {
            eprintln!("input is {:?}", input);
            if index == 0 {
                params.push(Value::Register(GPR::RDI));
            } else if index == 1 {
                params.push(Value::Register(GPR::RSI));
            } else if index == 2 {
                params.push(Value::Register(GPR::RDX));
            } else if index == 3 {
                params.push(Value::Register(GPR::RCX));
            } else if index == 4 {
                params.push(Value::Register(GPR::R8));
            } else if index == 5 {
                params.push(Value::Register(GPR::R9));
            } else {
                unimplemented!("Function args on the stack: arg no. {}", index);
            }
        }
        IronOxFunction {
            name: name.to_string(),
            basic_blocks: vec![],
            stack_size: 8,
            locals: vec![],
            params: params
        }
    }

    pub fn new_with_type(
        cx: &CodegenCx,
        name: &str,
        fn_type: Type) -> IronOxFunction {
        let mut params = vec![];
        match cx.types.borrow()[fn_type] {
            LLType::FnType { ref args, ref ret } => {
                for (index, arg) in args.iter().enumerate() {
                    if index == 0 {
                        params.push(Value::Register(GPR::RDI));
                    } else if index == 1 {
                        params.push(Value::Register(GPR::RSI));
                    } else if index == 2 {
                        params.push(Value::Register(GPR::RDX));
                    } else if index == 3 {
                        params.push(Value::Register(GPR::RCX));
                    } else if index == 4 {
                        params.push(Value::Register(GPR::R8));
                    } else if index == 5 {
                        params.push(Value::Register(GPR::R9));
                    } else {
                        unimplemented!("Function args on the stack: arg no. {}", index);
                    }
                }
            },
            _ => bug!("Expected LLFnType, found {}", fn_type)
        }
        IronOxFunction {
            name: name.to_string(),
            basic_blocks: vec![],
            stack_size: 8,
            locals: vec![],
            params: params
        }
    }

    pub fn get_param(&self, index: usize) -> Value {
        self.params[index]
    }

    pub fn alloca(
        &mut self,
        cx: &CodegenCx,
        ty: Type,
        name: &str,
        align: Align) -> usize {
        self.insert_local(cx, ty, name)
    }

    pub fn zext_local(
        &mut self,
        cx: &CodegenCx,
        val: Value,
        ty: Type) -> usize {
        //let local_idx = match val {
            //Value::Local(_, idx) => idx,
            //_ => bug!("expected local, found {:?}", val)
        //};
        let local_idx = self.locals.len();
        self.insert_local(cx, ty, &format!("zext_val{:?}", local_idx))
    }

    fn insert_local(
        &mut self,
        cx: &CodegenCx,
        ty: Type,
        name: &str) -> usize {
        // XXX ignore the alignment and name for now
        // map the local to its offset from rbp
        self.locals.push(self.stack_size);
        self.stack_size += cx.ty_size(ty);
        self.locals.len() - 1
    }

    pub fn rbp_offset(
        &self,
        local_idx: usize) -> u64 {
        self.locals[local_idx]
    }

    pub fn add_bb(&mut self, bb: BasicBlockData) -> usize {
        self.basic_blocks.push(bb);
        self.basic_blocks.len() - 1
    }
}
