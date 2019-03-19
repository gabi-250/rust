use ir::type_::Type;

/// The unique identifier of an IronOx value.
///
/// Each enum variant has one or more indices that can be used to retrieve the
/// value from the context.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    /// The index of an `IronOxFunction` function in the module.
    Function(usize),
    /// An uninitialized constant. This is just a wrapper around a `Type`.
    ConstUndef(Type),
    /// The index of an `UnsignedConst` in `u_consts`.
    ConstUint(usize),
    /// The index of a `SignedConst` in `i_consts`.
    ConstInt(usize),
    /// The index of an `IronOxStruct` in the `structs` vec from `ModuleIronOx`.
    ConstStruct(usize),
    /// The parameter of an `IronOxFunction`. This is just a wrapper around a
    /// `Type`. A parameter is an (fnindex, index, type) pair, where 'index' is the
    /// index of the parameter in the list of parameters of the function.
    Param { fn_idx: usize, param_idx: usize, ty: Type },
    /// An instruction: (functiton index, basic block index, instruction index).
    Instruction { fn_idx: usize, bb_idx: usize, idx: usize },
    Global(usize),
    ConstCstr(usize),
    ConstCast(usize),
    ConstFatPtr(usize),
    ConstBytes(usize),
    /// ptr_idx is the index of a constant struct
    /// offset is the offset into the struct
    ConstGep { ptr_idx: usize, offset: u64 },
}

impl Value {
    pub fn is_global(&self) -> bool {
        match *self {
            Value::Global(_) | Value::ConstCstr(_) => true,
            _ => false,
        }
    }

    pub fn fn_idx(&self) -> usize {
        match *self {
            Value::Function(idx) => idx,
            _ => bug!("Expected Value::Function, found {:?}", *self),
        }
    }
}
