use ir::type_::Type;

/// The unique identifier of an IronOx value.
///
/// Each enum variant has one or more indices that can be used to retrieve the
/// value from the context.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub enum Value {
    /// The index of an `OxFunction` in the module.
    Function(usize),
    /// A `(function index, local index)` pair that can be used to retrieve a
    /// local value.
    Local(usize, usize),
    /// An uninitialized constant. This is just a wrapper around a `Type`.
    ConstUndef(Type),
    /// The index of an `UnsignedConst` in `u_consts`.
    ConstUint(usize),
    /// The index of a `SignedConst` in `i_consts`.
    ConstInt(usize),
    /// The index of an `OxStruct` in the `structs` vec from `ModuleIronOx`.
    ConstStruct(usize),
    /// The parameter of an `OxFunction`. This is just a wrapper around a
    /// `Type`. A parameter is an (index, type) pair, where 'index' is the
    /// index of the parameter in the list of parameters of the function.
    Param(usize, Type),
    /// An instruction: (functiton index, basic block index, instruction index).
    Instruction(usize, usize, usize),
    /// A placeholder for unimplemented Values. This variant will be removed.
    None,
}
