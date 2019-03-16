use ir::type_::Type;

/// An unsigned constant. Its value must fit in a `u128`.
#[derive(Clone, Copy, Debug)]
pub struct OxUnsignedConst {
    /// The type of the constant.
    pub ty: Type,
    /// The value of the constant.
    pub value: u128,
}

/// A signed constant. Its value must fit in a `i128`.
#[derive(Clone, Copy, Debug)]
pub struct OxSignedConst {
    /// The type of the constant.
    pub ty: Type,
    /// The value of the constant.
    pub value: i128,
}
