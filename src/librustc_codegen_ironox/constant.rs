use type_::Type;

/// An unsigned constant. Its value must fit in a `u128`.
#[derive(Clone, Copy, Debug)]
pub struct UnsignedConst {
    /// The type of the constant.
    pub ty: Type,
    /// The value of the constant.
    pub value: u128,
}

/// A signed constant. Its value must fit in a `i128`.
#[derive(Clone, Copy, Debug)]
pub struct SignedConst {
    /// The type of the constant.
    pub ty: Type,
    /// The value of the constant.
    pub value: i128,
}
