use ir::value::Value;
use ir::type_::Type;

/// A `struct` type.
#[derive(Debug)]
pub struct OxStruct {
    /// The fields of the `struct`.
    pub components: Vec<Value>,
    pub ty: Type,
}

impl OxStruct {
    pub fn new(components: &[Value], ty: Type) -> OxStruct {
        OxStruct {
            components: components.to_vec(),
            ty,
        }
    }
}
