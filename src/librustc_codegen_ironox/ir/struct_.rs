use ir::value::Value;
use ir::type_::Type;

/// A `struct` type.
#[derive(Debug)]
pub struct OxStruct {
    pub name: String,
    /// The fields of the `struct`.
    pub components: Vec<Value>,
    pub ty: Type,
}

impl OxStruct {
    pub fn new(name: String, components: &[Value], ty: Type) -> OxStruct {
        OxStruct {
            name,
            components: components.to_vec(),
            ty,
        }
    }
}
