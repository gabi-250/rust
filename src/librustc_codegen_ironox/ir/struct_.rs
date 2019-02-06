use ir::value::Value;

/// A `struct` type.
#[derive(Debug)]
pub struct OxStruct {
    /// The fields of the `struct`.
    components: Vec<Value>,
}

impl OxStruct {
    pub fn new(components: &[Value]) -> OxStruct {
        OxStruct {
            components: components.to_vec()
        }
    }
}
