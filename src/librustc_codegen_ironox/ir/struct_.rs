use value::Value;

/// A `struct` type.
#[derive(Debug)]
pub struct IronOxStruct {
    /// The fields of the `struct`.
    pub components: Vec<Value>,
}

impl IronOxStruct {
    pub fn new(components: &[Value]) -> IronOxStruct {
        IronOxStruct {
            components: components.to_vec()
        }
    }
}
