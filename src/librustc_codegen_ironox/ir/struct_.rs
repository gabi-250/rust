use value::Value;

#[derive(Debug)]
pub struct IronOxStruct {
    components: Vec<Value>,
}

impl IronOxStruct {

    pub fn new(components: &[Value]) -> IronOxStruct {
        IronOxStruct {
            components: components.to_vec()
        }
    }
}
