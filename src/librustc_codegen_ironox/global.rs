use ir::type_::Type;
use ir::value::Value;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Global {
    pub ty: Type,
    pub name: String,
    pub constant: bool,
    pub initializer: Option<Value>,
}

impl Global {
    pub fn new(ty: Type, name: String) -> Global {
        Global { ty, name, constant: false, initializer: None }
    }

    pub fn set_initializer(&mut self, value: Value) {
        self.initializer = Some(value)
    }

    pub fn get_initializer(&self) -> Option<Value> {
        self.initializer
    }

    pub fn set_global_constant(&mut self, constant: bool) {
        self.constant = constant;
    }
}
