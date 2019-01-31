use ir::type_::Type;
use ir::value::Value;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Global {
    ty: Type,
    name: Option<String>,
    constant: bool,
    initializer: Option<Value>,
}

impl Global {
    pub fn new(ty: Type, name: Option<String>) -> Global {
        Global { ty, name, constant: false, initializer: None }
    }

    pub fn set_initializer(&mut self, value: Value) {
        self.initializer = Some(value)
    }

    pub fn set_global_constant(&mut self, constant: bool) {
        self.constant = constant;
    }
}
