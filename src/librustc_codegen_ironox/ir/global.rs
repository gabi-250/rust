use ir::type_::Type;
use ir::value::Value;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct OxGlobal {
    pub ty: Type,
    pub name: String,
    pub constant: bool,
    pub initializer: Option<Value>,
    pub private: bool,
}

impl OxGlobal {
    pub fn new(ty: Type, name: String, private: bool) -> OxGlobal {
        OxGlobal { ty, name, constant: false, initializer: None, private }
    }

    pub fn is_declaration(&self) -> bool {
        self.initializer.is_none()
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
