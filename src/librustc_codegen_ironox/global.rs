use ir::type_::Type;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Global {
    ty: Type,
    name: Option<String>,
}

impl Global {
    pub fn new(ty: Type, name: Option<String>) -> Global {
        Global { ty, name }
    }
}
