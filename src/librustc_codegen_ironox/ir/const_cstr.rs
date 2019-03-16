use ir::type_::Type;

#[derive(Debug)]
pub struct OxConstStr {
    pub name: String,
    pub ty: Type,
    pub ptr: *const u8,
    pub len: usize,
    pub null_terminated:bool,
}

impl OxConstStr {
    pub fn new(name: String,
               ty: Type,
               ptr: *const u8,
               len: usize,
               null_terminated: bool) -> OxConstStr {
        OxConstStr { name, ty, ptr, len, null_terminated }
    }
}
