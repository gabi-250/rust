use ir::type_::Type;

pub struct ConstCstr {
    pub ty: Type,
    pub ptr: *const u8,
    pub len: usize,
    pub null_terminated:bool,
}

impl ConstCstr {
    pub fn new(ty: Type,
               ptr: *const u8,
               len: usize,
               null_terminated: bool) -> ConstCstr {
        ConstCstr { ty, ptr, len, null_terminated }
    }
}
