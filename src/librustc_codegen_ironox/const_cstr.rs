pub struct ConstCstr {
    pub ptr: *const u8,
    pub len: usize,
    pub null_terminated:bool,
}

impl ConstCstr {
    pub fn new(ptr: *const u8, len: usize, null_terminated: bool) -> ConstCstr {
        ConstCstr { ptr, len, null_terminated }
    }
}
