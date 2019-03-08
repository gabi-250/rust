#[derive(Debug)]
pub struct ConstBytes {
    pub name: String,
    pub bytes: Vec<u8>,
}

impl ConstBytes {
    pub fn new(name: String, bytes: &[u8]) -> ConstBytes {
        ConstBytes {
            name,
            bytes: bytes.to_vec()
        }
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }
}
