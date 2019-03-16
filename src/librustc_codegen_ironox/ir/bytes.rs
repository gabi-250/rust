#[derive(Debug)]
pub struct OxConstBytes {
    pub name: String,
    pub bytes: Vec<u8>,
}

impl OxConstBytes {
    pub fn new(name: String, bytes: &[u8]) -> OxConstBytes {
        OxConstBytes {
            name,
            bytes: bytes.to_vec()
        }
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }
}
