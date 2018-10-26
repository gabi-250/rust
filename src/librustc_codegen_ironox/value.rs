use std::fmt;
use std::hash::{Hash, Hasher};

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub struct Value {}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (self as *const Self).hash(hasher);
    }
}
