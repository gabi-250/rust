use std::fmt;

#[derive(Clone, Debug)]
pub enum GasDirective {
    Ascii(Vec<String>),
    Global(String),
    Section(String),
    Size(String, usize),
    Text,
    Type(String, GasType),
    Quad(Vec<BigNum>),
    Long(Vec<u32>),
    Byte(Vec<u8>),
}

#[derive(Clone, Debug)]
pub enum BigNum {
    Immediate(u64),
    Sym(String),
}

impl fmt::Display for BigNum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BigNum::Immediate(num) => write!(f, "{}", num),
            BigNum::Sym(ref name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum GasType {
    Function,
    Object,
}

impl fmt::Display for GasType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            GasType::Function => write!(f, "@function"),
            GasType::Object => write!(f, "@object"),
        }
    }
}

impl fmt::Display for GasDirective {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            GasDirective::Ascii(ref strs) => {
                let strs: Vec<String> =
                    strs.iter().map(|x| format!("\"{}\"", x.to_string())).collect();
                write!(f, ".ascii\t{}", strs.join(","))
            }
            GasDirective::Global(ref name) => write!(f, ".globl\t{}", name),
            GasDirective::Section(ref name) => write!(f, ".section\t{}", name),
            GasDirective::Size(ref name, ref size) => {
                write!(f, ".size\t{},{}", name, size)
            },
            GasDirective::Text => write!(f, ".text"),
            GasDirective::Type(ref name, ref ty) => write!(f, ".type\t{},{}", name, ty),
            GasDirective::Byte(ref nums) => {
                let nums: Vec<String> = nums.iter().map(|x| x.to_string()).collect();
                write!(f, ".byte\t{}", nums.join(","))
            },
            GasDirective::Long(ref nums) => {
                let nums: Vec<String> = nums.iter().map(|x| x.to_string()).collect();
                write!(f, ".long\t{}", nums.join(","))
            },
            GasDirective::Quad(ref nums) => {
                let nums: Vec<String> = nums.iter().map(|x| x.to_string()).collect();
                write!(f, ".quad\t{}", nums.join(","))
            }
        }
    }
}
