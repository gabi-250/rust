use std::fmt;

#[derive(Clone, Debug)]
pub enum GasDirective {
    Section(String),
    Text,
    Global(String),
    Ascii(Vec<String>),
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
            GasDirective::Text => write!(f, ".text"),
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
