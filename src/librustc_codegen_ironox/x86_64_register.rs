use std::fmt;

#[derive(Clone, Debug)]
pub enum Operand {
    Loc(Location),
    Immediate(isize),
    Sym(String),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::Loc(ref l) => l.fmt(f),
            Operand::Immediate(val) => write!(f, "${}", val),
            Operand::Sym(ref s) => write!(f, "{}", s),
        }
    }
}

pub fn operand_access_mode(op1: &Operand, op2: &Operand) -> AccessMode {
    match (op1, op2) {
        (Operand::Loc(Location::Reg(r1)), Operand::Loc(Location::Reg(r2))) => {
            let am1 = r1.access_mode();
            let am2 = r2.access_mode();
            match (am1, am2) {
                (AccessMode::Low8, _) | (_, AccessMode::Low8) => AccessMode::Low8,
                (AccessMode::Low16, _) | (_, AccessMode::Low16) => AccessMode::Low16,
                (AccessMode::Low32, _) | (_, AccessMode::Low32) => AccessMode::Low32,
                _ => AccessMode::Full,
            }
        },
        (Operand::Loc(Location::Reg(r)), _) |
        (_, Operand::Loc(Location::Reg(r))) => r.access_mode(),
        _ => AccessMode::Full,
    }
}

#[derive(Clone, Debug)]
pub enum Location {
    Reg(Register),
    RbpOffset(isize),
    RipOffset(String),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Location::Reg(ref r) => r.fmt(f),
            Location::RbpOffset(ref offset) => write!(f, "{}(%rbp)", offset),
            Location::RipOffset(ref offset) => write!(f, "{}(%rip)", offset),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum GeneralPurposeReg {
    RAX,
    RBX,
    RCX,
    RDX,
    RBP,
    RSP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Clone, Copy, Debug)]
pub struct SubRegister {
    reg: GeneralPurposeReg,
    access_mode: AccessMode,
}

impl SubRegister {
    pub fn reg(reg: GeneralPurposeReg, access_mode: AccessMode) -> SubRegister {
        SubRegister { reg, access_mode }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum AccessMode {
    Full,
    Low32,
    Low16,
    Low8,
}

#[derive(Clone, Copy, Debug)]
pub enum Register {
    // %rax
    Direct(SubRegister),
    // (%rax)
    Indirect(SubRegister),
}

impl Register {
    pub fn access_mode(&self) -> AccessMode {
        match *self {
            Register::Direct(sr) | Register::Indirect(sr) => {
                sr.access_mode
            }
        }
    }
}

impl From<GeneralPurposeReg> for SubRegister {
    fn from(reg: GeneralPurposeReg) -> Self {
        SubRegister {
            reg,
            access_mode: AccessMode::Full,
        }
    }
}

impl Register {
    pub fn direct<U: Into<SubRegister>>(r: U) -> Register {
        Register::Direct(r.into())
    }

    pub fn indirect<U: Into<SubRegister>>(r: U) -> Register {
        Register::Indirect(r.into())
    }
}


impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Register::Direct(r) => write!(f, "{}", r),
            Register::Indirect(r) => write!(f, "({})", r),
        }
    }
}

impl From<String> for Operand {
    fn from(s: String) -> Self {
        Operand::Sym(s)
    }
}

impl From<Register> for Location {
    fn from(r: Register) -> Self {
        Location::Reg(r)
    }
}

impl From<Register> for Operand {
    fn from(r: Register) -> Self {
        Operand::Loc(Location::from(r))
    }
}

impl From<Location> for Operand {
    fn from(l: Location) -> Self {
        Operand::Loc(l)
    }
}

impl fmt::Display for SubRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let reg_str = match self.reg {
            GeneralPurposeReg::RAX => "a",
            GeneralPurposeReg::RBX => "b",
            GeneralPurposeReg::RCX => "c",
            GeneralPurposeReg::RDX => "d",
            GeneralPurposeReg::RBP => "bp",
            GeneralPurposeReg::RSP => "sp",
            GeneralPurposeReg::RSI => "si",
            GeneralPurposeReg::RDI => "di",
            _ => unimplemented!("reg_str of {:?}", *self),
        };
        let reg_str = if reg_str.len() == 1 {
            match self.access_mode {
                AccessMode::Full => format!("r{}x", reg_str),
                AccessMode::Low32 => format!("e{}x", reg_str),
                AccessMode::Low16 => format!("{}x", reg_str),
                AccessMode::Low8 => format!("{}l", reg_str),
            }
        } else {
            match self.access_mode {
                AccessMode::Full => format!("r{}", reg_str),
                AccessMode::Low32 => format!("e{}", reg_str),
                AccessMode::Low16 => format!("{}", reg_str),
                AccessMode::Low8 => format!("{}l", reg_str),
            }
        };
        write!(f, "%{}", reg_str)
    }
}
