use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug)]
pub enum Operand {
    Loc(Location),
    Immediate(isize, AccessMode),
    Sym(String),
    // FIXME: immediates can be dereferenced too...
    Deref(Location)
}

impl Operand {
    pub fn access_mode(&self) -> AccessMode {
        match *self {
            Operand::Loc(Location::Reg(r)) => r.access_mode(),
            Operand::Loc(Location::RbpOffset(isize, acc_mode)) => acc_mode,
            Operand::Immediate(_, acc_mode) => acc_mode,
            Operand::Loc(Location::RipOffset(_)) => unimplemented!("access mode of rip"),
            _ => AccessMode::Full,
        }
    }

    pub fn deref(self) -> Operand {
        match self {
            Operand::Loc(l) => Operand::Deref(l),
            _ => unimplemented!("deref: {:?}", self),
        }
    }

    pub fn is_reg(&self) -> bool {
        match *self {
            Operand::Loc(Location::Reg(_)) => true,
            _ => false,
        }
    }

    pub fn get_reg(&self) -> Register {
        match *self {
            Operand::Loc(Location::Reg(r)) => r,
            _ => bug!("Operand {:?} is not a register!", self),
        }
    }

    pub fn with_acc_mode(&mut self, acc_mode: AccessMode) -> Operand {
        match *self {
            Operand::Loc(Location::Reg(r)) => {
                let r = r.with_acc_mode(acc_mode);
                Operand::Loc(Location::Reg(r))
            }
            Operand::Loc(Location::RbpOffset(off, _)) => {
                Operand::Loc(Location::RbpOffset(off, acc_mode))
            }
            Operand::Immediate(val, _) => Operand::Immediate(val, acc_mode),
            Operand::Loc(Location::RipOffset(_)) => unimplemented!("access mode of rip"),
            _ => unimplemented!("with_acc_mode"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::Loc(ref l) => l.fmt(f),
            Operand::Immediate(val, _) => write!(f, "${}", val),
            Operand::Sym(ref s) => write!(f, "{}", s),
            Operand::Deref(ref l) => write!(f, "*{}", l),
        }
    }
}

pub fn operand_access_mode(op1: &Operand, op2: &Operand) -> AccessMode {
    let am1 = op1.access_mode();
    let am2 = op2.access_mode();
    match (am1, am2) {
        (AccessMode::Low8, _) | (_, AccessMode::Low8) => AccessMode::Low8,
        (AccessMode::Low16, _) | (_, AccessMode::Low16) => AccessMode::Low16,
        (AccessMode::Low32, _) | (_, AccessMode::Low32) => AccessMode::Low32,
        _ => AccessMode::Full,
    }
}

pub fn access_mode(size: u64) -> AccessMode {
    if size <= 8 {
        AccessMode::Low8
    } else if size <= 16 {
        AccessMode::Low16
    } else if size <= 32 {
        AccessMode::Low32
    } else if size <= 64 {
        AccessMode::Full
    } else {
        bug!("unsupported register size {}", size)
    }
}

#[derive(Clone, Debug)]
pub enum Location {
    Reg(Register),
    RbpOffset(isize, AccessMode),
    RipOffset(String),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Location::Reg(ref r) => r.fmt(f),
            Location::RbpOffset(ref offset, _) => write!(f, "{}(%rbp)", offset),
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AccessMode {
    Full,
    Low32,
    Low16,
    Low8,
}

impl PartialOrd for AccessMode {
    fn partial_cmp(&self, other: &AccessMode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AccessMode {
    fn cmp(&self, other: &AccessMode) -> Ordering {
        if *self == *other {
            return Ordering::Equal;
        }
        match (*self, *other) {
            (AccessMode::Full, _) |
            (AccessMode::Low32, AccessMode::Low16) |
            (AccessMode::Low32, AccessMode::Low8) |
            (AccessMode::Low16, AccessMode::Low8) => Ordering::Greater,
            _ => Ordering::Less
        }
    }
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

    pub fn with_acc_mode(&self, acc_mode: AccessMode) -> Register {
        match *self {
            Register::Direct(sr) => {
                let sr = SubRegister::reg(sr.reg, acc_mode);
                Register::Direct(sr)
            },
            Register::Indirect(sr) => {
                let sr = SubRegister::reg(sr.reg, acc_mode);
                Register::Indirect(sr)
            },
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
