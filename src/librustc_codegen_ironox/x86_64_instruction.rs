use gas_directive::GasDirective;
use x86_64_register::Operand;

use std::fmt;

#[allow(unused)]
#[derive(Clone, Debug)]
pub enum MachineInst {
    MOV(Operand, Operand),
    ADD(Operand, Operand),
    SUB(Operand, Operand),
    MUL(Operand),
    IMUL(Operand),
    XOR(Operand, Operand),
    LEA(Operand, Operand),
    CMP(Operand, Operand),
    JMP(Operand),
    JE(Operand),
    JL(Operand),
    NOT(Operand),
    SETO(Operand),
    SETNO(Operand),
    SETE(Operand),
    SETNE(Operand),
    SETA(Operand),
    SETG(Operand),
    SETAE(Operand),
    SETGE(Operand),
    SETB(Operand),
    SETNB(Operand),
    SETL(Operand),
    SETBE(Operand),
    SETLE(Operand),
    CALL(Operand),
    PUSH(Operand),
    LEAVE,
    RET,
    UD2,
    NOP,
    Label(String),
    Directive(GasDirective),
}

impl From<GasDirective> for MachineInst {
    fn from(d: GasDirective) -> Self {
        MachineInst::Directive(d)
    }
}

#[allow(unused)]
impl MachineInst {
    pub fn mov<U: Into<Operand>, V: Into<Operand>>(
        op: U,
        loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::MOV(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn lea<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::LEA(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn add<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::ADD(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn mul<V: Into<Operand>>(loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::MUL(loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn imul<V: Into<Operand>>(loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::IMUL(loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn xor<U: Into<Operand>, V: Into<Operand>>(op1: U, op2: V) -> MachineInst {
        MachineInst::XOR(op1.into(), op2.into())
    }

    pub fn sub<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::SUB(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn cmp<U: Into<Operand>, V: Into<Operand>>(op1: U, op2: V) -> MachineInst {
        MachineInst::CMP(op1.into(), op2.into())
    }

    pub fn jmp<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::JMP(op.into())
    }

    pub fn je<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::JE(op.into())
    }

    pub fn jl<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::JL(op.into())
    }

    pub fn not<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::NOT(op.into())
    }

    pub fn push<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::PUSH(op.into())
    }

    pub fn seto<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETO(op.into())
    }

    pub fn setno<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETNO(op.into())
    }

    pub fn sete<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETE(op.into())
    }

    pub fn setne<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETNE(op.into())
    }

    pub fn seta<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETA(op.into())
    }

    pub fn setg<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETG(op.into())
    }

    pub fn setae<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETAE(op.into())
    }

    pub fn setge<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETGE(op.into())
    }

    pub fn setb<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETB(op.into())
    }

    pub fn setnb<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETNB(op.into())
    }

    pub fn setl<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETL(op.into())
    }

    pub fn setbe<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETBE(op.into())
    }

    pub fn setle<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETL(op.into())
    }

    pub fn call<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::CALL(op.into())
    }
}

impl fmt::Display for MachineInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
           MachineInst::MOV(ref op, ref loc) => write!(f, "\tmov {}, {}\n", op, loc),
           MachineInst::ADD(ref op, ref loc) => write!(f, "\tadd {}, {}\n", op, loc),
           MachineInst::SUB(ref op, ref loc) => write!(f, "\tsub {}, {}\n", op, loc),
           MachineInst::MUL(ref loc) => write!(f, "\tmul {}\n", loc),
           MachineInst::IMUL(ref loc) => write!(f, "\timul {}\n", loc),
           MachineInst::XOR(ref op, ref loc) => write!(f, "\txor {}, {}\n", op, loc),
           MachineInst::LEA(ref op, ref loc) => write!(f, "\tlea {}, {}\n", op, loc),
           MachineInst::CMP(ref op, ref loc) => write!(f, "\tcmp {}, {}\n", op, loc),
           MachineInst::JMP(ref op) => write!(f, "\tjmp {}\n", op),
           MachineInst::JE(ref op) => write!(f, "\tje {}\n", op),
           MachineInst::JL(ref op) => write!(f, "\tjl {}\n", op),
           MachineInst::NOT(ref op) => write!(f, "\tnot {}\n", op),
           MachineInst::SETO(ref op) => write!(f, "\tseto {}\n", op),
           MachineInst::SETNO(ref op) => write!(f, "\tsetno {}\n", op),
           MachineInst::SETE(ref op) => write!(f, "\tsete {}\n", op),
           MachineInst::SETNE(ref op) => write!(f, "\tsetne {}\n", op),
           MachineInst::SETA(ref op) => write!(f, "\tseta {}\n", op),
           MachineInst::SETG(ref op) => write!(f, "\tsetg {}\n", op),
           MachineInst::SETAE(ref op) => write!(f, "\tsetae {}\n", op),
           MachineInst::SETGE(ref op) => write!(f, "\tsetge {}\n", op),
           MachineInst::SETB(ref op) => write!(f, "\tsetb {}\n", op),
           MachineInst::SETNB(ref op) => write!(f, "\tsetnb {}\n", op),
           MachineInst::SETL(ref op) => write!(f, "\tsetl {}\n", op),
           MachineInst::SETBE(ref op) => write!(f, "\tsetbe {}\n", op),
           MachineInst::SETLE(ref op) => write!(f, "\tsetle {}\n", op),
           MachineInst::CALL(ref op) => write!(f, "\tcall {}\n", op),
           MachineInst::PUSH(ref op) => write!(f, "\tpush {}\n", op),
           MachineInst::LEAVE => write!(f, "\tleave\n"),
           MachineInst::RET => write!(f, "\tret\n"),
           MachineInst::UD2 => write!(f, "\tud2\n"),
           MachineInst::Label(ref s) => write!(f, "{}:\n", s),
           MachineInst::Directive(ref d) => write!(f, "\t{}\n", d),
           MachineInst::NOP => write!(f, "\tnop\n"),
        }
    }
}
