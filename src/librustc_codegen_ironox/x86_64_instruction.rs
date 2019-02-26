use gas_directive::GasDirective;
use x86_64_register::{Location, Operand, Register};

use std::fmt;

#[derive(Clone, Debug)]
pub enum MachineInst {
    MOV(Operand, Operand),
    ADD(Operand, Operand),
    SUB(Operand, Operand),
    XOR(Operand, Operand),
    LEA(Operand, Operand),
    CMP(Operand, Operand),
    JMP(Operand),
    JE(Operand),
    JL(Operand),
    NOT(Operand),
    SETB(Operand),
    SETO(Operand),
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

    pub fn setb<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETB(op.into())
    }

    pub fn seto<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SETO(op.into())
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
           MachineInst::XOR(ref op, ref loc) => write!(f, "\txor {}, {}\n", op, loc),
           MachineInst::LEA(ref op, ref loc) => write!(f, "\tlea {}, {}\n", op, loc),
           MachineInst::CMP(ref op, ref loc) => write!(f, "\tcmp {}, {}\n", op, loc),
           MachineInst::JMP(ref op) => write!(f, "\tjmp {}\n", op),
           MachineInst::JE(ref op) => write!(f, "\tje {}\n", op),
           MachineInst::JL(ref op) => write!(f, "\tjl {}\n", op),
           MachineInst::NOT(ref op) => write!(f, "\tnot {}\n", op),
           MachineInst::SETB(ref op) => write!(f, "\tsetb {}\n", op),
           MachineInst::SETO(ref op) => write!(f, "\tseto {}\n", op),
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
