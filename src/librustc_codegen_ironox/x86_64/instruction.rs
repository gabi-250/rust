use x86_64::gas_directive::GasDirective;
use x86_64::register::Operand;

use std::fmt;

#[allow(unused)]
#[derive(Clone, Debug)]
pub enum MachineInst {
    Mov(Operand, Operand),
    And(Operand, Operand),
    Add(Operand, Operand),
    Sub(Operand, Operand),
    Mul(Operand),
    IMul(Operand),
    Div(Operand),
    IDiv(Operand),
    Xor(Operand, Operand),
    Lea(Operand, Operand),
    Cmp(Operand, Operand),
    Jmp(Operand),
    Je(Operand),
    Jl(Operand),
    Not(Operand),
    Neg(Operand),
    SetO(Operand),
    SetNo(Operand),
    SetE(Operand),
    SetNe(Operand),
    SetA(Operand),
    SetG(Operand),
    SetAe(Operand),
    SetGe(Operand),
    SetB(Operand),
    SetNb(Operand),
    SetL(Operand),
    SetBe(Operand),
    SetLe(Operand),
    Call(Operand),
    Push(Operand),
    Pop(Operand),
    Leave,
    Ret,
    Ud2,
    Nop,
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
            Operand::Loc(_) => MachineInst::Mov(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn lea<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::Lea(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn and<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::And(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn add<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::Add(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn mul<V: Into<Operand>>(loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::Mul(loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn imul<V: Into<Operand>>(loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::IMul(loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn div<V: Into<Operand>>(loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::Div(loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn idiv<V: Into<Operand>>(loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::IDiv(loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn xor<U: Into<Operand>, V: Into<Operand>>(op1: U, op2: V) -> MachineInst {
        MachineInst::Xor(op1.into(), op2.into())
    }

    pub fn sub<U: Into<Operand>, V: Into<Operand>>(op: U, loc: V) -> MachineInst {
        let loc = loc.into();
        match loc {
            Operand::Loc(_) => MachineInst::Sub(op.into(), loc),
            _ => bug!("destination has to be a location"),
        }
    }

    pub fn cmp<U: Into<Operand>, V: Into<Operand>>(op1: U, op2: V) -> MachineInst {
        MachineInst::Cmp(op1.into(), op2.into())
    }

    pub fn jmp<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Jmp(op.into())
    }

    pub fn je<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Je(op.into())
    }

    pub fn jl<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Jl(op.into())
    }

    pub fn not<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Not(op.into())
    }

    pub fn neg<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Neg(op.into())
    }

    pub fn push<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Push(op.into())
    }

    pub fn pop<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Pop(op.into())
    }

    pub fn seto<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetO(op.into())
    }

    pub fn setno<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetNo(op.into())
    }

    pub fn sete<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetE(op.into())
    }

    pub fn setne<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetNe(op.into())
    }

    pub fn seta<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetA(op.into())
    }

    pub fn setg<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetG(op.into())
    }

    pub fn setae<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetAe(op.into())
    }

    pub fn setge<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetGe(op.into())
    }

    pub fn setb<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetB(op.into())
    }

    pub fn setnb<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetNb(op.into())
    }

    pub fn setl<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetL(op.into())
    }

    pub fn setbe<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetBe(op.into())
    }

    pub fn setle<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::SetL(op.into())
    }

    pub fn call<U: Into<Operand>>(op: U) -> MachineInst {
        MachineInst::Call(op.into())
    }
}

impl fmt::Display for MachineInst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
           MachineInst::Mov(ref op, ref loc) => write!(f, "\tmov {}, {}\n", op, loc),
           MachineInst::And(ref op, ref loc) => write!(f, "\tand {}, {}\n", op, loc),
           MachineInst::Add(ref op, ref loc) => write!(f, "\tadd {}, {}\n", op, loc),
           MachineInst::Sub(ref op, ref loc) => write!(f, "\tsub {}, {}\n", op, loc),
           MachineInst::Mul(ref loc) => write!(f, "\tmul {}\n", loc),
           MachineInst::IMul(ref loc) => write!(f, "\timul {}\n", loc),
           MachineInst::Div(ref loc) => write!(f, "\tdiv {}\n", loc),
           MachineInst::IDiv(ref loc) => write!(f, "\tidiv {}\n", loc),
           MachineInst::Xor(ref op, ref loc) => write!(f, "\txor {}, {}\n", op, loc),
           MachineInst::Lea(ref op, ref loc) => write!(f, "\tlea {}, {}\n", op, loc),
           MachineInst::Cmp(ref op, ref loc) => write!(f, "\tcmp {}, {}\n", op, loc),
           MachineInst::Jmp(ref op) => write!(f, "\tjmp {}\n", op),
           MachineInst::Je(ref op) => write!(f, "\tje {}\n", op),
           MachineInst::Jl(ref op) => write!(f, "\tjl {}\n", op),
           MachineInst::Not(ref op) => write!(f, "\tnot {}\n", op),
           MachineInst::Neg(ref op) => write!(f, "\tneg {}\n", op),
           MachineInst::SetO(ref op) => write!(f, "\tseto {}\n", op),
           MachineInst::SetNo(ref op) => write!(f, "\tsetno {}\n", op),
           MachineInst::SetE(ref op) => write!(f, "\tsete {}\n", op),
           MachineInst::SetNe(ref op) => write!(f, "\tsetne {}\n", op),
           MachineInst::SetA(ref op) => write!(f, "\tseta {}\n", op),
           MachineInst::SetG(ref op) => write!(f, "\tsetg {}\n", op),
           MachineInst::SetAe(ref op) => write!(f, "\tsetae {}\n", op),
           MachineInst::SetGe(ref op) => write!(f, "\tsetge {}\n", op),
           MachineInst::SetB(ref op) => write!(f, "\tsetb {}\n", op),
           MachineInst::SetNb(ref op) => write!(f, "\tsetnb {}\n", op),
           MachineInst::SetL(ref op) => write!(f, "\tsetl {}\n", op),
           MachineInst::SetBe(ref op) => write!(f, "\tsetbe {}\n", op),
           MachineInst::SetLe(ref op) => write!(f, "\tsetle {}\n", op),
           MachineInst::Call(ref op) => write!(f, "\tcall {}\n", op),
           MachineInst::Push(ref op) => write!(f, "\tpush {}\n", op),
           MachineInst::Pop(ref op) => write!(f, "\tpop {}\n", op),
           MachineInst::Leave => write!(f, "\tleave\n"),
           MachineInst::Ret => write!(f, "\tret\n"),
           MachineInst::Ud2 => write!(f, "\tud2\n"),
           MachineInst::Label(ref s) => write!(f, "{}:\n", s),
           MachineInst::Directive(ref d) => write!(f, "\t{}\n", d),
           MachineInst::Nop => write!(f, "\tnop\n"),
        }
    }
}
