use crate::{
    cfg::Block,
    lir::{PhysReg, RegClass, StackSlot},
    machine::MachineCore,
};

mod lower;

pub const RC_GPR: RegClass = RegClass::new(0);

pub const REG_RAX: PhysReg = PhysReg::new(0);
pub const REG_RBX: PhysReg = PhysReg::new(1);
pub const REG_RCX: PhysReg = PhysReg::new(2);
pub const REG_RDX: PhysReg = PhysReg::new(3);
pub const REG_RDI: PhysReg = PhysReg::new(4);
pub const REG_RSI: PhysReg = PhysReg::new(5);

pub const REG_R8: PhysReg = PhysReg::new(8);
pub const REG_R9: PhysReg = PhysReg::new(9);
pub const REG_R10: PhysReg = PhysReg::new(10);
pub const REG_R11: PhysReg = PhysReg::new(11);
pub const REG_R12: PhysReg = PhysReg::new(12);
pub const REG_R13: PhysReg = PhysReg::new(13);
pub const REG_R14: PhysReg = PhysReg::new(14);
pub const REG_R15: PhysReg = PhysReg::new(15);

#[derive(Debug, Clone, Copy)]
pub enum OperandSize {
    S8,
    S16,
    S32,
    S64,
}

#[derive(Debug, Clone, Copy)]
pub enum ExtWidth {
    Ext8_32,
    Ext8_64,
    Ext16_32,
    Ext16_64,
    Ext32_64,
}

#[derive(Debug, Clone, Copy)]
pub enum ConvertWordWidth {
    Cwd,
    Cdq,
    Cqo,
}

#[derive(Debug, Clone, Copy)]
pub enum CondCode {
    /// Overflow (OF = 1)
    O,
    /// No Overflow (OF = 0)
    No,
    /// Below (CF = 1)
    B,
    /// Above or Equal (CF = 0)
    Ae,
    /// Equal (ZF = 1)
    E,
    /// Not Equal (ZF = 0)
    Ne,
    /// Below or Equal ((CF | ZF) = 1)
    Be,
    /// Above ((CF | ZF) = 0)
    A,
    /// Sign (SF = 1)
    S,
    /// No Sign (SF = 0)
    Ns,
    /// Parity (PF = 1)
    P,
    /// No Parity (PF = 0)
    Np,
    /// Less ((SF ^ OF) = 1)
    L,
    /// Greater or Equal ((SF ^ OF) = 0)
    Ge,
    /// Less or Equal (((SF ^ OF) | ZF) = 1)
    Le,
    /// Greater (((SF ^ OF) | ZF) = 0)
    G,
}

#[derive(Debug, Clone, Copy)]
pub enum AluOp {
    Add,
    And,
    Cmp,
    Or,
    Sub,
    Test,
    Xor,
    Imul,
}

#[derive(Debug, Clone, Copy)]
pub enum ShiftOp {
    Shl,
    Shr,
    Sar,
}

#[derive(Debug, Clone, Copy)]
pub enum X64Instr {
    AluRRm(OperandSize, AluOp),
    ShiftRmR(OperandSize, ShiftOp),
    Div(OperandSize),
    Idiv(OperandSize),
    ConvertWord(ConvertWordWidth),
    /// Special version of `MovRI` for zero, when clobbering flags is allowed
    MovRZ,
    MovRI(OperandSize, u64),
    MovzxRR(ExtWidth),
    Setcc(CondCode),
    /// Load from [rbp + offset]
    MovRRbp {
        offset: i32,
    },
    StackAddr(StackSlot),
    MovRStack(StackSlot, OperandSize),
    MovStackR(StackSlot, OperandSize),
    MovRM(OperandSize),
    MovMR(OperandSize),
    MovzxRM(ExtWidth),
    Ret,
    Jump(Block),
    Jumpcc(CondCode, Block, Block),
}

pub struct X64Machine;

impl MachineCore for X64Machine {
    type Instr = X64Instr;

    fn reg_class_name(class: RegClass) -> &'static str {
        match class {
            RC_GPR => "gpr",
            _ => panic!("unknown register class"),
        }
    }

    fn reg_name(reg: PhysReg) -> &'static str {
        match reg {
            REG_RAX => "rax",
            REG_RBX => "rbx",
            REG_RCX => "rcx",
            REG_RDX => "rdx",
            REG_RDI => "rdi",
            REG_RSI => "rsi",
            REG_R8 => "r8",
            REG_R9 => "r9",
            REG_R10 => "r10",
            REG_R11 => "r11",
            REG_R12 => "r12",
            REG_R13 => "r13",
            REG_R14 => "r14",
            REG_R15 => "r15",
            _ => panic!("unknown physical register"),
        }
    }

    fn usable_regs(&self, class: RegClass) -> &[PhysReg] {
        match class {
            RC_GPR => &[
                REG_RAX, REG_RBX, REG_RCX, REG_RDX, REG_RDI, REG_RSI, REG_R8, REG_R9, REG_R10,
                REG_R11, REG_R12, REG_R13, REG_R14, REG_R15,
            ],
            _ => panic!("unknown register class"),
        }
    }
}
