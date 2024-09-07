use ir::node::FunctionRef;

use crate::{
    cfg::Block,
    emit::RelocKind,
    lir::{MemLayout, PhysReg, RegClass, StackSlot},
    machine::{MachineCore, MachineRegalloc},
};

mod emit;
mod lower;

pub const RELOC_PC32: RelocKind = RelocKind(0);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandSize {
    S32,
    S64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FullOperandSize {
    S8,
    S16,
    S32,
    S64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtWidth {
    Ext8_32,
    Ext8_64,
    Ext16_32,
    Ext16_64,
    Ext32_64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftOp {
    Shl,
    Shr,
    Sar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DivOp {
    Div,
    Idiv,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexScale {
    One,
    Two,
    Four,
    Eight,
}

#[derive(Debug, Clone, Copy)]
pub enum X64Instr {
    AluRRm(OperandSize, AluOp),
    ShiftRmR(OperandSize, ShiftOp),
    ShiftRmI(OperandSize, ShiftOp, u8),
    Div(OperandSize, DivOp),
    ConvertWord(OperandSize),
    /// Special version of `MovRI` for zero, when clobbering flags is allowed
    MovRZ,
    MovRI(u64),
    MovsxRRm(ExtWidth),
    Setcc(CondCode),
    /// Load from [rbp + offset]
    MovRRbp {
        offset: i32,
    },
    StackAddr(StackSlot),
    MovRStack(StackSlot, FullOperandSize),
    MovStackR(StackSlot, FullOperandSize),
    Push,
    AddSp(i32),
    MovRM(FullOperandSize),
    MovMR(FullOperandSize),
    Ret,
    Call(FunctionRef),
    CallRm,
    Jump(Block),
    Jumpcc(CondCode, Block, Block),
    Ud2,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeModel {
    #[default]
    SmallPic,
    LargeAbs,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct X64MachineConfig {
    pub internal_code_model: CodeModel,
    pub extern_code_model: CodeModel,
}

#[derive(Default)]
pub struct X64Machine {
    _config: X64MachineConfig,
}

impl X64Machine {
    pub fn new(config: X64MachineConfig) -> Self {
        Self { _config: config }
    }
}

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
}

impl MachineRegalloc for X64Machine {
    fn phys_reg_count() -> u32 {
        16
    }

    fn usable_regs(&self, class: RegClass) -> &[PhysReg] {
        match class {
            RC_GPR => &GPR_ORDER,
            _ => panic!("unknown register class"),
        }
    }

    fn reg_class_spill_layout(&self, class: RegClass) -> MemLayout {
        match class {
            RC_GPR => MemLayout { size: 8, align: 8 },
            _ => panic!("unknown register class"),
        }
    }
}

const RC_GPR: RegClass = RegClass::new(0);

const REG_RAX: PhysReg = PhysReg::new(0);
const REG_RBX: PhysReg = PhysReg::new(1);
const REG_RCX: PhysReg = PhysReg::new(2);
const REG_RDX: PhysReg = PhysReg::new(3);
const REG_RDI: PhysReg = PhysReg::new(4);
const REG_RSI: PhysReg = PhysReg::new(5);

const REG_RBP: PhysReg = PhysReg::new(6);
const REG_RSP: PhysReg = PhysReg::new(7);

const REG_R8: PhysReg = PhysReg::new(8);
const REG_R9: PhysReg = PhysReg::new(9);
const REG_R10: PhysReg = PhysReg::new(10);
const REG_R11: PhysReg = PhysReg::new(11);
const REG_R12: PhysReg = PhysReg::new(12);
const REG_R13: PhysReg = PhysReg::new(13);
const REG_R14: PhysReg = PhysReg::new(14);
const REG_R15: PhysReg = PhysReg::new(15);

const CALLER_SAVED_REGS: [PhysReg; 9] = [
    REG_RAX, REG_RCX, REG_RDX, REG_RDI, REG_RSI, REG_R8, REG_R9, REG_R10, REG_R11,
];

const CALLEE_SAVED_REGS: [PhysReg; 5] = [REG_RBX, REG_R12, REG_R13, REG_R14, REG_R15];

// This should be a concatenation of `CALLER_SAVED_REGS` and `CALLEE_SAVED_REGS`.
// Prefer caller-saved regs to avoid unnecessary prologue/epilogue code.
const GPR_ORDER: [PhysReg; 14] = [
    REG_RAX, REG_RCX, REG_RDX, REG_RDI, REG_RSI, REG_R8, REG_R9, REG_R10, REG_R11, REG_RBX,
    REG_R12, REG_R13, REG_R14, REG_R15,
];
