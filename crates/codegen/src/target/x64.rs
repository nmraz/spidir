use core::fmt;

use ir::node::FunctionRef;

use crate::{
    cfg::Block,
    code_buffer::RelocKind,
    lir::{MemLayout, PhysReg, RegClass, StackSlot},
    machine::{MachineCore, MachineRegalloc},
    regalloc::RematCost,
};

mod emit;
mod lower;

pub const RELOC_PC32: RelocKind = RelocKind(0);
pub const RELOC_ABS64: RelocKind = RelocKind(1);

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

impl CondCode {
    fn negate(self) -> Self {
        match self {
            Self::O => Self::No,
            Self::No => Self::O,

            Self::B => Self::Ae,
            Self::Ae => Self::B,

            Self::E => Self::Ne,
            Self::Ne => Self::E,

            Self::Be => Self::A,
            Self::A => Self::Be,

            Self::S => Self::Ns,
            Self::Ns => Self::S,

            Self::P => Self::Np,
            Self::Np => Self::P,

            Self::L => Self::Ge,
            Self::Ge => Self::L,

            Self::Le => Self::G,
            Self::G => Self::Le,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AluBinOp {
    Add,
    And,
    Cmp,
    Or,
    Sub,
    Test,
    Xor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AluUnOp {
    Not,
    Neg,
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IndexScale {
    One,
    Two,
    Four,
    Eight,
}

impl fmt::Debug for IndexScale {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::One => write!(f, "reg"),
            Self::Two => write!(f, "2*reg"),
            Self::Four => write!(f, "4*reg"),
            Self::Eight => write!(f, "8*reg"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AddrBase {
    Reg,
    Rsp,
    Stack(StackSlot),
}

impl fmt::Debug for AddrBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg => write!(f, "reg"),
            Self::Rsp => write!(f, "rsp"),
            Self::Stack(slot) => write!(f, "{slot}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct AddrMode {
    pub base: Option<AddrBase>,
    pub index: Option<IndexScale>,
    pub offset: i32,
}

impl fmt::Debug for AddrMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        if let Some(base) = self.base {
            write!(f, "{base:?} + ")?;
        }
        if let Some(index) = self.index {
            write!(f, "{index:?} + ")?;
        }
        write!(f, "{}", self.offset)?;
        write!(f, "]")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum X64Instr {
    AluRRm(OperandSize, AluBinOp),
    AluRmI(OperandSize, AluBinOp, i32),
    AluRm(OperandSize, AluUnOp),
    ImulRRm(OperandSize),
    ImulRRmI(OperandSize, i32),
    ShiftRmR(OperandSize, ShiftOp),
    ShiftRmI(OperandSize, ShiftOp, u8),
    Div(OperandSize, DivOp),
    ConvertWord(OperandSize),
    MovRmS32(i32),
    MovRU32(u32),
    MovRI64(u64),
    MovzxRRm(FullOperandSize),
    MovsxRRm(ExtWidth),
    Setcc(CondCode),
    /// Load from [rbp + offset]
    MovRRbp {
        op_size: FullOperandSize,
        offset: i32,
    },
    MovsdRRbp {
        offset: i32,
    },
    StackAddr(StackSlot),
    Push,
    AddSp(i32),
    MovRM(FullOperandSize, AddrMode),
    MovMR(FullOperandSize, AddrMode),
    MovsdMR(AddrMode),
    Ret,
    FuncAddrRel(FunctionRef),
    FuncAddrAbs(FunctionRef),
    CallRel(FunctionRef),
    CallRm,
    Jump(Block),
    Jumpcc(CondCode, Block, Block),
    Ud2,
}

impl X64Instr {
    fn uses_flags(&self) -> bool {
        match self {
            X64Instr::AluRRm(..) => false,
            X64Instr::AluRmI(..) => false,
            X64Instr::AluRm(..) => false,
            X64Instr::ImulRRm(..) => false,
            X64Instr::ImulRRmI(..) => false,
            X64Instr::ShiftRmR(..) => false,
            X64Instr::ShiftRmI(..) => false,
            X64Instr::Div(..) => false,
            X64Instr::ConvertWord(..) => false,
            X64Instr::MovRmS32(..) => false,
            X64Instr::MovRU32(..) => false,
            X64Instr::MovRI64(..) => false,
            X64Instr::MovzxRRm(..) => false,
            X64Instr::MovsxRRm(..) => false,
            X64Instr::Setcc(..) => true,
            X64Instr::MovRRbp { .. } => false,
            X64Instr::MovsdRRbp { .. } => false,
            X64Instr::StackAddr(..) => false,
            X64Instr::Push => false,
            X64Instr::AddSp(..) => false,
            X64Instr::MovRM(..) => false,
            X64Instr::MovMR(..) => false,
            X64Instr::MovsdMR(..) => false,
            X64Instr::Ret => false,
            X64Instr::FuncAddrRel(..) => false,
            X64Instr::FuncAddrAbs(..) => false,
            X64Instr::CallRel(..) => false,
            X64Instr::CallRm => false,
            X64Instr::Jump(..) => false,
            X64Instr::Jumpcc(..) => true,
            X64Instr::Ud2 => false,
        }
    }

    fn defines_flags(&self) -> bool {
        match self {
            X64Instr::AluRRm(..) => true,
            X64Instr::AluRmI(..) => true,
            X64Instr::AluRm(..) => true,
            X64Instr::ImulRRm(..) => true,
            X64Instr::ImulRRmI(..) => true,
            X64Instr::ShiftRmR(..) => true,
            X64Instr::ShiftRmI(..) => true,
            X64Instr::Div(..) => true,
            X64Instr::ConvertWord(..) => false,
            X64Instr::MovRmS32(..) => false,
            X64Instr::MovRU32(..) => false,
            X64Instr::MovRI64(..) => false,
            X64Instr::MovzxRRm(..) => false,
            X64Instr::MovsxRRm(..) => false,
            X64Instr::Setcc(..) => false,
            X64Instr::MovRRbp { .. } => false,
            X64Instr::MovsdRRbp { .. } => false,
            X64Instr::StackAddr(..) => false,
            X64Instr::Push => false,
            X64Instr::AddSp(..) => true,
            X64Instr::MovRM(..) => false,
            X64Instr::MovMR(..) => false,
            X64Instr::MovsdMR(..) => false,
            X64Instr::Ret => false,
            X64Instr::FuncAddrRel(..) => false,
            X64Instr::FuncAddrAbs(..) => false,
            X64Instr::CallRel(..) => false,
            X64Instr::CallRm => false,
            X64Instr::Jump(..) => false,
            X64Instr::Jumpcc(..) => false,
            X64Instr::Ud2 => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CodeModel {
    SmallPic,
    LargeAbs,
}

#[derive(Debug, Clone, Copy)]
pub struct X64MachineConfig {
    pub internal_code_model: CodeModel,
    pub extern_code_model: CodeModel,
}

impl Default for X64MachineConfig {
    fn default() -> Self {
        Self {
            internal_code_model: CodeModel::SmallPic,
            extern_code_model: CodeModel::LargeAbs,
        }
    }
}

#[derive(Default)]
pub struct X64Machine {
    config: X64MachineConfig,
}

impl X64Machine {
    pub fn new(config: X64MachineConfig) -> Self {
        Self { config }
    }

    fn code_model_for_function(&self, func: FunctionRef) -> CodeModel {
        match func {
            FunctionRef::Internal(_) => self.config.internal_code_model,
            FunctionRef::External(_) => self.config.extern_code_model,
        }
    }
}

impl MachineCore for X64Machine {
    type Instr = X64Instr;

    fn reg_class_name(class: RegClass) -> &'static str {
        match class {
            RC_GPR => "gpr",
            RC_XMM => "xmm",
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
            REG_XMM0 => "xmm0",
            REG_XMM1 => "xmm1",
            REG_XMM2 => "xmm2",
            REG_XMM3 => "xmm3",
            REG_XMM4 => "xmm4",
            REG_XMM5 => "xmm5",
            REG_XMM6 => "xmm6",
            REG_XMM7 => "xmm7",
            REG_XMM8 => "xmm8",
            REG_XMM9 => "xmm9",
            REG_XMM10 => "xmm10",
            REG_XMM11 => "xmm11",
            REG_XMM12 => "xmm12",
            REG_XMM13 => "xmm13",
            REG_XMM14 => "xmm14",
            REG_XMM15 => "xmm15",
            _ => panic!("unknown physical register"),
        }
    }
}

impl MachineRegalloc for X64Machine {
    fn phys_reg_count() -> u32 {
        32
    }

    fn usable_regs(&self, class: RegClass) -> &[PhysReg] {
        match class {
            RC_GPR => &GPR_ORDER,
            RC_XMM => &XMM_ORDER,
            _ => panic!("unknown register class"),
        }
    }

    fn reg_class_spill_layout(&self, class: RegClass) -> MemLayout {
        match class {
            RC_GPR => MemLayout { size: 8, align: 8 },
            RC_XMM => MemLayout {
                size: 16,
                align: 16,
            },
            _ => panic!("unknown register class"),
        }
    }

    fn remat_cost(&self, instr: &X64Instr) -> Option<RematCost> {
        match instr {
            X64Instr::MovRmS32(..)
            | X64Instr::MovRU32(..)
            | X64Instr::FuncAddrRel(..)
            | X64Instr::StackAddr(..) => Some(RematCost::CheapAsCopy),
            X64Instr::MovRI64(..)
            | X64Instr::FuncAddrAbs(..)
            | X64Instr::MovRRbp { .. }
            | X64Instr::MovsdRRbp { .. } => Some(RematCost::CheapAsLoad),
            _ => None,
        }
    }
}

const RC_GPR: RegClass = RegClass::new(0);
const RC_XMM: RegClass = RegClass::new(1);

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

const REG_XMM0: PhysReg = PhysReg::new(16);
const REG_XMM1: PhysReg = PhysReg::new(17);
const REG_XMM2: PhysReg = PhysReg::new(18);
const REG_XMM3: PhysReg = PhysReg::new(19);
const REG_XMM4: PhysReg = PhysReg::new(20);
const REG_XMM5: PhysReg = PhysReg::new(21);
const REG_XMM6: PhysReg = PhysReg::new(22);
const REG_XMM7: PhysReg = PhysReg::new(23);
const REG_XMM8: PhysReg = PhysReg::new(24);
const REG_XMM9: PhysReg = PhysReg::new(25);
const REG_XMM10: PhysReg = PhysReg::new(26);
const REG_XMM11: PhysReg = PhysReg::new(27);
const REG_XMM12: PhysReg = PhysReg::new(28);
const REG_XMM13: PhysReg = PhysReg::new(29);
const REG_XMM14: PhysReg = PhysReg::new(30);
const REG_XMM15: PhysReg = PhysReg::new(31);

const CALLER_SAVED_REGS: [PhysReg; 25] = [
    REG_RAX, REG_RCX, REG_RDX, REG_RDI, REG_RSI, REG_R8, REG_R9, REG_R10, REG_R11, REG_XMM0,
    REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7, REG_XMM8, REG_XMM9,
    REG_XMM10, REG_XMM11, REG_XMM12, REG_XMM13, REG_XMM14, REG_XMM15,
];

const CALLEE_SAVED_REGS: [PhysReg; 5] = [REG_RBX, REG_R12, REG_R13, REG_R14, REG_R15];

// This should be a concatenation of `CALLER_SAVED_REGS` and `CALLEE_SAVED_REGS`.
// Prefer caller-saved regs to avoid unnecessary prologue/epilogue code.
const GPR_ORDER: [PhysReg; 14] = [
    REG_RAX, REG_RCX, REG_RDX, REG_RDI, REG_RSI, REG_R8, REG_R9, REG_R10, REG_R11, REG_RBX,
    REG_R12, REG_R13, REG_R14, REG_R15,
];

const XMM_ORDER: [PhysReg; 16] = [
    REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7, REG_XMM8,
    REG_XMM9, REG_XMM10, REG_XMM11, REG_XMM12, REG_XMM13, REG_XMM14, REG_XMM15,
];
