use core::mem;

use crate::{
    emit::{CodeBuffer, EmitContext},
    frame::FrameLayout,
    lir::{Lir, PhysReg, PhysRegSet},
    machine::{FixupKind, MachineEmit},
    num_utils::{align_up, is_sint, is_uint},
    regalloc::{Assignment, OperandAssignment},
    target::x86_64::CALLEE_SAVED_REGS,
};

use super::{
    AluOp, CondCode, OperandSize, ShiftOp, X64Instr, X64Machine, REG_R10, REG_R11, REG_R12,
    REG_R13, REG_R14, REG_R15, REG_R8, REG_R9, REG_RAX, REG_RBP, REG_RBX, REG_RCX, REG_RDI,
    REG_RDX, REG_RSI, REG_RSP,
};

#[derive(Debug, Clone, Copy)]
pub enum X64Fixup {}

impl FixupKind for X64Fixup {
    fn byte_size(&self) -> usize {
        todo!()
    }

    fn apply(&self, _offset: u32, _label_offset: u32, _bytes: &mut [u8]) {
        todo!()
    }
}

enum FrameRealign {
    None,
    AlignTo(i32),
}

enum FrameRestoreMethod {
    None,
    AddSp(i32),
    FromRbp,
}

pub struct X64FrameInfo {
    _frame_layout: FrameLayout,
    saved_regs: PhysRegSet,
    raw_frame_size: i32,
    realign: FrameRealign,
    restore_method: FrameRestoreMethod,
}

const DEFAULT_FRAME_ALIGN: u32 = 16;

impl MachineEmit for X64Machine {
    type FrameInfo = X64FrameInfo;
    type Fixup = X64Fixup;

    fn compute_frame_info(&self, lir: &Lir<Self>, assignment: &Assignment) -> X64FrameInfo {
        let frame_layout = FrameLayout::compute(lir, assignment);
        let saved_regs =
            &assignment.compute_global_clobbers(lir) & &PhysRegSet::from_iter(CALLEE_SAVED_REGS);

        let full_frame_layout = frame_layout.full_layout;
        let mut raw_frame_size = align_up(full_frame_layout.size, DEFAULT_FRAME_ALIGN);

        let realign = if full_frame_layout.align > DEFAULT_FRAME_ALIGN {
            FrameRealign::AlignTo(full_frame_layout.align.try_into().unwrap())
        } else {
            if saved_regs.len() % 2 != 0 {
                // The stack will be 8 bytes off from 16 after all registers are saved, so fix it as
                // part of the frame.
                raw_frame_size += 8;
            }
            FrameRealign::None
        };

        let raw_frame_size = raw_frame_size.try_into().unwrap();

        let restore_method = if matches!(realign, FrameRealign::AlignTo(_)) {
            // We won't actually know how much to add, so always use rbp.
            FrameRestoreMethod::FromRbp
        } else if raw_frame_size != 0 {
            FrameRestoreMethod::AddSp(raw_frame_size)
        } else {
            FrameRestoreMethod::None
        };

        X64FrameInfo {
            _frame_layout: frame_layout,
            saved_regs,
            raw_frame_size,
            realign,
            restore_method,
        }
    }

    fn emit_prologue(&self, ctx: &EmitContext<Self>, buffer: &mut CodeBuffer<Self>) {
        let frame_info = &ctx.frame_info;

        emit_push(buffer, REG_RBP);
        emit_mov_rr(buffer, REG_RBP, REG_RSP);

        for saved_reg in frame_info.saved_regs.iter().rev() {
            emit_push(buffer, saved_reg);
        }

        if frame_info.raw_frame_size > 0 {
            emit_add_sp(buffer, -frame_info.raw_frame_size);
        }

        if let FrameRealign::AlignTo(align) = frame_info.realign {
            emit_alu_r64i(buffer, AluOp::And, REG_RSP, -align);
        }
    }

    fn emit_instr(
        &self,
        ctx: &EmitContext<Self>,
        buffer: &mut CodeBuffer<Self>,
        instr: &X64Instr,
        defs: &[OperandAssignment],
        uses: &[OperandAssignment],
    ) {
        match instr {
            &X64Instr::AluRRm(op_size, op @ (AluOp::Cmp | AluOp::Test)) => {
                // `cmp` and `test` are special because they don't have explicit outputs.

                // TODO: spilled arg0.
                let arg0 = uses[0].as_reg().unwrap();
                let arg1 = uses[1].as_reg().unwrap();
                emit_alu_rr(buffer, op, op_size, arg0, arg1);
            }
            &X64Instr::AluRRm(op_size, op) => {
                // TODO: spilled src.
                let dest = defs[0].as_reg().unwrap();
                let src = uses[1].as_reg().unwrap();
                emit_alu_rr(buffer, op, op_size, dest, src);
            }
            &X64Instr::ShiftRmR(op_size, op) => {
                // TODO: spilled arg.
                emit_shift_r_cx(buffer, op, op_size, uses[0].as_reg().unwrap())
            }
            // TODO: spilled divisor.
            &X64Instr::Div(op_size) => emit_div_r(buffer, 0x6, op_size, uses[2].as_reg().unwrap()),
            &X64Instr::Idiv(op_size) => emit_div_r(buffer, 0x7, op_size, uses[2].as_reg().unwrap()),
            &X64Instr::ConvertWord(op_size) => {
                let mut rex = RexPrefix::new();
                rex.encode_operand_size(op_size);
                rex.emit(buffer);
                buffer.emit(&[0x99]);
            }
            &X64Instr::Setcc(code) => {
                let dest = defs[0].as_reg().unwrap();
                emit_setcc_r(buffer, code, dest);
            }
            &X64Instr::MovRI(val) => emit_mov_ri(buffer, defs[0].as_reg().unwrap(), val),
            X64Instr::MovRZ => {
                let dest = defs[0].as_reg().unwrap();
                // Note: renamers often recognize only the 32-bit instruction as a zeroing idiom.
                emit_alu_rr(buffer, AluOp::Xor, OperandSize::S32, dest, dest);
            }
            &X64Instr::MovRRbp { offset } => {
                emit_mov_r_rbp_disp(buffer, defs[0].as_reg().unwrap(), offset)
            }
            &X64Instr::AddSp(offset) => emit_add_sp(buffer, offset),
            X64Instr::Ret => {
                emit_epilogue(buffer, &ctx.frame_info);
                buffer.emit(&[0xc3]);
            }
            _ => todo!(),
        }
    }

    fn emit_copy(
        &self,
        _ctx: &EmitContext<Self>,
        buffer: &mut CodeBuffer<Self>,
        from: OperandAssignment,
        to: OperandAssignment,
    ) {
        // TODO: spills.
        emit_mov_rr(buffer, to.as_reg().unwrap(), from.as_reg().unwrap());
    }
}

// Code sequence emission helpers

fn emit_epilogue(buffer: &mut CodeBuffer<X64Machine>, frame_info: &X64FrameInfo) {
    match frame_info.restore_method {
        FrameRestoreMethod::None => {}
        FrameRestoreMethod::AddSp(offset) => emit_add_sp(buffer, offset),
        FrameRestoreMethod::FromRbp => emit_mov_rr(buffer, REG_RSP, REG_RBP),
    }

    for saved_reg in frame_info.saved_regs.iter() {
        emit_pop(buffer, saved_reg);
    }

    emit_pop(buffer, REG_RBP);
}

fn emit_add_sp(buffer: &mut CodeBuffer<X64Machine>, offset: i32) {
    if offset == -8 {
        // This generates smaller code without penalizing performance.
        emit_push(buffer, REG_RAX);
    } else if offset < 0 {
        emit_alu_r64i(buffer, AluOp::Sub, REG_RSP, -offset);
    } else {
        emit_alu_r64i(buffer, AluOp::Add, REG_RSP, offset);
    }
}

// Single-instruction emission helpers

fn emit_push(buffer: &mut CodeBuffer<X64Machine>, reg: PhysReg) {
    let mut rex = RexPrefix::new();
    let reg = rex.encode_modrm_rm(reg);
    rex.emit(buffer);
    buffer.emit(&[0x50 + reg]);
}

fn emit_pop(buffer: &mut CodeBuffer<X64Machine>, reg: PhysReg) {
    let mut rex = RexPrefix::new();
    let reg = rex.encode_modrm_rm(reg);
    rex.emit(buffer);
    buffer.emit(&[0x58 + reg]);
}

fn emit_mov_r_rbp_disp(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, disp: i32) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(OperandSize::S64);

    let dest = rex.encode_modrm_reg(dest);

    let mode = if disp == 0 {
        MODE_M
    } else if is_sint::<8>(disp as u64) {
        MODE_M_DISP8
    } else {
        MODE_M_DISP32
    };

    rex.emit(buffer);
    buffer.emit(&[0x8b, encode_modrm(mode, dest, RM_BP)]);
    match mode {
        MODE_M => {}
        MODE_M_DISP8 => buffer.emit(&[disp as u8]),
        MODE_M_DISP32 => buffer.emit(&disp.to_le_bytes()),
        _ => unreachable!(),
    }
}

fn emit_mov_rr(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, src: PhysReg) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(OperandSize::S64);

    let reg = rex.encode_modrm_reg(src);
    let rm = rex.encode_modrm_rm(dest);

    rex.emit(buffer);
    buffer.emit(&[0x89, encode_modrm(MODE_R, reg, rm)]);
}

fn emit_setcc_r(buffer: &mut CodeBuffer<X64Machine>, code: CondCode, dest: PhysReg) {
    let mut rex = RexPrefix::new();
    let rm = rex.encode_modrm_rm(dest);
    rex.emit(buffer);
    buffer.emit(&[
        0xf,
        0x90 | encode_cond_code(code),
        encode_modrm(MODE_R, 0, rm),
    ]);
}

// Instruction group emission helpers

fn emit_alu_rr(
    buffer: &mut CodeBuffer<X64Machine>,
    op: AluOp,
    op_size: OperandSize,
    arg0: PhysReg,
    arg1: PhysReg,
) {
    // By default, use the more "canonical" `op r/m, r` encoding.
    let mut rm = arg0;
    let mut reg = arg1;

    let opcode: &[u8] = match op {
        AluOp::Add => &[0x1],
        AluOp::And => &[0x21],
        AluOp::Cmp => &[0x39],
        AluOp::Or => &[0x9],
        AluOp::Sub => &[0x29],
        AluOp::Test => &[0x85],
        AluOp::Xor => &[0x31],
        AluOp::Imul => {
            // `imul` only has the `imul r, r/m` form.
            mem::swap(&mut rm, &mut reg);
            &[0xf, 0xaf]
        }
    };

    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);

    let rm = rex.encode_modrm_rm(rm);
    let reg = rex.encode_modrm_reg(reg);

    rex.emit(buffer);
    buffer.emit(opcode);
    buffer.emit(&[encode_modrm(MODE_R, reg, rm)]);
}

fn emit_div_r(
    buffer: &mut CodeBuffer<X64Machine>,
    reg_opcode: u8,
    op_size: OperandSize,
    arg: PhysReg,
) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);
    let arg = rex.encode_modrm_rm(arg);
    rex.emit(buffer);
    buffer.emit(&[0xf7, encode_modrm(MODE_R, reg_opcode, arg)]);
}

fn emit_shift_r_cx(
    buffer: &mut CodeBuffer<X64Machine>,
    op: ShiftOp,
    op_size: OperandSize,
    arg: PhysReg,
) {
    let reg_opcode = match op {
        ShiftOp::Shl => 4,
        ShiftOp::Shr => 5,
        ShiftOp::Sar => 7,
    };

    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);
    let arg = rex.encode_modrm_rm(arg);
    rex.emit(buffer);
    buffer.emit(&[0xd3, encode_modrm(MODE_R, reg_opcode, arg)]);
}

fn emit_mov_ri(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, imm: u64) {
    let mut rex = RexPrefix::new();
    let dest = rex.encode_modrm_rm(dest);

    if is_uint::<32>(imm) {
        // Smallest case: move imm32 to r32, clearing upper bits.
        rex.emit(buffer);
        buffer.emit(&[0xb8 + dest]);
        buffer.emit(&(imm as u32).to_le_bytes());
    } else if is_sint::<32>(imm) {
        // Next smallest case: sign-extend imm32 to r64.
        rex.encode_operand_size(OperandSize::S64);
        rex.emit(buffer);
        buffer.emit(&[0xc7, encode_modrm(MODE_R, 0, dest)]);
        buffer.emit(&(imm as u32).to_le_bytes());
    } else {
        // Large case: use full 64-bit immediate.
        rex.encode_operand_size(OperandSize::S64);
        rex.emit(buffer);
        buffer.emit(&[0xb8 + dest]);
        buffer.emit(&imm.to_le_bytes());
    }
}

fn emit_alu_r64i(buffer: &mut CodeBuffer<X64Machine>, op: AluOp, dest: PhysReg, imm: i32) {
    let mut is_imm8 = is_sint::<8>(imm as u64);
    let mut opcode = if is_imm8 { 0x83 } else { 0x81 };

    let reg = match op {
        AluOp::Add => 0x0,
        AluOp::And => 0x4,
        AluOp::Cmp => 0x7,
        AluOp::Or => 0x1,
        AluOp::Sub => 0x5,
        AluOp::Test => {
            // `test` is special. Who knows why.
            opcode = 0xf7;
            is_imm8 = false;
            0x0
        }
        AluOp::Xor => 0x6,
        AluOp::Imul => unimplemented!(),
    };

    let mut rex = RexPrefix::new();
    rex.encode_operand_size(OperandSize::S64);
    let rm = rex.encode_modrm_rm(dest);
    rex.emit(buffer);
    buffer.emit(&[opcode, encode_modrm(MODE_R, reg, rm)]);

    if is_imm8 {
        buffer.emit(&[imm as u8]);
    } else {
        buffer.emit(&imm.to_le_bytes());
    }
}

// Prefixes and encoding

// Addressing modes for the ModRM byte
const MODE_M: u8 = 0b00;
const MODE_M_DISP8: u8 = 0b01;
const MODE_M_DISP32: u8 = 0b10;
const MODE_R: u8 = 0b11;

// RM encodings for memory addressing modes
const RM_BP: u8 = 0b101;

fn encode_modrm(mode: u8, reg: u8, rm: u8) -> u8 {
    (mode << 6) | (reg << 3) | rm
}

struct RexPrefix {
    b: bool,
    x: bool,
    r: bool,
    w: bool,
}

impl RexPrefix {
    fn new() -> Self {
        Self {
            b: false,
            x: false,
            r: false,
            w: false,
        }
    }

    fn encode_operand_size(&mut self, size: OperandSize) {
        if size == OperandSize::S64 {
            self.w = true;
        }
    }

    fn encode_modrm_reg(&mut self, reg: PhysReg) -> u8 {
        let reg = encode_reg(reg);
        self.r = reg & 0b1000 != 0;
        reg & 0b111
    }

    fn encode_modrm_rm(&mut self, reg: PhysReg) -> u8 {
        let reg = encode_reg(reg);
        self.b = reg & 0b1000 != 0;
        reg & 0b111
    }

    fn emit(self, buffer: &mut CodeBuffer<X64Machine>) {
        let value = 0x40
            | (self.b as u8)
            | ((self.x as u8) << 1)
            | ((self.r as u8) << 2)
            | ((self.w as u8) << 3);
        if value != 0x40 {
            buffer.emit(&[value]);
        }
    }
}

fn encode_reg(reg: PhysReg) -> u8 {
    match reg {
        REG_RAX => 0,
        REG_RBX => 3,
        REG_RCX => 1,
        REG_RDX => 2,
        REG_RDI => 7,
        REG_RSI => 6,
        REG_RBP => 5,
        REG_RSP => 4,

        REG_R8 => 8,
        REG_R9 => 9,
        REG_R10 => 10,
        REG_R11 => 11,
        REG_R12 => 12,
        REG_R13 => 13,
        REG_R14 => 14,
        REG_R15 => 15,

        _ => unreachable!("unknown register"),
    }
}

fn encode_cond_code(code: CondCode) -> u8 {
    match code {
        CondCode::O => 0x0,
        CondCode::No => 0x1,
        CondCode::B => 0x2,
        CondCode::Ae => 0x3,
        CondCode::E => 0x4,
        CondCode::Ne => 0x5,
        CondCode::Be => 0x6,
        CondCode::A => 0x7,
        CondCode::S => 0x8,
        CondCode::Ns => 0x9,
        CondCode::P => 0xa,
        CondCode::Np => 0xb,
        CondCode::L => 0xc,
        CondCode::Ge => 0xd,
        CondCode::Le => 0xe,
        CondCode::G => 0xf,
    }
}
