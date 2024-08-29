// Until things settle down a bit:
#![allow(dead_code, unused)]

use crate::{
    emit::{CodeBuffer, EmitContext},
    frame::FrameLayout,
    lir::{Lir, PhysReg, PhysRegSet},
    machine::{FixupKind, MachineEmit},
    regalloc::{Assignment, OperandAssignment},
    target::x86_64::CALLEE_SAVED_REGS,
};

use super::{
    AluOp, OperandSize, X64Instr, X64Machine, REG_R10, REG_R11, REG_R12, REG_R13, REG_R14, REG_R15,
    REG_R8, REG_R9, REG_RAX, REG_RBP, REG_RBX, REG_RCX, REG_RDI, REG_RDX, REG_RSI, REG_RSP,
};

#[derive(Debug, Clone, Copy)]
pub enum X64Fixup {}

impl FixupKind for X64Fixup {
    fn byte_size(&self) -> usize {
        todo!()
    }

    fn apply(&self, offset: u32, label_offset: u32, bytes: &mut [u8]) {
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
    frame_layout: FrameLayout,
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
        let mut raw_frame_size = full_frame_layout.size;

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
            frame_layout,
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
            // TODO: a push can be smaller for frames of size 8.
            emit_sub_r64i32(buffer, REG_RSP, frame_info.raw_frame_size);
        }

        if let FrameRealign::AlignTo(align) = frame_info.realign {
            todo!("realign frame to {align}");
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
            &X64Instr::AluRRm(op_size, AluOp::Add) => {
                // TODO: spilled src.
                let dest = defs[0].as_reg().unwrap();
                let src = uses[1].as_reg().unwrap();
                emit_add_rr(buffer, op_size, dest, src);
            }
            X64Instr::Ret => {
                emit_epilogue(buffer, &ctx.frame_info);
                buffer.emit(&[0xc3]);
            }
            _ => todo!(),
        }
    }

    fn emit_copy(
        &self,
        ctx: &EmitContext<Self>,
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
        FrameRestoreMethod::AddSp(offset) => emit_add_r64i32(buffer, REG_RSP, offset),
        FrameRestoreMethod::FromRbp => emit_mov_rr(buffer, REG_RSP, REG_RBP),
    }

    for saved_reg in frame_info.saved_regs.iter() {
        emit_pop(buffer, saved_reg);
    }

    emit_pop(buffer, REG_RBP);
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

fn emit_mov_rr(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, src: PhysReg) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(OperandSize::S64);

    let reg = rex.encode_modrm_reg(src);
    let rm = rex.encode_modrm_rm(dest);

    rex.emit(buffer);
    buffer.emit(&[0x89, encode_modrm(MODE_R, reg, rm)]);
}

fn emit_add_rr(
    buffer: &mut CodeBuffer<X64Machine>,
    op_size: OperandSize,
    dest: PhysReg,
    src: PhysReg,
) {
    emit_alu_rr(buffer, 0x1, op_size, dest, src);
}

fn emit_add_r64i32(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, imm: i32) {
    emit_alu_r64i32(buffer, 0x81, 0, dest, imm);
}

fn emit_sub_r64i32(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, imm: i32) {
    emit_alu_r64i32(buffer, 0x81, 5, dest, imm);
}

// Instruction group emission helpers

// TODO: use imm8 when small enough.
fn emit_alu_r64i32(
    buffer: &mut CodeBuffer<X64Machine>,
    opcode: u8,
    reg_opcode: u8,
    dest: PhysReg,
    imm: i32,
) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(OperandSize::S64);
    let rm = rex.encode_modrm_rm(dest);
    rex.emit(buffer);
    buffer.emit(&[opcode, encode_modrm(MODE_R, reg_opcode, rm)]);
    buffer.emit(&imm.to_le_bytes());
}

fn emit_alu_rr(
    buffer: &mut CodeBuffer<X64Machine>,
    opcode: u8,
    op_size: OperandSize,
    dest: PhysReg,
    src: PhysReg,
) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);
    rex.emit(buffer);
    let rm = rex.encode_modrm_rm(dest);
    let reg = rex.encode_modrm_reg(src);
    buffer.emit(&[opcode, encode_modrm(MODE_R, reg, rm)]);
}

// Prefixes and encoding

// Addressing modes for the ModRM byte
const MODE_R: u8 = 0b11;

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

    fn emit(&self, buffer: &mut CodeBuffer<X64Machine>) {
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
