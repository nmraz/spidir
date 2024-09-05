use core::mem;

use crate::{
    emit::{BlockLabelMap, CodeBuffer},
    frame::FrameLayout,
    lir::{Lir, PhysReg, PhysRegSet, StackSlot},
    machine::{FixupKind, MachineEmit},
    num_utils::{align_up, is_sint, is_uint},
    regalloc::{Assignment, OperandAssignment},
    target::x86_64::CALLEE_SAVED_REGS,
};

use super::{
    AluOp, CondCode, ExtWidth, FullOperandSize, IndexScale, OperandSize, ShiftOp, X64Instr,
    X64Machine, REG_R10, REG_R11, REG_R12, REG_R13, REG_R14, REG_R15, REG_R8, REG_R9, REG_RAX,
    REG_RBP, REG_RBX, REG_RCX, REG_RDI, REG_RDX, REG_RSI, REG_RSP,
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

pub struct X64EmitState {
    frame_layout: FrameLayout,
    saved_regs: PhysRegSet,
    raw_frame_size: i32,
    realign: FrameRealign,
    restore_method: FrameRestoreMethod,
}

impl X64EmitState {
    fn stack_slot_addr(&self, slot: StackSlot) -> BaseIndexOff {
        let offset = self.frame_layout.stack_slot_offsets[slot];
        BaseIndexOff {
            base: Some(REG_RSP),
            index: None,
            disp: offset.try_into().unwrap(),
        }
    }
}

const DEFAULT_FRAME_ALIGN: u32 = 16;

impl MachineEmit for X64Machine {
    type EmitState = X64EmitState;
    type Fixup = X64Fixup;

    fn prepare_state(&self, lir: &Lir<Self>, assignment: &Assignment) -> X64EmitState {
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

        X64EmitState {
            frame_layout,
            saved_regs,
            raw_frame_size,
            realign,
            restore_method,
        }
    }

    fn emit_prologue(&self, state: &mut X64EmitState, buffer: &mut CodeBuffer<Self>) {
        emit_push(buffer, REG_RBP);
        emit_mov_rr(buffer, REG_RBP, REG_RSP);

        for saved_reg in state.saved_regs.iter().rev() {
            emit_push(buffer, saved_reg);
        }

        if state.raw_frame_size > 0 {
            emit_add_sp(buffer, -state.raw_frame_size);
        }

        if let FrameRealign::AlignTo(align) = state.realign {
            emit_alu_r64i(buffer, AluOp::And, REG_RSP, -align);
        }
    }

    fn emit_instr(
        &self,
        state: &mut X64EmitState,
        buffer: &mut CodeBuffer<Self>,
        _block_labels: &BlockLabelMap,
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
            &X64Instr::ShiftRmI(op_size, op, imm) => {
                // TODO: spilled arg.
                emit_shift_ri(buffer, op, op_size, uses[0].as_reg().unwrap(), imm)
            }
            &X64Instr::Div(op_size) => {
                // TODO: spilled divisor.
                emit_div_r(buffer, 0x6, op_size, uses[2].as_reg().unwrap())
            }
            &X64Instr::Idiv(op_size) => {
                // TODO: spilled divisor.
                emit_div_r(buffer, 0x7, op_size, uses[2].as_reg().unwrap())
            }
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
            &X64Instr::MovsxRRm(width) => {
                // TODO: spilled arg.
                emit_movsx_rr(
                    buffer,
                    width,
                    defs[0].as_reg().unwrap(),
                    uses[0].as_reg().unwrap(),
                )
            }
            &X64Instr::MovRRbp { offset } => emit_mov_rm(
                buffer,
                FullOperandSize::S64,
                defs[0].as_reg().unwrap(),
                BaseIndexOff {
                    base: Some(REG_RBP),
                    index: None,
                    disp: offset,
                },
            ),
            &X64Instr::AddSp(offset) => emit_add_sp(buffer, offset),
            &X64Instr::MovRM(full_op_size) => emit_mov_rm(
                buffer,
                full_op_size,
                defs[0].as_reg().unwrap(),
                BaseIndexOff {
                    base: Some(uses[0].as_reg().unwrap()),
                    index: None,
                    disp: 0,
                },
            ),
            &X64Instr::MovRStack(slot, full_op_size) => emit_mov_rm(
                buffer,
                full_op_size,
                defs[0].as_reg().unwrap(),
                state.stack_slot_addr(slot),
            ),
            &X64Instr::MovMR(full_op_size) => emit_mov_mr(
                buffer,
                full_op_size,
                BaseIndexOff {
                    base: Some(uses[0].as_reg().unwrap()),
                    index: None,
                    disp: 0,
                },
                uses[1].as_reg().unwrap(),
            ),
            &X64Instr::MovStackR(slot, full_op_size) => emit_mov_mr(
                buffer,
                full_op_size,
                state.stack_slot_addr(slot),
                uses[0].as_reg().unwrap(),
            ),
            &X64Instr::StackAddr(slot) => emit_lea(
                buffer,
                defs[0].as_reg().unwrap(),
                state.stack_slot_addr(slot),
            ),
            X64Instr::Ret => {
                emit_epilogue(buffer, state);
                emit_ret(buffer);
            }
            X64Instr::Ud2 => emit_ud2(buffer),
            _ => todo!(),
        }
    }

    fn emit_copy(
        &self,
        _state: &mut X64EmitState,
        buffer: &mut CodeBuffer<Self>,
        from: OperandAssignment,
        to: OperandAssignment,
    ) {
        // TODO: spills.
        emit_mov_rr(buffer, to.as_reg().unwrap(), from.as_reg().unwrap());
    }
}

// Code sequence emission helpers

fn emit_epilogue(buffer: &mut CodeBuffer<X64Machine>, state: &X64EmitState) {
    match state.restore_method {
        FrameRestoreMethod::None => {}
        FrameRestoreMethod::AddSp(offset) => emit_add_sp(buffer, offset),
        FrameRestoreMethod::FromRbp => emit_mov_rr(buffer, REG_RSP, REG_RBP),
    }

    for saved_reg in state.saved_regs.iter() {
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

fn emit_mov_rr(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, src: PhysReg) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(OperandSize::S64);

    let reg = rex.encode_modrm_reg(src);
    let rm = rex.encode_modrm_rm(dest);

    rex.emit(buffer);
    buffer.emit(&[0x89, encode_modrm_r(reg, rm)]);
}

fn emit_mov_rm(
    buffer: &mut CodeBuffer<X64Machine>,
    full_op_size: FullOperandSize,
    dest: PhysReg,
    addr: BaseIndexOff,
) {
    // For anything less than 64 bits, write to a 32-bit register, which will implicitly clear the
    // high bits.
    let op_size = match full_op_size {
        FullOperandSize::S64 => OperandSize::S64,
        _ => OperandSize::S32,
    };

    let (rex, modrm_sib) = encode_mem_parts(addr, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(dest)
    });

    rex.emit(buffer);

    match full_op_size {
        FullOperandSize::S8 => {
            // Emit a `movzx r32, r/m8`, which will also implicitly clear the high bits.
            buffer.emit(&[0xf, 0xb6])
        }
        FullOperandSize::S16 => {
            // Emit a `movzx r32, r/m16`, which will also implicitly clear the high bits.
            buffer.emit(&[0xf, 0xb7])
        }
        FullOperandSize::S32 | FullOperandSize::S64 => {
            // Emit a simple `mov`, which will implicitly clear the high bits in the 32-bit case.
            buffer.emit(&[0x8b])
        }
    }

    modrm_sib.emit(buffer);
}

fn emit_lea(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, addr: BaseIndexOff) {
    let (rex, modrm_sib) = encode_mem_parts(addr, |rex| {
        rex.encode_operand_size(OperandSize::S64);
        rex.encode_modrm_reg(dest)
    });

    rex.emit(buffer);
    buffer.emit(&[0x8d]);
    modrm_sib.emit(buffer);
}

fn emit_mov_mr(
    buffer: &mut CodeBuffer<X64Machine>,
    full_op_size: FullOperandSize,
    addr: BaseIndexOff,
    src: PhysReg,
) {
    let op_size = match full_op_size {
        FullOperandSize::S64 => OperandSize::S64,
        _ => OperandSize::S32,
    };

    let (rex, modrm_sib) = encode_mem_parts(addr, |rex| {
        rex.encode_operand_size(op_size);
        if full_op_size == FullOperandSize::S8 {
            rex.use_reg8(src);
        }
        rex.encode_modrm_reg(src)
    });

    if full_op_size == FullOperandSize::S16 {
        buffer.emit(&[PREFIX_OPERAND_SIZE]);
    }

    rex.emit(buffer);
    if full_op_size == FullOperandSize::S8 {
        buffer.emit(&[0x88]);
    } else {
        buffer.emit(&[0x89]);
    }

    modrm_sib.emit(buffer);
}

fn emit_setcc_r(buffer: &mut CodeBuffer<X64Machine>, code: CondCode, dest: PhysReg) {
    let mut rex = RexPrefix::new();
    let rm = rex.encode_modrm_rm(dest);
    rex.emit(buffer);
    buffer.emit(&[0xf, 0x90 | encode_cond_code(code), encode_modrm_r(0, rm)]);
}

fn emit_movsx_rr(
    buffer: &mut CodeBuffer<X64Machine>,
    width: ExtWidth,
    dest: PhysReg,
    src: PhysReg,
) {
    let (opcode, op_size): (&[u8], _) = match width {
        ExtWidth::Ext8_32 => (&[0xf, 0xbe], OperandSize::S32),
        ExtWidth::Ext8_64 => (&[0xf, 0xbe], OperandSize::S64),
        ExtWidth::Ext16_32 => (&[0xf, 0xbf], OperandSize::S32),
        ExtWidth::Ext16_64 => (&[0xf, 0xbf], OperandSize::S64),
        ExtWidth::Ext32_64 => (&[0x63], OperandSize::S64),
    };

    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);

    if matches!(width, ExtWidth::Ext8_32 | ExtWidth::Ext8_64) {
        rex.use_reg8(src);
    }

    let dest = rex.encode_modrm_reg(dest);
    let src = rex.encode_modrm_rm(src);

    rex.emit(buffer);
    buffer.emit(opcode);
    buffer.emit(&[encode_modrm_r(dest, src)]);
}

fn emit_ret(buffer: &mut CodeBuffer<X64Machine>) {
    buffer.emit(&[0xc3]);
}

fn emit_ud2(buffer: &mut CodeBuffer<X64Machine>) {
    buffer.emit(&[0xf, 0xb]);
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
    buffer.emit(&[encode_modrm_r(reg, rm)]);
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
    buffer.emit(&[0xf7, encode_modrm_r(reg_opcode, arg)]);
}

fn emit_shift_r_cx(
    buffer: &mut CodeBuffer<X64Machine>,
    op: ShiftOp,
    op_size: OperandSize,
    arg: PhysReg,
) {
    let reg_opcode = encode_shift_op(op);

    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);
    let arg = rex.encode_modrm_rm(arg);
    rex.emit(buffer);
    buffer.emit(&[0xd3, encode_modrm_r(reg_opcode, arg)]);
}

fn emit_shift_ri(
    buffer: &mut CodeBuffer<X64Machine>,
    op: ShiftOp,
    op_size: OperandSize,
    arg: PhysReg,
    imm: u8,
) {
    let (opcode, emit_imm) = if imm == 1 {
        (0xd1, false)
    } else {
        (0xc1, true)
    };

    let reg_opcode = encode_shift_op(op);

    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);
    let arg = rex.encode_modrm_rm(arg);
    rex.emit(buffer);
    buffer.emit(&[opcode, encode_modrm_r(reg_opcode, arg)]);
    if emit_imm {
        buffer.emit(&[imm]);
    }
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
        buffer.emit(&[0xc7, encode_modrm_r(0, dest)]);
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
    buffer.emit(&[opcode, encode_modrm_r(reg, rm)]);

    if is_imm8 {
        buffer.emit(&[imm as u8]);
    } else {
        buffer.emit(&imm.to_le_bytes());
    }
}

// Prefixes and encoding

struct BaseIndexOff {
    base: Option<PhysReg>,
    index: Option<(IndexScale, PhysReg)>,
    disp: i32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum MemMode {
    NoDisp = 0b00,
    Disp8 = 0b01,
    Disp32 = 0b11,
}

struct ModRmSib {
    modrm: u8,
    sib: Option<u8>,
    disp: i32,
    mem_mode: MemMode,
}

impl ModRmSib {
    fn emit(self, buffer: &mut CodeBuffer<X64Machine>) {
        buffer.emit(&[self.modrm]);
        if let Some(sib) = self.sib {
            buffer.emit(&[sib]);
        }
        match self.mem_mode {
            MemMode::NoDisp => {}
            MemMode::Disp8 => buffer.emit(&[self.disp as u8]),
            MemMode::Disp32 => buffer.emit(&self.disp.to_le_bytes()),
        }
    }
}

struct RexPrefix {
    force: bool,
    b: bool,
    x: bool,
    r: bool,
    w: bool,
}

impl RexPrefix {
    fn new() -> Self {
        Self {
            force: false,
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

    fn encode_sib_index(&mut self, reg: PhysReg) -> u8 {
        let reg = encode_reg(reg);
        self.x = reg & 0b1000 != 0;
        reg & 0b111
    }

    fn use_reg8(&mut self, reg: PhysReg) {
        if ![REG_RAX, REG_RBX, REG_RCX, REG_RDX].contains(&reg) {
            // Accessing `sil`, `dil`, etc. always requires a REX prefix.
            self.force = true;
        }
    }

    fn emit(self, buffer: &mut CodeBuffer<X64Machine>) {
        let value = 0x40
            | (self.b as u8)
            | ((self.x as u8) << 1)
            | ((self.r as u8) << 2)
            | ((self.w as u8) << 3);
        if self.force || value != 0x40 {
            buffer.emit(&[value]);
        }
    }
}

fn encode_mem_parts(
    addr: BaseIndexOff,
    get_reg: impl FnOnce(&mut RexPrefix) -> u8,
) -> (RexPrefix, ModRmSib) {
    let mut rex = RexPrefix::new();
    let reg = get_reg(&mut rex);

    let mode = if (addr.disp == 0 && addr.base != Some(REG_RBP)) || addr.base.is_none() {
        MemMode::NoDisp
    } else if is_sint::<8>(addr.disp as u64) {
        MemMode::Disp8
    } else {
        MemMode::Disp32
    };

    let need_sib = !matches!(
        addr.base,
        Some(REG_RAX | REG_RBX | REG_RCX | REG_RDX | REG_RBP | REG_RSI | REG_RDI)
    ) || addr.index.is_some();

    if need_sib {
        let base = addr
            .base
            .map_or(SIB_BASE_NONE, |base| rex.encode_modrm_rm(base));
        let rm = RM_SIB;
        let (scale, index) = match addr.index {
            Some((scale, index)) => {
                let scale = match scale {
                    IndexScale::One => 0,
                    IndexScale::Two => 1,
                    IndexScale::Four => 2,
                    IndexScale::Eight => 3,
                };
                let index = rex.encode_sib_index(index);
                (scale, index)
            }
            None => (0, SIB_INDEX_NONE),
        };

        let sib = (scale << 6) | (index << 3) | base;

        (
            rex,
            ModRmSib {
                modrm: encode_modrm_mem(mode, reg, rm),
                sib: Some(sib),
                disp: addr.disp,
                mem_mode: mode,
            },
        )
    } else {
        // Note: we always need an SIB when no base is specified.
        let rm = encode_reg(addr.base.unwrap());

        (
            rex,
            ModRmSib {
                modrm: encode_modrm_mem(mode, reg, rm),
                sib: None,
                disp: addr.disp,
                mem_mode: mode,
            },
        )
    }
}

fn encode_modrm_r(reg: u8, rm: u8) -> u8 {
    encode_modrm(0b11, reg, rm)
}

fn encode_modrm_mem(mode: MemMode, reg: u8, rm: u8) -> u8 {
    encode_modrm(mode as u8, reg, rm)
}

fn encode_modrm(mode: u8, reg: u8, rm: u8) -> u8 {
    (mode << 6) | (reg << 3) | rm
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

fn encode_shift_op(op: ShiftOp) -> u8 {
    match op {
        ShiftOp::Shl => 4,
        ShiftOp::Shr => 5,
        ShiftOp::Sar => 7,
    }
}

// RM encodings for memory addressing modes
const RM_SIB: u8 = 0b100;

// SIB encodings for lack of base/index registers.
const SIB_BASE_NONE: u8 = 0b101;
const SIB_INDEX_NONE: u8 = 0b100;

// Legacy prefixes
const PREFIX_OPERAND_SIZE: u8 = 0x66;
