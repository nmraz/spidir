use crate::{
    emit::{BlockLabelMap, CodeBuffer, Label},
    frame::FrameLayout,
    lir::{Lir, PhysReg, PhysRegSet, StackSlot},
    machine::{FixupKind, MachineEmit},
    num_utils::{align_up, is_sint, is_uint},
    regalloc::{Assignment, OperandAssignment, SpillSlot},
    target::x86_64::CALLEE_SAVED_REGS,
};

use super::{
    AluOp, CondCode, DivOp, ExtWidth, FullOperandSize, IndexScale, OperandSize, ShiftOp, X64Instr,
    X64Machine, REG_R10, REG_R11, REG_R12, REG_R13, REG_R14, REG_R15, REG_R8, REG_R9, REG_RAX,
    REG_RBP, REG_RBX, REG_RCX, REG_RDI, REG_RDX, REG_RSI, REG_RSP,
};

#[derive(Debug, Clone, Copy)]
pub enum X64Fixup {
    Rela4(i32),
}

impl FixupKind for X64Fixup {
    fn byte_size(&self) -> usize {
        match self {
            X64Fixup::Rela4(_) => 4,
        }
    }

    fn apply(&self, offset: u32, label_offset: u32, bytes: &mut [u8]) {
        match self {
            &X64Fixup::Rela4(addend) => {
                let rel = label_offset.wrapping_sub(offset) as i32 + addend;
                bytes.copy_from_slice(&rel.to_le_bytes());
            }
        }
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
    sp_frame_offset: i32,
    realign: FrameRealign,
    restore_method: FrameRestoreMethod,
}

impl X64EmitState {
    fn operand_reg_mem(&self, operand: OperandAssignment) -> RegMem {
        match operand {
            OperandAssignment::Reg(reg) => RegMem::Reg(reg),
            OperandAssignment::Spill(spill) => RegMem::Mem(self.spill_slot_addr(spill)),
        }
    }

    fn stack_slot_addr(&self, slot: StackSlot) -> BaseIndexOff {
        self.stack_addr(self.frame_layout.stack_slot_offsets[slot])
    }

    fn spill_slot_addr(&self, spill: SpillSlot) -> BaseIndexOff {
        self.stack_addr(self.frame_layout.spill_slot_offsets[spill])
    }

    fn stack_addr(&self, frame_offset: u32) -> BaseIndexOff {
        let offset: i32 = frame_offset.try_into().unwrap();
        let offset = offset + self.sp_frame_offset;

        BaseIndexOff {
            base: Some(REG_RSP),
            index: None,
            disp: offset,
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
            sp_frame_offset: 0,
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
        block_labels: &BlockLabelMap,
        instr: &X64Instr,
        defs: &[OperandAssignment],
        uses: &[OperandAssignment],
    ) {
        match instr {
            &X64Instr::AluRRm(op_size, op) => {
                let arg0 = uses[0].as_reg().unwrap();
                let arg1 = state.operand_reg_mem(uses[1]);
                emit_alu_r_rm(buffer, op, op_size, arg0, arg1);
            }
            &X64Instr::ShiftRmR(op_size, op) => {
                emit_shift_rm_cx(buffer, op, op_size, state.operand_reg_mem(uses[0]))
            }
            &X64Instr::ShiftRmI(op_size, op, imm) => {
                emit_shift_rm_i(buffer, op, op_size, state.operand_reg_mem(uses[0]), imm)
            }
            &X64Instr::Div(op_size, op) => {
                emit_div_rm(buffer, op, op_size, state.operand_reg_mem(uses[2]))
            }
            &X64Instr::ConvertWord(op_size) => {
                emit_convert_word(buffer, op_size);
            }
            &X64Instr::Setcc(code) => {
                let dest = defs[0].as_reg().unwrap();
                emit_setcc_r(buffer, code, dest);
            }
            &X64Instr::MovRI(val) => emit_mov_ri(buffer, defs[0].as_reg().unwrap(), val),
            X64Instr::MovRZ => {
                let dest = defs[0].as_reg().unwrap();
                // Note: some renamers recognize only the 32-bit instruction as a zeroing idiom.
                emit_alu_r_rm(
                    buffer,
                    AluOp::Xor,
                    OperandSize::S32,
                    dest,
                    RegMem::Reg(dest),
                );
            }
            &X64Instr::MovsxRRm(width) => emit_movsx_r_rm(
                buffer,
                width,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            &X64Instr::MovRRbp { offset } => emit_movzx_r_rm(
                buffer,
                FullOperandSize::S64,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(BaseIndexOff {
                    base: Some(REG_RBP),
                    index: None,
                    disp: offset,
                }),
            ),
            &X64Instr::MovRM(full_op_size) => emit_movzx_r_rm(
                buffer,
                full_op_size,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(BaseIndexOff {
                    base: Some(uses[0].as_reg().unwrap()),
                    index: None,
                    disp: 0,
                }),
            ),
            &X64Instr::MovRStack(slot, full_op_size) => emit_movzx_r_rm(
                buffer,
                full_op_size,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(state.stack_slot_addr(slot)),
            ),
            &X64Instr::MovMR(full_op_size) => emit_mov_mr_r(
                buffer,
                full_op_size,
                RegMem::Mem(BaseIndexOff {
                    base: Some(uses[0].as_reg().unwrap()),
                    index: None,
                    disp: 0,
                }),
                uses[1].as_reg().unwrap(),
            ),
            &X64Instr::MovStackR(slot, full_op_size) => emit_mov_mr_r(
                buffer,
                full_op_size,
                RegMem::Mem(state.stack_slot_addr(slot)),
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
            &X64Instr::AddSp(offset) => {
                emit_add_sp(buffer, offset);
                state.sp_frame_offset -= offset;
            }
            X64Instr::Push => {
                emit_push(buffer, uses[0].as_reg().unwrap());
                state.sp_frame_offset += 8;
            }
            X64Instr::Call(_func) => {
                // TODO: Other code models?
                // TODO: Record relocation.
                emit_call_rel(buffer);
            }
            &X64Instr::Jump(target) => {
                emit_jmp(buffer, block_labels[target]);
            }
            &X64Instr::Jumpcc(code, true_target, false_target) => {
                emit_jcc(buffer, code, block_labels[true_target]);
                emit_jmp(buffer, block_labels[false_target]);
            }
        }
    }

    fn emit_copy(
        &self,
        state: &mut X64EmitState,
        buffer: &mut CodeBuffer<Self>,
        from: OperandAssignment,
        to: OperandAssignment,
    ) {
        match (from, to) {
            (OperandAssignment::Reg(from), OperandAssignment::Reg(to)) => {
                emit_mov_rr(buffer, to, from)
            }
            (OperandAssignment::Spill(from), OperandAssignment::Reg(to)) => emit_movzx_r_rm(
                buffer,
                FullOperandSize::S64,
                to,
                RegMem::Mem(state.spill_slot_addr(from)),
            ),
            (OperandAssignment::Reg(from), OperandAssignment::Spill(to)) => emit_mov_mr_r(
                buffer,
                FullOperandSize::S64,
                RegMem::Mem(state.spill_slot_addr(to)),
                from,
            ),
            (OperandAssignment::Spill(_), OperandAssignment::Spill(_)) => {
                unreachable!("mem-to-mem copy")
            }
        }
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
    let reg = rex.encode_modrm_base(reg);
    rex.emit(buffer);
    buffer.emit(&[0x50 + reg]);
}

fn emit_pop(buffer: &mut CodeBuffer<X64Machine>, reg: PhysReg) {
    let mut rex = RexPrefix::new();
    let reg = rex.encode_modrm_base(reg);
    rex.emit(buffer);
    buffer.emit(&[0x58 + reg]);
}

fn emit_mov_rr(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, src: PhysReg) {
    emit_mov_mr_r(buffer, FullOperandSize::S64, RegMem::Reg(dest), src);
}

fn emit_movzx_r_rm(
    buffer: &mut CodeBuffer<X64Machine>,
    full_op_size: FullOperandSize,
    dest: PhysReg,
    src: RegMem,
) {
    // For anything less than 64 bits, write to a 32-bit register, which will implicitly clear the
    // high bits.
    let op_size = match full_op_size {
        FullOperandSize::S64 => OperandSize::S64,
        _ => OperandSize::S32,
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(src, |rex| {
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
    let (rex, modrm_sib) = encode_mem_parts(addr, |rex: &mut RexPrefix| {
        rex.encode_operand_size(OperandSize::S64);
        rex.encode_modrm_reg(dest)
    });

    rex.emit(buffer);
    buffer.emit(&[0x8d]);
    modrm_sib.emit(buffer);
}

fn emit_mov_mr_r(
    buffer: &mut CodeBuffer<X64Machine>,
    full_op_size: FullOperandSize,
    dest: RegMem,
    src: PhysReg,
) {
    let op_size = match full_op_size {
        FullOperandSize::S64 => OperandSize::S64,
        _ => OperandSize::S32,
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(dest, |rex| {
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
    let dest = rex.encode_modrm_base(dest);
    rex.emit(buffer);
    buffer.emit(&[0xf, 0x90 | encode_cond_code(code), encode_modrm_r(0, dest)]);
}

fn emit_movsx_r_rm(
    buffer: &mut CodeBuffer<X64Machine>,
    width: ExtWidth,
    dest: PhysReg,
    src: RegMem,
) {
    let (opcode, op_size): (&[u8], _) = match width {
        ExtWidth::Ext8_32 => (&[0xf, 0xbe], OperandSize::S32),
        ExtWidth::Ext8_64 => (&[0xf, 0xbe], OperandSize::S64),
        ExtWidth::Ext16_32 => (&[0xf, 0xbf], OperandSize::S32),
        ExtWidth::Ext16_64 => (&[0xf, 0xbf], OperandSize::S64),
        ExtWidth::Ext32_64 => (&[0x63], OperandSize::S64),
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(src, |rex| {
        rex.encode_operand_size(op_size);
        if matches!(width, ExtWidth::Ext8_32 | ExtWidth::Ext8_64) {
            if let RegMem::Reg(src) = src {
                rex.use_reg8(src);
            }
        }
        rex.encode_modrm_reg(dest)
    });

    rex.emit(buffer);
    buffer.emit(opcode);
    modrm_sib.emit(buffer);
}

fn emit_call_rel(buffer: &mut CodeBuffer<X64Machine>) {
    buffer.emit(&[0xe8]);
    buffer.emit(&0u32.to_le_bytes());
}

fn emit_ret(buffer: &mut CodeBuffer<X64Machine>) {
    buffer.emit(&[0xc3]);
}

fn emit_ud2(buffer: &mut CodeBuffer<X64Machine>) {
    buffer.emit(&[0xf, 0xb]);
}

// Instruction group emission helpers

fn emit_alu_r_rm(
    buffer: &mut CodeBuffer<X64Machine>,
    op: AluOp,
    op_size: OperandSize,
    arg0: PhysReg,
    arg1: RegMem,
) {
    let opcode: &[u8] = match op {
        AluOp::Add => &[0x3],
        AluOp::And => &[0x23],
        AluOp::Cmp => &[0x3b],
        AluOp::Or => &[0xb],
        AluOp::Sub => &[0x2b],
        AluOp::Test => {
            // This encoding is actually backwards (`test r/m, r`), but it doesn't matter because
            // `test` is commutative and has no outputs other than flags.
            &[0x85]
        }
        AluOp::Xor => &[0x33],
        AluOp::Imul => &[0xf, 0xaf],
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(arg1, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(arg0)
    });

    rex.emit(buffer);
    buffer.emit(opcode);
    modrm_sib.emit(buffer);
}

fn emit_div_rm(buffer: &mut CodeBuffer<X64Machine>, op: DivOp, op_size: OperandSize, arg: RegMem) {
    let reg_opcode = match op {
        DivOp::Div => 0x6,
        DivOp::Idiv => 0x7,
    };
    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    rex.emit(buffer);
    buffer.emit(&[0xf7]);
    modrm_sib.emit(buffer);
}

fn emit_convert_word(buffer: &mut CodeBuffer<X64Machine>, op_size: OperandSize) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);
    rex.emit(buffer);
    buffer.emit(&[0x99]);
}

fn emit_shift_rm_cx(
    buffer: &mut CodeBuffer<X64Machine>,
    op: ShiftOp,
    op_size: OperandSize,
    arg: RegMem,
) {
    let reg_opcode = encode_shift_op(op);
    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    rex.emit(buffer);
    buffer.emit(&[0xd3]);
    modrm_sib.emit(buffer);
}

fn emit_shift_rm_i(
    buffer: &mut CodeBuffer<X64Machine>,
    op: ShiftOp,
    op_size: OperandSize,
    arg: RegMem,
    imm: u8,
) {
    let (opcode, emit_imm) = if imm == 1 {
        (0xd1, false)
    } else {
        (0xc1, true)
    };

    let reg_opcode = encode_shift_op(op);

    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    rex.emit(buffer);
    buffer.emit(&[opcode]);
    modrm_sib.emit(buffer);
    if emit_imm {
        buffer.emit(&[imm]);
    }
}

fn emit_mov_ri(buffer: &mut CodeBuffer<X64Machine>, dest: PhysReg, imm: u64) {
    let mut rex = RexPrefix::new();
    let dest = rex.encode_modrm_base(dest);

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
    let dest = rex.encode_modrm_base(dest);
    rex.emit(buffer);
    buffer.emit(&[opcode, encode_modrm_r(reg, dest)]);

    if is_imm8 {
        buffer.emit(&[imm as u8]);
    } else {
        buffer.emit(&imm.to_le_bytes());
    }
}

fn emit_jmp(buffer: &mut CodeBuffer<X64Machine>, target: Label) {
    buffer.emit(&[0xe9]);
    buffer.emit_fixup(target, X64Fixup::Rela4(-4));
    buffer.emit(&0u32.to_le_bytes());
}

fn emit_jcc(buffer: &mut CodeBuffer<X64Machine>, code: CondCode, target: Label) {
    buffer.emit(&[0xf, 0x80 + encode_cond_code(code)]);
    buffer.emit_fixup(target, X64Fixup::Rela4(-4));
    buffer.emit(&0u32.to_le_bytes());
}

// Prefixes and encoding

#[derive(Clone, Copy)]
enum RegMem {
    Reg(PhysReg),
    Mem(BaseIndexOff),
}

#[derive(Clone, Copy)]
struct BaseIndexOff {
    base: Option<PhysReg>,
    index: Option<(IndexScale, PhysReg)>,
    disp: i32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MemMode {
    NoDisp,
    Disp8,
    Disp32,
    Disp32Special,
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
            MemMode::Disp32 | MemMode::Disp32Special => buffer.emit(&self.disp.to_le_bytes()),
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

    fn encode_modrm_base(&mut self, base: PhysReg) -> u8 {
        let base = encode_reg(base);
        self.b = base & 0b1000 != 0;
        base & 0b111
    }

    fn encode_sib_index(&mut self, index: PhysReg) -> u8 {
        let index = encode_reg(index);
        self.x = index & 0b1000 != 0;
        index & 0b111
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

fn encode_reg_mem_parts(
    reg_mem: RegMem,
    get_reg: impl FnOnce(&mut RexPrefix) -> u8,
) -> (RexPrefix, ModRmSib) {
    match reg_mem {
        RegMem::Reg(rm) => {
            let mut rex = RexPrefix::new();
            let reg = get_reg(&mut rex);
            let rm = rex.encode_modrm_base(rm);
            let modrm_sib = ModRmSib {
                modrm: encode_modrm_r(reg, rm),
                sib: None,
                disp: 0,
                mem_mode: MemMode::NoDisp,
            };
            (rex, modrm_sib)
        }
        RegMem::Mem(addr) => encode_mem_parts(addr, get_reg),
    }
}

fn encode_mem_parts(
    addr: BaseIndexOff,
    get_reg: impl FnOnce(&mut RexPrefix) -> u8,
) -> (RexPrefix, ModRmSib) {
    let mut rex = RexPrefix::new();
    let reg = get_reg(&mut rex);

    // These values can have special meanings when placed in the R/M slot.
    let (base_bp, base_sp) = match addr.base {
        Some(base) => {
            let base_rm = encode_reg(base) & 0b111;
            (
                base_rm == encode_reg(REG_RBP),
                base_rm == encode_reg(REG_RSP),
            )
        }
        None => (false, false),
    };

    let mode = if addr.disp == 0 && !base_bp {
        MemMode::NoDisp
    } else if addr.base.is_none() {
        MemMode::Disp32Special
    } else if is_sint::<8>(addr.disp as u64) {
        MemMode::Disp8
    } else {
        MemMode::Disp32
    };

    let need_sib = addr.base.is_none() || base_sp || addr.index.is_some();

    if need_sib {
        let base = addr
            .base
            .map_or(SIB_BASE_NONE, |base| rex.encode_modrm_base(base));
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
        let base = rex.encode_modrm_base(addr.base.unwrap());

        (
            rex,
            ModRmSib {
                modrm: encode_modrm_mem(mode, reg, base),
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
    let mode = match mode {
        MemMode::NoDisp => 0b00,
        MemMode::Disp8 => 0b01,
        MemMode::Disp32 => 0b10,
        MemMode::Disp32Special => 0b00,
    };
    encode_modrm(mode, reg, rm)
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

#[cfg(test)]
mod tests;
