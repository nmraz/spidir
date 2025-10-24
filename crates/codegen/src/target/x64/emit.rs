use flag_liveness::BlockFlagLivenessTracker;
use ir::node::FunctionRef;

use crate::{
    cfg::Block,
    code_buffer::{BufferRelocTarget, CodeBuffer, FixupKind, InstrAnchor, InstrSink, Label},
    constpool::Constant,
    emit::{EmitContext, EmitCopyData, EmitInstrData},
    frame::FrameLayout,
    lir::{Instr, PhysReg, PhysRegSet, RegWidth, StackSlot},
    machine::MachineEmit,
    num_utils::{align_up, is_sint, is_uint},
    regalloc::{OperandAssignment, SpillSlot},
    target::x64::{
        AluCommBinOp, CALLEE_SAVED_REGS, CompoundCondCode, RB_GPR, RB_XMM, RW_32, RW_64,
        SseFpuCmpCode, SseFpuPrecision,
    },
};

use super::{
    AddrBase, AddrMode, AluBinOp, AluUnOp, CondCode, DivOp, ExtWidth, FullOperandSize, IndexScale,
    OperandSize, REG_R8, REG_R9, REG_R10, REG_R11, REG_R12, REG_R13, REG_R14, REG_R15, REG_RAX,
    REG_RBP, REG_RBX, REG_RCX, REG_RDI, REG_RDX, REG_RSI, REG_RSP, REG_XMM0, REG_XMM1, REG_XMM2,
    REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7, REG_XMM8, REG_XMM9, REG_XMM10, REG_XMM11,
    REG_XMM12, REG_XMM13, REG_XMM14, REG_XMM15, RELOC_ABS64, RELOC_PC32, ShiftOp, SseFpuBinOp,
    X64Instr, X64Machine,
};

mod flag_liveness;

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
    block_flag_liveness: BlockFlagLivenessTracker,
}

impl X64EmitState {
    fn operand_reg_mem(&self, operand: OperandAssignment) -> RegMem {
        match operand {
            OperandAssignment::Reg(reg) => RegMem::Reg(reg),
            OperandAssignment::Spill(spill) => RegMem::Mem(self.spill_slot_addr(spill)),
        }
    }

    fn lower_addr_mode(&self, addr_mode: &AddrMode, regs: &[OperandAssignment]) -> RawAddrMode {
        let mut regs = regs.iter().map(|reg| reg.as_reg().unwrap());
        let mut offset = 0;

        let base = match addr_mode.base {
            Some(AddrBase::Reg) => Some(regs.next().unwrap()),
            Some(AddrBase::Rsp) => Some(REG_RSP),
            Some(AddrBase::Stack(slot)) => {
                offset = self.sp_offset(self.frame_layout.stack_slot_offsets[slot]);
                Some(REG_RSP)
            }
            None => None,
        };

        let index = addr_mode.index.map(|scale| (scale, regs.next().unwrap()));
        offset = offset.checked_add(addr_mode.offset).unwrap();

        RawAddrMode::BaseIndexOff {
            base,
            index,
            offset,
        }
    }

    fn stack_slot_addr(&self, slot: StackSlot) -> RawAddrMode {
        self.stack_addr(self.frame_layout.stack_slot_offsets[slot])
    }

    fn spill_slot_addr(&self, spill: SpillSlot) -> RawAddrMode {
        self.stack_addr(self.frame_layout.spill_slot_offsets[spill])
    }

    fn stack_addr(&self, frame_offset: u32) -> RawAddrMode {
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSP),
            index: None,
            offset: self.sp_offset(frame_offset),
        }
    }

    fn sp_offset(&self, frame_offset: u32) -> i32 {
        let frame_offset: i32 = frame_offset.try_into().unwrap();
        frame_offset + self.sp_frame_offset
    }
}

const DEFAULT_FRAME_ALIGN: u32 = 16;

impl MachineEmit for X64Machine {
    type EmitState = X64EmitState;
    type Fixup = X64Fixup;

    fn prepare_state(&self, ctx: &EmitContext<'_, Self>) -> X64EmitState {
        let frame_layout = FrameLayout::compute(ctx.lir, ctx.assignment);
        let saved_regs = &ctx.assignment.compute_global_clobbers(ctx.lir)
            & &PhysRegSet::from_iter(CALLEE_SAVED_REGS);

        let full_frame_layout = frame_layout.full_layout;
        let mut raw_frame_size = align_up(full_frame_layout.size, DEFAULT_FRAME_ALIGN);

        let realign = if full_frame_layout.align > DEFAULT_FRAME_ALIGN {
            FrameRealign::AlignTo(full_frame_layout.align.try_into().unwrap())
        } else {
            if !saved_regs.len().is_multiple_of(2) {
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
        } else {
            FrameRestoreMethod::AddSp(raw_frame_size)
        };

        X64EmitState {
            frame_layout,
            saved_regs,
            raw_frame_size,
            sp_frame_offset: 0,
            realign,
            restore_method,
            block_flag_liveness: BlockFlagLivenessTracker::new(),
        }
    }

    fn emit_prologue(
        &self,
        _ctx: &EmitContext<'_, Self>,
        state: &mut X64EmitState,
        buffer: &mut CodeBuffer<X64Fixup>,
    ) {
        emit_push(buffer, REG_RBP);
        emit_mov_r_r(buffer, REG_RBP, REG_RSP);

        for saved_reg in state.saved_regs.iter().rev() {
            emit_push(buffer, saved_reg);
        }

        emit_add_sp(buffer, -state.raw_frame_size);

        if let FrameRealign::AlignTo(align) = state.realign {
            emit_alu_r64_i(buffer, RawAluBinOp::And, REG_RSP, -align);
        }
    }

    fn prepare_block(
        &self,
        _ctx: &EmitContext<'_, Self>,
        state: &mut Self::EmitState,
        _block: Block,
    ) {
        // Forget our flag liveness information now that we've entered a new block.
        state.block_flag_liveness.reset_block();
    }

    fn emit_instr(
        &self,
        ctx: &EmitContext<'_, Self>,
        pos: Instr,
        instr: &EmitInstrData<'_, Self>,
        state: &mut X64EmitState,
        buffer: &mut CodeBuffer<X64Fixup>,
    ) {
        let defs = instr.defs;
        let uses = instr.uses;

        match instr.instr {
            &X64Instr::AluRRm(op_size, op) => {
                let arg0 = uses[0].as_reg().unwrap();
                let arg1 = state.operand_reg_mem(uses[1]);

                emit_alu_r_rm(buffer, op.into(), op_size, arg0, arg1);
            }
            &X64Instr::AluRmI(op_size, op, imm) => {
                let arg = state.operand_reg_mem(uses[0]);
                emit_alu_rm_i(buffer, op.into(), op_size, arg, imm);
            }
            &X64Instr::AluCommRR(op_size, op) => {
                let dest = defs[0].as_reg().unwrap();
                let arg0 = uses[0].as_reg().unwrap();
                let arg1 = uses[1].as_reg().unwrap();

                if dest == arg0 {
                    emit_alu_r_rm(buffer, op.into(), op_size, arg0, RegMem::Reg(arg1));
                } else if dest == arg1 {
                    emit_alu_r_rm(buffer, op.into(), op_size, arg1, RegMem::Reg(arg0));
                } else {
                    emit_mov_rm_r(buffer, op_size.into(), RegMem::Reg(dest), arg0);
                    emit_alu_r_rm(buffer, op.into(), op_size, dest, RegMem::Reg(arg1));
                }
            }
            &X64Instr::AluCommRmI(op_size, op, imm) => {
                let arg = state.operand_reg_mem(uses[0]);
                emit_alu_rm_i(buffer, op.into(), op_size, arg, imm);
            }
            &X64Instr::AluRm(op_size, op) => {
                let arg = state.operand_reg_mem(uses[0]);
                emit_alu_rm(buffer, op, op_size, arg);
            }
            &X64Instr::AddRR(op_size) => {
                let dest = defs[0].as_reg().unwrap();
                let arg0 = uses[0].as_reg().unwrap();
                let arg1 = uses[1].as_reg().unwrap();

                if dest == arg0 {
                    emit_alu_r_rm(buffer, RawAluBinOp::Add, op_size, arg0, RegMem::Reg(arg1));
                } else if dest == arg1 {
                    emit_alu_r_rm(buffer, RawAluBinOp::Add, op_size, arg1, RegMem::Reg(arg0));
                } else {
                    emit_lea(
                        buffer,
                        op_size,
                        dest,
                        RawAddrMode::BaseIndexOff {
                            base: Some(arg0),
                            index: Some((IndexScale::One, arg1)),
                            offset: 0,
                        },
                    );
                }
            }
            &X64Instr::AddRI(op_size, imm) => {
                let dest = defs[0].as_reg().unwrap();
                let arg = uses[0].as_reg().unwrap();

                if dest == arg {
                    emit_alu_rm_i(buffer, RawAluBinOp::Add, op_size, RegMem::Reg(arg), imm);
                } else {
                    emit_lea(
                        buffer,
                        op_size,
                        dest,
                        RawAddrMode::BaseIndexOff {
                            base: Some(arg),
                            index: None,
                            offset: imm,
                        },
                    );
                }
            }
            &X64Instr::ImulRR(op_size) => {
                let dest = defs[0].as_reg().unwrap();
                let arg0 = uses[0].as_reg().unwrap();
                let arg1 = uses[1].as_reg().unwrap();

                if dest == arg0 {
                    emit_imul_r_rm(buffer, op_size, arg0, RegMem::Reg(arg1));
                } else if dest == arg1 {
                    emit_imul_r_rm(buffer, op_size, arg1, RegMem::Reg(arg0));
                } else {
                    emit_mov_rm_r(buffer, op_size.into(), RegMem::Reg(dest), arg0);
                    emit_imul_r_rm(buffer, op_size, dest, RegMem::Reg(arg1));
                }
            }
            &X64Instr::ImulRRmI(op_size, imm) => emit_imul_r_rm_i(
                buffer,
                op_size,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
                imm,
            ),
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
            &X64Instr::MovRmS32(val) => {
                let dest = state.operand_reg_mem(defs[0]);
                emit_mov_rm_s32(ctx, pos, state, buffer, dest, val)
            }
            &X64Instr::MovRU32(val) => emit_mov_r_u32(buffer, defs[0].as_reg().unwrap(), val),
            &X64Instr::MovRI64(val) => emit_movabs_r_i(buffer, defs[0].as_reg().unwrap(), val),
            &X64Instr::MovzxRRm(full_op_size) => emit_movzx_r_rm(
                buffer,
                full_op_size,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            &X64Instr::MovsxRRm(width) => emit_movsx_r_rm(
                buffer,
                width,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            &X64Instr::SseScalarFpuRRm(prec, op) => emit_sse_fpu_r_rm(
                buffer,
                prec,
                op,
                uses[0].as_reg().unwrap(),
                // Note: thank goodness for little endian, because we currently spill the full 16
                // bytes.
                state.operand_reg_mem(uses[1]),
            ),
            &X64Instr::Ucomi(prec) => emit_ucomi(
                buffer,
                prec,
                uses[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[1]),
            ),
            &X64Instr::SseMovRZ => {
                let dest = defs[0].as_reg().unwrap();
                emit_xorps(buffer, dest, RegMem::Reg(dest))
            }
            &X64Instr::MovssConstRel(val) => {
                let dest = defs[0].as_reg().unwrap();
                let src = buffer.get_constant(4, &val.to_le_bytes());
                emit_movs_r_rm_rip_reloc(
                    buffer,
                    SseFpuPrecision::Single,
                    dest,
                    BufferRelocTarget::Constant(src),
                );
            }
            &X64Instr::F32ConstAddrAbs(val) => {
                let dest = defs[0].as_reg().unwrap();
                let src = buffer.get_constant(4, &val.to_le_bytes());
                emit_movabs_r_i_reloc(buffer, dest, BufferRelocTarget::Constant(src));
            }
            &X64Instr::MovsdConstRel(val) => {
                let dest = defs[0].as_reg().unwrap();
                let src = buffer.get_constant(8, &val.to_le_bytes());
                emit_movs_r_rm_rip_reloc(
                    buffer,
                    SseFpuPrecision::Double,
                    dest,
                    BufferRelocTarget::Constant(src),
                );
            }
            &X64Instr::F64ConstAddrAbs(val) => {
                let dest = defs[0].as_reg().unwrap();
                let src = buffer.get_constant(8, &val.to_le_bytes());
                emit_movabs_r_i_reloc(buffer, dest, BufferRelocTarget::Constant(src));
            }
            &X64Instr::Cvtsi2s(op_size, prec) => emit_cvtsi2s(
                buffer,
                op_size,
                prec,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            &X64Instr::Cvts2si(op_size, prec) => emit_cvts2si(
                buffer,
                op_size,
                prec,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            X64Instr::Cvtss2sd => emit_cvtss2sd(
                buffer,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            X64Instr::Cvtsd2ss => emit_cvtsd2ss(
                buffer,
                defs[0].as_reg().unwrap(),
                state.operand_reg_mem(uses[0]),
            ),
            &X64Instr::PseudoUint64ToFloat(prec) => emit_uint64_to_float(
                buffer,
                prec,
                defs[0].as_reg().unwrap(),
                uses[0].as_reg().unwrap(),
                defs[1].as_reg().unwrap(),
                defs[2].as_reg().unwrap(),
            ),
            &X64Instr::PseudoFloatToUint64Rel(prec) => emit_float_to_uint64_rel(
                buffer,
                prec,
                defs[0].as_reg().unwrap(),
                uses[0].as_reg().unwrap(),
                defs[1].as_reg().unwrap(),
                defs[2].as_reg().unwrap(),
            ),
            &X64Instr::PseudoFloatToUint64Abs(prec) => emit_float_to_uint64_abs(
                buffer,
                prec,
                defs[0].as_reg().unwrap(),
                uses[0].as_reg().unwrap(),
                defs[1].as_reg().unwrap(),
                defs[2].as_reg().unwrap(),
            ),
            &X64Instr::MovGprmXmm(op_size) => emit_mov_gprm_xmm(
                buffer,
                op_size,
                state.operand_reg_mem(defs[0]),
                uses[0].as_reg().unwrap(),
            ),
            &X64Instr::MovRRbp { op_size, offset } => emit_movzx_r_rm(
                buffer,
                op_size,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(RawAddrMode::BaseIndexOff {
                    base: Some(REG_RBP),
                    index: None,
                    offset,
                }),
            ),
            &X64Instr::MovsRRbp { prec, offset } => emit_movs_r_rm(
                buffer,
                prec,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(RawAddrMode::BaseIndexOff {
                    base: Some(REG_RBP),
                    index: None,
                    offset,
                }),
            ),
            X64Instr::MovRM(full_op_size, addr_mode) => emit_movzx_r_rm(
                buffer,
                *full_op_size,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(state.lower_addr_mode(addr_mode, uses)),
            ),
            X64Instr::MovMR(full_op_size, addr_mode) => emit_mov_rm_r(
                buffer,
                *full_op_size,
                RegMem::Mem(state.lower_addr_mode(addr_mode, &uses[1..])),
                uses[0].as_reg().unwrap(),
            ),
            X64Instr::MovsRM(prec, addr_mode) => emit_movs_r_rm(
                buffer,
                *prec,
                defs[0].as_reg().unwrap(),
                RegMem::Mem(state.lower_addr_mode(addr_mode, uses)),
            ),
            X64Instr::MovsMR(prec, addr_mode) => emit_movs_rm_r(
                buffer,
                *prec,
                RegMem::Mem(state.lower_addr_mode(addr_mode, &uses[1..])),
                uses[0].as_reg().unwrap(),
            ),
            &X64Instr::StackAddr(slot) => emit_lea_or_mov(
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
            &X64Instr::FuncAddrRel(target) => {
                emit_lea_rip_reloc(buffer, defs[0].as_reg().unwrap(), target);
            }
            &X64Instr::FuncAddrAbs(target) => {
                emit_movabs_r_i_reloc(
                    buffer,
                    defs[0].as_reg().unwrap(),
                    BufferRelocTarget::Function(target),
                );
            }
            &X64Instr::CallRel(target) => {
                emit_call_rel(buffer, target);
            }
            &X64Instr::CallRm => emit_call_rm(buffer, state.operand_reg_mem(uses[0])),
            &X64Instr::Jump(target) => {
                emit_jmp(buffer, ctx.block_labels[target]);
            }
            &X64Instr::Jumpcc(code, true_target, false_target) => {
                emit_jcc(buffer, code, ctx.block_labels[true_target]);
                emit_jmp(buffer, ctx.block_labels[false_target]);
            }
            &X64Instr::CompundJumpcc(code, true_target, false_target) => {
                emit_compound_jcc(ctx, buffer, code, true_target, false_target);
            }
        }
    }

    fn emit_copy(
        &self,
        _ctx: &EmitContext<'_, Self>,
        _pos: Instr,
        copy: &EmitCopyData,
        state: &mut X64EmitState,
        buffer: &mut CodeBuffer<X64Fixup>,
    ) {
        let width = copy.class.width();

        match (copy.class.bank(), copy.from, copy.to) {
            (RB_GPR, OperandAssignment::Reg(from), OperandAssignment::Reg(to)) => {
                emit_mov_rm_r(buffer, op_size_for_reg_width(width), RegMem::Reg(to), from);
            }
            (RB_GPR, OperandAssignment::Spill(from), OperandAssignment::Reg(to)) => {
                emit_movzx_r_rm(
                    buffer,
                    op_size_for_reg_width(width),
                    to,
                    RegMem::Mem(state.spill_slot_addr(from)),
                );
            }
            (RB_GPR, OperandAssignment::Reg(from), OperandAssignment::Spill(to)) => {
                emit_mov_rm_r(
                    buffer,
                    op_size_for_reg_width(width),
                    RegMem::Mem(state.spill_slot_addr(to)),
                    from,
                );
            }

            (RB_XMM, OperandAssignment::Reg(from), OperandAssignment::Reg(to)) => {
                emit_movaps_r_rm(buffer, to, RegMem::Reg(from));
            }
            (RB_XMM, OperandAssignment::Spill(from), OperandAssignment::Reg(to)) => emit_movs_r_rm(
                buffer,
                fpu_precision_for_reg_width(width),
                to,
                RegMem::Mem(state.spill_slot_addr(from)),
            ),
            (RB_XMM, OperandAssignment::Reg(from), OperandAssignment::Spill(to)) => {
                emit_movs_rm_r(
                    buffer,
                    fpu_precision_for_reg_width(width),
                    RegMem::Mem(state.spill_slot_addr(to)),
                    from,
                );
            }

            (_, OperandAssignment::Spill(_), OperandAssignment::Spill(_)) => {
                unreachable!("mem-to-mem copy")
            }

            _ => unreachable!("unknown register bank"),
        }
    }
}

fn op_size_for_reg_width(width: RegWidth) -> FullOperandSize {
    match width {
        RW_32 => FullOperandSize::S32,
        RW_64 => FullOperandSize::S64,
        _ => unreachable!(),
    }
}

fn fpu_precision_for_reg_width(width: RegWidth) -> SseFpuPrecision {
    match width {
        RW_32 => SseFpuPrecision::Single,
        RW_64 => SseFpuPrecision::Double,
        _ => unreachable!(),
    }
}

// Code sequence emission helpers

fn emit_epilogue(buffer: &mut CodeBuffer<X64Fixup>, state: &X64EmitState) {
    match state.restore_method {
        FrameRestoreMethod::AddSp(offset) => emit_add_sp(buffer, offset),
        FrameRestoreMethod::FromRbp => {
            let saved_reg_size = (state.saved_regs.len() * 8) as i32;
            emit_lea_or_mov(
                buffer,
                REG_RSP,
                RawAddrMode::BaseIndexOff {
                    base: Some(REG_RBP),
                    index: None,
                    offset: -saved_reg_size,
                },
            );
        }
    }

    for saved_reg in state.saved_regs.iter() {
        emit_pop(buffer, saved_reg);
    }

    emit_pop(buffer, REG_RBP);
}

fn emit_add_sp(buffer: &mut CodeBuffer<X64Fixup>, offset: i32) {
    if offset == -8 {
        // This generates smaller code without penalizing performance.
        emit_push(buffer, REG_RAX);
    } else if offset < 0 {
        emit_alu_r64_i(buffer, RawAluBinOp::Sub, REG_RSP, -offset);
    } else if offset > 0 {
        emit_alu_r64_i(buffer, RawAluBinOp::Add, REG_RSP, offset);
    }
}

fn emit_compound_jcc(
    ctx: &EmitContext<'_, X64Machine>,
    buffer: &mut CodeBuffer<X64Fixup>,
    code: CompoundCondCode,
    true_target: Block,
    false_target: Block,
) {
    let true_target = ctx.block_labels[true_target];
    let false_target = ctx.block_labels[false_target];

    match code {
        CompoundCondCode::FpuOeq => {
            emit_jcc(buffer, CondCode::Ne, false_target);
            emit_jcc(buffer, CondCode::P, false_target);
        }
        CompoundCondCode::FpuUne => {
            emit_jcc(buffer, CondCode::Ne, true_target);
            emit_jcc(buffer, CondCode::Np, false_target);
        }
    }
    emit_jmp(buffer, true_target);
}

fn emit_uint64_to_float(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: PhysReg,
    tmp_gpr1: PhysReg,
    tmp_gpr2: PhysReg,
) {
    // Emit the following:
    //
    //         test src, src
    //         js has_high_bit
    //         cvtsi2s[sd] dest, src
    //         jmp done
    //     has_high_bit:
    //         mov tmp_gpr1, src
    //         mov tmp_gpr2, src
    //         shr tmp_gpr1
    //         and tmp_gpr2, 1
    //         or tmp_gpr1, tmp_gpr2
    //         cvtsi2s[sd] dest, tmp_gpr1
    //         addsd dest, dest
    //     done:

    let has_high_bit = buffer.create_label();
    let done = buffer.create_label();

    emit_alu_r_rm(
        buffer,
        RawAluBinOp::Test,
        OperandSize::S64,
        src,
        RegMem::Reg(src),
    );
    emit_jcc(buffer, CondCode::S, has_high_bit);

    emit_cvtsi2s(buffer, OperandSize::S64, prec, dest, RegMem::Reg(src));
    emit_jmp(buffer, done);

    buffer.bind_label(has_high_bit);

    if tmp_gpr1 != src {
        emit_mov_r_r(buffer, tmp_gpr1, src);
    }

    if tmp_gpr2 != src {
        emit_mov_r_r(buffer, tmp_gpr2, src);
    }

    emit_shift_rm_i(
        buffer,
        ShiftOp::Shr,
        OperandSize::S64,
        RegMem::Reg(tmp_gpr1),
        1,
    );

    emit_alu_rm_i(
        buffer,
        RawAluBinOp::And,
        OperandSize::S64,
        RegMem::Reg(tmp_gpr2),
        1,
    );

    emit_alu_r_rm(
        buffer,
        RawAluBinOp::Or,
        OperandSize::S64,
        tmp_gpr1,
        RegMem::Reg(tmp_gpr2),
    );

    emit_cvtsi2s(buffer, OperandSize::S64, prec, dest, RegMem::Reg(tmp_gpr1));
    emit_sse_fpu_r_rm(buffer, prec, SseFpuBinOp::Add, dest, RegMem::Reg(dest));

    buffer.bind_label(done);
}

fn emit_float_to_uint64_rel(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: PhysReg,
    tmp_xmm1: PhysReg,
    tmp_xmm2: PhysReg,
) {
    let f_1p63 = get_f_1p63(buffer, prec);
    emit_movs_r_rm_rip_reloc(buffer, prec, tmp_xmm1, BufferRelocTarget::Constant(f_1p63));
    emit_float_to_uint64_common(buffer, prec, dest, src, tmp_xmm1, tmp_xmm2);
}

fn emit_float_to_uint64_abs(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: PhysReg,
    tmp_xmm1: PhysReg,
    tmp_xmm2: PhysReg,
) {
    let f_1p63 = get_f_1p63(buffer, prec);

    // Use `dest` as a temporary for the constant pool access first.
    emit_movabs_r_i_reloc(buffer, dest, BufferRelocTarget::Constant(f_1p63));
    emit_movs_r_rm(
        buffer,
        prec,
        tmp_xmm1,
        RegMem::Mem(RawAddrMode::BaseIndexOff {
            base: Some(dest),
            index: None,
            offset: 0,
        }),
    );

    emit_float_to_uint64_common(buffer, prec, dest, src, tmp_xmm1, tmp_xmm2);
}

fn emit_float_to_uint64_common(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: PhysReg,
    tmp_xmm1: PhysReg,
    tmp_xmm2: PhysReg,
) {
    // Emit the following:
    //
    //         ucomis[sd] src, tmp_xmm1
    //         jae has_high_bit
    //         cvts[sd]2si dest, src
    //         jmp done
    //     has_high_bit:
    //         mov tmp_xmm2, src
    //         subsd tmp_xmm2, tmp_xmm1
    //         cvts[sd]2si dest, tmp_xmm2
    //         btc dest
    //     done:
    //
    // assuming `tmp_xmm1` already contains the correct constant.

    let high_bit_set = buffer.create_label();
    let done = buffer.create_label();

    emit_ucomi(buffer, prec, src, RegMem::Reg(tmp_xmm1));
    emit_jcc(buffer, CondCode::Ae, high_bit_set);

    emit_cvts2si(buffer, OperandSize::S64, prec, dest, RegMem::Reg(src));
    emit_jmp(buffer, done);

    buffer.bind_label(high_bit_set);
    if tmp_xmm2 != src {
        emit_movaps_r_rm(buffer, tmp_xmm2, RegMem::Reg(src));
    }
    emit_sse_fpu_r_rm(
        buffer,
        prec,
        SseFpuBinOp::Sub,
        tmp_xmm2,
        RegMem::Reg(tmp_xmm1),
    );
    emit_cvts2si(buffer, OperandSize::S64, prec, dest, RegMem::Reg(tmp_xmm2));
    emit_btc_rm_i(buffer, OperandSize::S64, RegMem::Reg(dest), 63);

    buffer.bind_label(done);
}

fn get_f_1p63(buffer: &mut CodeBuffer<X64Fixup>, prec: SseFpuPrecision) -> Constant {
    match prec {
        SseFpuPrecision::Single => buffer.get_constant(4, &0x5f000000u32.to_le_bytes()),
        SseFpuPrecision::Double => buffer.get_constant(8, &0x43e0000000000000u64.to_le_bytes()),
    }
}

// Single-instruction emission helpers

fn emit_push(buffer: &mut CodeBuffer<X64Fixup>, reg: PhysReg) {
    let mut rex = RexPrefix::new();
    let reg = rex.encode_modrm_base(reg);

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0x50 + reg]);
    });
}

fn emit_pop(buffer: &mut CodeBuffer<X64Fixup>, reg: PhysReg) {
    let mut rex = RexPrefix::new();
    let reg = rex.encode_modrm_base(reg);

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0x58 + reg]);
    });
}

fn emit_mov_r_r(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, src: PhysReg) {
    emit_mov_rm_r(buffer, FullOperandSize::S64, RegMem::Reg(dest), src);
}

fn emit_mov_rm_s32(
    ctx: &EmitContext<'_, X64Machine>,
    pos: Instr,
    state: &mut X64EmitState,
    buffer: &mut CodeBuffer<X64Fixup>,
    dest: RegMem,
    imm: i32,
) {
    // Use a zeroing idiom or the shorter `mov r32, imm32` encoding when possible.

    if let RegMem::Reg(dest) = dest {
        if imm == 0
            && !state
                .block_flag_liveness
                .flags_live_before(ctx.cfg_ctx, ctx.lir, pos)
        {
            // Note: some renamers recognize only the 32-bit instruction as a zeroing idiom.
            emit_alu_r_rm(
                buffer,
                RawAluBinOp::Xor,
                OperandSize::S32,
                dest,
                RegMem::Reg(dest),
            );
            return;
        }

        // Note: sign-extend the immediate before checking.
        if is_uint::<32>(imm as i64 as u64) {
            emit_mov_r_u32(buffer, dest, imm as u32);
            return;
        }
    }

    // Otherwise: move sign-extended imm32 to r/m64.

    let (rex, modrm_sib) = encode_reg_mem_parts(dest, |rex| {
        rex.encode_operand_size(OperandSize::S64);
        0
    });
    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xc7]);
        modrm_sib.emit(sink);
        sink.emit(&(imm as u32).to_le_bytes());
    });
}

fn emit_mov_r_u32(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, imm: u32) {
    let mut rex = RexPrefix::new();
    let dest = rex.encode_modrm_base(dest);
    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xb8 + dest]);
        sink.emit(&imm.to_le_bytes());
    });
}

fn emit_movabs_r_i(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, imm: u64) {
    buffer.instr(|sink| {
        emit_movabs_r_i_instr(sink, dest, imm);
    });
}

fn emit_movabs_r_i_reloc(
    buffer: &mut CodeBuffer<X64Fixup>,
    dest: PhysReg,
    target: BufferRelocTarget,
) {
    buffer.instr_with_reloc(target, 0, RELOC_ABS64, |sink| {
        emit_movabs_r_i_instr(sink, dest, 0)
    });
}

fn emit_movabs_r_i_instr(sink: &mut InstrSink<'_>, dest: PhysReg, imm: u64) -> InstrAnchor {
    let mut rex = RexPrefix::new();
    let dest = rex.encode_modrm_base(dest);
    rex.encode_operand_size(OperandSize::S64);
    rex.emit(sink);
    sink.emit(&[0xb8 + dest]);
    let anchor = sink.anchor();
    sink.emit(&imm.to_le_bytes());
    anchor
}

fn emit_movzx_r_rm(
    buffer: &mut CodeBuffer<X64Fixup>,
    full_op_size: FullOperandSize,
    dest: PhysReg,
    src: RegMem,
) {
    let (opcode, op_size): (&[u8], _) = match full_op_size {
        FullOperandSize::S8 => {
            // Emit a `movzx r32, r/m8`, which will also implicitly clear the high bits.
            (&[0xf, 0xb6], OperandSize::S32)
        }
        FullOperandSize::S16 => {
            // Emit a `movzx r32, r/m16`, which will also implicitly clear the high bits.
            (&[0xf, 0xb7], OperandSize::S32)
        }
        FullOperandSize::S32 => {
            // Emit a simple 32-bit `mov`, which will implicitly clear the high bits.
            (&[0x8b], OperandSize::S32)
        }
        FullOperandSize::S64 => {
            // Emit a simple 64-bit `mov`.
            (&[0x8b], OperandSize::S64)
        }
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(src, |rex| {
        rex.encode_operand_size(op_size);
        if full_op_size == FullOperandSize::S8
            && let RegMem::Reg(src) = src
        {
            rex.use_reg8(src);
        }
        rex.encode_modrm_reg(dest)
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(opcode);
        modrm_sib.emit(sink);
    });
}

fn emit_lea_or_mov(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, addr: RawAddrMode) {
    match addr {
        RawAddrMode::BaseIndexOff {
            base: Some(base),
            index: None,
            offset: 0,
        } => emit_mov_r_r(buffer, dest, base),
        _ => emit_lea(buffer, OperandSize::S64, dest, addr),
    }
}

fn emit_lea(
    buffer: &mut CodeBuffer<X64Fixup>,
    op_size: OperandSize,
    dest: PhysReg,
    addr: RawAddrMode,
) {
    buffer.instr(|sink| {
        emit_lea_instr(sink, op_size, dest, addr);
    });
}

fn emit_lea_rip_reloc(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, target: FunctionRef) {
    buffer.instr_with_reloc(
        BufferRelocTarget::Function(target),
        -4,
        RELOC_PC32,
        |sink| {
            emit_lea_instr(
                sink,
                OperandSize::S64,
                dest,
                RawAddrMode::RipOff { offset: 0 },
            )
        },
    );
}

fn emit_lea_instr(
    sink: &mut InstrSink<'_>,
    op_size: OperandSize,
    dest: PhysReg,
    addr: RawAddrMode,
) -> InstrAnchor {
    let (rex, modrm_sib) = encode_mem_parts(addr, |rex: &mut RexPrefix| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(dest)
    });
    rex.emit(sink);
    sink.emit(&[0x8d]);
    modrm_sib.emit(sink)
}

fn emit_mov_rm_r(
    buffer: &mut CodeBuffer<X64Fixup>,
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

    buffer.instr(|sink| {
        if full_op_size == FullOperandSize::S16 {
            sink.emit(&[PREFIX_OPERAND_SIZE]);
        }

        rex.emit(sink);
        if full_op_size == FullOperandSize::S8 {
            sink.emit(&[0x88]);
        } else {
            sink.emit(&[0x89]);
        }

        modrm_sib.emit(sink);
    });
}

fn emit_imul_r_rm(
    buffer: &mut CodeBuffer<X64Fixup>,
    op_size: OperandSize,
    arg0: PhysReg,
    arg1: RegMem,
) {
    let (rex, modrm_sib) = encode_reg_mem_parts(arg1, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(arg0)
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xf, 0xaf]);
        modrm_sib.emit(sink);
    });
}

fn emit_imul_r_rm_i(
    buffer: &mut CodeBuffer<X64Fixup>,
    op_size: OperandSize,
    dest: PhysReg,
    op1: RegMem,
    imm: i32,
) {
    let use_imm8 = is_sint::<8>(imm as u64);
    let opcode = if use_imm8 { 0x6b } else { 0x69 };

    let (rex, modrm_sib) = encode_reg_mem_parts(op1, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(dest)
    });

    emit_rm_with_imm(buffer, rex, opcode, modrm_sib, imm, use_imm8);
}

fn emit_div_rm(buffer: &mut CodeBuffer<X64Fixup>, op: DivOp, op_size: OperandSize, arg: RegMem) {
    let reg_opcode = match op {
        DivOp::Div => 0x6,
        DivOp::Idiv => 0x7,
    };
    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xf7]);
        modrm_sib.emit(sink);
    });
}

fn emit_convert_word(buffer: &mut CodeBuffer<X64Fixup>, op_size: OperandSize) {
    let mut rex = RexPrefix::new();
    rex.encode_operand_size(op_size);

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0x99]);
    });
}

fn emit_shift_rm_cx(
    buffer: &mut CodeBuffer<X64Fixup>,
    op: ShiftOp,
    op_size: OperandSize,
    arg: RegMem,
) {
    let reg_opcode = encode_shift_op(op);
    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xd3]);
        modrm_sib.emit(sink);
    });
}

fn emit_shift_rm_i(
    buffer: &mut CodeBuffer<X64Fixup>,
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

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[opcode]);
        modrm_sib.emit(sink);
        if emit_imm {
            sink.emit(&[imm]);
        }
    });
}

fn emit_setcc_r(buffer: &mut CodeBuffer<X64Fixup>, code: CondCode, dest: PhysReg) {
    let mut rex = RexPrefix::new();
    rex.use_reg8(dest);
    let dest = rex.encode_modrm_base(dest);

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xf, 0x90 | encode_cond_code(code), encode_modrm_r(0, dest)]);
    });
}

fn emit_btc_rm_i(buffer: &mut CodeBuffer<X64Fixup>, op_size: OperandSize, arg: RegMem, imm: u8) {
    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        0x7
    });
    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xf, 0xba]);
        modrm_sib.emit(sink);
        sink.emit(&[imm]);
    });
}

fn emit_movsx_r_rm(buffer: &mut CodeBuffer<X64Fixup>, width: ExtWidth, dest: PhysReg, src: RegMem) {
    let (opcode, op_size): (&[u8], _) = match width {
        ExtWidth::Ext8_32 => (&[0xf, 0xbe], OperandSize::S32),
        ExtWidth::Ext8_64 => (&[0xf, 0xbe], OperandSize::S64),
        ExtWidth::Ext16_32 => (&[0xf, 0xbf], OperandSize::S32),
        ExtWidth::Ext16_64 => (&[0xf, 0xbf], OperandSize::S64),
        ExtWidth::Ext32_64 => (&[0x63], OperandSize::S64),
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(src, |rex| {
        rex.encode_operand_size(op_size);
        if matches!(width, ExtWidth::Ext8_32 | ExtWidth::Ext8_64)
            && let RegMem::Reg(src) = src
        {
            rex.use_reg8(src);
        }
        rex.encode_modrm_reg(dest)
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(opcode);
        modrm_sib.emit(sink);
    });
}

fn emit_call_rel(buffer: &mut CodeBuffer<X64Fixup>, target: FunctionRef) {
    buffer.instr_with_reloc(
        BufferRelocTarget::Function(target),
        -4,
        RELOC_PC32,
        |sink| {
            sink.emit(&[0xe8]);
            let anchor = sink.anchor();
            sink.emit(&0u32.to_le_bytes());
            anchor
        },
    );
}

fn emit_call_rm(buffer: &mut CodeBuffer<X64Fixup>, target: RegMem) {
    let (rex, modrm_sib) = encode_reg_mem_parts(target, |_rex| 2);
    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xff]);
        modrm_sib.emit(sink);
    });
}

fn emit_jmp(buffer: &mut CodeBuffer<X64Fixup>, target: Label) {
    buffer.uncond_branch(target, X64Fixup::Rela4(-4), |sink| {
        sink.emit(&[0xe9]);
        let anchor = sink.anchor();
        sink.emit(&0u32.to_le_bytes());
        anchor
    });
}

fn emit_jcc(buffer: &mut CodeBuffer<X64Fixup>, code: CondCode, target: Label) {
    buffer.cond_branch(
        target,
        X64Fixup::Rela4(-4),
        |sink| emit_jcc_instr(sink, code),
        |sink| {
            emit_jcc_instr(sink, code.negate());
        },
    );
}

fn emit_jcc_instr(sink: &mut InstrSink<'_>, code: CondCode) -> InstrAnchor {
    let code = encode_cond_code(code);
    sink.emit(&[0xf, 0x80 + code]);
    let anchor = sink.anchor();
    sink.emit(&0u32.to_le_bytes());
    anchor
}

fn emit_ret(buffer: &mut CodeBuffer<X64Fixup>) {
    buffer.instr(|sink| sink.emit(&[0xc3]));
}

fn emit_ud2(buffer: &mut CodeBuffer<X64Fixup>) {
    buffer.instr(|sink| sink.emit(&[0xf, 0xb]));
}

fn emit_movaps_r_rm(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, src: RegMem) {
    emit_sse_fpu_with_legacy_op_size(buffer, SseFpuPrecision::Single, 0x28, dest, src);
}

fn emit_movs_r_rm(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: RegMem,
) {
    emit_sse_fpu_with_mandatory_prefix(buffer, prec, 0x10, dest, src);
}

fn emit_movs_r_rm_rip_reloc(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: BufferRelocTarget,
) {
    let (rex, modrm_sib) =
        encode_reg_mem_parts(RegMem::Mem(RawAddrMode::RipOff { offset: 0 }), |rex| {
            rex.encode_modrm_reg(dest)
        });
    buffer.instr_with_reloc(src, -4, RELOC_PC32, |sink| {
        sink.emit(&[sse_mandatory_prefix_for_prec(prec)]);
        rex.emit(sink);
        sink.emit(&[0xf, 0x10]);
        modrm_sib.emit(sink)
    });
}

fn emit_movs_rm_r(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    dest: RegMem,
    src: PhysReg,
) {
    emit_sse_fpu_with_mandatory_prefix(buffer, prec, 0x11, src, dest);
}

fn emit_cmps(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    code: SseFpuCmpCode,
    arg0_dest: PhysReg,
    arg1: RegMem,
) {
    let (rex, modrm_sib) = encode_reg_mem_parts(arg1, |rex| rex.encode_modrm_reg(arg0_dest));
    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[sse_mandatory_prefix_for_prec(prec), 0xf, 0xc2]);
        modrm_sib.emit(sink);
        sink.emit(&[code as u8]);
    });
}

fn emit_ucomi(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    arg0: PhysReg,
    arg1: RegMem,
) {
    // For some reason, even though this opcode is scalar, it uses the operand-size encoding
    // typically used by the packed instructions.
    emit_sse_fpu_with_legacy_op_size(buffer, prec, 0x2e, arg0, arg1);
}

fn emit_xorps(buffer: &mut CodeBuffer<X64Fixup>, arg0: PhysReg, arg1: RegMem) {
    emit_sse_fpu_with_legacy_op_size(buffer, SseFpuPrecision::Single, 0x57, arg0, arg1);
}

fn emit_cvtsi2s(
    buffer: &mut CodeBuffer<X64Fixup>,
    op_size: OperandSize,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: RegMem,
) {
    emit_sse_fpu_with_mandatory_prefix_and_op_size(buffer, prec, op_size, 0x2a, dest, src);
}

fn emit_cvts2si(
    buffer: &mut CodeBuffer<X64Fixup>,
    op_size: OperandSize,
    prec: SseFpuPrecision,
    dest: PhysReg,
    src: RegMem,
) {
    emit_sse_fpu_with_mandatory_prefix_and_op_size(buffer, prec, op_size, 0x2d, dest, src);
}

fn emit_cvtss2sd(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, src: RegMem) {
    emit_sse_fpu_with_mandatory_prefix(buffer, SseFpuPrecision::Single, 0x5a, dest, src);
}

fn emit_cvtsd2ss(buffer: &mut CodeBuffer<X64Fixup>, dest: PhysReg, src: RegMem) {
    emit_sse_fpu_with_mandatory_prefix(buffer, SseFpuPrecision::Double, 0x5a, dest, src);
}

fn emit_mov_gprm_xmm(
    buffer: &mut CodeBuffer<X64Fixup>,
    op_size: OperandSize,
    dest: RegMem,
    src: PhysReg,
) {
    let (rex, modrm_sib) = encode_reg_mem_parts(dest, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(src)
    });
    buffer.instr(|sink| {
        // This operand size prefix indicates we want XMM and not MM.
        sink.emit(&[PREFIX_OPERAND_SIZE]);
        rex.emit(sink);
        sink.emit(&[0xf, 0x7e]);
        modrm_sib.emit(sink);
    });
}

// Instruction group emission helpers

enum RawAluBinOp {
    Add,
    And,
    Cmp,
    Or,
    Sub,
    Test,
    Xor,
}

impl From<AluBinOp> for RawAluBinOp {
    fn from(op: AluBinOp) -> Self {
        match op {
            AluBinOp::Cmp => Self::Cmp,
            AluBinOp::Sub => Self::Sub,
            AluBinOp::Test => Self::Test,
        }
    }
}

impl From<AluCommBinOp> for RawAluBinOp {
    fn from(op: AluCommBinOp) -> Self {
        match op {
            AluCommBinOp::And => Self::And,
            AluCommBinOp::Or => Self::Or,
            AluCommBinOp::Xor => Self::Xor,
        }
    }
}

fn emit_alu_r_rm(
    buffer: &mut CodeBuffer<X64Fixup>,
    op: RawAluBinOp,
    op_size: OperandSize,
    arg0: PhysReg,
    arg1: RegMem,
) {
    let opcode = match op {
        RawAluBinOp::Add => 0x3,
        RawAluBinOp::And => 0x23,
        RawAluBinOp::Cmp => 0x3b,
        RawAluBinOp::Or => 0xb,
        RawAluBinOp::Sub => 0x2b,
        RawAluBinOp::Test => {
            // This encoding is actually backwards (`test r/m, r`), but it doesn't matter because
            // `test` is commutative and has no outputs other than flags.
            0x85
        }
        RawAluBinOp::Xor => 0x33,
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(arg1, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(arg0)
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[opcode]);
        modrm_sib.emit(sink);
    });
}

fn emit_alu_r64_i(buffer: &mut CodeBuffer<X64Fixup>, op: RawAluBinOp, dest: PhysReg, imm: i32) {
    emit_alu_rm_i(buffer, op, OperandSize::S64, RegMem::Reg(dest), imm);
}

fn emit_alu_rm_i(
    buffer: &mut CodeBuffer<X64Fixup>,
    op: RawAluBinOp,
    op_size: OperandSize,
    arg: RegMem,
    imm: i32,
) {
    let mut use_imm8 = is_sint::<8>(imm as u64);
    let mut opcode = if use_imm8 { 0x83 } else { 0x81 };

    let reg_opcode = match op {
        RawAluBinOp::Add => 0x0,
        RawAluBinOp::And => 0x4,
        RawAluBinOp::Cmp => 0x7,
        RawAluBinOp::Or => 0x1,
        RawAluBinOp::Sub => 0x5,
        RawAluBinOp::Test => {
            // `test` is special. Who knows why.
            opcode = 0xf7;
            use_imm8 = false;
            0x0
        }
        RawAluBinOp::Xor => 0x6,
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    emit_rm_with_imm(buffer, rex, opcode, modrm_sib, imm, use_imm8);
}

fn emit_rm_with_imm(
    buffer: &mut CodeBuffer<X64Fixup>,
    rex: RexPrefix,
    opcode: u8,
    modrm_sib: ModRmSib,
    imm: i32,
    use_imm8: bool,
) {
    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[opcode]);
        modrm_sib.emit(sink);

        if use_imm8 {
            sink.emit(&[imm as u8]);
        } else {
            sink.emit(&imm.to_le_bytes());
        }
    });
}

fn emit_alu_rm(buffer: &mut CodeBuffer<X64Fixup>, op: AluUnOp, op_size: OperandSize, arg: RegMem) {
    let reg_opcode = match op {
        AluUnOp::Not => 0x2,
        AluUnOp::Neg => 0x3,
    };

    let (rex, modrm_sib) = encode_reg_mem_parts(arg, |rex| {
        rex.encode_operand_size(op_size);
        reg_opcode
    });

    buffer.instr(|sink| {
        rex.emit(sink);
        sink.emit(&[0xf7]);
        modrm_sib.emit(sink);
    });
}

fn emit_sse_fpu_r_rm(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    op: SseFpuBinOp,
    arg0: PhysReg,
    arg1: RegMem,
) {
    let opcode = match op {
        SseFpuBinOp::Add => 0x58,
        SseFpuBinOp::Sub => 0x5c,
        SseFpuBinOp::Mul => 0x59,
        SseFpuBinOp::Div => 0x5e,
        // This encoding has an extra immediate, so use a dedicated function.
        SseFpuBinOp::Cmp(code) => return emit_cmps(buffer, prec, code, arg0, arg1),
    };

    emit_sse_fpu_with_mandatory_prefix(buffer, prec, opcode, arg0, arg1);
}

fn emit_sse_fpu_with_legacy_op_size(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    opcode: u8,
    reg: PhysReg,
    reg_mem: RegMem,
) {
    let (rex, modrm_sib) = encode_reg_mem_parts(reg_mem, |rex| rex.encode_modrm_reg(reg));
    buffer.instr(|sink| {
        if prec == SseFpuPrecision::Double {
            sink.emit(&[PREFIX_OPERAND_SIZE]);
        }
        rex.emit(sink);
        sink.emit(&[0xf, opcode]);
        modrm_sib.emit(sink);
    });
}

fn emit_sse_fpu_with_mandatory_prefix(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    opcode: u8,
    reg: PhysReg,
    reg_mem: RegMem,
) {
    emit_sse_fpu_with_mandatory_prefix_and_op_size(
        buffer,
        prec,
        OperandSize::S32,
        opcode,
        reg,
        reg_mem,
    );
}

fn emit_sse_fpu_with_mandatory_prefix_and_op_size(
    buffer: &mut CodeBuffer<X64Fixup>,
    prec: SseFpuPrecision,
    op_size: OperandSize,
    opcode: u8,
    reg: PhysReg,
    reg_mem: RegMem,
) {
    let (rex, modrm_sib) = encode_reg_mem_parts(reg_mem, |rex| {
        rex.encode_operand_size(op_size);
        rex.encode_modrm_reg(reg)
    });
    buffer.instr(|sink| {
        sink.emit(&[sse_mandatory_prefix_for_prec(prec)]);
        rex.emit(sink);
        sink.emit(&[0xf, opcode]);
        modrm_sib.emit(sink);
    });
}

// Prefixes and encoding

#[derive(Clone, Copy)]
enum RegMem {
    Reg(PhysReg),
    Mem(RawAddrMode),
}

#[derive(Clone, Copy)]
enum RawAddrMode {
    BaseIndexOff {
        base: Option<PhysReg>,
        index: Option<(IndexScale, PhysReg)>,
        offset: i32,
    },
    RipOff {
        offset: i32,
    },
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
    fn emit(self, sink: &mut InstrSink<'_>) -> InstrAnchor {
        sink.emit(&[self.modrm]);
        if let Some(sib) = self.sib {
            sink.emit(&[sib]);
        }
        let anchor = sink.anchor();
        match self.mem_mode {
            MemMode::NoDisp => {}
            MemMode::Disp8 => sink.emit(&[self.disp as u8]),
            MemMode::Disp32 | MemMode::Disp32Special => sink.emit(&self.disp.to_le_bytes()),
        }
        anchor
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

    fn emit(self, sink: &mut InstrSink<'_>) {
        let value = 0x40
            | (self.b as u8)
            | ((self.x as u8) << 1)
            | ((self.r as u8) << 2)
            | ((self.w as u8) << 3);
        if self.force || value != 0x40 {
            sink.emit(&[value]);
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
    addr: RawAddrMode,
    get_reg: impl FnOnce(&mut RexPrefix) -> u8,
) -> (RexPrefix, ModRmSib) {
    match addr {
        RawAddrMode::BaseIndexOff {
            base,
            index,
            offset: disp,
        } => encode_base_index_off_mem_parts(base, index, disp, get_reg),
        RawAddrMode::RipOff { offset } => encode_rip_off_mem_parts(offset, get_reg),
    }
}

fn encode_base_index_off_mem_parts(
    base: Option<PhysReg>,
    index: Option<(IndexScale, PhysReg)>,
    offset: i32,
    get_reg: impl FnOnce(&mut RexPrefix) -> u8,
) -> (RexPrefix, ModRmSib) {
    let mut rex = RexPrefix::new();
    let reg = get_reg(&mut rex);

    // These values can have special meanings when placed in the R/M slot.
    let (base_bp, base_sp) = match base {
        Some(base) => {
            let base_rm = encode_reg(base) & 0b111;
            (
                base_rm == encode_reg(REG_RBP),
                base_rm == encode_reg(REG_RSP),
            )
        }
        None => (false, false),
    };

    let mode = if offset == 0 && !base_bp {
        MemMode::NoDisp
    } else if base.is_none() {
        MemMode::Disp32Special
    } else if is_sint::<8>(offset as u64) {
        MemMode::Disp8
    } else {
        MemMode::Disp32
    };

    let need_sib = base.is_none() || base_sp || index.is_some();

    if need_sib {
        let base = base.map_or(SIB_BASE_NONE, |base| rex.encode_modrm_base(base));
        let rm = RM_SIB;
        let (scale, index) = match index {
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
                disp: offset,
                mem_mode: mode,
            },
        )
    } else {
        // Note: we always need an SIB when no base is specified.
        let base = rex.encode_modrm_base(base.unwrap());

        (
            rex,
            ModRmSib {
                modrm: encode_modrm_mem(mode, reg, base),
                sib: None,
                disp: offset,
                mem_mode: mode,
            },
        )
    }
}

fn encode_rip_off_mem_parts(
    offset: i32,
    get_reg: impl FnOnce(&mut RexPrefix) -> u8,
) -> (RexPrefix, ModRmSib) {
    let mut rex = RexPrefix::new();
    let reg = get_reg(&mut rex);
    let mem_mode = MemMode::Disp32Special;

    (
        rex,
        ModRmSib {
            modrm: encode_modrm_mem(mem_mode, reg, RM_NONE),
            sib: None,
            disp: offset,
            mem_mode,
        },
    )
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

        REG_XMM0 => 0,
        REG_XMM1 => 1,
        REG_XMM2 => 2,
        REG_XMM3 => 3,
        REG_XMM4 => 4,
        REG_XMM5 => 5,
        REG_XMM6 => 6,
        REG_XMM7 => 7,
        REG_XMM8 => 8,
        REG_XMM9 => 9,
        REG_XMM10 => 10,
        REG_XMM11 => 11,
        REG_XMM12 => 12,
        REG_XMM13 => 13,
        REG_XMM14 => 14,
        REG_XMM15 => 15,

        _ => unreachable!("unknown register"),
    }
}

fn encode_cond_code(code: CondCode) -> u8 {
    code as u8
}

fn encode_shift_op(op: ShiftOp) -> u8 {
    match op {
        ShiftOp::Shl => 4,
        ShiftOp::Shr => 5,
        ShiftOp::Sar => 7,
    }
}

fn sse_mandatory_prefix_for_prec(prec: SseFpuPrecision) -> u8 {
    match prec {
        SseFpuPrecision::Single => 0xf3,
        SseFpuPrecision::Double => 0xf2,
    }
}

// RM encodings for memory addressing modes
const RM_SIB: u8 = 0b100;
const RM_NONE: u8 = 0b101;

// SIB encodings for lack of base/index registers.
const SIB_BASE_NONE: u8 = 0b101;
const SIB_INDEX_NONE: u8 = 0b100;

// Legacy prefixes
const PREFIX_OPERAND_SIZE: u8 = 0x66;

#[cfg(test)]
mod tests;
