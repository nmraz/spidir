use core::mem;

use alloc::vec::Vec;

use ir::{
    node::{BitwiseF64, FcmpKind, FunctionRef, IcmpKind, MemSize, NodeKind, Type},
    valgraph::{DepValue, Node},
};
use smallvec::{SmallVec, smallvec};
use valmatch::match_value;

use crate::{
    cfg::Block,
    isel::{IselContext, MachineIselError, ParamLoc},
    lir::{
        DefOperand, DefOperandConstraint, OperandPos, PhysReg, PhysRegSet, RegClass, UseOperand,
        VirtReg,
    },
    machine::MachineLower,
    num_utils::{is_sint, is_uint},
    target::x64::{AluCommBinOp, CompoundCondCode, SseFpuBinOp, SseFpuCmpCode, SseFpuPrecision},
};

use super::{
    AddrBase, AddrMode, AluBinOp, AluUnOp, CALLER_SAVED_REGS, CodeModel, CondCode, DivOp, ExtWidth,
    FullOperandSize, OperandSize, RC_GPR, RC_XMM, REG_R8, REG_R9, REG_RAX, REG_RCX, REG_RDI,
    REG_RDX, REG_RSI, REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6,
    REG_XMM7, ShiftOp, X64Instr, X64Machine,
};

const FIXED_ARG_GPR_COUNT: usize = 6;
const FIXED_ARG_GPRS: [PhysReg; FIXED_ARG_GPR_COUNT] =
    [REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9];

const FIXED_ARG_XMM_COUNT: usize = 8;
const FIXED_ARG_XMMS: [PhysReg; FIXED_ARG_XMM_COUNT] = [
    REG_XMM0, REG_XMM1, REG_XMM2, REG_XMM3, REG_XMM4, REG_XMM5, REG_XMM6, REG_XMM7,
];

const FIXED_ARG_REG_COUNT: usize = FIXED_ARG_GPR_COUNT + FIXED_ARG_XMM_COUNT;

impl MachineLower for X64Machine {
    fn reg_class_for_type(&self, ty: Type) -> RegClass {
        match ty {
            Type::I32 | Type::I64 | Type::Ptr => RC_GPR,
            Type::F64 => RC_XMM,
        }
    }

    fn param_locs(&self, param_types: &[Type]) -> Vec<ParamLoc> {
        let mut args = Vec::new();

        let mut fixed_gpr_iter = FIXED_ARG_GPRS.iter();
        let mut fixed_xmm_iter = FIXED_ARG_XMMS.iter();

        let mut stack_idx = 0;

        for &param_type in param_types {
            let reg = match param_type {
                Type::I32 | Type::I64 | Type::Ptr => fixed_gpr_iter.next(),
                Type::F64 => fixed_xmm_iter.next(),
            };

            match reg {
                Some(&reg) => args.push(ParamLoc::Reg { reg }),
                None => {
                    // Frame layout recap:
                    //
                    // +-------+
                    // |  ...  |
                    // +-------+
                    // |  A7   |
                    // +-------+
                    // |  A6   |
                    // +-------+ < rbp + 16
                    // |  RA   |
                    // +-------+ < rbp + 8
                    // |  RBP  |
                    // +-------+ < rbp
                    args.push(ParamLoc::Stack {
                        fp_offset: 16 + 8 * stack_idx,
                    });
                    stack_idx += 1;
                }
            }
        }

        args
    }

    fn make_jump(&self, block: Block) -> Self::Instr {
        X64Instr::Jump(block)
    }

    fn make_fp_relative_load(&self, ty: Type, offset: i32) -> Self::Instr {
        match ty {
            Type::I32 => X64Instr::MovRRbp {
                op_size: FullOperandSize::S32,
                offset,
            },
            Type::I64 | Type::Ptr => X64Instr::MovRRbp {
                op_size: FullOperandSize::S64,
                offset,
            },
            Type::F64 => X64Instr::MovsRRbp {
                prec: SseFpuPrecision::Double,
                offset,
            },
        }
    }

    fn select_instr(
        &self,
        node: Node,
        targets: &[Block],
        ctx: &mut IselContext<'_, '_, Self>,
    ) -> Result<(), MachineIselError>
    where
        Self: Sized,
    {
        let _ = targets;
        match ctx.node_kind(node) {
            &NodeKind::Iconst(val) => select_iconst(ctx, node, val),
            NodeKind::Iadd => select_add(ctx, node),
            NodeKind::And => select_alu_comm(ctx, node, AluCommBinOp::And),
            NodeKind::Or => select_alu_comm(ctx, node, AluCommBinOp::Or),
            NodeKind::Isub => select_alu(ctx, node, AluBinOp::Sub),
            NodeKind::Xor => select_alu_comm(ctx, node, AluCommBinOp::Xor),
            NodeKind::Shl => select_shift(ctx, node, ShiftOp::Shl),
            NodeKind::Lshr => select_shift(ctx, node, ShiftOp::Shr),
            NodeKind::Ashr => select_shift(ctx, node, ShiftOp::Sar),
            NodeKind::Imul => select_imul(ctx, node),
            NodeKind::Udiv => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);
                let (rax, _) = emit_udiv(ctx, op1, op2);
                ctx.copy_vreg(output, rax);
            }
            NodeKind::Urem => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);
                let (_, rdx) = emit_udiv(ctx, op1, op2);
                ctx.copy_vreg(output, rdx);
            }
            NodeKind::Sdiv => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);
                let (rax, _) = emit_sdiv(ctx, op1, op2);
                ctx.copy_vreg(output, rax);
            }
            NodeKind::Srem => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);
                let (_, rdx) = emit_sdiv(ctx, op1, op2);
                ctx.copy_vreg(output, rdx);
            }
            NodeKind::Iext | NodeKind::Itrunc | NodeKind::IntToPtr | NodeKind::PtrToInt => {
                let [input] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let input = ctx.get_value_vreg(input);
                let output = ctx.get_value_vreg(output);
                ctx.copy_vreg(output, input);
            }
            NodeKind::Sfill(width) => {
                let [input] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let ty = ctx.value_type(output);

                match (ty, width) {
                    (Type::I32, 8) => emit_movsx_rr(ctx, input, output, ExtWidth::Ext8_32),
                    (Type::I32, 16) => emit_movsx_rr(ctx, input, output, ExtWidth::Ext16_32),
                    (Type::I64, 8) => emit_movsx_rr(ctx, input, output, ExtWidth::Ext8_64),
                    (Type::I64, 16) => emit_movsx_rr(ctx, input, output, ExtWidth::Ext16_64),
                    (Type::I64, 32) => emit_movsx_rr(ctx, input, output, ExtWidth::Ext32_64),
                    _ => {
                        let full_width = ty.bit_width() as u8;
                        let shift_width = full_width - width;
                        let input = ctx.get_value_vreg(input);
                        let output = ctx.get_value_vreg(output);
                        let temp = ctx.create_temp_vreg(RC_GPR);
                        emit_shift_ri(ctx, ty, input, shift_width, temp, ShiftOp::Shl);
                        emit_shift_ri(ctx, ty, temp, shift_width, output, ShiftOp::Sar);
                    }
                }
            }
            &NodeKind::Icmp(kind) => {
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);

                // Generate an `xor; cmp; setcc`. This is preferable to `cmp; setcc; movzx` because
                // `setcc` modifies only the low byte of its output operand, so clearing the
                // register first avoids the (false) partial dependency on the previous value of the
                // `setcc` output register.
                emit_setcc_sequence(ctx, output, |ctx| select_icmp(ctx, node, kind));
            }
            &NodeKind::Fconst64(val) => select_fconst64(self, ctx, node, val),
            NodeKind::Fadd => emit_fpu_rr(ctx, node, SseFpuBinOp::Add),
            NodeKind::Fsub => emit_fpu_rr(ctx, node, SseFpuBinOp::Sub),
            NodeKind::Fmul => emit_fpu_rr(ctx, node, SseFpuBinOp::Mul),
            NodeKind::Fdiv => emit_fpu_rr(ctx, node, SseFpuBinOp::Div),
            &NodeKind::Fcmp(kind) => select_direct_fcmp(ctx, node, kind),
            NodeKind::SintToFloat => emit_cvtsi2s(ctx, node),
            NodeKind::UintToFloat => select_uinttofloat(ctx, node),
            NodeKind::FloatToSint => emit_cvts2si(ctx, node),
            NodeKind::FloatToUint => select_floattouint(self, ctx, node),
            NodeKind::PtrOff => select_add(ctx, node),
            &NodeKind::Load(mem_size) => select_load(ctx, node, mem_size),
            &NodeKind::Store(mem_size) => select_store(ctx, node, mem_size),
            NodeKind::StackSlot { .. } => {
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);
                let slot = ctx.node_stack_slot(node);
                ctx.emit_instr(
                    X64Instr::StackAddr(slot),
                    &[DefOperand::any_reg(output)],
                    &[],
                );
            }
            NodeKind::BrCond => select_brcond(ctx, node, targets[0], targets[1]),
            NodeKind::Return => match ctx.node_inputs(node).next() {
                None => ctx.emit_instr(X64Instr::Ret, &[], &[]),
                Some(retval) => {
                    let retval_vreg = ctx.get_value_vreg(retval);
                    let retval_reg = match ctx.value_type(retval) {
                        Type::I32 | Type::I64 | Type::Ptr => REG_RAX,
                        Type::F64 => REG_XMM0,
                    };
                    ctx.emit_instr(
                        X64Instr::Ret,
                        &[],
                        &[UseOperand::fixed(retval_vreg, retval_reg)],
                    );
                }
            },
            &NodeKind::FuncAddr(func) => emit_funcaddr(self, ctx, node, func),
            &NodeKind::Call(func) => emit_call(self, ctx, node, func),
            NodeKind::CallInd(_) => {
                let mut vals = ctx.node_inputs(node);
                let target =
                    ctx.get_value_vreg(vals.next().expect("expected indirect call target"));
                emit_callind(ctx, node, target, vals);
            }
            NodeKind::Unreachable => ctx.emit_instr(X64Instr::Ud2, &[], &[]),
            _ => return Err(MachineIselError),
        }
        Ok(())
    }
}

// "Selection" (matching + emission) helpers

fn select_iconst(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, val: u64) {
    let [output] = ctx.node_outputs_exact(node);
    let output = ctx.get_value_vreg(output);

    if is_sint::<32>(val) {
        // Prefer the signed 32-bit variant where possible, since we can use both registers and
        // memory there.
        ctx.emit_instr(
            X64Instr::MovRmS32(val as i32),
            &[DefOperand::any(output)],
            &[],
        );
    } else if is_uint::<32>(val) {
        // If that doesn't fit, use the unsigned 32-bit variant, which only works for registers
        // because it depends on the upper 32 bits being cleared by a 32-bit operation.
        ctx.emit_instr(
            X64Instr::MovRU32(val as u32),
            &[DefOperand::any_reg(output)],
            &[],
        );
    } else {
        // Otherwise, go for the full 64 bits.
        ctx.emit_instr(X64Instr::MovRI64(val), &[DefOperand::any_reg(output)], &[]);
    }
}

fn select_alu(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: AluBinOp) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);
    let op_size = operand_size_for_ty(ty);
    let output = ctx.get_value_vreg(output);

    if op == AluBinOp::Sub && match_iconst(ctx, op1) == Some(0) {
        let op2 = ctx.get_value_vreg(op2);
        emit_alu_r(ctx, ty, op2, output, AluUnOp::Neg);
        return;
    }

    let op1 = ctx.get_value_vreg(op1);

    if let Some(imm) = match_imm32(ctx, op2) {
        ctx.emit_instr(
            X64Instr::AluRmI(op_size, op, imm),
            &[DefOperand::any(output)],
            &[UseOperand::tied(op1, 0)],
        );
        return;
    }

    let op2 = ctx.get_value_vreg(op2);
    ctx.emit_instr(
        X64Instr::AluRRm(op_size, op),
        &[DefOperand::any_reg(output)],
        &[UseOperand::tied(op1, 0), UseOperand::any(op2)],
    );
}

fn select_alu_comm(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: AluCommBinOp) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);
    let op_size = operand_size_for_ty(ty);
    let output = ctx.get_value_vreg(output);

    let c2 = match_iconst(ctx, op2);

    if op == AluCommBinOp::Xor && c2 == Some(ty.all_ones_val()) {
        let op1 = ctx.get_value_vreg(op1);
        emit_alu_r(ctx, ty, op1, output, AluUnOp::Not);
        return;
    }

    if let (AluCommBinOp::And, Some(c2)) = (op, c2) {
        let zext_size = match c2 {
            0xff => Some(FullOperandSize::S8),
            0xffff => Some(FullOperandSize::S16),
            0xffffffff => Some(FullOperandSize::S32),
            _ => None,
        };

        if let Some(zext_size) = zext_size {
            let op1 = ctx.get_value_vreg(op1);
            ctx.emit_instr(
                X64Instr::MovzxRRm(zext_size),
                &[DefOperand::any_reg(output)],
                &[UseOperand::any(op1)],
            );
            return;
        }
    }

    let op1 = ctx.get_value_vreg(op1);

    if let Some(imm) = c2.and_then(|c2| as_imm32(ty, c2)) {
        ctx.emit_instr(
            X64Instr::AluCommRmI(op_size, op, imm),
            &[DefOperand::any(output)],
            &[UseOperand::tied(op1, 0)],
        );
        return;
    }

    let op2 = ctx.get_value_vreg(op2);
    ctx.emit_instr(
        X64Instr::AluCommRR(op_size, op),
        &[DefOperand::any_reg(output)],
        &[UseOperand::soft_tied(op1, 0), UseOperand::soft_tied(op2, 0)],
    );
}

fn select_add(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);
    let op_size = operand_size_for_ty(ty);
    let output = ctx.get_value_vreg(output);

    let op1 = ctx.get_value_vreg(op1);

    if let Some(imm) = match_imm32(ctx, op2) {
        ctx.emit_instr(
            X64Instr::AddRI(op_size, imm),
            &[DefOperand::any_reg(output)],
            &[UseOperand::soft_tied(op1, 0)],
        );
    } else {
        let op2 = ctx.get_value_vreg(op2);
        ctx.emit_instr(
            X64Instr::AddRR(op_size),
            &[DefOperand::any_reg(output)],
            &[UseOperand::soft_tied(op1, 0), UseOperand::soft_tied(op2, 0)],
        );
    }
}

fn select_shift(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: ShiftOp) {
    let [value, amount] = ctx.node_inputs_exact(node);
    let [output] = ctx.node_outputs_exact(node);
    let ty = ctx.value_type(output);

    let output = ctx.get_value_vreg(output);
    let value = ctx.get_value_vreg(value);

    if let Some(amount) = match_iconst(ctx, amount) {
        // Shifting past the bit width of the type produces an unspecified value in spidir anyway,
        // so it's easiest to just mask here.
        emit_shift_ri(ctx, ty, value, amount as u8, output, op);
    } else {
        let amount = ctx.get_value_vreg(amount);
        ctx.emit_instr(
            X64Instr::ShiftRmR(operand_size_for_ty(ty), op),
            &[DefOperand::any(output)],
            &[
                UseOperand::tied(value, 0),
                UseOperand::fixed(amount, REG_RCX),
            ],
        );
    }
}

fn select_imul(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);
    let op_size = operand_size_for_ty(ty);

    let output = ctx.get_value_vreg(output);
    let op1 = ctx.get_value_vreg(op1);

    if let Some(imm) = match_imm32(ctx, op2) {
        ctx.emit_instr(
            X64Instr::ImulRRmI(op_size, imm),
            &[DefOperand::any_reg(output)],
            &[UseOperand::any(op1)],
        );
    } else {
        let op2 = ctx.get_value_vreg(op2);
        ctx.emit_instr(
            X64Instr::ImulRR(op_size),
            &[DefOperand::any_reg(output)],
            &[UseOperand::soft_tied(op1, 0), UseOperand::soft_tied(op2, 0)],
        );
    }
}

fn select_icmp(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, kind: IcmpKind) -> CondCode {
    let [op1, op2] = ctx.node_inputs_exact(node);

    if let Some((op, imm, code)) = match_icmp_imm32(ctx, op1, op2, kind) {
        if imm == 0 {
            emit_alu_rr_discarded(ctx, op, op, AluBinOp::Test);
        } else {
            let op_size = operand_size_for_ty(ctx.value_type(op));
            let op = ctx.get_value_vreg(op);
            ctx.emit_instr(
                X64Instr::AluRmI(op_size, AluBinOp::Cmp, imm),
                &[],
                &[UseOperand::any(op)],
            );
        }
        return code;
    }

    emit_alu_rr_discarded(ctx, op1, op2, AluBinOp::Cmp);
    cond_code_for_icmp(kind)
}

fn select_fconst64(
    machine: &X64Machine,
    ctx: &mut IselContext<'_, '_, X64Machine>,
    node: Node,
    val: BitwiseF64,
) {
    let [output] = ctx.node_outputs_exact(node);
    let output = ctx.get_value_vreg(output);

    if val.bits() == 0 {
        ctx.emit_instr(X64Instr::SseMovRZ, &[DefOperand::any_reg(output)], &[]);
    } else {
        match machine.config.internal_code_model {
            CodeModel::SmallPic => {
                ctx.emit_instr(
                    X64Instr::MovsdConstRel(val.0),
                    &[DefOperand::any_reg(output)],
                    &[],
                );
            }
            CodeModel::LargeAbs => {
                let addr = ctx.create_temp_vreg(RC_GPR);
                ctx.emit_instr(
                    X64Instr::F64ConstAddrAbs(val.0),
                    &[DefOperand::any_reg(addr)],
                    &[],
                );
                ctx.emit_instr(
                    X64Instr::MovsRM(
                        SseFpuPrecision::Double,
                        AddrMode {
                            base: Some(AddrBase::Reg),
                            index: None,
                            offset: 0,
                        },
                    ),
                    &[DefOperand::any_reg(output)],
                    &[UseOperand::any_reg(addr)],
                );
            }
        }
    }
}

fn select_direct_fcmp(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, kind: FcmpKind) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let output = ctx.get_value_vreg(output);

    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    match kind {
        FcmpKind::Oeq => {
            // Unfortunately, unordered `ucomisd` results set ZF, so we need to be a bit more
            // creative in our choice of instructions here. One option is to emit extra code to
            // check PF as well, but LLVM appears to prefer a direct `cmpeqsd` followed by an
            // XMM-to-GPR move. Let's do the same for now, because it results in simpler code and
            // less register pressure.
            emit_fpu_cmp_sequence(ctx, SseFpuCmpCode::Eq, output, op1, op2);
        }
        FcmpKind::One => {
            // We can use a `ucomisd` here because unordered results set ZF, causing us to return
            // false.
            emit_fpu_ucomi_sequence(ctx, CondCode::Ne, output, op1, op2);
        }
        FcmpKind::Olt => {
            // Note: reverse the operands and the condition code so that CF=1, ZF=1 cause us to
            // return false when the result is unordered.
            emit_fpu_ucomi_sequence(ctx, CondCode::A, output, op2, op1);
        }
        FcmpKind::Ole => {
            // Note: reverse the operands and the condition code so that CF=1 causes us to return
            // false when the result is unordered.
            emit_fpu_ucomi_sequence(ctx, CondCode::Ae, output, op2, op1);
        }
        FcmpKind::Ueq => {
            // Now we actually do want unordered results to return true, so a simple `sete` is
            // correct.
            emit_fpu_ucomi_sequence(ctx, CondCode::E, output, op1, op2);
        }
        FcmpKind::Une => {
            // The `neq` predicate in SSE `cmps[sd]` returns the logical inverse of the `eq`
            // predicate, so DeMorgan's laws give us exactly what we want.
            emit_fpu_cmp_sequence(ctx, SseFpuCmpCode::Neq, output, op1, op2);
        }
        FcmpKind::Ult => {
            // Finally, the straightforward condition code actually does what we want (we also get
            // CF=1 on unordered inputs).
            emit_fpu_ucomi_sequence(ctx, CondCode::B, output, op1, op2);
        }
        FcmpKind::Ule => {
            // Once again, this condition code achieves exactly what we need.
            emit_fpu_ucomi_sequence(ctx, CondCode::Be, output, op1, op2);
        }
    };
}

fn select_uinttofloat(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [input] = ctx.node_inputs_exact(node);

    let input_ty = ctx.value_type(input);

    let output = ctx.get_value_vreg(output);
    let input = ctx.get_value_vreg(input);

    match input_ty {
        Type::I32 => {
            let tmp = ctx.create_temp_vreg(RC_GPR);
            ctx.emit_instr(
                X64Instr::MovzxRRm(FullOperandSize::S32),
                &[DefOperand::any_reg(tmp)],
                &[UseOperand::any(input)],
            );
            ctx.emit_instr(
                X64Instr::Cvtsi2s(OperandSize::S64, SseFpuPrecision::Double),
                &[DefOperand::any_reg(output)],
                &[UseOperand::any(tmp)],
            );
        }
        Type::I64 => {
            let tmp_gpr1 = ctx.create_temp_vreg(RC_GPR);
            let tmp_gpr2 = ctx.create_temp_vreg(RC_GPR);

            ctx.emit_instr(
                X64Instr::PseudoUint64ToFloat(SseFpuPrecision::Double),
                &[
                    DefOperand::any_reg(output),
                    DefOperand::any_reg(tmp_gpr1),
                    DefOperand::any_reg(tmp_gpr2),
                ],
                &[UseOperand::soft_tied(input, 1)],
            );
        }
        _ => unreachable!(),
    }
}

fn select_floattouint(machine: &X64Machine, ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [input] = ctx.node_inputs_exact(node);

    let output_ty = ctx.value_type(output);

    let output = ctx.get_value_vreg(output);
    let input = ctx.get_value_vreg(input);

    match output_ty {
        Type::I32 => {
            ctx.emit_instr(
                X64Instr::Cvts2si(OperandSize::S64, SseFpuPrecision::Double),
                &[DefOperand::any_reg(output)],
                &[UseOperand::any(input)],
            );
        }
        Type::I64 => {
            let instr = match machine.config.internal_code_model {
                CodeModel::SmallPic => X64Instr::PseudoFloatToUint64Rel(SseFpuPrecision::Double),
                CodeModel::LargeAbs => X64Instr::PseudoFloatToUint64Abs(SseFpuPrecision::Double),
            };

            let tmp_xmm1 = ctx.create_temp_vreg(RC_XMM);
            let tmp_xmm2 = ctx.create_temp_vreg(RC_XMM);

            ctx.emit_instr(
                instr,
                &[
                    DefOperand::any_reg(output),
                    DefOperand::new(tmp_xmm1, DefOperandConstraint::AnyReg, OperandPos::Early),
                    DefOperand::any_reg(tmp_xmm2),
                ],
                &[UseOperand::soft_tied(input, 2)],
            );
        }
        _ => unreachable!(),
    }
}

fn select_load(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, mem_size: MemSize) {
    let [addr] = ctx.node_inputs_exact(node);
    let [output] = ctx.node_outputs_exact(node);
    let ty = ctx.value_type(output);

    let output = ctx.get_value_vreg(output);

    let mut uses = SmallVec::new();
    let addr_mode = select_addr_mode(ctx, addr, &mut uses);

    match ty {
        Type::I32 | Type::I64 | Type::Ptr => {
            let op_size = operand_size_for_mem_size(mem_size);
            ctx.emit_instr(
                X64Instr::MovRM(op_size, addr_mode),
                &[DefOperand::any_reg(output)],
                &uses,
            );
        }
        Type::F64 => {
            ctx.emit_instr(
                X64Instr::MovsRM(SseFpuPrecision::Double, addr_mode),
                &[DefOperand::any_reg(output)],
                &uses,
            );
        }
    }
}

fn select_store(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, mem_size: MemSize) {
    let [value, addr] = ctx.node_inputs_exact(node);
    let ty = ctx.value_type(value);
    let value = ctx.get_value_vreg(value);

    let mut uses = smallvec![UseOperand::any_reg(value)];
    let addr_mode = select_addr_mode(ctx, addr, &mut uses);

    match ty {
        Type::I32 | Type::I64 | Type::Ptr => {
            let op_size = operand_size_for_mem_size(mem_size);
            ctx.emit_instr(X64Instr::MovMR(op_size, addr_mode), &[], &uses);
        }
        Type::F64 => {
            ctx.emit_instr(
                X64Instr::MovsMR(SseFpuPrecision::Double, addr_mode),
                &[],
                &uses,
            );
        }
    }
}

fn select_addr_mode(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    value: DepValue,
    uses: &mut SmallVec<[UseOperand; 4]>,
) -> AddrMode {
    match_value! {
        if let NodeKind::PtrOff[val base, &NodeKind::Iconst(offset)] = ctx, value {
            if let Some(offset) = as_imm32(Type::I64, offset) {
                let base = select_addr_base(ctx, base, uses);
                return AddrMode {
                    base: Some(base),
                    index: None,
                    offset
                };
            }
        }
    }

    let base = select_addr_base(ctx, value, uses);
    AddrMode {
        base: Some(base),
        index: None,
        offset: 0,
    }
}

fn select_addr_base(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    value: DepValue,
    uses: &mut SmallVec<[UseOperand; 4]>,
) -> AddrBase {
    match_value! {
        if let node n @ NodeKind::StackSlot { .. } = ctx, value {
            return AddrBase::Stack(ctx.node_stack_slot(n));
        }
    };

    uses.push(UseOperand::any_reg(ctx.get_value_vreg(value)));
    AddrBase::Reg
}

fn select_brcond(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    node: Node,
    true_target: Block,
    false_target: Block,
) {
    let [cond] = ctx.node_inputs_exact(node);
    if ctx.has_one_use(cond) {
        match_value! {
            if let node cond_node @ &NodeKind::Icmp(kind) = ctx, cond {
                let cond_code = select_icmp(ctx, cond_node, kind);
                ctx.emit_instr(
                    X64Instr::Jumpcc(cond_code, true_target, false_target),
                    &[],
                    &[],
                );
                return;
            }
        }

        match_value! {
            if let node cmp_node @ &NodeKind::Fcmp(kind) = ctx, cond {
                return select_fcmp_brcond(ctx, cmp_node, kind, true_target, false_target);
            }
        }
    }

    emit_alu_rr_discarded(ctx, cond, cond, AluBinOp::Test);
    ctx.emit_instr(
        X64Instr::Jumpcc(CondCode::Ne, true_target, false_target),
        &[],
        &[],
    );
}

fn select_fcmp_brcond(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    cmp_node: Node,
    cmp_kind: FcmpKind,
    true_target: Block,
    false_target: Block,
) {
    let [op1, op2] = ctx.node_inputs_exact(cmp_node);

    let mut op1 = ctx.get_value_vreg(op1);
    let mut op2 = ctx.get_value_vreg(op2);

    // See `select_direct_fcmp` for more detail about what's going on here.
    let (swap_cmp_operands, branch_instr) = match cmp_kind {
        FcmpKind::Oeq => (
            false,
            X64Instr::CompundJumpcc(CompoundCondCode::FpuOeq, true_target, false_target),
        ),
        FcmpKind::One => (
            false,
            X64Instr::Jumpcc(CondCode::Ne, true_target, false_target),
        ),
        FcmpKind::Olt => (
            true,
            X64Instr::Jumpcc(CondCode::A, true_target, false_target),
        ),
        FcmpKind::Ole => (
            true,
            X64Instr::Jumpcc(CondCode::Ae, true_target, false_target),
        ),
        FcmpKind::Ueq => (
            false,
            X64Instr::Jumpcc(CondCode::E, true_target, false_target),
        ),
        FcmpKind::Une => (
            false,
            X64Instr::CompundJumpcc(CompoundCondCode::FpuUne, true_target, false_target),
        ),
        FcmpKind::Ult => (
            false,
            X64Instr::Jumpcc(CondCode::B, true_target, false_target),
        ),
        FcmpKind::Ule => (
            false,
            X64Instr::Jumpcc(CondCode::Be, true_target, false_target),
        ),
    };

    if swap_cmp_operands {
        mem::swap(&mut op1, &mut op2);
    }

    ctx.emit_instr(
        X64Instr::Ucomi(SseFpuPrecision::Double),
        &[],
        &[UseOperand::any_reg(op1), UseOperand::any(op2)],
    );

    ctx.emit_instr(branch_instr, &[], &[]);
}

// Raw emission helpers

fn emit_funcaddr(
    machine: &X64Machine,
    ctx: &mut IselContext<'_, '_, X64Machine>,
    node: Node,
    func: FunctionRef,
) {
    let [output] = ctx.node_outputs_exact(node);
    let output = ctx.get_value_vreg(output);

    match machine.code_model_for_function(func) {
        CodeModel::SmallPic => {
            ctx.emit_instr(
                X64Instr::FuncAddrRel(func),
                &[DefOperand::any_reg(output)],
                &[],
            );
        }
        CodeModel::LargeAbs => {
            ctx.emit_instr(
                X64Instr::FuncAddrAbs(func),
                &[DefOperand::any_reg(output)],
                &[],
            );
        }
    }
}

fn emit_call(
    machine: &X64Machine,
    ctx: &mut IselContext<'_, '_, X64Machine>,
    node: Node,
    func: FunctionRef,
) {
    match machine.code_model_for_function(func) {
        CodeModel::SmallPic => emit_call_rel(ctx, node, func),
        CodeModel::LargeAbs => emit_call_abs(ctx, node, func),
    }
}

fn emit_call_rel(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, func: FunctionRef) {
    emit_call_wrapper(ctx, node, ctx.node_inputs(node), |ctx, retvals, args| {
        ctx.emit_instr_with_clobbers(
            X64Instr::CallRel(func),
            retvals,
            args,
            PhysRegSet::from_iter(CALLER_SAVED_REGS),
        );
    });
}

fn emit_call_abs(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, func: FunctionRef) {
    emit_call_wrapper(ctx, node, ctx.node_inputs(node), |ctx, retvals, args| {
        let target = ctx.create_temp_vreg(RC_GPR);
        ctx.emit_instr(
            X64Instr::FuncAddrAbs(func),
            &[DefOperand::any_reg(target)],
            &[],
        );

        let mut uses: SmallVec<[_; FIXED_ARG_REG_COUNT + 1]> = smallvec![UseOperand::any(target)];
        uses.extend_from_slice(args);
        ctx.emit_instr_with_clobbers(
            X64Instr::CallRm,
            retvals,
            &uses,
            PhysRegSet::from_iter(CALLER_SAVED_REGS),
        );
    })
}

fn emit_callind(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    node: Node,
    target: VirtReg,
    args: impl Iterator<Item = DepValue>,
) {
    emit_call_wrapper(ctx, node, args, |ctx, retvals, args| {
        let mut uses: SmallVec<[_; FIXED_ARG_REG_COUNT + 1]> = smallvec![UseOperand::any(target)];
        uses.extend_from_slice(args);
        ctx.emit_instr_with_clobbers(
            X64Instr::CallRm,
            retvals,
            &uses,
            PhysRegSet::from_iter(CALLER_SAVED_REGS),
        );
    })
}

fn emit_call_wrapper(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    node: Node,
    args: impl Iterator<Item = DepValue>,
    emit_inner: impl FnOnce(&mut IselContext<'_, '_, X64Machine>, &[DefOperand], &[UseOperand]),
) {
    let mut reg_args: SmallVec<[_; FIXED_ARG_REG_COUNT]> = SmallVec::new();
    let mut stack_args: SmallVec<[_; 4]> = SmallVec::new();
    let mut stack_size = 0;

    let mut fixed_gpr_iter = FIXED_ARG_GPRS.iter();
    let mut fixed_xmm_iter = FIXED_ARG_XMMS.iter();

    let mut has_stack_xmm = false;

    for arg in args {
        let ty = ctx.value_type(arg);
        let arg = ctx.get_value_vreg(arg);

        let is_xmm = match ty {
            Type::I32 | Type::I64 | Type::Ptr => false,
            Type::F64 => true,
        };

        let reg = if is_xmm {
            fixed_xmm_iter.next()
        } else {
            fixed_gpr_iter.next()
        };

        match reg {
            Some(&reg) => reg_args.push(UseOperand::fixed(arg, reg)),
            None => {
                stack_args.push((arg, ty));
                has_stack_xmm |= is_xmm;
                stack_size += 8;
            }
        }
    }

    if stack_size % 16 != 0 {
        // Just pushing these arguments would misalign the stack, so make sure to compensate.
        stack_size += 8;
    }

    if !has_stack_xmm {
        // When we have only GPRs passed on the stack, using pushes results in smaller code.

        // If the stack requires extra padding, adjust it manually. Note that this adjustment can
        // only ever be 8 bytes, which the emitter can encode compactly as a push.
        let stack_arg_size = 8 * stack_args.len() as i32;
        if stack_size != stack_arg_size {
            ctx.emit_instr(X64Instr::AddSp(stack_arg_size - stack_size), &[], &[]);
        }

        for &(stack_arg, _) in stack_args.iter().rev() {
            ctx.emit_instr(X64Instr::Push, &[], &[UseOperand::any_reg(stack_arg)]);
        }
    } else {
        // When we need to pass values held XMM registers via the stack, we need a manual subtract
        // and move anyway, so use it for all stack arguments.
        ctx.emit_instr(X64Instr::AddSp(-stack_size), &[], &[]);

        let mut offset = 0;
        for &(stack_arg, ty) in &stack_args {
            let addr_mode = AddrMode {
                base: Some(AddrBase::Rsp),
                index: None,
                offset,
            };

            let instr = match ty {
                Type::I32 => X64Instr::MovMR(FullOperandSize::S32, addr_mode),
                Type::I64 | Type::Ptr => X64Instr::MovMR(FullOperandSize::S64, addr_mode),
                Type::F64 => X64Instr::MovsMR(SseFpuPrecision::Double, addr_mode),
            };

            ctx.emit_instr(instr, &[], &[UseOperand::any_reg(stack_arg)]);

            offset += 8;
        }
    }

    let retval = ctx.node_outputs(node).next().map(|retval: DepValue| {
        let reg = match ctx.value_type(retval) {
            Type::I32 | Type::I64 | Type::Ptr => REG_RAX,
            Type::F64 => REG_XMM0,
        };
        let retval = ctx.get_value_vreg(retval);
        DefOperand::fixed(retval, reg)
    });

    let call_defs = retval.as_slice();
    emit_inner(ctx, call_defs, &reg_args);

    if stack_size > 0 {
        ctx.emit_instr(X64Instr::AddSp(stack_size), &[], &[]);
    }
}

fn emit_udiv(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    op1: DepValue,
    op2: DepValue,
) -> (VirtReg, VirtReg) {
    let ty = ctx.value_type(op1);
    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    let rdx_in = ctx.create_temp_vreg(RC_GPR);
    emit_mov_rz(ctx, rdx_in);

    let rax_out = ctx.create_temp_vreg(RC_GPR);
    let rdx_out = ctx.create_temp_vreg(RC_GPR);
    ctx.emit_instr(
        X64Instr::Div(operand_size_for_ty(ty), DivOp::Div),
        &[
            DefOperand::fixed(rax_out, REG_RAX),
            DefOperand::fixed(rdx_out, REG_RDX),
        ],
        &[
            UseOperand::fixed(op1, REG_RAX),
            UseOperand::fixed(rdx_in, REG_RDX),
            UseOperand::any(op2),
        ],
    );

    (rax_out, rdx_out)
}

fn emit_sdiv(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    op1: DepValue,
    op2: DepValue,
) -> (VirtReg, VirtReg) {
    let ty = ctx.value_type(op1);
    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    let rdx_in = ctx.create_temp_vreg(RC_GPR);
    ctx.emit_instr(
        X64Instr::ConvertWord(operand_size_for_ty(ty)),
        &[DefOperand::fixed(rdx_in, REG_RDX)],
        &[UseOperand::fixed(op1, REG_RAX)],
    );

    let rax_out = ctx.create_temp_vreg(RC_GPR);
    let rdx_out = ctx.create_temp_vreg(RC_GPR);
    ctx.emit_instr(
        X64Instr::Div(operand_size_for_ty(ty), DivOp::Idiv),
        &[
            DefOperand::fixed(rax_out, REG_RAX),
            DefOperand::fixed(rdx_out, REG_RDX),
        ],
        &[
            UseOperand::fixed(op1, REG_RAX),
            UseOperand::fixed(rdx_in, REG_RDX),
            UseOperand::any(op2),
        ],
    );

    (rax_out, rdx_out)
}

fn emit_mov_rz(ctx: &mut IselContext<'_, '_, X64Machine>, output: VirtReg) {
    ctx.emit_instr(X64Instr::MovRmS32(0), &[DefOperand::any(output)], &[]);
}

fn emit_movsx_rr(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    input: DepValue,
    output: DepValue,
    width: ExtWidth,
) {
    let input = ctx.get_value_vreg(input);
    let output = ctx.get_value_vreg(output);
    ctx.emit_instr(
        X64Instr::MovsxRRm(width),
        &[DefOperand::any_reg(output)],
        &[UseOperand::any(input)],
    );
}

fn emit_shift_ri(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    ty: Type,
    input: VirtReg,
    amount: u8,
    output: VirtReg,
    op: ShiftOp,
) {
    ctx.emit_instr(
        X64Instr::ShiftRmI(operand_size_for_ty(ty), op, amount),
        &[DefOperand::any(output)],
        &[UseOperand::tied(input, 0)],
    );
}

fn emit_alu_rr_discarded(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    op1: DepValue,
    op2: DepValue,
    op: AluBinOp,
) {
    let ty = ctx.value_type(op1);

    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    ctx.emit_instr(
        X64Instr::AluRRm(operand_size_for_ty(ty), op),
        &[],
        &[UseOperand::any_reg(op1), UseOperand::any(op2)],
    );
}

fn emit_alu_r(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    ty: Type,
    input: VirtReg,
    output: VirtReg,
    op: AluUnOp,
) {
    ctx.emit_instr(
        X64Instr::AluRm(operand_size_for_ty(ty), op),
        &[DefOperand::any(output)],
        &[UseOperand::tied(input, 0)],
    );
}

fn emit_setcc_sequence(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    output: VirtReg,
    f: impl FnOnce(&mut IselContext<'_, '_, X64Machine>) -> CondCode,
) {
    let temp = ctx.create_temp_vreg(RC_GPR);

    // Note: put the move of 0 above so flags aren't live and it can be turned into an `xor`.
    emit_mov_rz(ctx, temp);

    // Emit the flag-affecting instruction/sequence
    let cond_code = f(ctx);

    // Perform the flag test.
    ctx.emit_instr(
        X64Instr::Setcc(cond_code),
        &[DefOperand::any_reg(output)],
        &[UseOperand::tied(temp, 0)],
    );
}

fn emit_fpu_rr(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: SseFpuBinOp) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let output = ctx.get_value_vreg(output);
    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    ctx.emit_instr(
        X64Instr::SseScalarFpuRRm(SseFpuPrecision::Double, op),
        &[DefOperand::any_reg(output)],
        &[UseOperand::tied(op1, 0), UseOperand::any(op2)],
    );
}

fn emit_fpu_cmp_sequence(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    code: SseFpuCmpCode,
    output: VirtReg,
    op1: VirtReg,
    op2: VirtReg,
) {
    let tmp_xmm_out = ctx.create_temp_vreg(RC_XMM);
    let tmp_gpr_out = ctx.create_temp_vreg(RC_GPR);
    ctx.emit_instr(
        X64Instr::SseScalarFpuRRm(SseFpuPrecision::Double, SseFpuBinOp::Cmp(code)),
        &[DefOperand::any_reg(tmp_xmm_out)],
        &[UseOperand::tied(op1, 0), UseOperand::any(op2)],
    );
    ctx.emit_instr(
        X64Instr::MovGprmXmm(OperandSize::S64),
        &[DefOperand::any(tmp_gpr_out)],
        &[UseOperand::any_reg(tmp_xmm_out)],
    );
    ctx.emit_instr(
        X64Instr::AluCommRmI(OperandSize::S32, AluCommBinOp::And, 1),
        &[DefOperand::any(output)],
        &[UseOperand::tied(tmp_gpr_out, 0)],
    );
}

fn emit_fpu_ucomi_sequence(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    code: CondCode,
    output: VirtReg,
    op1: VirtReg,
    op2: VirtReg,
) {
    emit_setcc_sequence(ctx, output, |ctx| {
        ctx.emit_instr(
            X64Instr::Ucomi(SseFpuPrecision::Double),
            &[],
            &[UseOperand::any_reg(op1), UseOperand::any(op2)],
        );
        code
    });
}

fn emit_cvtsi2s(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [input] = ctx.node_inputs_exact(node);

    let input_ty = ctx.value_type(input);

    let input = ctx.get_value_vreg(input);
    let output = ctx.get_value_vreg(output);

    ctx.emit_instr(
        X64Instr::Cvtsi2s(operand_size_for_ty(input_ty), SseFpuPrecision::Double),
        &[DefOperand::any_reg(output)],
        &[UseOperand::any(input)],
    );
}

fn emit_cvts2si(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [input] = ctx.node_inputs_exact(node);

    let output_ty = ctx.value_type(output);

    let input = ctx.get_value_vreg(input);
    let output = ctx.get_value_vreg(output);

    ctx.emit_instr(
        X64Instr::Cvts2si(operand_size_for_ty(output_ty), SseFpuPrecision::Double),
        &[DefOperand::any_reg(output)],
        &[UseOperand::any(input)],
    );
}

// Matching helpers

fn match_icmp_imm32(
    ctx: &IselContext<'_, '_, X64Machine>,
    op1: DepValue,
    op2: DepValue,
    mut kind: IcmpKind,
) -> Option<(DepValue, i32, CondCode)> {
    if let Some(mut c2) = match_imm32(ctx, op2) {
        // Convert canonical `x < 1` to `x <= 0`.
        if c2 == 1 && kind == IcmpKind::Slt {
            c2 = 0;
            kind = IcmpKind::Sle;
        }

        return Some((op1, c2, cond_code_for_icmp_ri(kind, c2)));
    }

    if let Some(mut c1) = match_imm32(ctx, op1) {
        // Convert canonical `-1 < x` to `0 <= x`.
        if c1 == -1 && kind == IcmpKind::Slt {
            c1 = 0;
            kind = IcmpKind::Sle;
        }

        return Some((op2, c1, cond_code_for_icmp_ir(kind, c1)));
    }

    None
}

fn match_imm32(ctx: &IselContext<'_, '_, X64Machine>, value: DepValue) -> Option<i32> {
    match_iconst(ctx, value).and_then(|const_val| as_imm32(ctx.value_type(value), const_val))
}

fn as_imm32(ty: Type, val: u64) -> Option<i32> {
    if ty == Type::I32 || is_sint::<32>(val) {
        return Some(val as i32);
    }

    None
}

fn match_iconst(ctx: &IselContext<'_, '_, X64Machine>, value: DepValue) -> Option<u64> {
    match_value! {
        if let &NodeKind::Iconst(val) = ctx, value {
            return Some(val);
        }
    }
    None
}

fn cond_code_for_icmp_ri(kind: IcmpKind, imm: i32) -> CondCode {
    if imm == 0 {
        cond_code_for_icmp_rz(kind)
    } else {
        cond_code_for_icmp(kind)
    }
}

fn cond_code_for_icmp_ir(kind: IcmpKind, imm: i32) -> CondCode {
    if imm == 0 {
        cond_code_for_icmp_zr(kind)
    } else {
        cond_code_for_flipped_icmp(kind)
    }
}

fn cond_code_for_icmp_rz(kind: IcmpKind) -> CondCode {
    match kind {
        IcmpKind::Eq => CondCode::E,
        IcmpKind::Ne => CondCode::Ne,
        IcmpKind::Slt => CondCode::S,
        IcmpKind::Sle => CondCode::Le,
        IcmpKind::Ult => CondCode::B,
        IcmpKind::Ule => CondCode::E,
    }
}

fn cond_code_for_icmp_zr(kind: IcmpKind) -> CondCode {
    match kind {
        IcmpKind::Eq => CondCode::E,
        IcmpKind::Ne => CondCode::Ne,
        IcmpKind::Slt => CondCode::G,
        IcmpKind::Sle => CondCode::Ns,
        IcmpKind::Ult => CondCode::Ne,
        IcmpKind::Ule => CondCode::Ae,
    }
}

fn cond_code_for_icmp(kind: IcmpKind) -> CondCode {
    match kind {
        IcmpKind::Eq => CondCode::E,
        IcmpKind::Ne => CondCode::Ne,
        IcmpKind::Slt => CondCode::L,
        IcmpKind::Sle => CondCode::Le,
        IcmpKind::Ult => CondCode::B,
        IcmpKind::Ule => CondCode::Be,
    }
}

fn cond_code_for_flipped_icmp(kind: IcmpKind) -> CondCode {
    match kind {
        IcmpKind::Eq => CondCode::E,
        IcmpKind::Ne => CondCode::Ne,
        IcmpKind::Slt => CondCode::G,
        IcmpKind::Sle => CondCode::Ge,
        IcmpKind::Ult => CondCode::A,
        IcmpKind::Ule => CondCode::Ae,
    }
}

fn operand_size_for_ty(ty: Type) -> OperandSize {
    match ty {
        Type::I32 => OperandSize::S32,
        Type::I64 => OperandSize::S64,
        Type::F64 => OperandSize::S64,
        Type::Ptr => OperandSize::S64,
    }
}

fn operand_size_for_mem_size(mem_size: MemSize) -> FullOperandSize {
    match mem_size {
        MemSize::S1 => FullOperandSize::S8,
        MemSize::S2 => FullOperandSize::S16,
        MemSize::S4 => FullOperandSize::S32,
        MemSize::S8 => FullOperandSize::S64,
    }
}
