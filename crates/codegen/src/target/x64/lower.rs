use core::slice;

use alloc::vec::Vec;

use ir::{
    node::{FunctionRef, IcmpKind, MemSize, NodeKind, Type},
    valgraph::{DepValue, Node},
};
use smallvec::{smallvec, SmallVec};

use crate::{
    cfg::Block,
    isel::IselContext,
    lir::{DefOperand, PhysReg, PhysRegSet, RegClass, StackSlot, UseOperand, VirtReg},
    machine::{MachineIselError, MachineLower, ParamLoc},
    num_utils::{is_sint, is_uint},
};

use super::{
    AluOp, CodeModel, CondCode, DivOp, ExtWidth, FullOperandSize, OperandSize, ShiftOp, X64Instr,
    X64Machine, CALLER_SAVED_REGS, RC_GPR, REG_R8, REG_R9, REG_RAX, REG_RCX, REG_RDI, REG_RDX,
    REG_RSI,
};

const FIXED_ARG_COUNT: usize = 6;
const FIXED_ARG_REGS: [PhysReg; FIXED_ARG_COUNT] =
    [REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9];

impl MachineLower for X64Machine {
    fn reg_class_for_type(&self, ty: Type) -> RegClass {
        match ty {
            Type::I32 | Type::I64 | Type::Ptr => RC_GPR,
            Type::F64 => unimplemented!("floating point"),
        }
    }

    fn param_locs(&self, param_types: &[Type]) -> Vec<ParamLoc> {
        assert!(
            param_types
                .iter()
                .all(|&ty| self.reg_class_for_type(ty) == RC_GPR),
            "non-integer arguments not supported"
        );

        let mut args: Vec<_> = FIXED_ARG_REGS
            .into_iter()
            .take(param_types.len())
            .map(|reg| ParamLoc::Reg { reg })
            .collect();

        if param_types.len() > FIXED_ARG_REGS.len() {
            for i in 0..param_types.len() - FIXED_ARG_REGS.len() {
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
                    fp_offset: 16 + 8 * i as i32,
                });
            }
        }

        args
    }

    fn make_jump(&self, block: Block) -> Self::Instr {
        X64Instr::Jump(block)
    }

    fn make_fp_relative_load(&self, offset: i32) -> Self::Instr {
        X64Instr::MovRRbp { offset }
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
            NodeKind::IConst(val) => select_iconst(ctx, node, val),
            NodeKind::Iadd => select_alu(ctx, node, AluOp::Add),
            NodeKind::And => select_alu(ctx, node, AluOp::And),
            NodeKind::Or => select_alu(ctx, node, AluOp::Or),
            NodeKind::Isub => select_alu(ctx, node, AluOp::Sub),
            NodeKind::Xor => select_alu(ctx, node, AluOp::Xor),
            NodeKind::Shl => select_shift(ctx, node, ShiftOp::Shl),
            NodeKind::Lshr => select_shift(ctx, node, ShiftOp::Shr),
            NodeKind::Ashr => select_shift(ctx, node, ShiftOp::Sar),
            NodeKind::Imul => emit_imul_rr(ctx, node),
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
                        let full_width = ty.bit_width().unwrap() as u8;
                        let shift_width = full_width - width;
                        let input = ctx.get_value_vreg(input);
                        let output = ctx.get_value_vreg(output);
                        let temp = ctx.create_temp_vreg(RC_GPR);
                        emit_shift_ri(ctx, ty, input, shift_width, temp, ShiftOp::Shl);
                        emit_shift_ri(ctx, ty, temp, shift_width, output, ShiftOp::Sar);
                    }
                }
            }
            NodeKind::Icmp(kind) => {
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);

                // Generate an `xor; cmp; setcc`. This is preferable to `cmp; setcc; movzx` because
                // `setcc` modifies only the low byte of its output operand, so clearing the
                // register first avoids the (false) partial dependency on the previous value of the
                // `setcc` output register.

                let temp = ctx.create_temp_vreg(RC_GPR);

                // Note: the `xor` must precede the `cmp` because it clobbers flags.
                emit_mov_rz(ctx, temp);
                let cond_code = select_icmp(ctx, node, kind);
                ctx.emit_instr(
                    X64Instr::Setcc(cond_code),
                    &[DefOperand::any_reg(output)],
                    &[UseOperand::tied(temp, 0)],
                );
            }
            NodeKind::PtrOff => select_alu(ctx, node, AluOp::Add),
            NodeKind::Load(mem_size) => select_load(ctx, node, mem_size),
            NodeKind::Store(mem_size) => select_store(ctx, node, mem_size),
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
            NodeKind::BrCond => {
                let [cond] = ctx.node_inputs_exact(node);
                if ctx.has_one_use(cond) {
                    if let Some((cond_node, 0)) = ctx.value_def(cond) {
                        if let NodeKind::Icmp(kind) = ctx.node_kind(cond_node) {
                            let cond_code = select_icmp(ctx, cond_node, kind);
                            ctx.emit_instr(
                                X64Instr::Jumpcc(cond_code, targets[0], targets[1]),
                                &[],
                                &[],
                            );
                            return Ok(());
                        }
                    }
                }

                emit_alu_rr_discarded(ctx, cond, cond, AluOp::Test);
                ctx.emit_instr(
                    X64Instr::Jumpcc(CondCode::Ne, targets[0], targets[1]),
                    &[],
                    &[],
                );
            }
            NodeKind::Return => match ctx.node_inputs(node).next() {
                None => ctx.emit_instr(X64Instr::Ret, &[], &[]),
                Some(retval) => {
                    if !ctx.value_type(retval).is_integer_or_pointer() {
                        return Err(MachineIselError);
                    }

                    let retval = ctx.get_value_vreg(retval);
                    ctx.emit_instr(X64Instr::Ret, &[], &[UseOperand::fixed(retval, REG_RAX)]);
                }
            },
            NodeKind::Call(func) => emit_call(self, ctx, node, func),
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

        let mut uses: SmallVec<[_; FIXED_ARG_COUNT + 1]> = smallvec![UseOperand::any(target)];
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
        let mut uses: SmallVec<[_; FIXED_ARG_COUNT + 1]> = smallvec![UseOperand::any(target)];
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
    mut args: impl Iterator<Item = DepValue>,
    emit_inner: impl FnOnce(&mut IselContext<'_, '_, X64Machine>, &[DefOperand], &[UseOperand]),
) {
    // The `take` here is important, because `zip` will advance the argument iterator before
    // realizing it has run out of physical registers, causing the first stack argument to be
    // dropped.
    let reg_args: SmallVec<[_; FIXED_ARG_COUNT]> = args
        .by_ref()
        .take(FIXED_ARG_COUNT)
        .zip(FIXED_ARG_REGS)
        .map(|(arg, reg)| {
            let arg = ctx.get_value_vreg(arg);
            UseOperand::fixed(arg, reg)
        })
        .collect();

    let stack_args: SmallVec<[_; 4]> = args.map(|arg| ctx.get_value_vreg(arg)).collect();

    let mut stack_size = 0;
    if stack_args.len() % 2 != 0 {
        // Just pushing these arguments would misalign the stack, so make sure to compensate.
        ctx.emit_instr(X64Instr::AddSp(-8), &[], &[]);
        stack_size += 8;
    }

    for &stack_arg in stack_args.iter().rev() {
        ctx.emit_instr(X64Instr::Push, &[], &[UseOperand::any_reg(stack_arg)]);
        stack_size += 8;
    }

    let retval = ctx.node_outputs(node).next().map(|retval| {
        let retval = ctx.get_value_vreg(retval);
        DefOperand::fixed(retval, REG_RAX)
    });

    let call_defs = retval.as_ref().map(slice::from_ref).unwrap_or_default();
    emit_inner(ctx, call_defs, &reg_args);

    if stack_size > 0 {
        ctx.emit_instr(X64Instr::AddSp(stack_size), &[], &[]);
    }
}

fn select_iconst(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, val: u64) {
    let [output] = ctx.node_outputs_exact(node);
    let output = ctx.get_value_vreg(output);

    if val == 0 {
        emit_mov_rz(ctx, output);
    } else if is_sint::<32>(val) {
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

fn select_alu(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: AluOp) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);
    let op_size = operand_size_for_ty(ty);
    let output = ctx.get_value_vreg(output);

    if let Some(imm) = match_imm32(ctx, op2) {
        let op1 = ctx.get_value_vreg(op1);
        ctx.emit_instr(
            X64Instr::AluRmI(op_size, op, imm),
            &[DefOperand::any(output)],
            &[UseOperand::tied(op1, 0)],
        );
    } else {
        let op1 = ctx.get_value_vreg(op1);
        let op2 = ctx.get_value_vreg(op2);
        ctx.emit_instr(
            X64Instr::AluRRm(op_size, op),
            &[DefOperand::any_reg(output)],
            &[UseOperand::tied(op1, 0), UseOperand::any(op2)],
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

fn select_icmp(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, kind: IcmpKind) -> CondCode {
    let [op1, op2] = ctx.node_inputs_exact(node);

    if match_iconst(ctx, op1) == Some(0) {
        emit_alu_rr_discarded(ctx, op2, op2, AluOp::Test);
        return cond_code_for_icmp_zr(kind);
    }

    if match_iconst(ctx, op2) == Some(0) {
        emit_alu_rr_discarded(ctx, op1, op1, AluOp::Test);
        return cond_code_for_icmp_rz(kind);
    }

    emit_alu_rr_discarded(ctx, op1, op2, AluOp::Cmp);
    cond_code_for_icmp(kind)
}

fn select_load(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, mem_size: MemSize) {
    let [addr] = ctx.node_inputs_exact(node);
    let [output] = ctx.node_outputs_exact(node);

    let output = ctx.get_value_vreg(output);
    let op_size = operand_size_for_mem_size(mem_size);

    match match_stack_slot(ctx, addr) {
        Some(stack_slot) => {
            ctx.emit_instr(
                X64Instr::MovRStack(stack_slot, op_size),
                &[DefOperand::any_reg(output)],
                &[],
            );
        }
        None => {
            let addr = ctx.get_value_vreg(addr);
            ctx.emit_instr(
                X64Instr::MovRM(op_size),
                &[DefOperand::any_reg(output)],
                &[UseOperand::any_reg(addr)],
            )
        }
    }
}

fn select_store(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, mem_size: MemSize) {
    let [value, addr] = ctx.node_inputs_exact(node);
    let value = ctx.get_value_vreg(value);
    let op_size = operand_size_for_mem_size(mem_size);

    match match_stack_slot(ctx, addr) {
        Some(stack_slot) => {
            ctx.emit_instr(
                X64Instr::MovStackR(stack_slot, op_size),
                &[],
                &[UseOperand::any_reg(value)],
            );
        }
        None => {
            let addr = ctx.get_value_vreg(addr);
            ctx.emit_instr(
                X64Instr::MovMR(op_size),
                &[],
                &[UseOperand::any_reg(addr), UseOperand::any_reg(value)],
            );
        }
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
    ctx.emit_instr(X64Instr::MovRZ, &[DefOperand::any_reg(output)], &[]);
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

fn emit_imul_rr(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);

    let output = ctx.get_value_vreg(output);
    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    ctx.emit_instr(
        X64Instr::ImulRRm(operand_size_for_ty(ty)),
        &[DefOperand::any_reg(output)],
        &[UseOperand::tied(op1, 0), UseOperand::any(op2)],
    );
}

fn emit_alu_rr_discarded(
    ctx: &mut IselContext<'_, '_, X64Machine>,
    op1: DepValue,
    op2: DepValue,
    op: AluOp,
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

fn match_imm32(ctx: &IselContext<'_, '_, X64Machine>, value: DepValue) -> Option<i32> {
    if let Some(const_val) = match_iconst(ctx, value) {
        let ty = ctx.value_type(value);
        if ty == Type::I32 || is_sint::<32>(const_val) {
            return Some(const_val as i32);
        }
    }

    None
}

fn match_iconst(ctx: &IselContext<'_, '_, X64Machine>, value: DepValue) -> Option<u64> {
    if let Some((node, 0)) = ctx.value_def(value) {
        if let NodeKind::IConst(val) = ctx.node_kind(node) {
            return Some(val);
        }
    }

    None
}

fn match_stack_slot(ctx: &IselContext<'_, '_, X64Machine>, value: DepValue) -> Option<StackSlot> {
    if let Some((node, 0)) = ctx.value_def(value) {
        if matches!(ctx.node_kind(node), NodeKind::StackSlot { .. }) {
            return Some(ctx.node_stack_slot(node));
        }
    }

    None
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
