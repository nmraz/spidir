use alloc::vec::Vec;

use ir::{
    node::{IcmpKind, MemSize, NodeKind, Type},
    valgraph::{DepValue, Node},
};

use crate::{
    cfg::Block,
    isel::IselContext,
    lir::{DefOperand, PhysReg, RegClass, UseOperand},
    machine::{MachineIselError, MachineLower, ParamLoc},
};

use super::{
    AluOp, CondCode, ExtWidth, OperandSize, ShiftOp, X64Instr, X64Machine, RC_GPR, REG_R8, REG_R9,
    REG_RAX, REG_RCX, REG_RDI, REG_RDX, REG_RSI,
};

impl MachineLower for X64Machine {
    fn reg_class_for_type(&self, ty: Type) -> RegClass {
        match ty {
            Type::I32 | Type::I64 | Type::Ptr => RC_GPR,
            Type::F64 => unimplemented!("floating point"),
        }
    }

    fn param_locs(&self, param_types: &[Type]) -> Vec<ParamLoc> {
        const FIXED_ARG_REGS: [PhysReg; 6] = [REG_RDI, REG_RSI, REG_RDX, REG_RCX, REG_R8, REG_R9];

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
            NodeKind::IConst(val) => {
                let [output] = ctx.node_outputs_exact(node);
                let op_size = operand_size_for_ty(ctx.value_type(output));
                let output = ctx.get_value_vreg(output);
                ctx.emit_instr(
                    X64Instr::MovRI(op_size, val),
                    &[DefOperand::any_reg(output)],
                    &[],
                );
            }
            NodeKind::Iadd => emit_alu_rr(ctx, node, AluOp::Add),
            NodeKind::And => emit_alu_rr(ctx, node, AluOp::And),
            NodeKind::Or => emit_alu_rr(ctx, node, AluOp::Or),
            NodeKind::Isub => emit_alu_rr(ctx, node, AluOp::Sub),
            NodeKind::Xor => emit_alu_rr(ctx, node, AluOp::Xor),
            NodeKind::Shl => emit_shift_rr(ctx, node, ShiftOp::Shl),
            NodeKind::Lshr => emit_shift_rr(ctx, node, ShiftOp::Shr),
            NodeKind::Ashr => emit_shift_rr(ctx, node, ShiftOp::Sar),
            NodeKind::Imul => emit_alu_rr(ctx, node, AluOp::Imul),
            NodeKind::Udiv => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let ty = ctx.value_type(output);

                let op1 = ctx.get_value_vreg(op1);
                let op2 = ctx.get_value_vreg(op2);
                let output = ctx.get_value_vreg(output);

                let rdx_in = ctx.create_temp_vreg(RC_GPR);
                ctx.emit_instr(X64Instr::MovRZ, &[DefOperand::any_reg(rdx_in)], &[]);

                let rdx_out = ctx.create_temp_vreg(RC_GPR);
                ctx.emit_instr(
                    X64Instr::Div(operand_size_for_ty(ty)),
                    &[
                        DefOperand::fixed(output, REG_RAX),
                        DefOperand::fixed(rdx_out, REG_RDX),
                    ],
                    &[
                        UseOperand::fixed(op1, REG_RAX),
                        UseOperand::fixed(rdx_in, REG_RDX),
                        UseOperand::any(op2),
                    ],
                );
            }
            NodeKind::Sdiv => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let ty = ctx.value_type(output);

                let op1 = ctx.get_value_vreg(op1);
                let op2 = ctx.get_value_vreg(op2);
                let output = ctx.get_value_vreg(output);

                let rdx_in = ctx.create_temp_vreg(RC_GPR);
                ctx.emit_instr(
                    cdo_op_for_ty(ty),
                    &[DefOperand::fixed(rdx_in, REG_RDX)],
                    &[UseOperand::fixed(op1, REG_RAX)],
                );

                let rdx_out = ctx.create_temp_vreg(RC_GPR);
                ctx.emit_instr(
                    X64Instr::Idiv(operand_size_for_ty(ty)),
                    &[
                        DefOperand::fixed(output, REG_RAX),
                        DefOperand::fixed(rdx_out, REG_RDX),
                    ],
                    &[
                        UseOperand::fixed(op1, REG_RAX),
                        UseOperand::fixed(rdx_in, REG_RDX),
                        UseOperand::any(op2),
                    ],
                );
            }
            NodeKind::Icmp(kind) => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let output = ctx.get_value_vreg(output);

                // Generate an `xor; cmp; setcc`. This is preferable to `cmp; setcc; movzx` because
                // `setcc` modifies only the low byte of its output operand, so clearing the
                // register first avoids the (false) partial dependency on the previous value of the
                // `setcc` output register.

                let temp = ctx.create_temp_vreg(RC_GPR);

                // Note: the `xor` must precede the `cmp` because it clobbers flags.
                ctx.emit_instr(X64Instr::MovRZ, &[DefOperand::any_reg(temp)], &[]);
                emit_alu_rr_discarded(ctx, op1, op2, AluOp::Cmp);
                ctx.emit_instr(
                    X64Instr::Setcc(cond_code_for_icmp(kind)),
                    &[DefOperand::any_reg(output)],
                    &[UseOperand::tied(temp, 0)],
                );
            }
            NodeKind::PtrOff => emit_alu_rr(ctx, node, AluOp::Add),
            NodeKind::Load(mem_size) => {
                let [addr] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);
                let ty = ctx.value_type(output);

                let addr = ctx.get_value_vreg(addr);
                let output = ctx.get_value_vreg(output);

                match (ty, mem_size) {
                    (Type::I32, MemSize::S4) => ctx.emit_instr(
                        X64Instr::MovRM(OperandSize::S32),
                        &[DefOperand::any_reg(output)],
                        &[UseOperand::any_reg(addr)],
                    ),
                    (Type::I64 | Type::Ptr, MemSize::S8) => ctx.emit_instr(
                        X64Instr::MovRM(OperandSize::S64),
                        &[DefOperand::any_reg(output)],
                        &[UseOperand::any_reg(addr)],
                    ),
                    _ => ctx.emit_instr(
                        X64Instr::MovzxRM(load_ext_width_for_ty(ty, mem_size)),
                        &[DefOperand::any_reg(output)],
                        &[UseOperand::any_reg(addr)],
                    ),
                }
            }
            NodeKind::Store(mem_size) => {
                let [value, addr] = ctx.node_inputs_exact(node);
                let value = ctx.get_value_vreg(value);
                let addr = ctx.get_value_vreg(addr);

                ctx.emit_instr(
                    X64Instr::MovMR(operand_size_for_mem_size(mem_size)),
                    &[],
                    &[UseOperand::any_reg(addr), UseOperand::any_reg(value)],
                );
            }
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
                            let [op1, op2] = ctx.node_inputs_exact(cond_node);
                            emit_alu_rr_discarded(ctx, op1, op2, AluOp::Cmp);
                            ctx.emit_instr(
                                X64Instr::Jumpcc(cond_code_for_icmp(kind), targets[0], targets[1]),
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
            _ => return Err(MachineIselError),
        }
        Ok(())
    }
}

fn emit_shift_rr(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: ShiftOp) {
    let [value, amount] = ctx.node_inputs_exact(node);
    let [output] = ctx.node_outputs_exact(node);
    let ty = ctx.value_type(output);

    let value = ctx.get_value_vreg(value);
    let amount = ctx.get_value_vreg(amount);
    let output = ctx.get_value_vreg(output);

    ctx.emit_instr(
        X64Instr::ShiftRmR(operand_size_for_ty(ty), op),
        &[DefOperand::any(output)],
        &[
            UseOperand::any_reg(value),
            UseOperand::fixed(amount, REG_RCX),
        ],
    );
}

fn emit_alu_rr(ctx: &mut IselContext<'_, '_, X64Machine>, node: Node, op: AluOp) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);

    let output = ctx.get_value_vreg(output);
    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    ctx.emit_instr(
        X64Instr::AluRRm(operand_size_for_ty(ty), op),
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

fn cond_code_for_icmp(icmp: IcmpKind) -> CondCode {
    match icmp {
        IcmpKind::Eq => CondCode::E,
        IcmpKind::Ne => CondCode::Le,
        IcmpKind::Slt => CondCode::L,
        IcmpKind::Sle => CondCode::Le,
        IcmpKind::Ult => CondCode::B,
        IcmpKind::Ule => CondCode::Be,
    }
}

fn load_ext_width_for_ty(ty: Type, mem_size: MemSize) -> ExtWidth {
    match (ty, mem_size) {
        (Type::I32, MemSize::S1) => ExtWidth::Ext8_32,
        (Type::I32, MemSize::S2) => ExtWidth::Ext16_32,
        (Type::I64, MemSize::S1) => ExtWidth::Ext8_64,
        (Type::I64, MemSize::S2) => ExtWidth::Ext16_64,
        (Type::I64, MemSize::S4) => ExtWidth::Ext32_64,
        _ => panic!("unsupported extending load type ({ty}, {mem_size:?})"),
    }
}

fn cdo_op_for_ty(ty: Type) -> X64Instr {
    match ty {
        Type::I32 => X64Instr::Cdq,
        Type::I64 => X64Instr::Cqo,
        _ => panic!("unexpected type {ty}"),
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

fn operand_size_for_mem_size(mem_size: MemSize) -> OperandSize {
    match mem_size {
        MemSize::S1 => OperandSize::S8,
        MemSize::S2 => OperandSize::S16,
        MemSize::S4 => OperandSize::S32,
        MemSize::S8 => OperandSize::S64,
    }
}
