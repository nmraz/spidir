use alloc::vec::Vec;

use ir::{
    node::{IcmpKind, MemSize, NodeKind, Type},
    valgraph::{DepValue, Node},
};

use crate::{
    cfg::Block,
    isel::IselContext,
    lir::{DefOperand, PhysReg, RegClass, UseOperand},
    machine::{MachineCore, MachineIselError, MachineLower, ParamLoc},
};

pub const RC_GPR: RegClass = RegClass::new(0);

pub const REG_RAX: PhysReg = PhysReg::new(0);
pub const REG_RBX: PhysReg = PhysReg::new(1);
pub const REG_RCX: PhysReg = PhysReg::new(2);
pub const REG_RDX: PhysReg = PhysReg::new(3);
pub const REG_RDI: PhysReg = PhysReg::new(4);
pub const REG_RSI: PhysReg = PhysReg::new(5);

pub const REG_R8: PhysReg = PhysReg::new(8);
pub const REG_R9: PhysReg = PhysReg::new(9);
pub const REG_R10: PhysReg = PhysReg::new(10);
pub const REG_R11: PhysReg = PhysReg::new(11);
pub const REG_R12: PhysReg = PhysReg::new(12);
pub const REG_R13: PhysReg = PhysReg::new(13);
pub const REG_R14: PhysReg = PhysReg::new(14);
pub const REG_R15: PhysReg = PhysReg::new(15);

#[derive(Debug, Clone, Copy)]
pub enum OperandSize {
    S8,
    S16,
    S32,
    S64,
}

#[derive(Debug, Clone, Copy)]
pub enum ExtWidth {
    Ext8_32,
    Ext8_64,
    Ext16_32,
    Ext16_64,
    Ext32_64,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum AluOp {
    Add,
    And,
    Cmp,
    Or,
    Sub,
    Test,
    Xor,
}

#[derive(Debug, Clone, Copy)]
pub enum X64Instr {
    LoadRbp { offset: i32 },
    AluRRm(OperandSize, AluOp),
    // Special version of `MovRI` for zero, when clobbering flags is allowed
    MovRZ,
    MovRI(OperandSize, u64),
    MovRM(OperandSize),
    MovMR(OperandSize),
    MovzxRM(ExtWidth),
    MovzxRR(ExtWidth),
    Setcc(CondCode),
    Ret,
    Jump(Block),
    Jumpcc(CondCode, Block, Block),
}

pub struct X64Machine;

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

    fn usable_regs(&self, class: RegClass) -> &[PhysReg] {
        match class {
            RC_GPR => &[
                REG_RAX, REG_RBX, REG_RCX, REG_RDX, REG_RDI, REG_RSI, REG_R8, REG_R9, REG_R10,
                REG_R11, REG_R12, REG_R13, REG_R14, REG_R15,
            ],
            _ => panic!("unknown register class"),
        }
    }
}

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
        X64Instr::LoadRbp { offset }
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
