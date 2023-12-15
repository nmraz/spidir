use alloc::vec::Vec;

use ir::{
    node::{IcmpKind, NodeKind, Type},
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
    S8S32,
    S8S64,
    S16S32,
    S16S64,
    S32S64,
}

#[derive(Debug, Clone, Copy)]
pub enum CondCode {
    /// Overflow (OF = 1)
    O,
    /// No Overflow (OF = 0)
    NO,
    /// Below (CF = 1)
    B,
    /// Above or Equal (CF = 0)
    AE,
    /// Equal (ZF = 1)
    E,
    /// Not Equal (ZF = 0)
    NE,
    /// Below or Equal ((CF | ZF) = 1)
    BE,
    /// Above ((CF | ZF) = 0)
    A,
    /// Sign (SF = 1)
    S,
    /// No Sign (SF = 0)
    NS,
    /// Parity (PF = 1)
    P,
    /// No Parity (PF = 0)
    NP,
    /// Less ((SF ^ OF) = 1)
    L,
    /// Greater or Equal ((SF ^ OF) = 0)
    GE,
    /// Less or Equal (((SF ^ OF) | ZF) = 1)
    LE,
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
pub enum X86Instr {
    LoadRbp { offset: i32 },
    AluRmR(OperandSize, AluOp),
    MovzxRR(ExtWidth),
    Setcc(CondCode),
    Ret,
    Jump(Block),
    Jumpcc(CondCode, Block, Block),
}

pub struct X86Machine;

impl MachineCore for X86Machine {
    type Instr = X86Instr;

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

impl MachineLower for X86Machine {
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
        X86Instr::Jump(block)
    }

    fn make_fp_relative_load(&self, offset: i32) -> Self::Instr {
        X86Instr::LoadRbp { offset }
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
            NodeKind::Iadd => emit_alu_rr(ctx, node, AluOp::Add),
            NodeKind::And => emit_alu_rr(ctx, node, AluOp::And),
            NodeKind::Or => emit_alu_rr(ctx, node, AluOp::Or),
            NodeKind::Isub => emit_alu_rr(ctx, node, AluOp::Sub),
            NodeKind::Xor => emit_alu_rr(ctx, node, AluOp::Xor),
            NodeKind::Icmp(kind) => {
                let [op1, op2] = ctx.node_inputs_exact(node);
                let [output] = ctx.node_outputs_exact(node);

                emit_alu_rr_discarded(ctx, op1, op2, AluOp::Cmp);

                let output_ty = ctx.value_type(output);
                let output = ctx.get_value_vreg(output);

                let temp = ctx.create_temp_vreg(RC_GPR);
                ctx.emit_instr(
                    X86Instr::Setcc(cond_code_for_icmp(kind)),
                    &[DefOperand::any_reg(temp)],
                    &[],
                );
                ctx.emit_instr(
                    X86Instr::MovzxRR(byte_ext_width_for_ty(output_ty)),
                    &[DefOperand::any_reg(output)],
                    &[UseOperand::any_reg(temp)],
                );
            }
            NodeKind::BrCond => {
                let [cond] = ctx.node_inputs_exact(node);

                emit_alu_rr_discarded(ctx, cond, cond, AluOp::Test);
                ctx.emit_instr(
                    X86Instr::Jumpcc(CondCode::NE, targets[0], targets[1]),
                    &[],
                    &[],
                );
            }
            NodeKind::Return => match ctx.node_inputs(node).next() {
                None => ctx.emit_instr(X86Instr::Ret, &[], &[]),
                Some(retval) => {
                    if !ctx.value_type(retval).is_integer() {
                        return Err(MachineIselError);
                    }

                    let retval = ctx.get_value_vreg(retval);
                    ctx.emit_instr(X86Instr::Ret, &[], &[UseOperand::fixed(retval, REG_RAX)]);
                }
            },
            _ => return Err(MachineIselError),
        }
        Ok(())
    }
}

fn emit_alu_rr(ctx: &mut IselContext<'_, '_, X86Machine>, node: Node, op: AluOp) {
    let [output] = ctx.node_outputs_exact(node);
    let [op1, op2] = ctx.node_inputs_exact(node);

    let ty = ctx.value_type(output);

    let output = ctx.get_value_vreg(output);
    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    ctx.emit_instr(
        X86Instr::AluRmR(operand_size_for_ty(ty), op),
        &[DefOperand::any(output)],
        &[UseOperand::tied(op1, 0), UseOperand::any_reg(op2)],
    );
}

fn emit_alu_rr_discarded(
    ctx: &mut IselContext<'_, '_, X86Machine>,
    op1: DepValue,
    op2: DepValue,
    op: AluOp,
) {
    let ty = ctx.value_type(op1);

    let op1 = ctx.get_value_vreg(op1);
    let op2 = ctx.get_value_vreg(op2);

    ctx.emit_instr(
        X86Instr::AluRmR(operand_size_for_ty(ty), op),
        &[],
        &[UseOperand::any(op1), UseOperand::any_reg(op2)],
    );
}

fn cond_code_for_icmp(icmp: IcmpKind) -> CondCode {
    match icmp {
        IcmpKind::Eq => CondCode::E,
        IcmpKind::Ne => CondCode::LE,
        IcmpKind::Slt => CondCode::L,
        IcmpKind::Sle => CondCode::LE,
        IcmpKind::Ult => CondCode::B,
        IcmpKind::Ule => CondCode::BE,
    }
}

fn byte_ext_width_for_ty(ty: Type) -> ExtWidth {
    match ty {
        Type::I32 => ExtWidth::S8S32,
        Type::I64 => ExtWidth::S8S64,
        _ => panic!("unexpected extension target {ty}"),
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
