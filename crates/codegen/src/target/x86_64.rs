use alloc::vec::Vec;

use ir::{
    node::{NodeKind, Type},
    valgraph::Node,
};

use crate::{
    cfg::Block,
    isel::IselContext,
    lir::{
        DefOperand, DefOperandConstraint, OperandPos, PhysReg, RegClass, UseOperand,
        UseOperandConstraint,
    },
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
    AluRr(OperandSize, AluOp),
    Ret,
    Jump(Block),
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
            NodeKind::Iadd => {
                let [output] = ctx.node_outputs_exact(node);
                let [op1, op2] = ctx.node_inputs_exact(node);

                let ty = ctx.value_type(output);

                let output = ctx.get_value_vreg(output);
                let op1 = ctx.get_value_vreg(op1);
                let op2 = ctx.get_value_vreg(op2);

                ctx.emit_instr(
                    X86Instr::AluRr(operand_size_for_ty(ty), AluOp::Add),
                    &[DefOperand::new(
                        output,
                        DefOperandConstraint::Any,
                        OperandPos::Late,
                    )],
                    &[
                        UseOperand::new(op1, UseOperandConstraint::TiedToDef(0), OperandPos::Early),
                        UseOperand::new(op2, UseOperandConstraint::AnyReg, OperandPos::Early),
                    ],
                );
                Ok(())
            }
            NodeKind::Return => {
                match ctx.node_inputs(node).next() {
                    Some(retval) => {
                        if !ctx.value_type(retval).is_integer() {
                            return Err(MachineIselError);
                        }

                        let retval = ctx.get_value_vreg(retval);

                        ctx.emit_instr(
                            X86Instr::Ret,
                            &[],
                            &[UseOperand::new(
                                retval,
                                UseOperandConstraint::Fixed(REG_RAX),
                                OperandPos::Early,
                            )],
                        );
                    }
                    None => {
                        ctx.emit_instr(X86Instr::Ret, &[], &[]);
                    }
                }
                Ok(())
            }
            _ => Err(MachineIselError),
        }
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
