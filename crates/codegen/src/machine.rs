use core::fmt::Debug;

use ir::{node::Type, valgraph::Node};

use crate::{
    cfg::Block,
    isel::IselContext,
    lir::{PhysReg, RegClass},
};

pub trait MachineInstr: Copy + Debug {
    fn make_jump(block: Block) -> Self;
}

pub trait MachineCore {
    type Instr: MachineInstr;

    fn reg_class_name(class: RegClass) -> &'static str;
    fn reg_name(reg: PhysReg) -> &'static str;

    fn usable_regs(&self, class: RegClass) -> &[PhysReg];
}

pub struct SingleIselFailed;

pub trait MachineLower: MachineCore {
    fn reg_class_for_type(&self, ty: Type) -> RegClass;

    fn select_instr(
        &self,
        instr: Node,
        targets: &[Block],
        ctx: &mut IselContext<'_, '_, Self>,
    ) -> Result<(), SingleIselFailed>
    where
        Self: Sized;
}
