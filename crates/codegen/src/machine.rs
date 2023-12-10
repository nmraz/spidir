use alloc::vec::Vec;
use core::fmt::Debug;

use ir::{node::Type, valgraph::Node};

use crate::{
    cfg::Block,
    isel::IselContext,
    lir::{PhysReg, RegClass},
};

pub trait MachineCore {
    type Instr: Copy + Debug;

    fn reg_class_name(class: RegClass) -> &'static str;
    fn reg_name(reg: PhysReg) -> &'static str;

    fn usable_regs(&self, class: RegClass) -> &[PhysReg];
}

pub enum ParamLoc {
    Reg { reg: PhysReg },
    Stack { fp_offset: i32 },
}

pub struct MachineIselError;

pub trait MachineLower: MachineCore {
    fn reg_class_for_type(&self, ty: Type) -> RegClass;
    fn param_locs(&self, param_types: &[Type]) -> Vec<ParamLoc>;

    fn make_jump(&self, block: Block) -> Self::Instr;
    fn make_fp_relative_load(&self, offset: i32) -> Self::Instr;

    fn select_instr(
        &self,
        instr: Node,
        targets: &[Block],
        ctx: &mut IselContext<'_, '_, Self>,
    ) -> Result<(), MachineIselError>
    where
        Self: Sized;
}
