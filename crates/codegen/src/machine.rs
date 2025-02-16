use alloc::vec::Vec;
use core::fmt::Debug;

use ir::{node::Type, valgraph::Node};

use crate::{
    cfg::Block,
    code_buffer::{CodeBuffer, FixupKind},
    emit::{EmitContext, EmitInstrData},
    isel::{IselContext, MachineIselError, ParamLoc},
    lir::{Instr, MemLayout, PhysReg, RegClass},
    regalloc::{OperandAssignment, RematCost},
};

pub trait Machine: MachineCore + MachineLower + MachineRegalloc + MachineEmit {}
impl<M: MachineCore + MachineLower + MachineRegalloc + MachineEmit> Machine for M {}

pub trait MachineCore {
    type Instr: Copy + Debug;

    fn reg_class_name(class: RegClass) -> &'static str;
    fn reg_name(reg: PhysReg) -> &'static str;
}

pub trait MachineLower: MachineCore {
    fn reg_class_for_type(&self, ty: Type) -> RegClass;
    fn param_locs(&self, param_types: &[Type]) -> Vec<ParamLoc>;

    fn make_jump(&self, block: Block) -> Self::Instr;
    fn make_fp_relative_load(&self, offset: i32) -> Self::Instr;

    fn select_instr(
        &self,
        node: Node,
        targets: &[Block],
        ctx: &mut IselContext<'_, '_, Self>,
    ) -> Result<(), MachineIselError>
    where
        Self: Sized;
}

pub trait MachineRegalloc: MachineCore {
    fn phys_reg_count() -> u32;
    fn usable_regs(&self, class: RegClass) -> &[PhysReg];
    fn reg_class_spill_layout(&self, class: RegClass) -> MemLayout;

    fn remat_cost(&self, instr: &Self::Instr) -> Option<RematCost> {
        let _ = instr;
        None
    }
}

pub trait MachineEmit: MachineCore + Sized {
    type EmitState;
    type Fixup: FixupKind;

    fn prepare_state(&self, ctx: &EmitContext<'_, Self>) -> Self::EmitState;

    fn emit_prologue(
        &self,
        ctx: &EmitContext<'_, Self>,
        state: &mut Self::EmitState,
        buffer: &mut CodeBuffer<Self::Fixup>,
    );

    fn prepare_block(&self, ctx: &EmitContext<'_, Self>, state: &mut Self::EmitState, block: Block);

    fn emit_instr(
        &self,
        ctx: &EmitContext<'_, Self>,
        pos: Instr,
        instr: &EmitInstrData<'_, Self>,
        state: &mut Self::EmitState,
        buffer: &mut CodeBuffer<Self::Fixup>,
    );

    fn emit_copy(
        &self,
        ctx: &EmitContext<'_, Self>,
        pos: Instr,
        from: OperandAssignment,
        to: OperandAssignment,
        state: &mut Self::EmitState,
        buffer: &mut CodeBuffer<Self::Fixup>,
    );
}
