use alloc::vec::Vec;
use core::fmt::Debug;

use ir::{node::Type, valgraph::Node};

use crate::{
    cfg::Block,
    code_buffer::{CodeBuffer, FixupKind},
    emit::BlockLabelMap,
    isel::{IselContext, MachineIselError, ParamLoc},
    lir::{Lir, MemLayout, PhysReg, RegClass},
    regalloc::{Assignment, OperandAssignment},
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

    fn can_remat(&self, instr: &Self::Instr) -> bool {
        let _ = instr;
        false
    }
}

pub trait MachineEmit: MachineCore + Sized {
    type EmitState;
    type Fixup: FixupKind;

    fn prepare_state(&self, lir: &Lir<Self>, assignment: &Assignment) -> Self::EmitState;

    fn emit_prologue(&self, state: &mut Self::EmitState, buffer: &mut CodeBuffer<Self::Fixup>);

    fn emit_instr(
        &self,
        state: &mut Self::EmitState,
        buffer: &mut CodeBuffer<Self::Fixup>,
        block_labels: &BlockLabelMap,
        instr: &Self::Instr,
        defs: &[OperandAssignment],
        uses: &[OperandAssignment],
    );

    fn emit_copy(
        &self,
        state: &mut Self::EmitState,
        buffer: &mut CodeBuffer<Self::Fixup>,
        from: OperandAssignment,
        to: OperandAssignment,
    );
}
