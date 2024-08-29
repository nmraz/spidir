use alloc::vec::Vec;
use core::fmt::Debug;

use ir::{node::Type, valgraph::Node};

use crate::{
    cfg::Block,
    emit::{CodeBuffer, EmitContext},
    isel::IselContext,
    lir::{Lir, MemLayout, PhysReg, RegClass},
    regalloc::{Assignment, OperandAssignment},
};

pub trait MachineCore {
    type Instr: Copy + Debug;

    fn reg_class_name(class: RegClass) -> &'static str;
    fn reg_name(reg: PhysReg) -> &'static str;
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
}

pub trait FixupKind: Copy {
    fn byte_size(&self) -> usize;
    fn apply(&self, offset: u32, label_offset: u32, bytes: &mut [u8]);
}

pub trait MachineEmit: MachineCore + Sized {
    type FrameInfo;
    type Fixup: FixupKind;

    fn compute_frame_info(&self, lir: &Lir<Self>, assignment: &Assignment) -> Self::FrameInfo;

    fn emit_prologue(&self, ctx: &EmitContext<Self>, buffer: &mut CodeBuffer<Self>);
    fn emit_instr(
        &self,
        ctx: &EmitContext<Self>,
        buffer: &mut CodeBuffer<Self>,
        instr: &Self::Instr,
        defs: &[OperandAssignment],
        uses: &[OperandAssignment],
    );
    fn emit_copy(
        &self,
        ctx: &EmitContext<Self>,
        buffer: &mut CodeBuffer<Self>,
        from: OperandAssignment,
        to: OperandAssignment,
    );
}
