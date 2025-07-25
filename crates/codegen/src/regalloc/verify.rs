use core::fmt;

use alloc::{
    borrow::ToOwned,
    boxed::Box,
    collections::VecDeque,
    format,
    string::{String, ToString},
};

use cranelift_entity::SecondaryMap;
use fx_utils::FxHashMap;
use itertools::{Itertools, izip};
use log::trace;
use smallvec::{SmallVec, smallvec};

use crate::{
    cfg::{Block, CfgContext},
    lir::{
        DefOperand, DefOperandConstraint, Instr, Lir, UseOperand, UseOperandConstraint, VirtReg,
    },
    machine::MachineCore,
    regalloc::types::CopySourceAssignment,
};

use super::{Assignment, AssignmentCopy, OperandAssignment};

#[derive(Clone)]
pub enum VerifierError {
    BadUseCount(Instr),
    BadDefCount(Instr),
    UseConstraintViolation {
        instr: Instr,
        op: u32,
    },
    DefConstraintViolation {
        instr: Instr,
        op: u32,
    },
    BadUse {
        instr: Instr,
        op: u32,
        found_vregs: Option<Box<[VirtReg]>>,
    },
    IllegalSpillCopy {
        copy_idx: u32,
    },
    UndefCopySource {
        copy_idx: u32,
    },
    BadRematOperands {
        copy_idx: u32,
    },
    BadGhostCopyBlock {
        block: Block,
    },
    BadGhostCopySource {
        ghost_copy_idx: u32,
        found_vregs: Option<Box<[VirtReg]>>,
    },
}

impl VerifierError {
    pub fn display<'a, M: MachineCore>(
        &'a self,
        lir: &'a Lir<M>,
        assignment: &'a Assignment,
    ) -> DisplayVerifierError<'a, M> {
        DisplayVerifierError {
            error: self,
            lir,
            assignment,
        }
    }
}

pub struct DisplayVerifierError<'a, M: MachineCore> {
    error: &'a VerifierError,
    lir: &'a Lir<M>,
    assignment: &'a Assignment,
}

impl<M: MachineCore> fmt::Display for DisplayVerifierError<'_, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.error {
            VerifierError::BadUseCount(instr) => write!(
                f,
                "bad use count for {instr}, expected {} but got {}",
                self.lir.instr_uses(instr).len(),
                self.assignment.instr_use_assignments(instr).len()
            ),
            VerifierError::BadDefCount(instr) => write!(
                f,
                "bad def count for {instr}, expected {} but got {}",
                self.lir.instr_defs(instr).len(),
                self.assignment.instr_def_assignments(instr).len()
            ),
            VerifierError::UseConstraintViolation { instr, op } => {
                let assignment = self.assignment.instr_use_assignments(instr)[op as usize];
                let op = self.lir.instr_uses(instr)[op as usize];
                write!(
                    f,
                    "assignment '{}' violated constraint of '{}' in {instr}",
                    assignment.display::<M>(),
                    op.display::<M>()
                )
            }
            VerifierError::DefConstraintViolation { instr, op } => {
                let assignment = self.assignment.instr_def_assignments(instr)[op as usize];
                let op = self.lir.instr_defs(instr)[op as usize];
                write!(
                    f,
                    "assignment '{}' violated constraint of '{}' in {instr}",
                    assignment.display::<M>(),
                    op.display(self.lir)
                )
            }
            VerifierError::BadUse {
                instr,
                op,
                ref found_vregs,
            } => {
                let found_vregs = display_found_vregs(found_vregs.as_deref());
                let expected_vreg = self.lir.instr_uses(instr)[op as usize].reg();
                write!(
                    f,
                    "expected {instr} use {op} to be of {expected_vreg}, found {found_vregs}"
                )
            }
            VerifierError::IllegalSpillCopy { copy_idx } => {
                let copy = &self.assignment.copies[copy_idx as usize];
                write!(
                    f,
                    "copy before {} copies from non-register into spill",
                    copy.instr
                )
            }
            VerifierError::UndefCopySource { copy_idx } => {
                let copy = &self.assignment.copies[copy_idx as usize];
                write!(
                    f,
                    "copy `{} = {}` before {} has garbage source",
                    copy.copy.to.display::<M>(),
                    copy.copy.from.display(self.lir),
                    copy.instr
                )
            }
            VerifierError::BadRematOperands { copy_idx } => {
                let copy = &self.assignment.copies[copy_idx as usize];
                let remat_instr = match copy.copy.from {
                    CopySourceAssignment::Remat(instr) => instr,
                    _ => unreachable!(),
                };

                write!(
                    f,
                    "instruction {} rematerialized before {} has unsupported operands",
                    remat_instr, copy.instr
                )
            }
            VerifierError::BadGhostCopyBlock { block } => {
                write!(
                    f,
                    "block '{block}' contained exit ghost copies despite not having a single successor"
                )
            }
            VerifierError::BadGhostCopySource {
                ghost_copy_idx,
                ref found_vregs,
            } => {
                let found_vregs = display_found_vregs(found_vregs.as_deref());
                let ghost_copy = &self.assignment.block_exit_ghost_copies[ghost_copy_idx as usize];
                let block = ghost_copy.block;
                let expected_vreg = ghost_copy.from_vreg;
                write!(
                    f,
                    "expected ghost copy for '{}' at end of '{block}' to be from {expected_vreg}, found {found_vregs}",
                    ghost_copy.assignment.display::<M>(),
                )
            }
        }
    }
}

pub fn verify<M: MachineCore>(
    lir: &Lir<M>,
    cfg_ctx: &CfgContext,
    assignment: &Assignment,
) -> Result<(), VerifierError> {
    let mut block_entry_reg_states = BlockKnownRegState::with_capacity(cfg_ctx.block_order.len());

    let entry = cfg_ctx.block_order[0];

    let mut reg_state = KnownRegState::default();
    for (&live_in, &preg) in lir.block_params(entry).iter().zip(lir.live_in_regs()) {
        reg_state.insert(OperandAssignment::Reg(preg), smallvec![live_in]);
    }

    let mut worklist = VecDeque::new();
    worklist.push_back((entry, reg_state));

    while let Some((block, entry_state)) = worklist.pop_front() {
        let Some(mut reg_state) =
            get_new_block_reg_state(&mut block_entry_reg_states, block, entry_state)
        else {
            continue;
        };

        trace!("verifying {block}");

        verify_block_instrs(lir, assignment, block, &mut reg_state)?;
        verify_block_ghost_copies(cfg_ctx, assignment, block, &mut reg_state)?;

        for &succ in cfg_ctx.cfg.block_succs(block) {
            trace!("    succ {succ}");
            worklist.push_back((succ, reg_state.clone()));
        }
    }

    Ok(())
}

fn display_found_vregs(found_vregs: Option<&[VirtReg]>) -> String {
    match found_vregs {
        Some(&[found_vreg]) => found_vreg.to_string(),
        Some(found_vregs) => {
            format!("{{{}}}", found_vregs.iter().format(", "))
        }
        None => "garbage".to_owned(),
    }
}

type KnownRegState = FxHashMap<OperandAssignment, SmallVec<[VirtReg; 2]>>;
type BlockKnownRegState = SecondaryMap<Block, Option<KnownRegState>>;

fn verify_block_ghost_copies(
    cfg_ctx: &CfgContext,
    assignment: &Assignment,
    block: Block,
    reg_state: &mut KnownRegState,
) -> Result<(), VerifierError> {
    let ghost_copy_range = assignment.block_exit_ghost_copy_range(block);
    if !ghost_copy_range.is_empty() && cfg_ctx.cfg.block_succs(block).len() != 1 {
        return Err(VerifierError::BadGhostCopyBlock { block });
    }

    for ghost_copy_idx in ghost_copy_range {
        let ghost_copy = &assignment.block_exit_ghost_copies[ghost_copy_idx];
        let operand = ghost_copy.assignment;
        check_assigned_vreg(operand, ghost_copy.from_vreg, reg_state).map_err(|found_vregs| {
            VerifierError::BadGhostCopySource {
                ghost_copy_idx: ghost_copy_idx.try_into().unwrap(),
                found_vregs,
            }
        })?;

        // Ghost copies are special in that they copy between vregs in the same physical assignment
        // rather than between physical assignments in the same vreg. That means the same physical
        // assignment may now simultaneously house an additional vreg.
        let vregs = reg_state.get_mut(&operand).unwrap();
        if !vregs.contains(&ghost_copy.to_vreg) {
            vregs.push(ghost_copy.to_vreg);
        }
    }

    Ok(())
}

fn verify_block_instrs<M: MachineCore>(
    lir: &Lir<M>,
    assignment: &Assignment,
    block: Block,
    reg_state: &mut KnownRegState,
) -> Result<(), VerifierError> {
    let block_instrs = lir.block_instrs(block);
    let mut edits = assignment.edit_tracker_from(block_instrs.start);

    for instr in block_instrs {
        while let Some((copy_idx, copy)) = edits.next_copy_for(instr) {
            verify_copy(lir, copy_idx, &copy, reg_state)?;
        }

        if !edits.is_killed_remat_def(instr) {
            verify_instr(lir, assignment, instr, reg_state)?;
        }
    }

    Ok(())
}

fn verify_copy<M: MachineCore>(
    lir: &Lir<M>,
    copy_idx: u32,
    copy: &AssignmentCopy,
    reg_state: &mut KnownRegState,
) -> Result<(), VerifierError> {
    if !copy.from.is_reg() && copy.to.is_spill() {
        return Err(VerifierError::IllegalSpillCopy { copy_idx });
    }

    match copy.from {
        CopySourceAssignment::Operand(from) => {
            let from = reg_state
                .get(&from)
                .ok_or(VerifierError::UndefCopySource { copy_idx })?;
            reg_state.insert(copy.to, from.clone());
        }
        CopySourceAssignment::Remat(instr) => {
            if !lir.instr_uses(instr).is_empty() {
                return Err(VerifierError::BadRematOperands { copy_idx });
            }

            let [def] = lir.instr_defs(instr) else {
                return Err(VerifierError::BadRematOperands { copy_idx });
            };

            if !matches!(
                def.constraint(),
                DefOperandConstraint::Any | DefOperandConstraint::AnyReg
            ) {
                return Err(VerifierError::BadRematOperands { copy_idx });
            }

            reg_state.insert(copy.to, smallvec![def.reg()]);
        }
    };

    Ok(())
}

fn verify_instr<M: MachineCore>(
    lir: &Lir<M>,
    assignment: &Assignment,
    instr: Instr,
    reg_state: &mut KnownRegState,
) -> Result<(), VerifierError> {
    let use_ops = lir.instr_uses(instr);
    let use_assignments = assignment.instr_use_assignments(instr);
    if use_ops.len() != use_assignments.len() {
        return Err(VerifierError::BadUseCount(instr));
    }

    let def_ops = lir.instr_defs(instr);
    let def_assignments = assignment.instr_def_assignments(instr);
    if def_ops.len() != def_assignments.len() {
        return Err(VerifierError::BadDefCount(instr));
    }

    for (i, &use_op, &use_assignment) in izip!(0.., use_ops, use_assignments) {
        if !assignment_matches_use(use_op, use_assignment, def_ops, def_assignments) {
            return Err(VerifierError::UseConstraintViolation { instr, op: i });
        }
        check_use_assignment(use_op, use_assignment, reg_state).map_err(|found_vregs| {
            VerifierError::BadUse {
                instr,
                op: i,
                found_vregs,
            }
        })?;
    }

    for preg in lir.instr_clobbers(instr).iter() {
        reg_state.remove(&OperandAssignment::Reg(preg));
    }

    for (i, &def_op, &def_assignment) in izip!(0.., def_ops, def_assignments) {
        if !assignment_matches_def(def_op, def_assignment) {
            return Err(VerifierError::DefConstraintViolation { instr, op: i });
        }
        reg_state.insert(def_assignment, smallvec![def_op.reg()]);
    }

    Ok(())
}

fn check_use_assignment(
    use_op: UseOperand,
    use_assignment: OperandAssignment,
    reg_state: &KnownRegState,
) -> Result<(), Option<Box<[VirtReg]>>> {
    check_assigned_vreg(use_assignment, use_op.reg(), reg_state)
}

fn check_assigned_vreg(
    assignment: OperandAssignment,
    expected_vreg: VirtReg,
    reg_state: &KnownRegState,
) -> Result<(), Option<Box<[VirtReg]>>> {
    match reg_state.get(&assignment) {
        Some(found_vregs) => {
            if !found_vregs.contains(&expected_vreg) {
                return Err(Some(found_vregs.as_slice().into()));
            }

            Ok(())
        }
        None => Err(None),
    }
}

fn assignment_matches_use(
    use_op: UseOperand,
    assignment: OperandAssignment,
    def_ops: &[DefOperand],
    def_assignments: &[OperandAssignment],
) -> bool {
    match use_op.constraint() {
        UseOperandConstraint::Any => true,
        UseOperandConstraint::AnyReg => matches!(assignment, OperandAssignment::Reg(_)),
        UseOperandConstraint::Fixed(preg) => assignment == OperandAssignment::Reg(preg),
        UseOperandConstraint::TiedToDef(i) => assignment == def_assignments[i as usize],
        UseOperandConstraint::SoftTiedToDef(i) => {
            assignment_matches_def(def_ops[i as usize], assignment)
        }
    }
}

fn assignment_matches_def(def_op: DefOperand, assignment: OperandAssignment) -> bool {
    match def_op.constraint() {
        DefOperandConstraint::Any => true,
        DefOperandConstraint::AnyReg => matches!(assignment, OperandAssignment::Reg(_)),
        DefOperandConstraint::Fixed(preg) => assignment == OperandAssignment::Reg(preg),
    }
}

fn get_new_block_reg_state(
    block_entry_reg_states: &mut BlockKnownRegState,
    block: Block,
    entry_state: KnownRegState,
) -> Option<KnownRegState> {
    let reg_state = match &mut block_entry_reg_states[block] {
        Some(existing_reg_state) => {
            if !merge_reg_state(existing_reg_state, &entry_state) {
                return None;
            }
            existing_reg_state.clone()
        }
        None => {
            block_entry_reg_states[block] = Some(entry_state.clone());
            entry_state
        }
    };

    Some(reg_state)
}

fn merge_reg_state(
    existing_reg_state: &mut KnownRegState,
    incoming_reg_state: &KnownRegState,
) -> bool {
    let mut changed = false;

    existing_reg_state.retain(|operand, known_regs| {
        let retain = match incoming_reg_state.get(operand) {
            Some(incoming) => {
                // This can end up quadratic in the worst case, but these lists will almost always
                // have 1 or 2 elements in practice. We also don't care that much about the
                // performance of the verifier as long as it isn't unbearably slow - it's just a
                // debugging aid.
                known_regs.retain(|reg| incoming.contains(reg));
                !known_regs.is_empty()
            }
            None => false,
        };

        if !retain {
            changed = true;
        }
        retain
    });

    changed
}
