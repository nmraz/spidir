use core::fmt;

use alloc::{borrow::ToOwned, collections::VecDeque, string::ToString};

use cranelift_entity::SecondaryMap;
use fx_utils::FxHashMap;
use itertools::izip;
use log::trace;

use crate::{
    cfg::{Block, CfgContext},
    lir::{
        DefOperand, DefOperandConstraint, Instr, Lir, UseOperand, UseOperandConstraint, VirtReg,
    },
    machine::MachineCore,
    regalloc::types::CopySourceAssignment,
};

use super::{Assignment, AssignmentCopy, OperandAssignment};

#[derive(Clone, Copy)]
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
        found_vreg: Option<VirtReg>,
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
        found_vreg: Option<VirtReg>,
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
                found_vreg,
            } => {
                let found_vreg = found_vreg.map_or("garbage".to_owned(), |found_vreg: VirtReg| {
                    found_vreg.to_string()
                });
                let expected_vreg = self.lir.instr_uses(instr)[op as usize].reg();
                write!(
                    f,
                    "expected {instr} use {op} to be of {expected_vreg}, found {found_vreg}"
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
                write!(f, "block '{block}' contained exit ghost copies despite not having a single successor")
            }
            VerifierError::BadGhostCopySource {
                ghost_copy_idx,
                found_vreg,
            } => {
                let found_vreg =
                    found_vreg.map_or("garbage".to_owned(), |found_vreg| found_vreg.to_string());
                let ghost_copy = &self.assignment.block_exit_ghost_copies[ghost_copy_idx as usize];
                let block = ghost_copy.block;
                let expected_vreg = ghost_copy.from_vreg;
                write!(
                    f,
                    "expected ghost copy for '{}' at end of '{block}' to be from {expected_vreg}, found {found_vreg}",
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
        reg_state.insert(OperandAssignment::Reg(preg), live_in);
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

type KnownRegState = FxHashMap<OperandAssignment, VirtReg>;
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
        check_assigned_vreg(operand, ghost_copy.from_vreg, &*reg_state).map_err(|found_vreg| {
            VerifierError::BadGhostCopySource {
                ghost_copy_idx: ghost_copy_idx.try_into().unwrap(),
                found_vreg,
            }
        })?;
        reg_state.insert(operand, ghost_copy.to_vreg);
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
        verify_instr(lir, assignment, instr, reg_state)?;
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

    let from_vreg = match copy.from {
        CopySourceAssignment::Operand(from) => *reg_state
            .get(&from)
            .ok_or(VerifierError::UndefCopySource { copy_idx })?,
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

            def.reg()
        }
    };

    reg_state.insert(copy.to, from_vreg);

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
        if !use_matches_constraint(use_op, use_assignment, def_assignments) {
            return Err(VerifierError::UseConstraintViolation { instr, op: i });
        }
        check_use_assignment(use_op, use_assignment, reg_state).map_err(|found_vreg| {
            VerifierError::BadUse {
                instr,
                op: i,
                found_vreg,
            }
        })?;
    }

    for preg in lir.instr_clobbers(instr).iter() {
        reg_state.remove(&OperandAssignment::Reg(preg));
    }

    for (i, &def_op, &def_assignment) in izip!(0.., def_ops, def_assignments) {
        if !def_matches_constraint(def_op, def_assignment) {
            return Err(VerifierError::DefConstraintViolation { instr, op: i });
        }
        reg_state.insert(def_assignment, def_op.reg());
    }

    Ok(())
}

fn check_use_assignment(
    use_op: UseOperand,
    use_assignment: OperandAssignment,
    reg_state: &KnownRegState,
) -> Result<(), Option<VirtReg>> {
    check_assigned_vreg(use_assignment, use_op.reg(), reg_state)
}

fn check_assigned_vreg(
    assignment: OperandAssignment,
    expected_vreg: VirtReg,
    reg_state: &KnownRegState,
) -> Result<(), Option<VirtReg>> {
    let found_vreg = reg_state.get(&assignment).copied();
    if found_vreg != Some(expected_vreg) {
        return Err(found_vreg);
    }
    Ok(())
}

fn use_matches_constraint(
    use_op: UseOperand,
    use_assignment: OperandAssignment,
    def_assignments: &[OperandAssignment],
) -> bool {
    match use_op.constraint() {
        UseOperandConstraint::Any => true,
        UseOperandConstraint::AnyReg => matches!(use_assignment, OperandAssignment::Reg(_)),
        UseOperandConstraint::Fixed(preg) => use_assignment == OperandAssignment::Reg(preg),
        UseOperandConstraint::TiedToDef(i) => use_assignment == def_assignments[i as usize],
    }
}

fn def_matches_constraint(def_op: DefOperand, def_assignment: OperandAssignment) -> bool {
    match def_op.constraint() {
        DefOperandConstraint::Any => true,
        DefOperandConstraint::AnyReg => matches!(def_assignment, OperandAssignment::Reg(_)),
        DefOperandConstraint::Fixed(preg) => def_assignment == OperandAssignment::Reg(preg),
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

    existing_reg_state.retain(|operand, known_reg| {
        let retain = incoming_reg_state
            .get(operand)
            .is_some_and(|incoming| incoming == known_reg);
        if !retain {
            changed = true;
        }
        retain
    });

    changed
}
