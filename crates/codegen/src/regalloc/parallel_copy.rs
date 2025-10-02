use itertools::Itertools;
use smallvec::{SmallVec, smallvec};

use crate::lir::{Instr, PhysReg, RegBank, RegClass, RegWidth};

use super::{
    OperandAssignment, SpillSlot,
    types::{AssignmentCopy, CopySourceAssignment, ParallelCopy},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScavengedRegState {
    Available,
    InUse(RegWidth),
}

pub trait RegScavenger {
    fn scavenge_regs(&self, bank: RegBank) -> impl Iterator<Item = (PhysReg, ScavengedRegState)>;
    fn alloc_tmp_spill(&mut self, class: RegClass) -> SpillSlot;
    fn expand_tmp_spill(&mut self, spill: SpillSlot, class: RegClass);
}

pub struct ResolverState {
    available_spills: SmallVec<[SpillSlot; 2]>,
    // This is actually a map keyed by register, but it is expected to be so small that a fancier
    // data structure probably wouldn't be worth it.
    used_regs: SmallVec<[(PhysReg, RegWidth); 16]>,
}

impl ResolverState {
    pub fn new() -> Self {
        Self {
            available_spills: SmallVec::new(),
            used_regs: SmallVec::new(),
        }
    }
}

pub fn resolve(
    parallel_copies: &[ParallelCopy],
    state: &mut ResolverState,
    scavenger: &mut impl RegScavenger,
    mut collect: impl FnMut(&AssignmentCopy),
) {
    let mut operands = SmallVec::<[OperandAssignment; 16]>::new();

    for copy in parallel_copies {
        if let CopySourceAssignment::Operand(from) = copy.from {
            operands.push(from);
        }
        operands.push(copy.to);
    }

    operands.sort_unstable_by_key(operand_assignment_sort_key);
    operands.dedup();

    let mut copies_by_dest: SmallVec<[Option<TrackedCopyInfo>; 8]> =
        smallvec![None; operands.len()];

    let mut ctx = ResolvedCopyContext::prepare(state, scavenger);

    for copy in parallel_copies {
        // To keep complexity (somewhat) tame, we let the scavenger model copy sources/destinations
        // as "available" during resolution and manually ignore them ourselves. This lets scavengers
        // that actually probe live-range/allocation structures avoid doing extra work to deal with
        // annoying special cases, such as:
        //
        // * Copies at the end of live ranges, where the source might not be marked as live _at_ the
        //   copy point.
        //
        // * Copies across CFG edges, where the complexities of tracking liveness along edges mean
        //   the destination might not be marked as live in the source block (where the copies
        //   occur).
        if let CopySourceAssignment::Operand(OperandAssignment::Reg(from)) = copy.from {
            ctx.mark_reg_used(from, copy.class.width());
        }
        if let OperandAssignment::Reg(to) = copy.to {
            ctx.mark_reg_used(to, copy.class.width());
        }

        let from = match copy.from {
            CopySourceAssignment::Operand(from) => {
                TrackedCopySource::Operand(find_operand(&operands, from) as u32)
            }
            CopySourceAssignment::Remat(instr) => TrackedCopySource::Remat(instr),
        };

        let to = find_operand(&operands, copy.to);

        debug_assert!(
            copies_by_dest[to].is_none(),
            "multiple parallel copies into same destination"
        );

        copies_by_dest[to] = Some(TrackedCopyInfo {
            class: copy.class,
            from,
        });
    }

    // We now view the set of parallel copies as a directed graph, with edges running from copy
    // destinations to their respective sources. If this graph is acyclic, sorting it topologically
    // (i.e., taking an RPO) will give us a valid order in which the copies can be performed.
    //
    // If the graph does contain cycles, we take advantage of the fact that every node has at most
    // one outgoing edge. That property means that every disjoint subgraph contains at most one
    // simple cycle, which we can easily break apart by introducing a temporary.
    //
    // Perform a modified DFS of the graph to find the RPO we want (modulo discovered cycles, which
    // require a bit of special handling):

    let mut stack = SmallVec::<[usize; 8]>::new();
    let mut visit_states: SmallVec<[_; 8]> = smallvec![VisitState::Unvisited; operands.len()];

    for copy in parallel_copies {
        debug_assert!(stack.is_empty());

        let to = find_operand(&operands, copy.to);
        if visit_states[to] != VisitState::Unvisited {
            continue;
        }

        stack.push(to);

        // Step 1: Follow the newly-discovered `to -> from` copy "chain" as far in as possible.
        loop {
            let operand = *stack.last().unwrap();

            match visit_states[operand] {
                VisitState::Unvisited => {
                    visit_states[operand] = VisitState::Visiting;

                    // Only copies out of other operands can extend the chain (if those operands
                    // themselves need to be copied into); remats don't depend on any other operand
                    // values.
                    if let Some(TrackedCopyInfo {
                        from: TrackedCopySource::Operand(src),
                        ..
                    }) = copies_by_dest[operand]
                    {
                        stack.push(src as usize);
                        continue;
                    }
                }
                VisitState::Visiting => {
                    // We've encountered a copy cycle `r -> ... -> s -> r`; we'll break it up below
                    // by introducing a temporary `t`, saving `r` into it before the cyclic copies,
                    // and copying from `t` into `s` (instead of from `r`).

                    // Remove the duplicate `r` from the top of the stack, and let the logic below
                    // deal with breaking the cycle.
                    stack.pop();
                }
                VisitState::Visited => {
                    // We've hit something in a previously-resolved copy chain, so back up out of
                    // it and terminate our current chain.
                    stack.pop();
                }
            }

            // If we haven't found more unvisited operands copied out of, terminate this chain now.
            break;
        }

        let mut copy_cycle_break = None;

        // Step 2: Walk back out of the chain and record copies (in reverse order!) as we go.
        while let Some(to) = stack.pop() {
            visit_states[to] = VisitState::Visited;

            // If this operand isn't actually copied out of another one, we have nothing more to do.
            let Some(copy) = copies_by_dest[to] else {
                continue;
            };

            match copy.from {
                TrackedCopySource::Operand(from) => {
                    let from = from as usize;

                    match visit_states[from] {
                        VisitState::Unvisited | VisitState::Visited => {
                            // `from` isn't currently on the stack anywhere, so we can just emit
                            // a direct copy without being worried about cycles.
                            let from = operands[from];
                            let to = operands[to];
                            ctx.emit(copy.class, from.into(), to);
                        }
                        VisitState::Visiting => {
                            // `from` is itself somewhere lower on the destination stack (i.e.,
                            // there is a copy cycle), so we need to back it up into a temporary
                            // before it is overwritten and copy out of the temporary instead.

                            let tmp_op = ctx.alloc_tmp_op(copy.class);
                            copy_cycle_break = Some(CopyCycleBreak {
                                cycle_operand: from,
                                tmp_operand: tmp_op,
                                class: copy.class,
                            });

                            // Insert the final copy out of the temporary here (resolved
                            // assignments are in reverse order).
                            ctx.emit(copy.class, tmp_op.into(), operands[to]);
                        }
                    }
                }
                TrackedCopySource::Remat(instr) => {
                    ctx.emit(copy.class, CopySourceAssignment::Remat(instr), operands[to]);
                }
            }

            match &copy_cycle_break {
                Some(copy_cycle_break) if copy_cycle_break.cycle_operand == to => {
                    // We've found the start of a broken parallel copy cycle - make sure the
                    // original value of `operand` is saved before it is overwritten by the copy
                    // inserted above.
                    ctx.emit(
                        copy_cycle_break.class,
                        operands[to].into(),
                        copy_cycle_break.tmp_operand,
                    );
                    ctx.free_tmp_op(copy_cycle_break.tmp_operand);
                }
                _ => {}
            }
        }
    }

    // To get a proper topological sort of the copies, we need to invert our post-order traversal.
    for copy in ctx.copies.iter().rev() {
        collect(copy);
    }
}

struct CopyCycleBreak {
    cycle_operand: usize,
    tmp_operand: OperandAssignment,
    class: RegClass,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Unvisited,
    Visiting,
    Visited,
}

#[derive(Clone, Copy)]
struct TrackedCopyInfo {
    class: RegClass,
    from: TrackedCopySource,
}

#[derive(Debug, Clone, Copy)]
enum TrackedCopySource {
    Operand(u32),
    Remat(Instr),
}

fn find_operand(operands: &[OperandAssignment], op: OperandAssignment) -> usize {
    // This array is expected to be so small that binary search probably isn't worth it.
    operands.iter().position(|&it| it == op).unwrap()
}

fn operand_assignment_sort_key(op: &OperandAssignment) -> u32 {
    match op {
        OperandAssignment::Reg(reg) => reg.as_u8() as u32,
        OperandAssignment::Spill(slot) => {
            const SPILL_BIT: u32 = 1 << 31;
            let val = slot.as_u32();
            debug_assert!((val & SPILL_BIT) == 0);
            val | SPILL_BIT
        }
    }
}

struct ResolvedCopyContext<'s, S> {
    copies: SmallVec<[AssignmentCopy; 8]>,
    state: &'s mut ResolverState,
    scavenger: &'s mut S,
}

impl<'s, S: RegScavenger> ResolvedCopyContext<'s, S> {
    fn prepare(state: &'s mut ResolverState, scavenger: &'s mut S) -> Self {
        state.used_regs.clear();

        Self {
            copies: SmallVec::new(),
            state,
            scavenger,
        }
    }

    fn emit(&mut self, class: RegClass, from: CopySourceAssignment, to: OperandAssignment) {
        if from.is_reg() || to.is_reg() {
            // Copies involving at least one register can be performed directly.
            self.emit_raw(class, from, to);
        } else {
            // Stack-to-stack copies and remat-to-stack need to go through a temporary register.

            let (tmp_reg, tmp_reg_state) = self.scavenge_reg(class);
            let emergency_spill = match tmp_reg_state {
                ScavengedRegState::Available => None,
                ScavengedRegState::InUse(width) => {
                    let spill_class = class.with_width(width);
                    let emergency_spill = self.alloc_tmp_spill(spill_class);

                    // Restore the original value of `tmp_reg` from the emergency spill.
                    self.emit_raw(
                        spill_class,
                        OperandAssignment::Spill(emergency_spill).into(),
                        OperandAssignment::Reg(tmp_reg),
                    );

                    Some((emergency_spill, spill_class))
                }
            };

            self.emit_raw(class, OperandAssignment::Reg(tmp_reg).into(), to);
            self.emit_raw(class, from, OperandAssignment::Reg(tmp_reg));

            match emergency_spill {
                Some((emergency_spill, spill_class)) => {
                    // Back up `tmp_reg` into the emergency spill before it is used above.
                    self.emit_raw(
                        spill_class,
                        OperandAssignment::Reg(tmp_reg).into(),
                        OperandAssignment::Spill(emergency_spill),
                    );
                    self.free_tmp_spill(emergency_spill);
                }
                None => self.free_tmp_reg(tmp_reg),
            }
        }
    }

    fn emit_raw(&mut self, class: RegClass, from: CopySourceAssignment, to: OperandAssignment) {
        self.copies.push(AssignmentCopy { class, from, to });
    }

    fn alloc_tmp_op(&mut self, class: RegClass) -> OperandAssignment {
        let (reg, state) = self.scavenge_reg(class);
        match state {
            ScavengedRegState::Available => OperandAssignment::Reg(reg),
            ScavengedRegState::InUse(_) => OperandAssignment::Spill(self.alloc_tmp_spill(class)),
        }
    }

    fn free_tmp_op(&mut self, op: OperandAssignment) {
        match op {
            OperandAssignment::Reg(reg) => self.free_tmp_reg(reg),
            OperandAssignment::Spill(spill) => self.free_tmp_spill(spill),
        }
    }

    fn scavenge_reg(&mut self, class: RegClass) -> (PhysReg, ScavengedRegState) {
        let (reg, state) = self
            .scavenger
            .scavenge_regs(class.bank())
            .map(|(reg, state)| {
                let state = match state {
                    ScavengedRegState::Available => match self.tmp_reg_usage(reg) {
                        Some(width) => ScavengedRegState::InUse(width),
                        None => ScavengedRegState::Available,
                    },
                    state => state,
                };
                (reg, state)
            })
            .find_or_first(|(_reg, state)| *state == ScavengedRegState::Available)
            .expect("no registers to scavenge");

        if state == ScavengedRegState::Available {
            self.state.used_regs.push((reg, class.width()));
        }

        (reg, state)
    }

    fn free_tmp_reg(&mut self, reg: PhysReg) {
        self.state
            .used_regs
            .retain(|(entry_reg, _width)| *entry_reg != reg);
    }

    fn alloc_tmp_spill(&mut self, class: RegClass) -> SpillSlot {
        if let Some(spill) = self.state.available_spills.pop() {
            self.scavenger.expand_tmp_spill(spill, class);
            return spill;
        }
        self.scavenger.alloc_tmp_spill(class)
    }

    fn free_tmp_spill(&mut self, spill: SpillSlot) {
        self.state.available_spills.push(spill);
    }

    fn tmp_reg_usage(&self, reg: PhysReg) -> Option<RegWidth> {
        self.state
            .used_regs
            .iter()
            .find(|(entry_reg, _)| *entry_reg == reg)
            .map(|(_reg, width)| *width)
    }

    fn mark_reg_used(&mut self, reg: PhysReg, width: RegWidth) {
        if let Some((_reg, existing_width)) = self
            .state
            .used_regs
            .iter_mut()
            .find(|(entry_reg, _width)| *entry_reg == reg)
        {
            *existing_width = (*existing_width).max(width);
            return;
        }

        self.state.used_regs.push((reg, width));
    }
}

#[cfg(test)]
mod tests;
