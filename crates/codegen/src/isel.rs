use alloc::vec::Vec;
use core::array;

use cranelift_entity::SecondaryMap;
use fx_utils::FxHashMap;
use hashbrown::hash_map::Entry;
use ir::{
    module::{FunctionData, Module},
    node::{NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{dataflow_inputs, dataflow_outputs},
    write::display_node,
};
use log::{debug, trace};
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext, FunctionBlockMap},
    lir::{
        display::{display_block_params, display_instr_data},
        Builder as LirBuilder, DefOperand, InstrBuilder, Lir, MemLayout, PhysRegSet, RegClass,
        StackSlot, UseOperand, VirtReg,
    },
    machine::{MachineLower, ParamLoc},
    schedule::Schedule,
};

pub struct IselContext<'ctx, 's, M: MachineLower> {
    state: &'s mut IselState<'ctx, M>,
    builder: InstrBuilder<'ctx, 's, M>,
}

impl<'ctx, 's, M: MachineLower> IselContext<'ctx, 's, M> {
    pub fn module(&self) -> &'ctx Module {
        self.state.module
    }

    pub fn value_type(&self, value: DepValue) -> Type {
        self.state.func.graph.value_kind(value).as_value().unwrap()
    }

    pub fn value_def(&self, value: DepValue) -> Option<(Node, u32)> {
        let (node, output_idx) = self.state.func.graph.value_def(value);

        // Only allow pure nodes to be looked through for pattern matching.
        if !self.node_kind(node).has_side_effects() {
            Some((node, output_idx))
        } else {
            None
        }
    }

    pub fn value_use_count(&self, value: DepValue) -> u32 {
        self.state.value_use_counts[value]
    }

    pub fn has_one_use(&self, value: DepValue) -> bool {
        self.value_use_count(value) == 1
    }

    pub fn node_kind(&self, node: Node) -> NodeKind {
        *self.state.func.graph.node_kind(node)
    }

    pub fn node_stack_slot(&self, node: Node) -> StackSlot {
        *self
            .state
            .stack_slot_map
            .get(&node)
            .expect("node is not a stack slot")
    }

    pub fn node_inputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'ctx {
        dataflow_inputs(&self.state.func.graph, node)
    }

    pub fn node_outputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'ctx {
        dataflow_outputs(&self.state.func.graph, node)
    }

    pub fn node_inputs_exact<const N: usize>(&self, node: Node) -> [DepValue; N] {
        let mut inputs = self.node_inputs(node);
        let ret = array::from_fn(|_| inputs.next().unwrap());
        assert!(inputs.next().is_none());
        ret
    }

    pub fn node_outputs_exact<const N: usize>(&self, node: Node) -> [DepValue; N] {
        let mut inputs = self.node_outputs(node);
        let ret = array::from_fn(|_| inputs.next().unwrap());
        assert!(inputs.next().is_none());
        ret
    }

    pub fn get_value_vreg(&mut self, value: DepValue) -> VirtReg {
        get_value_vreg_helper(
            |value, class| {
                let vreg = self.builder.create_vreg(class);
                self.state.reg_value_map.insert(vreg, value);
                vreg
            },
            &mut self.state.value_reg_map,
            self.state.machine,
            &self.state.func.graph,
            value,
        )
    }

    pub fn create_temp_vreg(&mut self, class: RegClass) -> VirtReg {
        self.builder.create_vreg(class)
    }

    pub fn copy_vreg(&mut self, dest: VirtReg, src: VirtReg) {
        // We assume that the destination register is live here, meaning that it's okay to just bump
        // the use count of `src`.
        self.state.bump_vreg_use_count(src);
        self.builder.copy_vreg(dest, src)
    }

    pub fn emit_instr(&mut self, instr: M::Instr, defs: &[DefOperand], uses: &[UseOperand]) {
        self.emit_instr_with_clobbers(instr, defs, uses, PhysRegSet::empty());
    }

    pub fn emit_instr_with_clobbers(
        &mut self,
        instr: M::Instr,
        defs: &[DefOperand],
        uses: &[UseOperand],
        clobbers: PhysRegSet,
    ) {
        // Bump the use count for any operands that came from values in the original graph.
        for &use_op in uses {
            self.state.bump_vreg_use_count(use_op.reg());
        }
        emit_instr(&mut self.builder, instr, defs, uses, clobbers);
    }
}

pub struct IselError {
    pub node: Node,
}

pub fn select_instrs<M: MachineLower>(
    module: &Module,
    func: &FunctionData,
    schedule: &Schedule,
    cfg_ctx: &CfgContext,
    block_map: &FunctionBlockMap,
    machine: &M,
) -> Result<Lir<M>, IselError> {
    debug!("selecting instructions for: {}", func.metadata());

    let mut state = IselState::new(module, func, schedule, cfg_ctx, block_map, machine);
    let mut builder = LirBuilder::new(&cfg_ctx.block_order);
    state.prepare_for_isel(&mut builder);

    while let Some(block) = builder.advance_block() {
        state.select_block(&mut builder, block)?;
    }

    Ok(builder.finish())
}

type ValueRegMap = FxHashMap<DepValue, VirtReg>;
type RegValueMap = FxHashMap<VirtReg, DepValue>;
type ValueUseCounts = SecondaryMap<DepValue, u32>;
type NodeStackSlotMap = FxHashMap<Node, StackSlot>;

struct IselState<'ctx, M: MachineLower> {
    module: &'ctx Module,
    func: &'ctx FunctionData,
    schedule: &'ctx Schedule,
    cfg_ctx: &'ctx CfgContext,
    block_map: &'ctx FunctionBlockMap,
    machine: &'ctx M,
    value_reg_map: ValueRegMap,
    reg_value_map: RegValueMap,
    value_use_counts: ValueUseCounts,
    stack_slot_map: NodeStackSlotMap,
}

impl<'ctx, M: MachineLower> IselState<'ctx, M> {
    fn new(
        module: &'ctx Module,
        func: &'ctx FunctionData,
        schedule: &'ctx Schedule,
        cfg_ctx: &'ctx CfgContext,
        block_map: &'ctx FunctionBlockMap,
        machine: &'ctx M,
    ) -> Self {
        Self {
            module,
            func,
            schedule,
            cfg_ctx,
            block_map,
            machine,
            value_reg_map: Default::default(),
            reg_value_map: Default::default(),
            value_use_counts: Default::default(),
            stack_slot_map: Default::default(),
        }
    }

    fn prepare_for_isel(&mut self, builder: &mut LirBuilder<'ctx, M>) {
        // TODO: Classify side effects.

        for &block in &self.cfg_ctx.block_order {
            for &phi in self.schedule.block_phis(block) {
                // Values flowing into block parameters are always live.
                // TODO: Except for values from dead control inputs?
                for input in dataflow_inputs(&self.func.graph, phi) {
                    self.value_use_counts[input] += 1;
                }

                // Create a fresh register for every incoming block parameter (phi output).
                let output = self.func.graph.node_outputs(phi)[0];
                self.create_phi_vreg(builder, output);
            }

            for &node in self.schedule.scheduled_nodes(block) {
                // Values flowing into scheduled nodes start live, but may be made dead by the
                // instruction selection process.
                for input in dataflow_inputs(&self.func.graph, node) {
                    self.value_use_counts[input] += 1;
                }

                if let &NodeKind::StackSlot { size, align } = self.func.graph.node_kind(node) {
                    let stack_slot = builder.create_stack_slot(MemLayout { size, align });
                    self.stack_slot_map.insert(node, stack_slot);
                }
            }
        }
    }

    fn select_block(
        &mut self,
        builder: &mut LirBuilder<'ctx, M>,
        block: Block,
    ) -> Result<(), IselError> {
        trace!("{block}:");
        self.lower_block_params(builder, block);

        let is_terminated = self
            .schedule
            .scheduled_nodes(block)
            .last()
            .is_some_and(|&node| self.func.graph.node_kind(node).is_terminator());

        if !is_terminated {
            let succs = self.cfg_ctx.cfg.block_succs(block);
            assert!(
                succs.len() == 1,
                "unterminated block does not have 1 successor"
            );
            let machine = self.machine;
            builder.build_instrs(|mut builder| {
                emit_instr(
                    &mut builder,
                    machine.make_jump(succs[0]),
                    &[],
                    &[],
                    PhysRegSet::empty(),
                );
            });
        }

        for &node in self.schedule.scheduled_nodes(block).iter().rev() {
            trace!("  {}:", display_node(self.module, &self.func.graph, node));

            // Note: node inputs should be detached after selection so the selector sees correct use
            // counts for the current node's inputs.

            if !self.func.graph.node_kind(node).has_side_effects() && !self.is_node_used(node) {
                // This node is now dead as it was folded into a previous computation.
                trace!("    (dead)");
                self.detach_node_inputs(node);
                continue;
            }

            self.select_node(builder, block, node)?;
            self.detach_node_inputs(node);
        }

        Ok(())
    }

    fn select_node(
        &mut self,
        builder: &mut LirBuilder<'ctx, M>,
        block: Block,
        node: Node,
    ) -> Result<(), IselError> {
        let node_kind = self.func.graph.node_kind(node);
        match node_kind {
            NodeKind::Entry => {
                assert!(block == self.cfg_ctx.block_order[0], "misplaced entry node");
                let param_locs = self.machine.param_locs(&self.func.sig.param_types);
                let entry_outputs = self.func.graph.node_outputs(node);
                assert!(param_locs.len() == entry_outputs.len() - 1);

                let mut param_regs = SmallVec::<[VirtReg; 8]>::new();
                let mut live_in_regs = Vec::new();
                for (value, loc) in entry_outputs.into_iter().skip(1).zip(param_locs) {
                    if !self.is_value_used(value) {
                        continue;
                    }

                    let vreg = self.get_value_vreg(builder, value);
                    match loc {
                        ParamLoc::Reg { reg } => {
                            param_regs.push(vreg);
                            live_in_regs.push(reg);
                        }
                        ParamLoc::Stack { fp_offset } => builder.build_instrs(|mut builder| {
                            emit_instr(
                                &mut builder,
                                self.machine.make_fp_relative_load(fp_offset),
                                &[DefOperand::any_reg(vreg)],
                                &[],
                                PhysRegSet::empty(),
                            );
                        }),
                    }
                }

                builder.set_incoming_block_params(param_regs);
                builder.set_live_in_regs(live_in_regs);
            }
            _ => builder
                .build_instrs(|builder| {
                    let targets = if node_kind.is_terminator() {
                        self.cfg_ctx.cfg.block_succs(block)
                    } else {
                        &[]
                    };
                    let machine = self.machine;

                    let mut context = IselContext {
                        builder,
                        state: self,
                    };
                    machine.select_instr(node, targets, &mut context)
                })
                .map_err(|_| IselError { node })?,
        }

        Ok(())
    }

    fn lower_block_params(&mut self, builder: &mut LirBuilder<'ctx, M>, block: Block) {
        if self.cfg_ctx.cfg.block_preds(block).len() == 1 {
            // For single-predecessor blocks, just copy the values from the predecessor. This,
            // combined with critical edge splitting, means that we can always place parallel copies
            // for block param resolution in the predecessor later.
            for &phi in self.schedule.block_phis(block) {
                let input = self.get_value_vreg(builder, self.func.graph.node_inputs(phi)[1]);
                let output = self.value_reg_map[&self.func.graph.node_outputs(phi)[0]];
                builder.copy_vreg(output, input);
            }
        } else {
            // Multiple predecessors - the phis are really necessary.
            builder.set_incoming_block_params(self.schedule.block_phis(block).iter().map(|&phi| {
                let val = self.value_reg_map[&self.func.graph.node_outputs(phi)[0]];
                trace!("  {}", display_node(self.module, &self.func.graph, phi));
                val
            }));
        }

        let succs = self.cfg_ctx.cfg.block_succs(block);
        if succs.len() != 1 {
            // Make sure critical edges are split, because we depend on that fact for the LIR to
            // even be correct.
            for &succ in succs {
                assert!(
                    self.cfg_ctx.cfg.block_preds(succ).len() == 1,
                    "critical edge not split"
                );
                trace!("    => {}", succ);
            }
            return;
        }

        // We have a single successor; check if there are any outgoing params we need.
        let succ = succs[0];

        if self.cfg_ctx.cfg.block_preds(succ).len() == 1 {
            // This successor doesn't actually need block params because we are its only
            // predecessor. This is the second half of the "single-predecessor" case above.
            return;
        }

        // Try to find the actual valgraph predecessor. If there isn't one, the successor is a
        // split critical edge and not an IR-level block, so it can't have any parameters anyway.
        if let Some(valgraph_pred_idx) = self.block_map.valgraph_pred_index(succ, block) {
            let outgoing_params: SmallVec<[_; 4]> = self
                .schedule
                .block_phis(succ)
                .iter()
                .map(|&phi| {
                    // Note: input 0 is the selector from the containing block.
                    let input = self.func.graph.node_inputs(phi)[valgraph_pred_idx as usize + 1];
                    self.get_value_vreg(builder, input)
                })
                .collect();
            trace!(
                "    => {}{}",
                succ,
                display_block_params::<M>(&outgoing_params)
            );
            builder.set_outgoing_block_params(outgoing_params);
        }
    }

    fn get_value_vreg(&mut self, builder: &mut LirBuilder<'ctx, M>, value: DepValue) -> VirtReg {
        get_value_vreg_helper(
            |value, class| {
                let vreg = builder.create_vreg(class);
                self.reg_value_map.insert(vreg, value);
                vreg
            },
            &mut self.value_reg_map,
            self.machine,
            &self.func.graph,
            value,
        )
    }

    fn create_phi_vreg(&mut self, builder: &mut LirBuilder<'ctx, M>, value: DepValue) -> VirtReg {
        let class = self
            .machine
            .reg_class_for_type(self.func.graph.value_kind(value).as_value().unwrap());
        let vreg = builder.create_vreg(class);
        self.value_reg_map.insert(value, vreg);
        vreg
    }

    fn is_node_used(&self, node: Node) -> bool {
        dataflow_outputs(&self.func.graph, node).any(|output| self.is_value_used(output))
    }

    fn is_value_used(&self, value: DepValue) -> bool {
        self.value_use_counts[value] > 0
    }

    fn bump_vreg_use_count(&mut self, vreg: VirtReg) {
        if let Some(&value) = self.reg_value_map.get(&vreg) {
            self.value_use_counts[value] += 1;
        }
    }

    fn detach_node_inputs(&mut self, node: Node) {
        for input in dataflow_inputs(&self.func.graph, node) {
            self.value_use_counts[input] -= 1;
        }
    }
}

fn emit_instr<M: MachineLower>(
    builder: &mut InstrBuilder<'_, '_, M>,
    instr: M::Instr,
    defs: &[DefOperand],
    uses: &[UseOperand],
    clobbers: PhysRegSet,
) {
    trace!(
        "    {}",
        display_instr_data::<M>(instr, defs, uses, clobbers)
    );
    builder.push_instr(instr, defs.iter().copied(), uses.iter().copied(), clobbers);
}

fn get_value_vreg_helper<M: MachineLower>(
    builder: impl FnOnce(DepValue, RegClass) -> VirtReg,
    value_reg_map: &mut ValueRegMap,
    machine: &M,
    valgraph: &ValGraph,
    value: DepValue,
) -> VirtReg {
    let class = machine.reg_class_for_type(valgraph.value_kind(value).as_value().unwrap());
    match value_reg_map.entry(value) {
        Entry::Occupied(occupied) => {
            let vreg = *occupied.get();
            assert!(
                vreg.class() == class,
                "attempted to redefine vreg with different class"
            );
            vreg
        }
        Entry::Vacant(vacant) => {
            let vreg = builder(value, class);
            vacant.insert(vreg);
            vreg
        }
    }
}
