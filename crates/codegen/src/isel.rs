use alloc::vec::Vec;
use core::{array, fmt};

use cranelift_entity::{SecondaryMap, packed_option::PackedOption};
use fx_utils::FxHashMap;
use ir::{
    function::{FunctionBody, FunctionBorrow},
    module::ModuleMetadata,
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
        Builder as LirBuilder, DefOperand, InstrBuilder, Lir, MemLayout, PhysReg, PhysRegSet,
        RegClass, StackSlot, UseOperand, VirtReg,
    },
    machine::MachineLower,
    schedule::Schedule,
};

pub enum ParamLoc {
    Reg { reg: PhysReg },
    Stack { fp_offset: i32 },
}

pub struct MachineIselError;

pub struct IselContext<'ctx, 's, M: MachineLower> {
    state: &'s mut IselState<'ctx, M>,
    builder: InstrBuilder<'ctx, 's, M>,
}

impl<'ctx, M: MachineLower> IselContext<'ctx, '_, M> {
    pub fn module_metadata(&self) -> &'ctx ModuleMetadata {
        self.state.module_metadata
    }

    pub fn value_type(&self, value: DepValue) -> Type {
        self.state.graph().value_kind(value).as_value().unwrap()
    }

    pub fn value_def(&self, value: DepValue) -> (Node, u32) {
        self.state.graph().value_def(value)
    }

    pub fn value_use_count(&self, value: DepValue) -> u32 {
        self.state.value_use_counts[value]
    }

    pub fn has_one_use(&self, value: DepValue) -> bool {
        self.value_use_count(value) == 1
    }

    pub fn node_kind(&self, node: Node) -> &NodeKind {
        self.state.graph().node_kind(node)
    }

    pub fn node_stack_slot(&self, node: Node) -> StackSlot {
        *self
            .state
            .stack_slot_map
            .get(&node)
            .expect("node is not a stack slot")
    }

    pub fn node_inputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'ctx {
        dataflow_inputs(self.state.graph(), node)
    }

    pub fn node_outputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'ctx {
        dataflow_outputs(self.state.graph(), node)
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
        let graph = self.state.graph();
        get_value_vreg_helper(
            |value, class| {
                let vreg = self.builder.create_vreg(class);
                self.state.reg_value_map[vreg] = value.into();
                vreg
            },
            &mut self.state.value_reg_map,
            self.state.machine,
            graph,
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

#[derive(Debug, Clone, Copy)]
pub struct IselError {
    pub node: Node,
}

impl IselError {
    pub fn display<'a>(
        self,
        module_metadata: &'a ModuleMetadata,
        body: &'a FunctionBody,
    ) -> DisplayIselError<'a> {
        DisplayIselError {
            module_metadata,
            body,
            error: self,
        }
    }
}

pub struct DisplayIselError<'a> {
    module_metadata: &'a ModuleMetadata,
    body: &'a FunctionBody,
    error: IselError,
}

impl fmt::Display for DisplayIselError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to select `{}`",
            display_node(self.module_metadata, self.body, self.error.node)
        )
    }
}

pub fn select_instrs<M: MachineLower>(
    module_metadata: &ModuleMetadata,
    func: FunctionBorrow<'_>,
    schedule: &Schedule,
    cfg_ctx: &CfgContext,
    block_map: &FunctionBlockMap,
    machine: &M,
) -> Result<Lir<M>, IselError> {
    debug!("selecting instructions for: {}", func.metadata);

    let mut state = IselState::new(module_metadata, func, schedule, cfg_ctx, block_map, machine);
    let mut builder = LirBuilder::new(&cfg_ctx.block_order);
    state.prepare_for_isel(&mut builder);

    while let Some(block) = builder.advance_block() {
        state.select_block(&mut builder, block)?;
    }

    Ok(builder.finish())
}

type ValueRegMap = SecondaryMap<DepValue, PackedOption<VirtReg>>;
type RegValueMap = SecondaryMap<VirtReg, PackedOption<DepValue>>;
type ValueUseCounts = SecondaryMap<DepValue, u32>;
type NodeStackSlotMap = FxHashMap<Node, StackSlot>;

struct IselState<'ctx, M: MachineLower> {
    module_metadata: &'ctx ModuleMetadata,
    func: FunctionBorrow<'ctx>,
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
        module_metadata: &'ctx ModuleMetadata,
        func: FunctionBorrow<'ctx>,
        schedule: &'ctx Schedule,
        cfg_ctx: &'ctx CfgContext,
        block_map: &'ctx FunctionBlockMap,
        machine: &'ctx M,
    ) -> Self {
        Self {
            module_metadata,
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
                let phi_inputs = self.graph().node_inputs(phi);

                for &pred in self.cfg_ctx.cfg.block_preds(block) {
                    // Values flowing into block parameters are live if they come in a cross a live
                    // control edge; they are dead otherwise.
                    let Some(valgraph_pred_idx) = self.block_map.valgraph_pred_index(block, pred)
                    else {
                        continue;
                    };

                    // Note: input 0 is the selector from the containing block.
                    let input = phi_inputs[valgraph_pred_idx as usize + 1];
                    self.value_use_counts[input] += 1;
                }

                // Create a fresh register for every incoming block parameter (phi output).
                let output = self.graph().node_outputs(phi)[0];
                self.create_phi_vreg(builder, output);
            }

            for &node in self.schedule.scheduled_nodes(block) {
                // Values flowing into scheduled nodes start live, but may be made dead by the
                // instruction selection process.
                for input in dataflow_inputs(self.graph(), node) {
                    self.value_use_counts[input] += 1;
                }

                if let &NodeKind::StackSlot { size, align } = self.graph().node_kind(node) {
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
            .is_some_and(|&node| self.graph().node_kind(node).is_terminator());

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
            trace!(
                "  {}:",
                display_node(self.module_metadata, self.body(), node)
            );

            // Note: node inputs should be detached after selection so the selector sees correct use
            // counts for the current node's inputs.

            if !self.graph().node_kind(node).has_side_effects() && !self.is_node_used(node) {
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
        let node_kind = self.graph().node_kind(node);
        match node_kind {
            NodeKind::Entry => {
                assert!(block == self.cfg_ctx.block_order[0], "misplaced entry node");
                let param_locs = self.machine.param_locs(&self.func.metadata.sig.param_types);
                let entry_outputs = self.graph().node_outputs(node);
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
                            let ty = self.graph().value_kind(value).as_value().unwrap();
                            emit_instr(
                                &mut builder,
                                self.machine.make_fp_relative_load(ty, fp_offset),
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
        if let &[pred] = self.cfg_ctx.cfg.block_preds(block) {
            // For single-predecessor blocks, just copy the values from the predecessor. This,
            // combined with critical edge splitting, means that we can always place parallel copies
            // for block param resolution in the predecessor later.

            // If our predecessor came from the valgraph, make sure to take its actual index for the
            // phi input - it might not be 0 if some of the valgraph predecessors are dead!
            let valgraph_pred_idx = self.block_map.valgraph_pred_index(block, pred).unwrap_or(0);
            let phi_input = valgraph_pred_idx as usize + 1;

            for &phi in self.schedule.block_phis(block) {
                let input = self.get_value_vreg(builder, self.graph().node_inputs(phi)[phi_input]);
                let output = self.value_reg_map[self.graph().node_outputs(phi)[0]].unwrap();
                builder.copy_vreg(output, input);
            }
        } else {
            // Multiple predecessors - the phis are really necessary.
            builder.set_incoming_block_params(self.schedule.block_phis(block).iter().map(|&phi| {
                let val = self.value_reg_map[self.graph().node_outputs(phi)[0]].unwrap();
                trace!("  {}", display_node(self.module_metadata, self.body(), phi));
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
                    let input = self.graph().node_inputs(phi)[valgraph_pred_idx as usize + 1];
                    self.get_value_vreg(builder, input)
                })
                .collect();
            trace!(
                "    => {}{}",
                succ,
                builder.display_block_params(&outgoing_params)
            );
            builder.set_outgoing_block_params(outgoing_params);
        }
    }

    fn get_value_vreg(&mut self, builder: &mut LirBuilder<'ctx, M>, value: DepValue) -> VirtReg {
        let graph = self.graph();
        get_value_vreg_helper(
            |value, class| {
                let vreg = builder.create_vreg(class);
                self.reg_value_map[vreg] = value.into();
                vreg
            },
            &mut self.value_reg_map,
            self.machine,
            graph,
            value,
        )
    }

    fn create_phi_vreg(&mut self, builder: &mut LirBuilder<'ctx, M>, value: DepValue) -> VirtReg {
        let class = self
            .machine
            .reg_class_for_type(self.graph().value_kind(value).as_value().unwrap());
        let vreg = builder.create_vreg(class);
        self.value_reg_map[value] = vreg.into();
        vreg
    }

    fn is_node_used(&self, node: Node) -> bool {
        dataflow_outputs(self.graph(), node).any(|output| self.is_value_used(output))
    }

    fn is_value_used(&self, value: DepValue) -> bool {
        self.value_use_counts[value] > 0
    }

    fn bump_vreg_use_count(&mut self, vreg: VirtReg) {
        if let Some(value) = self.reg_value_map[vreg].expand() {
            self.value_use_counts[value] += 1;
        }
    }

    fn detach_node_inputs(&mut self, node: Node) {
        for input in dataflow_inputs(self.graph(), node) {
            self.value_use_counts[input] -= 1;
        }
    }

    fn graph(&self) -> &'ctx ValGraph {
        &self.body().graph
    }

    fn body(&self) -> &'ctx FunctionBody {
        self.func.body
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
        builder
            .lir_builder()
            .display_instr_data(instr, defs, uses, clobbers)
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
    match value_reg_map[value].expand() {
        Some(vreg) => vreg,
        None => {
            let vreg = builder(value, class);
            value_reg_map[value] = vreg.into();
            vreg
        }
    }
}
