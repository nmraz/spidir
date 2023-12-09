use core::array;

use cranelift_entity::SecondaryMap;
use fx_utils::FxHashMap;
use hashbrown::hash_map::Entry;
use ir::{
    node::{NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{dataflow_inputs, dataflow_outputs},
};
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext},
    lir::{Builder as LirBuilder, DefOperand, InstrBuilder, Lir, RegClass, UseOperand, VirtReg},
    machine::MachineLower,
    schedule::Schedule,
};

pub struct IselContext<'ctx, 's, M: MachineLower> {
    state: &'s mut IselState<'ctx, M>,
    builder: InstrBuilder<'ctx, 's, M>,
}

impl<'ctx, 's, M: MachineLower> IselContext<'ctx, 's, M> {
    pub fn value_type(&self, value: DepValue) -> Type {
        self.state.valgraph.value_kind(value).as_value().unwrap()
    }

    pub fn value_def(&self, value: DepValue) -> Option<(Node, u32)> {
        let (node, input_idx) = self.state.valgraph.value_def(value);

        // Only allow pure nodes to be looked through for pattern matching.
        if !self.node_kind(node).has_side_effects() {
            Some((node, input_idx))
        } else {
            None
        }
    }

    pub fn value_use_count(&self, value: DepValue) -> u32 {
        self.state.value_use_counts[value]
    }

    pub fn node_kind(&self, node: Node) -> NodeKind {
        *self.state.valgraph.node_kind(node)
    }

    pub fn node_inputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'ctx {
        dataflow_inputs(self.state.valgraph, node)
    }

    pub fn node_outputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'ctx {
        dataflow_outputs(self.state.valgraph, node)
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
            self.state.valgraph,
            value,
        )
    }

    pub fn create_temp_vreg(&mut self, class: RegClass) -> VirtReg {
        self.builder.create_vreg(class)
    }

    pub fn copy_vreg(&mut self, dest: VirtReg, src: VirtReg) {
        self.builder.copy_vreg(dest, src)
    }

    pub fn emit_instr(&mut self, instr: M::Instr, defs: &[DefOperand], uses: &[UseOperand]) {
        // Bump the use count for any operands that came from values in the original graph.
        for &use_op in uses {
            if let Some(&value) = self.state.reg_value_map.get(&use_op.reg()) {
                self.state.value_use_counts[value] += 1;
            }
        }
        self.builder
            .push_instr(instr, defs.iter().copied(), uses.iter().copied());
    }
}

pub struct IselError {
    pub node: Node,
}

pub fn select_instrs<M: MachineLower>(
    valgraph: &ValGraph,
    schedule: &Schedule,
    cfg_ctx: &CfgContext,
    machine: &M,
) -> Result<Lir<M>, IselError> {
    let mut state = IselState::new(valgraph, schedule, cfg_ctx, machine);
    let mut builder = LirBuilder::new(&cfg_ctx.block_order);
    state.prepare_for_isel(&mut builder);

    // TODO: Handle entry node
    while let Some(block) = builder.advance_block() {
        state.select_block(&mut builder, block)?;
    }

    Ok(builder.finish())
}

type ValueRegMap = FxHashMap<DepValue, VirtReg>;
type RegValueMap = FxHashMap<VirtReg, DepValue>;
type ValueUseCounts = SecondaryMap<DepValue, u32>;

struct IselState<'ctx, M: MachineLower> {
    valgraph: &'ctx ValGraph,
    schedule: &'ctx Schedule,
    cfg_ctx: &'ctx CfgContext,
    machine: &'ctx M,
    value_reg_map: ValueRegMap,
    reg_value_map: RegValueMap,
    value_use_counts: ValueUseCounts,
}

impl<'ctx, M: MachineLower> IselState<'ctx, M> {
    fn new(
        valgraph: &'ctx ValGraph,
        schedule: &'ctx Schedule,
        cfg_ctx: &'ctx CfgContext,
        backend: &'ctx M,
    ) -> Self {
        Self {
            valgraph,
            schedule,
            cfg_ctx,
            machine: backend,
            value_reg_map: Default::default(),
            reg_value_map: Default::default(),
            value_use_counts: Default::default(),
        }
    }

    fn prepare_for_isel(&mut self, builder: &mut LirBuilder<'ctx, M>) {
        // TODO: Classify side effects.
        // TODO: Collect stack slots and prepare dedicated stack slot mapping.

        for &block in &self.cfg_ctx.block_order {
            for &phi in self.schedule.block_phis(block) {
                // Values flowing into block parameters are always live.
                // TODO: Except for values from dead control inputs?
                for input in dataflow_inputs(self.valgraph, phi) {
                    self.value_use_counts[input] += 1;
                }

                // Create a fresh register for every incoming block parameter (phi output).
                let output = self.valgraph.node_outputs(phi)[0];
                self.create_phi_vreg(builder, output);
            }

            for &node in self.schedule.scheduled_nodes_rev(block) {
                // Values flowing into scheduled nodes start live, but may be made dead by the
                // instruction selection process.
                for input in dataflow_inputs(self.valgraph, node) {
                    self.value_use_counts[input] += 1;
                }
            }
        }
    }

    fn select_block(
        &mut self,
        builder: &mut LirBuilder<'ctx, M>,
        block: Block,
    ) -> Result<(), IselError> {
        self.lower_block_params(builder, block);

        let is_terminated = self
            .schedule
            .scheduled_nodes_rev(block)
            .first()
            .map_or(false, |&node| self.valgraph.node_kind(node).is_terminator());

        if !is_terminated {
            let succs = self.cfg_ctx.cfg.block_succs(block);
            assert!(
                succs.len() == 1,
                "unterminated block does not have 1 successor"
            );
            let machine = self.machine;
            builder.build_instrs(|mut builder| {
                builder.push_instr(machine.make_jump(succs[0]), [], []);
            });
        }

        for &node in self.schedule.scheduled_nodes_rev(block) {
            // Note: node inputs should be detached after selection so the selector sees correct use
            // counts for the current node's inputs.

            let node_kind = self.valgraph.node_kind(node);
            if !node_kind.has_side_effects() && !self.is_node_used(node) {
                // This node is now dead as it was folded into a previous computation.
                self.detach_node_inputs(node);
                continue;
            }

            builder
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
                .map_err(|_| IselError { node })?;

            self.detach_node_inputs(node);
        }

        Ok(())
    }

    fn lower_block_params(&mut self, builder: &mut LirBuilder<'ctx, M>, block: Block) {
        builder.set_incoming_block_params(
            self.schedule
                .block_phis(block)
                .iter()
                .map(|&phi| self.value_reg_map[&self.valgraph.node_outputs(phi)[0]]),
        );

        for &succ in self.cfg_ctx.cfg.block_succs(block).iter() {
            // TODO: Avoid linear search to deal with pathological cases?
            let pred_idx = self
                .cfg_ctx
                .cfg
                .block_preds(succ)
                .iter()
                .position(|&pred| pred == block)
                .unwrap();

            let Some(valgraph_pred_idx) =
                self.cfg_ctx.block_map.valgraph_pred_index(succ, pred_idx)
            else {
                // This successor didn't come from the valgraph (it was a split critical edge).
                continue;
            };

            let outgoing_params: SmallVec<[_; 4]> = self
                .schedule
                .block_phis(succ)
                .iter()
                .map(|&phi| {
                    let input = self.valgraph.node_inputs(phi)[valgraph_pred_idx as usize];
                    self.get_value_vreg(builder, input)
                })
                .collect();

            builder.add_succ_outgoing_block_params(outgoing_params);
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
            self.valgraph,
            value,
        )
    }

    fn create_phi_vreg(&mut self, builder: &mut LirBuilder<'ctx, M>, value: DepValue) -> VirtReg {
        let class = self
            .machine
            .reg_class_for_type(self.valgraph.value_kind(value).as_value().unwrap());
        let vreg = builder.create_vreg(class);
        self.value_reg_map.insert(value, vreg);
        vreg
    }

    fn is_node_used(&self, node: Node) -> bool {
        dataflow_outputs(self.valgraph, node).any(|output| self.value_use_counts[output] > 0)
    }

    fn detach_node_inputs(&mut self, node: Node) {
        for input in dataflow_inputs(self.valgraph, node) {
            self.value_use_counts[input] -= 1;
        }
    }
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
