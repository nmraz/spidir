use core::array;

use cranelift_entity::SecondaryMap;
use fx_utils::FxHashMap;
use hashbrown::hash_map::Entry;
use ir::{
    node::{NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{dataflow_inputs, dataflow_outputs, dataflow_preds},
};
use smallvec::SmallVec;

use crate::{
    cfg::{Block, CfgContext},
    lir::{Builder as LirBuilder, DefOperand, InstrBuilder, Lir, RegClass, UseOperand, VirtReg},
    schedule::Schedule,
};

pub struct IselContext<'ctx, 's, B: Backend> {
    state: &'s mut IselState<'ctx, B>,
    builder: InstrBuilder<'ctx, 's, B::Instr>,
}

impl<'ctx, 's, B: Backend> IselContext<'ctx, 's, B> {
    pub fn value_type(&self, value: DepValue) -> Type {
        self.state.valgraph.value_kind(value).as_value().unwrap()
    }

    pub fn value_def(&self, value: DepValue) -> Option<(Node, u32)> {
        let (node, input_idx) = self.state.valgraph.value_def(value);

        // Only allow pure nodes to be looked through for pattern matching.
        if !self.node_kind(node).has_control_flow() {
            Some((node, input_idx))
        } else {
            None
        }
    }

    pub fn node_use_count(&self, node: Node) -> u32 {
        self.state.node_use_counts[node]
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

    pub fn get_value_vreg(&mut self, backend: &impl Backend, value: DepValue) -> VirtReg {
        get_value_vreg_helper(
            |value, class| {
                let vreg = self.builder.create_vreg(class);
                self.state
                    .reg_node_map
                    .insert(vreg, self.state.valgraph.value_def(value).0);
                vreg
            },
            &mut self.state.value_reg_map,
            backend,
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

    pub fn emit_instr(&mut self, instr: B::Instr, defs: &[DefOperand], uses: &[UseOperand]) {
        // Bump the use count for any operands that came from nodes in the original graph.
        for &use_op in uses {
            if let Some(&node) = self.state.reg_node_map.get(&use_op.reg()) {
                self.state.node_use_counts[node] += 1;
            }
        }
        self.builder
            .push_instr(instr, defs.iter().copied(), uses.iter().copied());
    }
}

pub struct IselFailed;

pub trait Backend: Sized {
    type Instr;

    fn reg_class_for_type(&self, ty: Type) -> RegClass;
    fn create_jump(&self, target: Block) -> Self::Instr;

    fn select(
        &self,
        instr: Node,
        targets: &[Block],
        ctx: &mut IselContext<'_, '_, Self>,
    ) -> Result<(), IselFailed>;
}

pub fn select_instrs<B: Backend>(
    valgraph: &ValGraph,
    schedule: &Schedule,
    cfg_ctx: &CfgContext,
    backend: &B,
) -> Result<Lir<B::Instr>, IselFailed> {
    let mut state = IselState::new(valgraph, schedule, cfg_ctx, backend);
    let mut builder = LirBuilder::new(&cfg_ctx.block_order);
    state.prepare_for_isel(&mut builder);

    while let Some(block) = builder.advance_block() {
        state.select_block(&mut builder, block)?;
    }

    Ok(builder.finish())
}

type ValueRegMap = FxHashMap<DepValue, VirtReg>;
type RegNodeMap = FxHashMap<VirtReg, Node>;
type NodeUseCountMap = SecondaryMap<Node, u32>;

struct IselState<'ctx, B: Backend> {
    valgraph: &'ctx ValGraph,
    schedule: &'ctx Schedule,
    cfg_ctx: &'ctx CfgContext,
    backend: &'ctx B,
    value_reg_map: ValueRegMap,
    reg_node_map: RegNodeMap,
    node_use_counts: NodeUseCountMap,
}

impl<'ctx, B: Backend> IselState<'ctx, B> {
    fn new(
        valgraph: &'ctx ValGraph,
        schedule: &'ctx Schedule,
        cfg_ctx: &'ctx CfgContext,
        backend: &'ctx B,
    ) -> Self {
        Self {
            valgraph,
            schedule,
            cfg_ctx,
            backend,
            value_reg_map: Default::default(),
            reg_node_map: Default::default(),
            node_use_counts: Default::default(),
        }
    }

    fn prepare_for_isel(&mut self, builder: &mut LirBuilder<'ctx, B::Instr>) {
        // TODO: Classify side effects.
        // TODO: Collect stack slots and prepare dedicated stack slot mapping.

        for &block in &self.cfg_ctx.block_order {
            for &phi in self.schedule.block_phis(block) {
                // Values flowing into block parameters are always live.
                // TODO: Except for values from dead control inputs?
                for pred in dataflow_preds(self.valgraph, phi) {
                    self.node_use_counts[pred] += 1;
                }

                // Create a fresh register for every incoming block parameter (phi output).
                let output = self.valgraph.node_outputs(phi)[0];
                self.create_phi_vreg(builder, output);
            }

            for &node in self.schedule.scheduled_nodes_rev(block) {
                // Values flowing into scheduled nodes start live, but may be made dead by the
                // instruction selection process.
                for pred in dataflow_preds(self.valgraph, node) {
                    self.node_use_counts[pred] += 1;
                }
            }
        }
    }

    fn select_block(
        &mut self,
        builder: &mut LirBuilder<'ctx, B::Instr>,
        block: Block,
    ) -> Result<(), IselFailed> {
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
            builder.build_instrs(|mut builder| {
                builder.push_instr(self.backend.create_jump(succs[0]), [], []);
            });
        }

        for &node in self.schedule.scheduled_nodes_rev(block) {
            // Note: node inputs should be detached after selection so the selector sees correct use
            // counts for the current node's inputs.

            let node_kind = self.valgraph.node_kind(node);
            if !node_kind.has_control_flow() && self.node_use_counts[node] == 0 {
                // This node is now dead as it was folded into a previous computation.
                self.detach_node_inputs(node);
                continue;
            }

            builder.build_instrs(|builder| {
                let targets = if node_kind.is_terminator() {
                    self.cfg_ctx.cfg.block_succs(block)
                } else {
                    &[]
                };
                let backend = self.backend;

                let mut context = IselContext {
                    builder,
                    state: self,
                };
                backend.select(node, targets, &mut context)
            })?;

            self.detach_node_inputs(node);
        }

        Ok(())
    }

    fn lower_block_params(&mut self, builder: &mut LirBuilder<'ctx, B::Instr>, block: Block) {
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

    fn get_value_vreg(
        &mut self,
        builder: &mut LirBuilder<'ctx, B::Instr>,
        value: DepValue,
    ) -> VirtReg {
        get_value_vreg_helper(
            |value, class| {
                let vreg = builder.create_vreg(class);
                self.reg_node_map
                    .insert(vreg, self.valgraph.value_def(value).0);
                vreg
            },
            &mut self.value_reg_map,
            self.backend,
            self.valgraph,
            value,
        )
    }

    fn create_phi_vreg(
        &mut self,
        builder: &mut LirBuilder<'ctx, B::Instr>,
        value: DepValue,
    ) -> VirtReg {
        let class = self
            .backend
            .reg_class_for_type(self.valgraph.value_kind(value).as_value().unwrap());
        let vreg = builder.create_vreg(class);
        self.value_reg_map.insert(value, vreg);
        vreg
    }

    fn detach_node_inputs(&mut self, node: Node) {
        for pred in dataflow_preds(self.valgraph, node) {
            self.node_use_counts[pred] -= 1;
        }
    }
}

fn get_value_vreg_helper<B: Backend>(
    builder: impl FnOnce(DepValue, RegClass) -> VirtReg,
    value_reg_map: &mut ValueRegMap,
    backend: &B,
    valgraph: &ValGraph,
    value: DepValue,
) -> VirtReg {
    let class = backend.reg_class_for_type(valgraph.value_kind(value).as_value().unwrap());
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
