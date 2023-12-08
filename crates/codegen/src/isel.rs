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

type ValueRegMap = FxHashMap<DepValue, VirtReg>;
type RegNodeMap = FxHashMap<VirtReg, Node>;
type NodeUseCountMap = SecondaryMap<Node, u32>;

pub struct IselContext<'a, 'o, I> {
    valgraph: &'a ValGraph,
    builder: InstrBuilder<'a, 'o, I>,
    value_reg_map: &'a mut ValueRegMap,
    reg_node_map: &'a mut RegNodeMap,
    node_use_counts: &'a mut NodeUseCountMap,
}

impl<'a, 'b, I> IselContext<'a, 'b, I> {
    pub fn value_type(&self, value: DepValue) -> Type {
        self.valgraph.value_kind(value).as_value().unwrap()
    }

    pub fn value_def(&self, value: DepValue) -> Option<(Node, u32)> {
        // TODO: Only look through when allowed based on side effects
        Some(self.valgraph.value_def(value))
    }

    pub fn node_use_count(&self, node: Node) -> u32 {
        self.node_use_counts[node]
    }

    pub fn node_kind(&self, node: Node) -> NodeKind {
        *self.valgraph.node_kind(node)
    }

    pub fn node_inputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'a {
        dataflow_inputs(self.valgraph, node)
    }

    pub fn node_outputs(&self, node: Node) -> impl Iterator<Item = DepValue> + 'a {
        dataflow_outputs(self.valgraph, node)
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
                self.reg_node_map
                    .insert(vreg, self.valgraph.value_def(value).0);
                vreg
            },
            self.value_reg_map,
            backend,
            self.valgraph,
            value,
        )
    }

    pub fn create_temp_vreg(&mut self, class: RegClass) -> VirtReg {
        self.builder.create_vreg(class)
    }

    pub fn copy_vreg(&mut self, dest: VirtReg, src: VirtReg) {
        self.builder.copy_vreg(dest, src)
    }

    pub fn emit_instr(&mut self, instr: I, defs: &[DefOperand], uses: &[UseOperand]) {
        // Bump the use count for any operands that came from nodes in the original graph.
        for &use_op in uses {
            if let Some(&node) = self.reg_node_map.get(&use_op.reg()) {
                self.node_use_counts[node] += 1;
            }
        }
        self.builder
            .push_instr(instr, defs.iter().copied(), uses.iter().copied());
    }
}

pub struct IselFailed;

pub trait Backend {
    type Instr;

    fn reg_class_for_type(&self, ty: Type) -> RegClass;

    fn select(
        &self,
        instr: Node,
        targets: &[Block],
        ctx: &mut IselContext<'_, '_, Self::Instr>,
    ) -> Result<(), IselFailed>;

    fn emit_jump(
        &self,
        target: Block,
        ctx: &mut IselContext<'_, '_, Self::Instr>,
    ) -> Result<(), IselFailed>;
}

pub fn select_instrs<B: Backend>(
    valgraph: &ValGraph,
    schedule: &Schedule,
    cfg_ctx: &CfgContext,
    backend: &B,
) -> Result<Lir<B::Instr>, IselFailed> {
    // TODO: Classify side effects.
    // TODO: Collect stack slots and prepare dedicated stack slot mapping.

    let mut value_reg_map = ValueRegMap::default();
    let mut reg_node_map = RegNodeMap::default();
    let mut node_use_counts = NodeUseCountMap::new();

    let mut lir_builder = LirBuilder::new(&cfg_ctx.block_order);

    // Count original node uses before we start selection.
    for &block in &cfg_ctx.block_order {
        for &phi in schedule.block_phis(block) {
            // Values flowing into block parameters are always live.
            // TODO: Except for values from dead control inputs?
            for pred in dataflow_preds(valgraph, phi) {
                node_use_counts[pred] += 1;
            }

            // Create a fresh register for every incoming block parameter (phi output).
            let output = valgraph.node_outputs(phi)[0];
            create_phi_vreg(
                &mut lir_builder,
                &mut value_reg_map,
                backend,
                valgraph,
                output,
            );
        }

        for &node in schedule.scheduled_nodes_rev(block) {
            // Values flowing into scheduled nodes start live, but may be made dead by the
            // instruction selection process.
            for pred in dataflow_preds(valgraph, node) {
                node_use_counts[pred] += 1;
            }
        }
    }

    while let Some(block) = lir_builder.advance_block() {
        lir_builder.set_incoming_block_params(
            schedule
                .block_phis(block)
                .iter()
                .map(|&phi| value_reg_map[&valgraph.node_outputs(phi)[0]]),
        );

        for &succ in cfg_ctx.cfg.block_succs(block).iter() {
            // TODO: Avoid linear search to deal with pathological cases?
            let pred_idx = cfg_ctx
                .cfg
                .block_preds(succ)
                .iter()
                .position(|&pred| pred == block)
                .unwrap();

            let Some(valgraph_pred_idx) = cfg_ctx.block_map.valgraph_pred_index(succ, pred_idx)
            else {
                // This successor didn't come from the valgraph (it was a split critical edge).
                continue;
            };

            let outgoing_params: SmallVec<[_; 4]> = schedule
                .block_phis(succ)
                .iter()
                .map(|&phi| {
                    let input = valgraph.node_inputs(phi)[valgraph_pred_idx as usize];
                    get_value_vreg(
                        &mut lir_builder,
                        &mut value_reg_map,
                        &mut reg_node_map,
                        backend,
                        valgraph,
                        input,
                    )
                })
                .collect();

            lir_builder.add_succ_outgoing_block_params(outgoing_params)
        }

        // TODO: Branch at end of block if necessary.

        for &node in schedule.scheduled_nodes_rev(block) {
            // Note: node inputs should be detached after selection so the selector sees correct use
            // counts for the current node's inputs.

            if !valgraph.node_kind(node).has_control_flow() && node_use_counts[node] == 0 {
                detach_node_inputs(valgraph, node, &mut node_use_counts);
                continue;
            }

            lir_builder.build_instrs(|builder| {
                let mut context = IselContext {
                    valgraph,
                    builder,
                    value_reg_map: &mut value_reg_map,
                    node_use_counts: &mut node_use_counts,
                    reg_node_map: &mut reg_node_map,
                };

                // TODO: Branches need special handling.
                backend.select(node, &[], &mut context)
            })?;

            detach_node_inputs(valgraph, node, &mut node_use_counts);
        }
    }

    Ok(lir_builder.finish())
}

fn get_value_vreg<B: Backend>(
    lir_builder: &mut LirBuilder<'_, B::Instr>,
    value_reg_map: &mut ValueRegMap,
    reg_node_map: &mut RegNodeMap,
    backend: &B,
    valgraph: &ValGraph,
    value: DepValue,
) -> VirtReg {
    get_value_vreg_helper(
        |value, class| {
            let vreg = lir_builder.create_vreg(class);
            reg_node_map.insert(vreg, valgraph.value_def(value).0);
            vreg
        },
        value_reg_map,
        backend,
        valgraph,
        value,
    )
}

fn create_phi_vreg<B: Backend>(
    lir_builder: &mut LirBuilder<'_, B::Instr>,
    value_reg_map: &mut ValueRegMap,
    backend: &B,
    valgraph: &ValGraph,
    value: DepValue,
) -> VirtReg {
    let class = backend.reg_class_for_type(valgraph.value_kind(value).as_value().unwrap());
    let vreg = lir_builder.create_vreg(class);
    value_reg_map.insert(value, vreg);
    vreg
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

fn detach_node_inputs(valgraph: &ValGraph, node: Node, node_use_counts: &mut NodeUseCountMap) {
    for pred in dataflow_preds(valgraph, node) {
        node_use_counts[pred] -= 1;
    }
}
