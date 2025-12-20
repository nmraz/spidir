use alloc::collections::VecDeque;
use core::ops::{BitAnd, BitOr, BitXor};

use entity_utils::{set::DenseEntitySet, worklist::Worklist};
use fx_utils::FxHashMap;
use graphwalk::PostOrderContext;
use ir::{
    builder::{Builder, BuilderExt},
    function::FunctionBody,
    node::{NodeKind, Type},
    valgraph::{DepValue, Node},
    valwalk::{UnpinnedDataflowPreds, cfg_outputs, get_attached_phis},
};
use smallvec::SmallVec;

use crate::{state::EditContext, utils::replace_with_iconst};

pub fn do_sccp(ctx: &mut EditContext) {
    let (live_cfg_nodes, value_constant_states) = solve_sccp(ctx);

    let mut dead_cfg_input_indices = SmallVec::<[usize; 8]>::new();

    let live_nodes = ctx.live_nodes().clone();
    for node in live_nodes.iter() {
        let kind = ctx.graph().node_kind(node);
        if !kind.has_control_flow() {
            continue;
        }

        if live_cfg_nodes.contains(node) {
            match kind {
                NodeKind::Region => {
                    dead_cfg_input_indices.clear();
                    dead_cfg_input_indices.extend(
                        ctx.graph()
                            .node_inputs(node)
                            .into_iter()
                            .enumerate()
                            .filter(|&(_, input)| {
                                if !ctx.graph().value_kind(input).is_control() {
                                    return false;
                                }

                                let pred = ctx.graph().value_def(input).0;
                                !live_cfg_nodes.contains(pred)
                            })
                            .map(|(i, _)| i),
                    );
                    // TODO: Remove all dead inputs from node/phis.
                }
                NodeKind::BrCond => {
                    // TODO: Delete node if one output is dead.
                }
                _ => {}
            }
        } else {
            ctx.kill_node(node);
        }
    }

    for (&val, state) in &value_constant_states {
        if let &ConstantState::Constant(c) = state {
            replace_with_iconst(ctx, val, c);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstantState {
    SomeConstant,
    Constant(u64),
    NotConstant,
}

impl ConstantState {
    fn meet(&self, rhs: &Self) -> Self {
        match (self, rhs) {
            (Self::SomeConstant, rhs) => *rhs,
            (lhs, Self::SomeConstant) => *lhs,
            (Self::NotConstant, _) | (_, Self::NotConstant) => Self::NotConstant,
            (&Self::Constant(lhs), &Self::Constant(rhs)) => {
                if lhs == rhs {
                    Self::Constant(lhs)
                } else {
                    Self::NotConstant
                }
            }
        }
    }
}

struct SolverState<'b> {
    body: &'b FunctionBody,
    cfg_queue: VecDeque<Node>,
    dataflow_worklist: Worklist<Node>,
    live_cfg_nodes: DenseEntitySet<Node>,
    value_constant_states: FxHashMap<DepValue, ConstantState>,
    cfg_discovered_data_nodes: DenseEntitySet<Node>,
    cfg_visited_data_nodes: DenseEntitySet<Node>,
}

fn solve_sccp(ctx: &EditContext) -> (DenseEntitySet<Node>, FxHashMap<DepValue, ConstantState>) {
    let mut state = SolverState {
        body: ctx.body(),
        cfg_queue: VecDeque::new(),
        dataflow_worklist: Worklist::new(),
        live_cfg_nodes: DenseEntitySet::new(),
        value_constant_states: FxHashMap::default(),
        cfg_discovered_data_nodes: DenseEntitySet::new(),
        cfg_visited_data_nodes: DenseEntitySet::new(),
    };

    let body = ctx.body();
    let graph = &body.graph;

    state.cfg_queue.push_back(body.entry);

    let mut scratch_postorder = PostOrderContext::new();

    while !state.cfg_queue.is_empty() || !state.dataflow_worklist.is_empty() {
        while let Some(node) = state.dataflow_worklist.dequeue() {
            visit_node(&mut state, node);
        }

        while let Some(node) = state.cfg_queue.pop_front() {
            state.live_cfg_nodes.insert(node);

            // Visit any dataflow inputs to this node that we haven't seen yet, as well as the node
            // itself.
            scratch_postorder.reset([node]);
            let pred_graph = UnpinnedDataflowPreds::new(graph, ctx.live_nodes());
            while let Some(pred) =
                scratch_postorder.next(&pred_graph, &mut state.cfg_discovered_data_nodes)
            {
                visit_node(&mut state, pred);
                state.cfg_visited_data_nodes.insert(pred);
            }

            // Visit any phis attached to this node (if it is a region).
            for phi in get_attached_phis(graph, node) {
                visit_node(&mut state, phi);
            }
        }
    }

    (state.live_cfg_nodes, state.value_constant_states)
}

macro_rules! prop_bin_iconst {
    ($state:expr, $node:expr, $func:ident) => {{
        let graph = &$state.body.graph;
        let [out] = graph.node_outputs_exact($node);
        let [lhs, rhs] = graph.node_inputs_exact($node);
        let ty = graph.value_kind(out).as_value().unwrap();
        if let (ConstantState::Constant(lhs), ConstantState::Constant(rhs)) = (
            value_const_state($state, lhs),
            value_const_state($state, rhs),
        ) {
            let combined = if ty == Type::I32 {
                ((lhs as u32).$func(rhs as u32)) as u64
            } else {
                lhs.$func(rhs)
            };
            update_value_const_state($state, out, ConstantState::Constant(combined));
        }
    }};
}

fn visit_node(state: &mut SolverState<'_>, node: Node) {
    let graph = &state.body.graph;
    let kind = graph.node_kind(node);

    if kind.has_control_flow() {
        if matches!(kind, NodeKind::BrCond) {
            let cond = graph.node_inputs(node)[1];
            let [true_ctrl, false_ctrl] = graph.node_outputs_exact(node);

            if let ConstantState::Constant(c) = value_const_state(state, cond) {
                if c != 0 {
                    mark_cfg_live(state, true_ctrl);
                } else {
                    mark_cfg_live(state, false_ctrl);
                }
            } else {
                mark_cfg_live(state, true_ctrl);
                mark_cfg_live(state, false_ctrl);
            }
        } else {
            for edge in cfg_outputs(graph, node) {
                mark_cfg_live(state, edge);
            }
        }
    }

    match kind {
        &NodeKind::Iconst(c) => {
            let [out] = graph.node_outputs_exact(node);
            update_value_const_state(state, out, ConstantState::Constant(c))
        }
        NodeKind::Phi => {
            let [out] = graph.node_outputs_exact(node);

            let inputs = graph.node_inputs(node);
            let region = graph.value_def(inputs[0]).0;
            let ctrl_inputs = graph.node_inputs(region);
            let data_inputs = inputs.into_iter().skip(1).enumerate();

            let const_state =
                data_inputs.fold(ConstantState::SomeConstant, |const_state, (idx, input)| {
                    let cfg_pred = graph.value_def(ctrl_inputs[idx]).0;
                    if !state.live_cfg_nodes.contains(cfg_pred) {
                        return const_state;
                    }

                    const_state.meet(&value_const_state(state, input))
                });

            update_value_const_state(state, out, const_state);
        }
        NodeKind::Iadd => prop_bin_iconst!(state, node, wrapping_add),
        NodeKind::Isub => prop_bin_iconst!(state, node, wrapping_sub),
        NodeKind::And => prop_bin_iconst!(state, node, bitand),
        NodeKind::Or => prop_bin_iconst!(state, node, bitor),
        NodeKind::Xor => prop_bin_iconst!(state, node, bitxor),
        _ => {}
    }
}

fn mark_cfg_live(state: &mut SolverState<'_>, edge: DepValue) {
    let succ = state.body.graph.value_uses(edge).next().unwrap().0;
    if !state.live_cfg_nodes.contains(succ) {
        state.cfg_queue.push_back(succ);
    }
}

fn update_value_const_state(
    state: &mut SolverState<'_>,
    value: DepValue,
    const_state: ConstantState,
) {
    let old_const_state = value_const_state(state, value);
    state.value_constant_states.insert(value, const_state);

    if const_state != old_const_state {
        for (user, _) in state.body.graph.value_uses(value) {
            // Only place a user of the changed value back on the worklist if we've already visited
            // it in the first place. If we haven't visited it at this point, it is either:
            // (1) completely unreachable in the CFG, or
            // (2) reachable but still-unvisited (meaining it will be visited in the future).
            // In both cases, we don't want to explicitly visit it again.
            if state.cfg_visited_data_nodes.contains(user) {
                state.dataflow_worklist.enqueue(user);
            }
        }
    }
}

fn value_const_state(state: &SolverState<'_>, value: DepValue) -> ConstantState {
    state
        .value_constant_states
        .get(&value)
        .copied()
        .unwrap_or(ConstantState::SomeConstant)
}
