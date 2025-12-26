use alloc::collections::VecDeque;

use entity_utils::{set::DenseEntitySet, worklist::Worklist};
use fx_utils::FxHashMap;
use graphwalk::PostOrderContext;
use ir::{
    builder::{Builder, BuilderExt},
    node::{NodeKind, Type},
    valgraph::{DepValue, Node},
    valwalk::{UnpinnedDataflowPreds, cfg_outputs, dataflow_outputs, get_attached_phis},
};
use log::trace;
use smallvec::SmallVec;

use crate::{
    constfold::{
        fold_and, fold_ashr, fold_iadd, fold_icmp, fold_iext, fold_imul, fold_isub, fold_itrunc,
        fold_lshr, fold_or, fold_sdiv, fold_sfill, fold_shl, fold_srem, fold_udiv, fold_urem,
        fold_xor,
    },
    state::EditContext,
    utils::{match_iconst, replace_with_iconst},
};

pub fn do_sccp(ctx: &mut EditContext) {
    let solution = solve_sccp(ctx);

    trace!("killing dead nodes");

    let live_nodes = ctx.live_nodes().clone();

    let mut dead_cfg_input_indices = SmallVec::<[u32; 8]>::new();
    let mut attached_phis = SmallVec::<[Node; 8]>::new();

    for node in live_nodes.iter() {
        if !solution.live_nodes.contains(node) {
            ctx.kill_node(node);
            continue;
        }

        match ctx.graph().node_kind(node) {
            NodeKind::Region => {
                dead_cfg_input_indices.clear();
                dead_cfg_input_indices.extend(
                    ctx.graph()
                        .node_inputs(node)
                        .into_iter()
                        .zip(0..)
                        .filter(|&(edge, _)| !solution.live_cfg_edges.contains(edge))
                        .map(|(_, i)| i),
                );

                // Make sure we remove dead inputs in reverse order, so indices are stable.
                dead_cfg_input_indices.reverse();

                // TODO: This is quadratic in predecessor count.

                for &i in &dead_cfg_input_indices {
                    ctx.remove_node_input(node, i);
                }

                attached_phis.clear();
                attached_phis.extend(get_attached_phis(ctx.graph(), node));

                for &phi in &attached_phis {
                    for &i in &dead_cfg_input_indices {
                        // Note: the first input to every phi is the selector.
                        ctx.remove_node_input(phi, i + 1);
                    }
                }
            }
            NodeKind::BrCond => {
                let [in_ctrl, _] = ctx.graph().node_inputs_exact(node);
                let [true_ctrl, false_ctrl] = ctx.graph().node_outputs_exact(node);

                debug_assert!(
                    solution.live_cfg_edges.contains(true_ctrl)
                        || solution.live_cfg_edges.contains(false_ctrl),
                    "live brcond should have at least one live successor"
                );

                // If exactly one successor is live, remove the branch and reroute its input.
                if !solution.live_cfg_edges.contains(false_ctrl) {
                    ctx.replace_value_and_kill(true_ctrl, in_ctrl);
                } else if !solution.live_cfg_edges.contains(true_ctrl) {
                    ctx.replace_value_and_kill(false_ctrl, in_ctrl);
                }
            }
            _ => {}
        }
    }

    trace!("rewriting constants");

    for (&val, state) in &solution.value_constant_states {
        // Avoid replacing existing constants with themselves.
        if let &ConstantState::Constant(c) = state
            && match_iconst(ctx.graph(), val).is_none()
        {
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

    fn is_le(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (_, Self::SomeConstant) => true,
            (Self::NotConstant, _) => true,
            (&Self::Constant(lhs), &Self::Constant(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

struct Solution {
    live_nodes: DenseEntitySet<Node>,
    live_cfg_edges: DenseEntitySet<DepValue>,
    value_constant_states: FxHashMap<DepValue, ConstantState>,
}

struct SolverState {
    cfg_queue: VecDeque<Node>,
    dataflow_worklist: Worklist<Node>,
    live_cfg_edges: DenseEntitySet<DepValue>,
    value_constant_states: FxHashMap<DepValue, ConstantState>,
    scratch_postorder: PostOrderContext<Node>,
    cfg_discovered_nodes: DenseEntitySet<Node>,
    visited_nodes: DenseEntitySet<Node>,
}

fn solve_sccp(ctx: &EditContext) -> Solution {
    let mut state = SolverState {
        cfg_queue: VecDeque::new(),
        dataflow_worklist: Worklist::new(),
        live_cfg_edges: DenseEntitySet::new(),
        value_constant_states: FxHashMap::default(),
        scratch_postorder: PostOrderContext::new(),
        cfg_discovered_nodes: DenseEntitySet::new(),
        visited_nodes: DenseEntitySet::new(),
    };

    let body = ctx.body();
    let graph = &body.graph;

    state.cfg_queue.push_back(body.entry);

    trace!("solving SCCP");

    while !state.cfg_queue.is_empty() || !state.dataflow_worklist.is_empty() {
        while let Some(node) = state.dataflow_worklist.dequeue() {
            visit_node(ctx, &mut state, node);
        }

        while let Some(node) = state.cfg_queue.pop_front() {
            trace!("ctrl: {node} ({})", ctx.display_node(node));

            discover_node_and_dataflow_preds(ctx, &mut state, node);

            let node_inputs = graph.node_inputs(node);

            // Visit any phis attached to this node; we do this whenever the node/edge is revisited.
            for phi in get_attached_phis(graph, node) {
                // Visit phi inputs manually here so that we grab only live predecessors.
                for (i, phi_input) in graph.node_inputs(phi).into_iter().skip(1).enumerate() {
                    if !state.live_cfg_edges.contains(node_inputs[i]) {
                        continue;
                    }

                    // Discover phi inputs from newly-discovered control inputs.
                    let phi_pred = graph.value_def(phi_input).0;
                    discover_node_and_dataflow_preds(ctx, &mut state, phi_pred);
                }

                // Visit the phi itself now that its inputs are up-to-date.
                visit_node(ctx, &mut state, phi);
                state.visited_nodes.insert(phi);
            }
        }
    }

    trace!("SCCP solution found");

    Solution {
        live_nodes: state.visited_nodes,
        live_cfg_edges: state.live_cfg_edges,
        value_constant_states: state.value_constant_states,
    }
}

fn discover_node_and_dataflow_preds(ctx: &EditContext, state: &mut SolverState, node: Node) {
    // Visit any dataflow inputs to this node that we haven't seen yet, as well as the node
    // itself. This will only walk the nodes the first time we encounter them.
    state.scratch_postorder.reset([node]);
    let pred_graph = UnpinnedDataflowPreds::new(ctx.graph(), ctx.live_nodes());
    while let Some(pred) = state
        .scratch_postorder
        .next(&pred_graph, &mut state.cfg_discovered_nodes)
    {
        visit_node(ctx, state, pred);
        state.visited_nodes.insert(pred);
    }
}

fn visit_node(ctx: &EditContext, state: &mut SolverState, node: Node) {
    let graph = ctx.graph();
    let kind = graph.node_kind(node);

    trace!("visit: {node} ({})", ctx.display_node(node));

    if kind.has_control_flow() {
        if matches!(kind, NodeKind::BrCond) {
            let cond = graph.node_inputs(node)[1];
            let [true_ctrl, false_ctrl] = graph.node_outputs_exact(node);

            if let ConstantState::Constant(c) = value_const_state(state, cond) {
                if c != 0 {
                    mark_cfg_live(ctx, state, true_ctrl);
                } else {
                    mark_cfg_live(ctx, state, false_ctrl);
                }
            } else {
                mark_cfg_live(ctx, state, true_ctrl);
                mark_cfg_live(ctx, state, false_ctrl);
            }
        } else {
            for edge in cfg_outputs(graph, node) {
                mark_cfg_live(ctx, state, edge);
            }
        }
    }

    match kind {
        &NodeKind::Iconst(c) => {
            let [out] = graph.node_outputs_exact(node);
            update_value_const_state(ctx, state, out, ConstantState::Constant(c))
        }
        NodeKind::Phi => {
            let [out] = graph.node_outputs_exact(node);

            let inputs = graph.node_inputs(node);
            let region = graph.value_def(inputs[0]).0;
            let ctrl_inputs = graph.node_inputs(region);
            let data_inputs = inputs.into_iter().skip(1).enumerate();

            let const_state =
                data_inputs.fold(ConstantState::SomeConstant, |const_state, (idx, input)| {
                    if !state.live_cfg_edges.contains(ctrl_inputs[idx]) {
                        return const_state;
                    }

                    const_state.meet(&value_const_state(state, input))
                });

            update_value_const_state(ctx, state, out, const_state);
        }
        NodeKind::Iadd => prop_binop(ctx, state, node, fold_iadd),
        NodeKind::Isub => prop_binop(ctx, state, node, fold_isub),
        NodeKind::And => prop_binop(ctx, state, node, fold_and),
        NodeKind::Or => prop_binop(ctx, state, node, fold_or),
        NodeKind::Xor => prop_binop(ctx, state, node, fold_xor),
        NodeKind::Shl => prop_binop(ctx, state, node, fold_shl),
        NodeKind::Lshr => prop_binop(ctx, state, node, fold_lshr),
        NodeKind::Ashr => prop_binop(ctx, state, node, fold_ashr),
        NodeKind::Imul => prop_binop(ctx, state, node, fold_imul),
        NodeKind::Sdiv => prop_divrem(ctx, state, node, fold_sdiv),
        NodeKind::Udiv => prop_divrem(ctx, state, node, fold_udiv),
        NodeKind::Srem => prop_divrem(ctx, state, node, fold_srem),
        NodeKind::Urem => prop_divrem(ctx, state, node, fold_urem),
        NodeKind::Iext => prop_unop(ctx, state, node, |_, val| fold_iext(val)),
        NodeKind::Itrunc => prop_unop(ctx, state, node, |_, val| fold_itrunc(val)),
        &NodeKind::Sfill(width) => {
            prop_unop(ctx, state, node, |ty, val| fold_sfill(ty, val, width))
        }
        &NodeKind::Icmp(kind) => prop_binop(ctx, state, node, |arg_ty, lhs, rhs| {
            fold_icmp(arg_ty, kind, lhs, rhs)
        }),
        _ => {
            // We don't know anything about this node's outputs...
            for out in dataflow_outputs(graph, node) {
                update_value_const_state(ctx, state, out, ConstantState::NotConstant);
            }
        }
    }
}

fn prop_unop(
    ctx: &EditContext,
    state: &mut SolverState,
    node: Node,
    f: impl FnOnce(Type, u64) -> u64,
) {
    let graph = ctx.graph();
    let [out] = graph.node_outputs_exact(node);
    let [input] = graph.node_inputs_exact(node);
    let ty = graph.value_kind(out).as_value().unwrap();

    match value_const_state(state, input) {
        ConstantState::Constant(input) => {
            update_value_const_state(ctx, state, out, ConstantState::Constant(f(ty, input)));
        }
        ConstantState::NotConstant => {
            update_value_const_state(ctx, state, out, ConstantState::NotConstant);
        }
        _ => {}
    }
}

fn prop_binop(
    ctx: &EditContext,
    state: &mut SolverState,
    node: Node,
    f: impl FnOnce(Type, u64, u64) -> u64,
) {
    let graph = ctx.graph();
    let [out] = graph.node_outputs_exact(node);
    let [lhs, rhs] = graph.node_inputs_exact(node);
    let ty = graph.value_kind(out).as_value().unwrap();

    match (value_const_state(state, lhs), value_const_state(state, rhs)) {
        (ConstantState::Constant(lhs), ConstantState::Constant(rhs)) => {
            update_value_const_state(ctx, state, out, ConstantState::Constant(f(ty, lhs, rhs)));
        }
        (ConstantState::NotConstant, _) | (_, ConstantState::NotConstant) => {
            update_value_const_state(ctx, state, out, ConstantState::NotConstant);
        }
        _ => {}
    }
}

fn prop_divrem(
    ctx: &EditContext,
    state: &mut SolverState,
    node: Node,
    f: impl FnOnce(Type, u64, u64) -> Option<u64>,
) {
    let graph = ctx.graph();
    let [_, out] = graph.node_outputs_exact(node);
    let [_, lhs, rhs] = graph.node_inputs_exact(node);
    let ty = graph.value_kind(out).as_value().unwrap();

    match (value_const_state(state, lhs), value_const_state(state, rhs)) {
        (ConstantState::Constant(lhs), ConstantState::Constant(rhs)) => {
            if let Some(res) = f(ty, lhs, rhs) {
                update_value_const_state(ctx, state, out, ConstantState::Constant(res));
            }
        }
        (ConstantState::NotConstant, _) | (_, ConstantState::NotConstant) => {
            update_value_const_state(ctx, state, out, ConstantState::NotConstant);
        }
        _ => {}
    }
}

fn mark_cfg_live(ctx: &EditContext, state: &mut SolverState, edge: DepValue) {
    if state.live_cfg_edges.contains(edge) {
        return;
    }

    state.live_cfg_edges.insert(edge);

    let succ = ctx.graph().value_uses(edge).next().unwrap().0;
    trace!("    discover: {succ} ({})", ctx.display_node(succ));
    state.cfg_queue.push_back(succ);
}

fn update_value_const_state(
    ctx: &EditContext,
    state: &mut SolverState,
    value: DepValue,
    const_state: ConstantState,
) {
    let old_const_state = value_const_state(state, value);
    state.value_constant_states.insert(value, const_state);

    if const_state != old_const_state {
        trace!("    refine: {value} ({old_const_state:?} -> {const_state:?})");
        debug_assert!(const_state.is_le(&old_const_state));

        for (user, _) in ctx.graph().value_uses(value) {
            // Only place a user of the changed value back on the worklist if we've already visited
            // it in the first place. If we haven't visited it at this point, it is either:
            // (1) completely unreachable in the CFG, or
            // (2) reachable but still-unvisited (meaining it will be visited in the future).
            // In both cases, we don't want to explicitly visit it again.
            if state.visited_nodes.contains(user) {
                trace!("        requeue: {user} ({})", ctx.display_node(user));
                state.dataflow_worklist.enqueue(user);
            }
        }
    }
}

fn value_const_state(state: &SolverState, value: DepValue) -> ConstantState {
    state
        .value_constant_states
        .get(&value)
        .copied()
        .unwrap_or(ConstantState::SomeConstant)
}
