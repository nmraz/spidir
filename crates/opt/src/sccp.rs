use entity_utils::{set::DenseEntitySet, worklist::Worklist};
use fx_utils::FxHashMap;
use ir::{
    builder::{Builder, BuilderExt},
    function::FunctionBody,
    node::NodeKind,
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{cfg_preds, cfg_succs, get_attached_phis},
};

use crate::state::EditContext;

pub fn do_sccp(ctx: &mut EditContext) {
    let (live_cfg_nodes, value_constant_states) = solve_sccp(ctx);
}

struct SolverState {
    worklist: Worklist<Node>,
    live_cfg_nodes: DenseEntitySet<Node>,
    value_constant_states: FxHashMap<DepValue, ConstantState>,
}

fn solve_sccp(ctx: &EditContext) -> (DenseEntitySet<Node>, FxHashMap<DepValue, ConstantState>) {
    let mut state = SolverState {
        worklist: Worklist::from_iter(ctx.compute_live_node_rpo()),
        live_cfg_nodes: DenseEntitySet::new(),
        value_constant_states: FxHashMap::default(),
    };

    state.live_cfg_nodes.insert(ctx.body().entry);

    while let Some(node) = state.worklist.dequeue() {
        let node_kind = ctx.graph().node_kind(node);

        if node_kind.has_control_flow() {
            if !state.live_cfg_nodes.contains(node) {
                // Don't waste any more work (including constant propagation) on control nodes that
                // aren't live.
                continue;
            }

            match node_kind {
                &NodeKind::BrCond => {
                    // For conditional branches: successors are dead if we know which side of the
                    // branch was taken.

                    let cond = ctx.graph().node_inputs(node)[1];
                    let const_state = state
                        .value_constant_states
                        .get(&cond)
                        .unwrap_or(&ConstantState::SomeConstant);

                    let [true_ctrl, false_ctrl] = ctx.graph().node_outputs_exact(node);
                    let true_succ = ctx.graph().value_uses(true_ctrl).next().unwrap().0;
                    let false_succ = ctx.graph().value_uses(false_ctrl).next().unwrap().0;

                    if let &ConstantState::Constant(val) = const_state {
                        if val != 0 {
                            mark_cfg_live(&mut state, ctx.graph(), true_succ);
                        } else {
                            mark_cfg_live(&mut state, ctx.graph(), false_succ);
                        }
                    } else {
                        mark_cfg_live(&mut state, ctx.graph(), true_succ);
                        mark_cfg_live(&mut state, ctx.graph(), false_succ);
                    }
                }
                _ => {
                    // For all other CFG nodes: successors are live if the current node is live.
                    for succ in cfg_succs(ctx.graph(), node) {
                        mark_cfg_live(ctx.graph(), &mut worklist, &mut live_cfg_nodes, succ);
                    }
                }
            }
        }
    }

    (state.live_cfg_nodes, state.value_constant_states)
}

fn mark_cfg_live(state: &mut SolverState, graph: &ValGraph, node: Node) {
    if !state.live_cfg_nodes.contains(node) {
        state.live_cfg_nodes.insert(node);
        state.worklist.enqueue(node);
        for phi in get_attached_phis(graph, node) {
            state.worklist.enqueue(phi);
        }
    }
}

#[derive(Clone, Copy)]
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
