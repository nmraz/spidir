use core::{
    cmp::{self, Ordering},
    ops::ControlFlow,
};

use alloc::vec::Vec;

use cranelift_entity::{
    EntityList, EntityRef, ListPool, PrimaryMap, SecondaryMap,
    packed_option::{PackedOption, ReservedValue},
};
use entity_utils::{define_param_entity, set::DenseEntitySet};

use crate::{
    Graph, PredGraph,
    dfs::{PostOrderContext, VisitTracker, WalkPhase},
};

pub struct SccWalkContext<N: EntityRef> {
    postorder: PostOrderContext<N>,
    node_info: SecondaryMap<N, NodeWalkInfo>,
    next_preorder_num: u32,
    scc_stack: Vec<N>,
    scc_stack_set: DenseEntitySet<N>,
}

impl<N: EntityRef> SccWalkContext<N> {
    pub fn new() -> Self {
        Self {
            postorder: PostOrderContext::new(),
            node_info: SecondaryMap::new(),
            next_preorder_num: 0,
            scc_stack: Vec::new(),
            scc_stack_set: DenseEntitySet::new(),
        }
    }

    pub fn reset(&mut self, roots: impl IntoIterator<Item = N>) {
        self.scc_stack.clear();
        self.scc_stack_set.clear();
        self.node_info.clear();
        self.next_preorder_num = 0;

        self.postorder.reset(roots);
    }

    pub fn next<'s>(
        &mut self,
        graph: impl Graph<Node = N>,
        scratch: &'s mut Vec<N>,
    ) -> Option<&'s [N]> {
        while let Some((phase, node)) = self.postorder.next_event(
            &graph,
            &mut WalkVisitTracker::new(&mut self.node_info, &mut self.next_preorder_num),
        ) {
            match phase {
                WalkPhase::Pre => {
                    self.scc_stack.push(node);
                    self.scc_stack_set.insert(node);
                }
                WalkPhase::Post => {
                    let node_num = self.node_info[node].preorder_num;

                    graph.successors(node, |succ| {
                        let succ_num = self.node_info[succ].preorder_num;
                        match succ_num.cmp(&node_num) {
                            Ordering::Greater => {
                                // This successor is a proper descendent of ours; any lowlink
                                // reachable through it is also reachable through us.
                                self.node_info[node].lowlink = cmp::min(
                                    self.node_info[node].lowlink,
                                    self.node_info[succ].lowlink,
                                );
                            }
                            Ordering::Equal => {
                                // This is a self-loop; no special treatment is necessary.
                            }
                            Ordering::Less => {
                                // This is either a backedge or a cross-edge. If the successor is
                                // still on the stack, it belongs to this node's SCC and must be
                                // taken into account in `lowlink`. Note that we use Tarjan's
                                // original definition of `lowlink`, which allows exactly one
                                // cross- or back-edge.
                                if self.scc_stack_set.contains(succ) {
                                    self.node_info[node].lowlink = cmp::min(
                                        self.node_info[node].lowlink,
                                        self.node_info[succ].preorder_num,
                                    );
                                }
                            }
                        }
                    });

                    // Check whether `node` is an SCC root now that we have its final `lowlink`
                    // value, and collect its SCC if so.
                    if self.node_info[node].lowlink == node_num {
                        scratch.clear();

                        // The newly-completed SCC comprises everything above `node` on the stack,
                        // including itself; pop them all now.
                        loop {
                            let member = self.scc_stack.pop().unwrap();
                            debug_assert!(self.node_info[member].preorder_num >= node_num);
                            self.scc_stack_set.remove(member);
                            scratch.push(member);
                            if member == node {
                                break;
                            }
                        }

                        return Some(&scratch[..]);
                    }
                }
            }
        }

        None
    }
}

impl<N: EntityRef> Default for SccWalkContext<N> {
    fn default() -> Self {
        Self::new()
    }
}

const UNSET_PREORDER_NUM: u32 = 0;

#[derive(Clone, Copy, Default)]
struct NodeWalkInfo {
    preorder_num: u32,
    lowlink: u32,
}

struct WalkVisitTracker<'a, N: EntityRef> {
    node_info: &'a mut SecondaryMap<N, NodeWalkInfo>,
    next_preorder_num: &'a mut u32,
}

impl<'a, N: EntityRef> WalkVisitTracker<'a, N> {
    fn new(
        node_info: &'a mut SecondaryMap<N, NodeWalkInfo>,
        next_preorder_num: &'a mut u32,
    ) -> Self {
        Self {
            node_info,
            next_preorder_num,
        }
    }
}

impl<'a, N: EntityRef> VisitTracker<N> for WalkVisitTracker<'a, N> {
    fn is_visited(&self, node: N) -> bool {
        self.node_info[node].preorder_num != UNSET_PREORDER_NUM
    }

    fn mark_visited(&mut self, node: N) {
        *self.next_preorder_num += 1;
        self.node_info[node] = NodeWalkInfo {
            preorder_num: *self.next_preorder_num,
            lowlink: *self.next_preorder_num,
        };
    }
}

pub struct SccWalk<G>
where
    G: Graph,
    G::Node: EntityRef,
{
    graph: G,
    ctx: SccWalkContext<G::Node>,
}

impl<G> SccWalk<G>
where
    G: Graph,
    G::Node: EntityRef,
{
    pub fn new(graph: G, roots: impl IntoIterator<Item = G::Node>) -> Self {
        let mut ctx = SccWalkContext::new();
        ctx.reset(roots);
        Self { graph, ctx }
    }

    pub fn next<'s>(&mut self, scratch: &'s mut Vec<G::Node>) -> Option<&'s [G::Node]> {
        self.ctx.next(&self.graph, scratch)
    }
}

define_param_entity!(Scc<N>, "scc");

struct SccData<N: EntityRef + ReservedValue> {
    members: EntityList<N>,
    preds: EntityList<Scc<N>>,
    succs: EntityList<Scc<N>>,
}

pub struct Condensation<N: EntityRef + ReservedValue> {
    sccs: PrimaryMap<Scc<N>, SccData<N>>,
    scc_member_pool: ListPool<N>,
    scc_link_pool: ListPool<Scc<N>>,
    node_sccs: SecondaryMap<N, PackedOption<Scc<N>>>,
}

impl<N: EntityRef + ReservedValue> Condensation<N> {
    pub fn compute(graph: impl Graph<Node = N>, roots: impl IntoIterator<Item = N>) -> Self {
        let mut members = Vec::new();
        let mut walk = SccWalk::new(graph, roots);

        let mut condensation = Self {
            sccs: PrimaryMap::new(),
            scc_member_pool: ListPool::new(),
            scc_link_pool: ListPool::new(),
            node_sccs: SecondaryMap::new(),
        };

        while let Some(members) = walk.next(&mut members) {
            let interned_members =
                EntityList::from_slice(members, &mut condensation.scc_member_pool);

            let scc = condensation.sccs.push(SccData {
                members: interned_members,
                preds: EntityList::new(),
                succs: EntityList::new(),
            });

            for &member in members {
                condensation.node_sccs[member] = scc.into();
            }
        }

        let graph = walk.graph;

        for (node, scc) in condensation.node_sccs.iter() {
            let Some(scc) = scc.expand() else {
                continue;
            };

            graph.successors(node, |succ| {
                let succ_scc = condensation.node_sccs[succ].unwrap();
                if succ_scc != scc {
                    condensation.sccs[scc]
                        .succs
                        .push(succ_scc, &mut condensation.scc_link_pool);
                    condensation.sccs[succ_scc]
                        .preds
                        .push(scc, &mut condensation.scc_link_pool);
                }
            });
        }

        for scc_data in condensation.sccs.values_mut() {
            dedup_entity_list(&mut scc_data.preds, &mut condensation.scc_link_pool);
            dedup_entity_list(&mut scc_data.succs, &mut condensation.scc_link_pool);
        }

        condensation
    }

    pub fn scc_postorder(&self) -> impl DoubleEndedIterator<Item = Scc<N>> {
        self.sccs.keys()
    }

    pub fn node_scc(&self, node: N) -> Option<Scc<N>> {
        self.node_sccs[node].expand()
    }

    pub fn scc_members(&self, scc: Scc<N>) -> &[N] {
        self.sccs[scc].members.as_slice(&self.scc_member_pool)
    }

    pub fn scc_preds(&self, scc: Scc<N>) -> &[Scc<N>] {
        self.sccs[scc].preds.as_slice(&self.scc_link_pool)
    }

    pub fn scc_succs(&self, scc: Scc<N>) -> &[Scc<N>] {
        self.sccs[scc].succs.as_slice(&self.scc_link_pool)
    }
}

impl<N: EntityRef + ReservedValue> Graph for Condensation<N> {
    type Node = Scc<N>;

    fn try_successors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        self.scc_succs(node).iter().copied().try_for_each(f)
    }
}

impl<N: EntityRef + ReservedValue> PredGraph for Condensation<N> {
    fn try_predecessors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        self.scc_preds(node).iter().copied().try_for_each(f)
    }
}

fn dedup_entity_list<E: EntityRef + ReservedValue>(
    list: &mut EntityList<E>,
    pool: &mut ListPool<E>,
) {
    let contents = list.as_mut_slice(pool);
    contents.sort_unstable_by_key(|entity| entity.index());
    let new_len = slice_utils::dedup(contents);
    list.truncate(new_len, pool);
}
