use alloc::{vec, vec::Vec};
use core::cmp::{self, Ordering};
use hashbrown::hash_map::Entry;
use smallvec::SmallVec;

use fx_utils::FxHashMap;

use crate::{
    node::DepValueKind,
    valgraph::{Node, ValGraph},
};

type CfgMap<T> = FxHashMap<Node, T>;

pub struct DomTree {
    idoms: CfgMap<Node>,
}

impl DomTree {
    pub fn idom(&self, node: Node) -> Option<Node> {
        self.idoms.get(&node).copied()
    }

    pub fn compare(&self, _a: Node, _b: Node) -> Option<Ordering> {
        todo!()
    }

    pub fn dominates(&self, a: Node, b: Node) -> bool {
        matches!(self.compare(a, b), Some(Ordering::Less | Ordering::Equal))
    }

    pub fn strictly_dominates(&self, a: Node, b: Node) -> bool {
        self.compare(a, b) == Some(Ordering::Less)
    }
}

struct DfsFrame {
    node: Node,
    parent: Option<Node>,
}

enum IdomState {
    /// The immediate dominator is either completely unknown or has already been computed; no more
    /// work is necessary.
    Done,
    /// The immediate dominator has not been computed, but it is known to be equal to the immediate
    /// dominator of a different, earlier node.
    SameAs(Node),
}

struct NodeInfo {
    /// The numbering of the node in a preorder DFS traversal of the CFG, starting at 0 for the
    /// entry.
    _preorder_num: u32,
    /// The parent of the node in the DFS tree if it has not yet been linked in the link-eval
    /// forest, or some ancestor of the node in the forest otherwise.
    ancestor: Option<Node>,
    /// The preorder number of the semidominator of this node if it has been computed, or that of
    /// the node itself otherwise.
    sdom: u32,
    /// The computation state of the immediate dominator of this node.
    idom_state: IdomState,
}

type Buckets = SmallVec<[Node; 8]>;

pub fn compute(graph: &ValGraph, entry: Node) -> DomTree {
    let mut preorder = Vec::new();

    let mut idoms = CfgMap::default();

    // Maps each node to the nodes it semi-dominates.
    let mut buckets = CfgMap::<Buckets>::default();
    let mut node_info = CfgMap::default();
    let mut dfs_stack = vec![DfsFrame {
        node: entry,
        parent: None,
    }];

    while let Some(frame) = dfs_stack.pop() {
        let node = frame.node;
        match node_info.entry(node) {
            Entry::Vacant(entry) => {
                let preorder_num = preorder.len() as u32;
                preorder.push(node);
                entry.insert(NodeInfo {
                    _preorder_num: preorder_num,
                    // The ancestor starts as a plain DFS parent.
                    ancestor: frame.parent,
                    // We haven't yet computed the semidominator of `node`, so it should be set to
                    // `node` itself.
                    sdom: preorder_num,
                    // Somewhat counter-intuitively, we start by marking all immediate dominators as
                    // "done". Later on, if we notice that the immediate dominator is not equal to
                    // the semidominator, we mark the immediate dominator as needing further
                    // computation.
                    idom_state: IdomState::Done,
                });
                for succ in cfg_succs(graph, node) {
                    // Optimization: avoid placing the node on the stack at all if it has already
                    // been visited.
                    if !node_info.contains_key(&succ) {
                        dfs_stack.push(DfsFrame {
                            node: succ,
                            parent: Some(node),
                        });
                    }
                }
            }
            Entry::Occupied(_) => {
                // Already discovered
            }
        }
    }

    // Pass 1: Compute semidominators for all nodes other than `entry` by traversing the DFS tree in
    // reverse preorder, and find all nodes whose immediate dominator can be known immediately based
    // on Theorem 2.
    for i in (1..preorder.len() as u32).rev() {
        let node = preorder[i as usize];

        // Try to identify any new nodes `u` for which we can prove `idom(u) = sdom(u)` based on
        // Theorem 2, and record those immediate dominators.
        if let Some(bucket) = buckets.get(&node) {
            for &semi_dominee in bucket {
                let ancestor = eval(graph, &mut node_info, i + 1, semi_dominee);

                // Note: we are guaranteed that `sdom(semi_dominee) < ancestor <= semi_dominee` in
                // the preorder, but `sdom(semi_dominee) == node`, so the semidominator of ancestor
                // must have already been computed and
                // `node_info[&ancestor].sdom == sdom(ancestor)`.
                let ancestor_sdom = node_info[&ancestor].sdom;

                // Apply Corollary 1
                if ancestor_sdom == i {
                    idoms.insert(semi_dominee, node);
                } else {
                    // We don't know the immediate dominator yet, but we do know that
                    // `idom(semi_dominee) == idom(ancestor)` because `ancestor` has minimal
                    // `sdom(ancestor)` among all nodes on the tree path from `parent` to
                    // `semi_dominee`.
                    node_info.get_mut(&semi_dominee).unwrap().idom_state =
                        IdomState::SameAs(ancestor);
                };
            }
        }

        // Compute semidominators of `node` based on CFG predecessors, using the equivalent
        // defintion of semidominators proven in Theorem 4 of the LT paper.
        // This only works because we are traversing in reverse preorder, which means that the
        // semidominators for any nodes with a higher preorder number are already correct.
        for pred in cfg_preds(graph, node) {
            if !node_info.contains_key(&pred) {
                // This predecessor isn't reachable in the CFG, so it can (and should) be completely
                // ignored.
                continue;
            }

            let pred_ancestor = eval(graph, &mut node_info, i + 1, pred);
            let pred_ancestor_sdom = node_info[&pred_ancestor].sdom;

            // Apply Theorem 4.
            // Note that we're somewhat cheekily relying on
            let info = node_info.get_mut(&node).unwrap();
            info.sdom = cmp::min(info.sdom, pred_ancestor_sdom);
        }

        let sdom = node_info[&node].sdom;

        // Note: the `ancestor` is still guaranteed to be the parent at this point because the
        // lowest node linked is `i + 1`.
        let parent = node_info[&node].ancestor.expect("node should not be root");

        if preorder[sdom as usize] == parent {
            // Optimization: if the semidominator is the parent, it must be the immediate dominator
            // as a rather trivial corollary of Theorem 2. This case is very common in our
            // sea-of-nodes representation because "ordinary" basic blocks are actually long chains
            // of nodes that all have a single control input/output.
            // Avoid the ceremony and just say we've found the immediate dominator now.
            idoms.insert(node, parent);
        } else {
            // Place `node` in the bucket of its semidominator for processing later.
            buckets
                .entry(preorder[sdom as usize])
                .or_default()
                .push(node);
        }
    }

    // Pass 2: Fill in immediate dominators for nodes marked as `SameAs` by traversing the DFS tree
    // in preorder.
    for &node in preorder.iter().skip(1) {
        if let IdomState::SameAs(prev) = node_info[&node].idom_state {
            let idom = *idoms
                .get(&prev)
                .expect("immediate dominator should already have been computed for previous node");
            idoms.insert(node, idom);
        }
    }

    DomTree { idoms }
}

fn eval(
    _graph: &ValGraph,
    _node_info: &mut CfgMap<NodeInfo>,
    _lowest_linked: u32,
    _node: Node,
) -> Node {
    todo!()
}

fn cfg_succs(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .filter(|&output| graph.value_kind(output) == DepValueKind::Control)
        .flat_map(|output| graph.value_uses(output))
        .map(|(succ_node, _succ_input_idx)| succ_node)
}

fn cfg_preds(graph: &ValGraph, node: Node) -> impl Iterator<Item = Node> + '_ {
    graph
        .node_inputs(node)
        .into_iter()
        .filter(|&input| graph.value_kind(input) == DepValueKind::Control)
        .map(|input| graph.value_def(input).0)
}
