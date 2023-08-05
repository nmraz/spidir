use alloc::{vec, vec::Vec};
use core::{
    cmp::{self, Ordering},
    ops::{Index, IndexMut},
};
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

type PreorderNum = u32;

struct DfsFrame {
    node: Node,
    parent: Option<PreorderNum>,
}

struct NodeInfo {
    /// The node in the graph corresponding to this entry.
    node: Node,

    /// The preorder number of the parent of the node in the DFS tree if it has not yet been linked
    /// in the link-eval forest, or some ancestor of the node in the forest otherwise.
    ancestor: Option<PreorderNum>,

    /// If this node is a tree root in the link-eval forest, points to the node itself.
    /// Otherwise, contains a node `w` with minimal `info.sdom` such that
    ///
    /// r -+-> w -*-> node
    ///
    /// in the link-eval forest, where `r` is the root of the tree containing `node`.
    ///
    /// This value is updated lazily whenever it is requested, if new ancestors have
    /// been added to the tree since the last time it was queried.
    ///
    /// If `ancestor` is a tree root in the link-eval forest, this value is guaranteed to be
    /// correct; otherwise, it is a node with minimal semidominator along the path to `ancestor`.
    label: PreorderNum,

    /// The semidominator of this node if it has been computed, or the node itself otherwise.
    sdom: PreorderNum,

    /// Once this node's semidominator has been visited:
    /// * If the immediate dominator is the semidominator itself, holds the immediate dominator.
    /// * Otherwise, holds the relative dominator (a node with the same immediate dominator as this
    ///   one).
    dom: PreorderNum,
}

struct Preorder(Vec<NodeInfo>);

impl Preorder {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn next_num(&self) -> u32 {
        self.0.len() as u32
    }
}

impl Index<PreorderNum> for Preorder {
    type Output = NodeInfo;

    fn index(&self, index: PreorderNum) -> &NodeInfo {
        &self.0[index as usize]
    }
}

impl IndexMut<PreorderNum> for Preorder {
    fn index_mut(&mut self, index: PreorderNum) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

type Bucket = SmallVec<[PreorderNum; 2]>;

pub fn compute(graph: &ValGraph, entry: Node) -> DomTree {
    // Setup: Perform the DFS and initialize per-node data structures used in the remainder of the
    // algorithm.
    let (mut preorder, preorder_by_node) = do_dfs(graph, entry);

    // Pass 1: Compute semidominators and use Theorem 2 to find either relative or immediate
    // dominators for each node.
    compute_reldoms(graph, &mut preorder, preorder_by_node);

    // Pass 2: Fill in all immediate dominators by walking down the DFS tree and applying the
    // information recorded in the previous pass.
    let idoms = compute_idoms_from_reldoms(&mut preorder);

    DomTree { idoms }
}

fn do_dfs(graph: &ValGraph, entry: Node) -> (Preorder, CfgMap<PreorderNum>) {
    let mut preorder_by_node = CfgMap::default();
    let mut preorder = Preorder::new();

    let mut dfs_stack = vec![DfsFrame {
        node: entry,
        parent: None,
    }];

    while let Some(frame) = dfs_stack.pop() {
        let node = frame.node;
        match preorder_by_node.entry(node) {
            Entry::Vacant(entry) => {
                let num = preorder.next_num();
                preorder.0.push(NodeInfo {
                    node,
                    // The ancestor starts as a plain DFS parent.
                    ancestor: frame.parent,
                    // The link-eval forest is currently a bunch of single-node trees, so every node
                    // is its own label as there are no other nodes on the path.
                    label: num,
                    // We haven't yet computed the semidominator of `node`, so it should be set to
                    // `node` itself.
                    sdom: num,
                    // This will be filled in later once the node's semidominator is visited.
                    dom: 0,
                });
                entry.insert(num);

                for succ in cfg_succs(graph, node) {
                    // Optimization: avoid placing the node on the stack at all if it has already
                    // been visited.
                    if !preorder_by_node.contains_key(&succ) {
                        dfs_stack.push(DfsFrame {
                            node: succ,
                            parent: Some(num),
                        });
                    }
                }
            }
            Entry::Occupied(_) => {
                // Already discovered
            }
        }
    }

    (preorder, preorder_by_node)
}

fn compute_reldoms(
    graph: &ValGraph,
    preorder: &mut Preorder,
    preorder_by_node: CfgMap<PreorderNum>,
) {
    // Maps each node to the nodes it semi-dominates, based on preorder numbering.
    let mut buckets = vec![Bucket::new(); preorder.0.len()];
    let mut eval_ctx = EvalContext::new();

    // Compute semidominators for all nodes other than `entry` by traversing the DFS tree in reverse
    // preorder, and find all nodes whose immediate dominator can be known immediately based
    // on Theorem 2.
    for node in (1..preorder.next_num()).rev() {
        // This is the lowest-numbered node with a parent in the link-eval forest, since every loop
        // iteration ends by linking the current node to its DFS parent.
        // Note that this value is still correct when processing highest-numbered node in the graph;
        // it just means that all nodes in the graph will be considered as tree roots, which is
        // exactly what we want.
        let lowest_linked = node + 1;

        // Try to identify any new nodes `u` for which we can prove `idom(u) = sdom(u)` based on
        // Theorem 2, and record those immediate dominators.
        for &semi_dominee in &buckets[node as usize] {
            let reldom = eval_ctx.eval(preorder, lowest_linked, semi_dominee);

            // Note: we are guaranteed that
            // `sdom(semi_dominee) == node < lowest_linked <= reldom <= semi_dominee`
            // in the preorder, so the semidominator of reldom must have already been computed and
            // `preorder[reldom].sdom == sdom(reldom)`.
            // This means that `reldom` really has minimal `sdom(reldom)` on the DFS tree path
            // `node -+-> reldom -*-> semi_dominee`.
            let reldom_sdom = preorder[reldom].sdom;

            // Apply Corollary 1
            preorder[semi_dominee].dom = if reldom_sdom == node {
                // The semidominator is the immediate dominator in this case.
                node
            } else {
                // We don't know the immediate dominator yet, but we do know that
                // `idom(semi_dominee) == idom(reldom)` because `reldom` has minimal `sdom(reldom)`
                // among all nodes on the tree path from `parent` to `semi_dominee`.
                reldom
            };
        }

        // Compute semidominators of `node` based on CFG predecessors, using the equivalent
        // defintion of semidominators proven in Theorem 4 of the LT paper.
        //
        // This only works because we are traversing in reverse preorder, which means that the
        // semidominators for any nodes with a higher preorder number are already correct.
        for pred in cfg_preds(graph, preorder[node].node) {
            let Some(&pred) = preorder_by_node.get(&pred) else {
                // This predecessor isn't reachable in the CFG, so it can (and should) be completely
                // ignored.
                continue;
            };

            // Apply Theorem 4.
            //
            // Note that we're somewhat cheekily relying on the fact that `eval` and the `sdom`
            // field can mean one of several different things depending on the relation depending on
            // the relation between `node` and `pred`:
            //
            // 1. If `pred < node`, `pred` has not yet been linked and so `eval(pred) == pred`.
            //    We also have `info[pred].sdom == pred` as it has not yet been computed, so
            //    `pred_ancestor_sdom` ends up just being a roundabout way of naming `pred`.
            //
            // 2. If `pred == node`, `eval(pred) == pred` and the computation has no effect.
            //
            // 3. If `pred > node`, `eval(pred)` is an ancestor of `pred` in the link-eval forest
            //    (and thus in the DFS tree). Since all ancestors of `pred` in the forest must be
            //    greater than `node` at this point (nothing less than or equal to `node` has been
            //    linked), `info[eval(pred)].sdom` will be its true semidominator.
            //
            // Cases (1) and (3) exactly cover the disjoint union presented in Theorem 4.
            // (3) covers all cases in the second set because every node greater than `node` is
            // already linked in the link-eval forest.

            let pred_ancestor = eval_ctx.eval(preorder, lowest_linked, pred);
            let pred_ancestor_sdom = preorder[pred_ancestor].sdom;

            let info = &mut preorder[node];
            info.sdom = cmp::min(info.sdom, pred_ancestor_sdom);
        }

        let sdom = preorder[node].sdom;

        // Note: the `ancestor` is still guaranteed to be the parent at this point because the
        // lowest node linked is `node + 1`.
        let parent = preorder[node].ancestor.expect("node should not be root");

        if sdom == parent {
            // Optimization: if the semidominator is the parent, it must be the immediate dominator
            // as a rather trivial corollary of Theorem 2. This case is very common in our
            // sea-of-nodes representation because "ordinary" basic blocks are actually long chains
            // of nodes that all have a single control input/output.
            // Avoid the ceremony and just say we've found the immediate dominator now.
            preorder[node].dom = parent;
        } else {
            // Place `node` in the bucket of its semidominator for processing later.
            buckets[sdom as usize].push(node);
        }

        // Implicit: we now consider `node` to be linked to its parent in the link-eval forest;
        // it will be `lowest_linked` at the next iteration.
    }
}

fn compute_idoms_from_reldoms(preorder: &mut Preorder) -> CfgMap<Node> {
    // Maps each `ValGraph` node to its immediate dominator.
    let mut idoms = CfgMap::default();

    // Fill in immediate dominators for all nodes based on their `dom`, by traversing the DFS tree
    // in preorder.
    for node in 1..preorder.next_num() {
        // If the recorded dominator is not the same as the semidominator, it is actually a relative
        // dominator and not the immediate dominator. In that case, grab the immediate dominator of
        // the relative dominator, using the fact that it will have already been computed in a
        // previous iteration.
        if preorder[node].dom != preorder[node].sdom {
            preorder[node].dom = preorder[preorder[node].dom].dom;
        }
        idoms.insert(preorder[node].node, preorder[preorder[node].dom].node);
    }

    idoms
}

struct EvalContext {
    // Keep a container ready for whenever we need to compute paths to save allocations.
    path: SmallVec<[PreorderNum; 8]>,
}

impl EvalContext {
    fn new() -> Self {
        Self {
            path: SmallVec::new(),
        }
    }

    fn eval(
        &mut self,
        preorder: &mut Preorder,
        lowest_linked: PreorderNum,
        node: PreorderNum,
    ) -> PreorderNum {
        debug_assert!(
            lowest_linked > 0,
            "root node should never be linked in link-eval forest"
        );

        let Some(mut ancestor) = preorder[node].ancestor else {
            // The DFS root is never linked, so its `eval` always ends up returning itself.
            return node;
        };

        if ancestor < lowest_linked {
            // `ancestor` is already the root of a tree, so either we are a root as well (and `ancestor`
            // is still just our DFS parent) or the path is already as compressed as can be. In either
            // case, return the label we already have.
            return preorder[node].label;
        }

        // Compress the path from `node` to its tree root to exactly one edge and update labels along
        // the way.

        self.path.clear();

        // Walk up to the root of the containing tree, gathering all nodes whose ancestor is still a
        // strict descendant of the root. These nodes will have their ancestors compressed directly to
        // the root.
        let mut cur_node = node;
        while ancestor >= lowest_linked {
            self.path.push(cur_node);
            cur_node = ancestor;
            // Note: the ancestor should always have a well-defined ancestor, as `lowest_linked` is
            // always nonzero.
            ancestor = preorder[ancestor]
                .ancestor
                .expect("linked ancestor cannot be entry node");
        }

        // At this point, `cur_node` is a direct descendant of the tree root containing `node`, and
        // `path` contains all nodes *after* `cur_node` on the path to `node`.
        // We now want to walk down the path to `node` and update children based on information from
        // their parents.
        let root = ancestor;
        let mut parent = cur_node;

        for &node in self.path.iter().rev() {
            // Compress all paths directly to the root.
            preorder[node].ancestor = Some(root);
            // We know that `preorder[node].label` was the correct minimum at least along the path
            // from `node` to `parent`, so to get it all the way to `root` just combine it with the
            // already-correct value of the parent.
            preorder[node].label = cmp::min(preorder[node].label, preorder[parent].label);
            parent = node;
        }

        // The label is valid now.
        preorder[node].label
    }
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

#[cfg(test)]
mod tests;
