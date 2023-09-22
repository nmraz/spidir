use core::{
    cmp,
    ops::{Index, IndexMut},
};

use alloc::{vec, vec::Vec};

use cranelift_entity::{
    entity_impl,
    packed_option::{PackedOption, ReservedValue},
    EntityList, EntityRef, ListPool, PrimaryMap, SecondaryMap,
};
use graphwalk::{PredGraphRef, TreePostOrder, TreePreOrder, WalkPhase};
use smallvec::SmallVec;

use crate::IntoCfg;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DomTreeNode(u32);
entity_impl!(DomTreeNode);

type TreeNodeList = EntityList<DomTreeNode>;

struct DomTreeNodeData<N> {
    cfg_node: N,
    idom: PackedOption<DomTreeNode>,
    children: TreeNodeList,
    dfs_entry: u32,
    dfs_exit: u32,
}

pub struct DomTree<N: EntityRef> {
    tree: PrimaryMap<DomTreeNode, DomTreeNodeData<N>>,
    // Note: if valgraph control nodes are too sparse, we might want to consider going back to
    // hashmaps.
    tree_nodes_by_node: SecondaryMap<N, PackedOption<DomTreeNode>>,
    child_pool: ListPool<DomTreeNode>,
}

impl<N: EntityRef> DomTree<N> {
    pub fn compute(graph: impl IntoCfg<Node = N>, entry: N) -> Self {
        // Find dominators as discussed in the original 1979 Lengauer-Tarjan (LT) paper
        // "A Fast Algorithm for Finding Dominators in a Flowgraph".
        // We use the "simple" variant of the algorithm, since the sophisticated version is dramatically
        // more complicated and has been observed to perform worse in practice; this is consistent with
        // most production compilers.

        let graph = graph.into_cfg();

        // Setup: Perform the DFS and initialize per-node data structures used in the remainder of the
        // algorithm.
        let (mut preorder, preorder_by_node) = do_dfs(&graph, entry);

        // Pass 1: Compute semidominators and use Theorem 2 to find either relative or immediate
        // dominators for each node.
        compute_reldoms(&graph, &mut preorder, preorder_by_node);

        // Pass 2: Fill in all immediate dominators by walking down the DFS tree and applying the
        // information recorded in the previous pass.
        compute_domtree_from_reldoms(&mut preorder)
    }

    #[inline]
    pub fn is_cfg_reachable(&self, node: N) -> bool {
        self.get_tree_node(node).is_some()
    }

    pub fn cfg_idom(&self, node: N) -> Option<N> {
        let node = self.get_tree_node(node)?;
        Some(self.get_cfg_node(self.idom(node)?))
    }

    pub fn cfg_dominates(&self, a: N, b: N) -> bool {
        let Some(a) = self.get_tree_node(a) else {
            return false;
        };
        let Some(b) = self.get_tree_node(b) else {
            return false;
        };

        self.dominates(a, b)
    }

    pub fn cfg_strictly_dominates(&self, a: N, b: N) -> bool {
        a != b && self.cfg_dominates(a, b)
    }

    #[inline]
    pub fn get_tree_node(&self, node: N) -> Option<DomTreeNode> {
        self.tree_nodes_by_node[node].expand()
    }

    #[inline]
    pub fn get_cfg_node(&self, node: DomTreeNode) -> N {
        self.tree[node].cfg_node
    }

    #[inline]
    pub fn root(&self) -> DomTreeNode {
        // This is verified below when constructing the tree.
        DomTreeNode::from_u32(0)
    }

    #[inline]
    pub fn idom(&self, node: DomTreeNode) -> Option<DomTreeNode> {
        self.tree[node].idom.expand()
    }

    #[inline]
    pub fn children(&self, node: DomTreeNode) -> &[DomTreeNode] {
        self.tree[node].children.as_slice(&self.child_pool)
    }

    pub fn dominates(&self, a: DomTreeNode, b: DomTreeNode) -> bool {
        let a = &self.tree[a];
        let b = &self.tree[b];
        // Apply the parenthesis theorem to determine whether `a` is an ancestor of `b`.
        a.dfs_entry <= b.dfs_entry && b.dfs_exit <= a.dfs_exit
    }

    pub fn strictly_dominates(&self, a: DomTreeNode, b: DomTreeNode) -> bool {
        a != b && self.dominates(a, b)
    }

    pub fn preorder(&self) -> TreePreOrder<&Self> {
        TreePreOrder::new(self, [self.root()])
    }

    pub fn postorder(&self) -> TreePostOrder<&Self> {
        TreePostOrder::new(self, [self.root()])
    }

    fn compute_dfs_times(&mut self) {
        let root = self.root();
        let mut postorder = TreePostOrder::new(self, [root]);

        let mut timestamp = 0;
        while let Some((phase, node)) = postorder.next_event() {
            let this = postorder.graph_mut();
            match phase {
                WalkPhase::Pre => {
                    this.tree[node].dfs_entry = timestamp;
                }
                WalkPhase::Post => {
                    this.tree[node].dfs_exit = timestamp;
                }
            }
            timestamp += 1;
        }
    }

    fn add_child(&mut self, idom: DomTreeNode, child: DomTreeNode) {
        self.tree[idom].children.push(child, &mut self.child_pool);
    }

    fn insert_node(&mut self, node: N, idom: Option<DomTreeNode>) -> DomTreeNode {
        let tree_node = self.tree.push(DomTreeNodeData {
            cfg_node: node,
            idom: idom.into(),
            children: TreeNodeList::new(),
            dfs_entry: 0,
            dfs_exit: 0,
        });
        self.tree_nodes_by_node[node] = tree_node.into();
        tree_node
    }
}

impl<N: EntityRef> graphwalk::GraphRef for &'_ DomTree<N> {
    type Node = DomTreeNode;

    fn successors(&self, node: Self::Node, mut f: impl FnMut(Self::Node)) {
        for &child in self.children(node) {
            f(child);
        }
    }
}

impl<N: EntityRef> graphwalk::GraphRef for &'_ mut DomTree<N> {
    type Node = DomTreeNode;

    fn successors(&self, node: Self::Node, f: impl FnMut(Self::Node)) {
        (&**self).successors(node, f);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct PreorderNum(u32);

impl PreorderNum {
    fn new(val: u32) -> Self {
        assert!(val < u32::MAX);
        Self(val)
    }

    fn get(self) -> u32 {
        self.0
    }
}

impl ReservedValue for PreorderNum {
    fn reserved_value() -> Self {
        PreorderNum(u32::MAX)
    }

    fn is_reserved_value(&self) -> bool {
        self.0 == u32::MAX
    }
}

struct DfsFrame<N> {
    node: N,
    parent: PackedOption<PreorderNum>,
}

struct NodeInfo<N> {
    /// The node in the graph corresponding to this entry.
    node: N,

    /// The preorder number of the parent of the node in the DFS tree if it has not yet been linked
    /// in the link-eval forest, or some ancestor of the node in the forest otherwise.
    ancestor: PackedOption<PreorderNum>,

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

struct Preorder<N>(Vec<NodeInfo<N>>);

impl<N> Preorder<N> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn next_num(&self) -> PreorderNum {
        PreorderNum::new(self.0.len() as u32)
    }

    fn iter(&self) -> impl DoubleEndedIterator<Item = PreorderNum> + ExactSizeIterator {
        (0..self.next_num().get()).map(PreorderNum::new)
    }
}

impl<N> Index<PreorderNum> for Preorder<N> {
    type Output = NodeInfo<N>;

    fn index(&self, index: PreorderNum) -> &NodeInfo<N> {
        &self.0[index.get() as usize]
    }
}

impl<N> IndexMut<PreorderNum> for Preorder<N> {
    fn index_mut(&mut self, index: PreorderNum) -> &mut Self::Output {
        &mut self.0[index.get() as usize]
    }
}

type PreorderByNode<N> = SecondaryMap<N, PackedOption<PreorderNum>>;

type Bucket = SmallVec<[PreorderNum; 2]>;

fn do_dfs<N: EntityRef>(
    graph: &impl PredGraphRef<Node = N>,
    entry: N,
) -> (Preorder<N>, PreorderByNode<N>) {
    let mut preorder_by_node = PreorderByNode::default();
    let mut preorder = Preorder::new();

    let mut dfs_stack = vec![DfsFrame {
        node: entry,
        parent: None.into(),
    }];

    while let Some(frame) = dfs_stack.pop() {
        let node = frame.node;
        if preorder_by_node[node].is_none() {
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
                dom: PreorderNum::new(0),
            });
            preorder_by_node[node] = num.into();

            graph.successors(node, |succ| {
                // Optimization: avoid placing the node on the stack at all if it has already
                // been visited.
                if preorder_by_node[succ].is_none() {
                    dfs_stack.push(DfsFrame {
                        node: succ,
                        parent: num.into(),
                    });
                }
            });
        }
    }

    (preorder, preorder_by_node)
}

fn compute_reldoms<N: EntityRef>(
    graph: &impl PredGraphRef<Node = N>,
    preorder: &mut Preorder<N>,
    preorder_by_node: PreorderByNode<N>,
) {
    // Maps each node to the nodes it semi-dominates, based on preorder numbering.
    let mut buckets = vec![Bucket::new(); preorder.0.len()];
    let mut eval_ctx = EvalContext::new();

    // Compute semidominators for all nodes other than `entry` by traversing the DFS tree in reverse
    // preorder, and find all nodes whose immediate dominator can be known immediately based
    // on Theorem 2.
    for node in preorder.iter().skip(1).rev() {
        // This is the lowest-numbered node with a parent in the link-eval forest, since every loop
        // iteration ends by linking the current node to its DFS parent.
        // Note that this value is still correct when processing highest-numbered node in the graph;
        // it just means that all nodes in the graph will be considered as tree roots, which is
        // exactly what we want.
        let lowest_linked = PreorderNum::new(node.get() + 1);

        // Try to identify any new nodes `u` for which we can prove `idom(u) = sdom(u)` based on
        // Theorem 2, and record those immediate dominators.
        for &semi_dominee in &buckets[node.get() as usize] {
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
        graph.predecessors(preorder[node].node, |pred| {
            let Some(pred) = preorder_by_node[pred].expand() else {
                // This predecessor isn't reachable in the CFG, so it can (and should) be completely
                // ignored.
                return;
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
        });

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
            buckets[sdom.get() as usize].push(node);
        }

        // Implicit: we now consider `node` to be linked to its parent in the link-eval forest;
        // it will be `lowest_linked` at the next iteration.
    }
}

fn compute_domtree_from_reldoms<N: EntityRef>(preorder: &mut Preorder<N>) -> DomTree<N> {
    let mut domtree = DomTree {
        tree: PrimaryMap::new(),
        tree_nodes_by_node: SecondaryMap::default(),
        child_pool: ListPool::new(),
    };

    let root = domtree.insert_node(preorder[PreorderNum::new(0)].node, None);
    assert!(root == domtree.root());

    // Fill in immediate dominators for all nodes based on their `dom`, by traversing the DFS tree
    // in preorder.
    for node in preorder.iter().skip(1) {
        // If the recorded dominator is not the same as the semidominator, it is actually a relative
        // dominator and not the immediate dominator. In that case, grab the immediate dominator of
        // the relative dominator, using the fact that it will have already been computed in a
        // previous iteration.
        if preorder[node].dom != preorder[node].sdom {
            preorder[node].dom = preorder[preorder[node].dom].dom;
        }

        // The idom must have been visited earlier as we are in preorder.
        let idom = domtree
            .get_tree_node(preorder[preorder[node].dom].node)
            .expect("idom should already have been visited");
        let node = preorder[node].node;

        let node = domtree.insert_node(node, Some(idom));
        domtree.add_child(idom, node);
    }

    domtree.compute_dfs_times();
    domtree
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

    fn eval<N>(
        &mut self,
        preorder: &mut Preorder<N>,
        lowest_linked: PreorderNum,
        node: PreorderNum,
    ) -> PreorderNum {
        debug_assert!(
            lowest_linked.get() > 0,
            "root node should never be linked in link-eval forest"
        );

        let Some(mut ancestor) = preorder[node].ancestor.expand() else {
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
            preorder[node].ancestor = root.into();
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
