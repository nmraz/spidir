use alloc::vec::Vec;
use core::{marker::PhantomData, ops::ControlFlow};

use cranelift_entity::EntityRef;
use entity_utils::{define_param_entity, set::DenseEntitySet};
use graphwalk::{
    Graph, PredGraph,
    dfs::{TreePostOrder, TreePreOrder},
    scc::SccWalk,
};
use smallvec::SmallVec;

use crate::{
    IntoCfg,
    domtree::{DomTree, DomTreeNode},
};

define_param_entity!(PostDomTreeCfgNode<N>, "pdn");

impl<N: EntityRef> PostDomTreeCfgNode<N> {
    fn virtual_exit() -> Self {
        Self::from_u32(0)
    }

    fn from_cfg_node(node: N) -> Self {
        Self::from_u32((node.index() + 1).try_into().unwrap())
    }

    fn to_cfg_node(self) -> N {
        debug_assert!(self.as_u32() != 0);
        N::new((self.as_u32() - 1) as usize)
    }
}

pub type PostDomTreeNode<N> = DomTreeNode<PostDomTreeCfgNode<N>>;

pub struct PostDomTree<N> {
    domtree: DomTree<PostDomTreeCfgNode<N>>,
    _marker: PhantomData<N>,
}

impl<N: EntityRef> PostDomTree<N> {
    pub fn compute(graph: impl IntoCfg<Node = N>, entry: N) -> Self {
        let graph = graph.into_cfg();

        let (mut exits, reachable_nodes) = find_graph_exits(&graph, entry);

        exits.sort_unstable_by_key(|&node| node.index());

        let virtual_graph = PostDomTreeCfg {
            graph: &graph,
            exits: &exits,
            reachable_nodes: &reachable_nodes,
        };

        let domtree = DomTree::compute(virtual_graph, PostDomTreeCfgNode::virtual_exit());

        Self {
            domtree,
            _marker: PhantomData,
        }
    }

    pub fn cfg_ipdom(&self, node: N) -> Option<N> {
        let node = self.get_tree_node(node)?;
        Some(self.get_cfg_node(self.ipdom(node)?))
    }

    pub fn cfg_postdominates(&self, a: N, b: N) -> bool {
        let Some(a) = self.get_tree_node(a) else {
            return false;
        };
        let Some(b) = self.get_tree_node(b) else {
            return false;
        };

        self.postdominates(a, b)
    }

    pub fn cfg_strictly_postdominates(&self, a: N, b: N) -> bool {
        a != b && self.cfg_postdominates(a, b)
    }

    #[inline]
    pub fn get_tree_node(&self, node: N) -> Option<PostDomTreeNode<N>> {
        self.domtree
            .get_tree_node(PostDomTreeCfgNode::from_cfg_node(node))
    }

    #[inline]
    pub fn get_cfg_node(&self, node: PostDomTreeNode<N>) -> N {
        self.domtree.get_cfg_node(node).to_cfg_node()
    }

    #[inline]
    pub fn roots(&self) -> &[PostDomTreeNode<N>] {
        self.domtree.children(self.domtree.root())
    }

    #[inline]
    pub fn ipdom(&self, node: PostDomTreeNode<N>) -> Option<PostDomTreeNode<N>> {
        self.domtree
            .idom(node)
            .filter(|&ipdom| ipdom != self.domtree.root())
    }

    #[inline]
    pub fn children(&self, node: PostDomTreeNode<N>) -> &[PostDomTreeNode<N>] {
        self.domtree.children(node)
    }

    pub fn postdominates(&self, a: PostDomTreeNode<N>, b: PostDomTreeNode<N>) -> bool {
        self.domtree.dominates(a, b)
    }

    pub fn strictly_postdominates(&self, a: PostDomTreeNode<N>, b: PostDomTreeNode<N>) -> bool {
        self.domtree.strictly_dominates(a, b)
    }

    pub fn preorder(&self) -> TreePreOrder<&Self> {
        TreePreOrder::new(self, self.roots().iter().copied())
    }

    pub fn postorder(&self) -> TreePostOrder<&Self> {
        TreePostOrder::new(self, self.roots().iter().copied())
    }
}

impl<N: EntityRef> graphwalk::Graph for PostDomTree<N> {
    type Node = PostDomTreeNode<N>;

    fn try_successors(
        &self,
        node: Self::Node,
        mut f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        for &child in self.children(node) {
            f(child)?;
        }
        ControlFlow::Continue(())
    }
}

fn find_graph_exits<N: EntityRef>(
    graph: &impl Graph<Node = N>,
    entry: N,
) -> (SmallVec<[N; 8]>, DenseEntitySet<N>) {
    let mut exits = SmallVec::new();

    let mut reachable_nodes = DenseEntitySet::new();

    let mut scc_members = Vec::new();
    let mut walk = SccWalk::new(graph, [entry]);

    while let Some(scc_members) = walk.next(&mut scc_members) {
        let scc_has_exits = scc_members.iter().any(|&member| {
            graph
                .try_successors(member, |succ| {
                    // If we find an edge to a node currently marked as reachable, it must not be
                    // part of the same SCC, because we haven't marked this SCC yet. If that is the
                    // case, we know that the current node (and by extension, the current SCC) can
                    // be exited.
                    if reachable_nodes.contains(succ) {
                        ControlFlow::Break(())
                    } else {
                        ControlFlow::Continue(())
                    }
                })
                .is_break()
        });

        for &member in scc_members {
            reachable_nodes.insert(member);
        }

        if !scc_has_exits {
            // This SCC is either an infinite loop or a standalone exit node; pick an arbitrary
            // representative as the true "exit" point.
            exits.push(scc_members[0]);
        }
    }

    (exits, reachable_nodes)
}

struct PostDomTreeCfg<'a, G: PredGraph> {
    graph: &'a G,
    exits: &'a [G::Node],
    reachable_nodes: &'a DenseEntitySet<G::Node>,
}

impl<'a, G> Graph for PostDomTreeCfg<'a, G>
where
    G: PredGraph,
    G::Node: EntityRef,
{
    type Node = PostDomTreeCfgNode<G::Node>;

    fn try_successors(
        &self,
        node: PostDomTreeCfgNode<G::Node>,
        mut f: impl FnMut(PostDomTreeCfgNode<G::Node>) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        // Note: take predecessors here so control edges are reversed.
        if node == PostDomTreeCfgNode::virtual_exit() {
            self.exits
                .iter()
                .try_for_each(|&node| f(PostDomTreeCfgNode::from_cfg_node(node)))
        } else {
            self.graph.try_predecessors(node.to_cfg_node(), |succ| {
                // Make sure not to follow edges from nodes that aren't forward-reachable.
                if self.reachable_nodes.contains(succ) {
                    f(PostDomTreeCfgNode::from_cfg_node(succ))?;
                }
                ControlFlow::Continue(())
            })
        }
    }
}

impl<'a, G> PredGraph for PostDomTreeCfg<'a, G>
where
    G: PredGraph,
    G::Node: EntityRef,
{
    fn try_predecessors(
        &self,
        node: Self::Node,
        mut f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        // Note: take successors here so control edges are reversed.
        let node = node.to_cfg_node();
        self.graph
            .try_successors(node, |pred| f(PostDomTreeCfgNode::from_cfg_node(pred)))?;

        if self
            .exits
            .binary_search_by_key(&node.index(), |exit| exit.index())
            .is_ok()
        {
            f(PostDomTreeCfgNode::virtual_exit())?;
        }

        ControlFlow::Continue(())
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use itertools::Itertools;

    use graphmock::{Graph, graph};

    use super::*;

    fn stringify_exits(g: &Graph) -> String {
        find_graph_exits(&g, g.entry())
            .0
            .iter()
            .map(|&node| g.name(node))
            .format(" ")
            .to_string()
    }

    macro_rules! test_exits {
        ($name:ident, $graph:literal, $expected:expr) => {
            #[test]
            fn $name() {
                let g = graph($graph);
                $expected.assert_eq(&stringify_exits(&g));
            }
        };
    }

    test_exits! {
        straight_line,
        "a -> b
        b -> c",
        expect!["c"]
    }

    test_exits! {
        split,
        "a -> b
        b -> c
        b -> d",
        expect!["d c"]
    }

    test_exits! {
        diamond,
        "a -> b
        a -> c
        b, c -> d",
        expect!["d"]
    }

    test_exits! {
        unreachable_subgraph,
        "entry -> exit
        a -> b, c
        b, c -> exit",
        expect!["exit"]
    }

    test_exits! {
        simple_loop_with_exit,
        "a -> b
        b -> c
        c -> b, e",
        expect!["e"]
    }

    test_exits! {
        multi_backedge_loop_with_exit,
        "entry -> header
        header -> a, b
        a, b -> header
        a -> exit",
        expect!["exit"]
    }

    test_exits! {
        tight_infinite_loop,
        "a -> b
        b -> b",
        expect!["b"]
    }

    test_exits! {
        split_with_infinite_loop,
        "a -> b, c
        b -> b",
        expect!["c b"]
    }

    // Note: the exit here is an arbitrary loop member.
    test_exits! {
        multi_backedge_infinite_loop,
        "entry -> header
        header -> a, b
        a, b -> header",
        expect!["a"]
    }

    test_exits! {
        irreducible_cycle,
        "entry -> c1, c2
        c1 -> c2, exit
        c2 -> c1, exit
        exit -> ret",
        expect!["ret"]
    }

    test_exits! {
        infinite_irreducible_cycle,
        "entry -> c1, c2
        c1 -> c2
        c2 -> c1",
        expect!["c1"]
    }
}
