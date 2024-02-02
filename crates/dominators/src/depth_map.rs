use cranelift_entity::{EntityRef, SecondaryMap};

use crate::{
    domtree::{DomTree, DomTreeNode},
    loops::LoopForest,
};

#[derive(Clone, Copy, Default)]
struct DepthData {
    domtree_depth: u32,
    loop_depth: u32,
}

pub struct DepthMap {
    data: SecondaryMap<DomTreeNode, DepthData>,
}

impl DepthMap {
    pub fn compute<N: EntityRef>(domtree: &DomTree<N>, loop_forest: &LoopForest) -> Self {
        let mut depth_map = Self {
            data: SecondaryMap::new(),
        };

        for domtree_node in domtree.preorder() {
            // We make use of two facts here:
            // 1. Loop headers dominate all nodes in the loop (including subloops).
            // 2. We are walking down the dominator tree in preorder.
            // This means that whenever we are walking through a loop, at least its parent's header will
            // have been traversed and have correct depth information.
            let cur_loop = loop_forest.containing_loop(domtree_node);
            let loop_depth = match cur_loop {
                Some(cur_loop) => {
                    let parent_loop = loop_forest.loop_parent(cur_loop);
                    let parent_depth = parent_loop.map_or(0, |parent_loop| {
                        depth_map.data[loop_forest.loop_header(parent_loop)].loop_depth
                    });
                    parent_depth + 1
                }
                None => 0,
            };

            let domtree_depth = domtree
                .idom(domtree_node)
                .map_or(0, |idom| depth_map.data[idom].domtree_depth + 1);

            depth_map.data[domtree_node] = DepthData {
                domtree_depth,
                loop_depth,
            };
        }

        depth_map
    }

    #[inline]
    pub fn loop_depth(&self, node: DomTreeNode) -> u32 {
        self.data[node].loop_depth
    }

    #[inline]
    pub fn domtree_depth(&self, node: DomTreeNode) -> u32 {
        self.data[node].domtree_depth
    }

    pub fn domtree_lca<N: EntityRef>(
        &self,
        domtree: &DomTree<N>,
        mut a: DomTreeNode,
        mut b: DomTreeNode,
    ) -> DomTreeNode {
        while self.data[a].domtree_depth > self.data[b].domtree_depth {
            // Note: `a` must have an immediate dominator here because its depth is nonzero.
            a = domtree.idom(a).unwrap();
        }

        while self.data[b].domtree_depth > self.data[a].domtree_depth {
            // Note: `b` must have an immediate dominator here because its depth is nonzero.
            b = domtree.idom(b).unwrap();
        }

        debug_assert!(self.data[a].domtree_depth == self.data[b].domtree_depth);
        while a != b {
            a = domtree.idom(a).unwrap();
            b = domtree.idom(b).unwrap();
        }

        a
    }
}
