use core::hash::{Hash, Hasher};

use fx_utils::FxHasher;
use hashbrown::raw::RawTable;
use smallvec::SmallVec;

use crate::{
    builder::Builder,
    function::FunctionBody,
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
};

#[derive(Default, Clone)]
pub struct NodeCache(RawTable<Node>);

impl NodeCache {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

pub struct CachingBuilder<'a> {
    body: &'a mut FunctionBody,
    cache: &'a mut NodeCache,
}

impl<'a> CachingBuilder<'a> {
    #[inline]
    pub fn new(body: &'a mut FunctionBody, cache: &'a mut NodeCache) -> Self {
        Self { body, cache }
    }
}

impl<'a> Builder for CachingBuilder<'a> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let graph = &mut self.body.graph;

        if !should_cache_node_kind(&kind) {
            return graph.create_node(kind, inputs, output_kinds);
        }

        let inputs: SmallVec<[_; 4]> = inputs.into_iter().collect();
        let output_kinds: SmallVec<[_; 4]> = output_kinds.into_iter().collect();

        let hash = hash_node(&kind, inputs.iter().copied(), output_kinds.iter().copied());
        let entry = self.cache.0.get(hash, |&node| {
            existing_node_equal(graph, node, &kind, &inputs, &output_kinds)
        });

        match entry {
            Some(node) => *node,
            None => {
                let node = graph.create_node(kind, inputs, output_kinds);
                self.cache.0.insert(hash, node, |&existing_node| {
                    hash_existing_node(graph, existing_node)
                });
                node
            }
        }
    }

    fn body(&self) -> &FunctionBody {
        self.body
    }

    fn body_mut(&mut self) -> &mut FunctionBody {
        self.body
    }
}

fn existing_node_equal(
    graph: &ValGraph,
    node: Node,
    new_kind: &NodeKind,
    new_inputs: &[DepValue],
    new_output_kinds: &[DepValueKind],
) -> bool {
    new_kind == graph.node_kind(node)
        && new_inputs.iter().copied().eq(graph.node_inputs(node))
        && new_output_kinds
            .iter()
            .copied()
            .eq(node_output_kinds(graph, node))
}

fn hash_existing_node(graph: &ValGraph, node: Node) -> u64 {
    hash_node(
        graph.node_kind(node),
        graph.node_inputs(node).into_iter(),
        node_output_kinds(graph, node),
    )
}

fn node_output_kinds(
    graph: &ValGraph,
    node: Node,
) -> impl ExactSizeIterator<Item = DepValueKind> + '_ {
    graph
        .node_outputs(node)
        .into_iter()
        .map(|value| graph.value_kind(value))
}

fn hash_node(
    kind: &NodeKind,
    inputs: impl ExactSizeIterator<Item = DepValue>,
    output_kinds: impl ExactSizeIterator<Item = DepValueKind>,
) -> u64 {
    let mut state = FxHasher::default();
    kind.hash(&mut state);
    state.write_usize(inputs.len());
    for input in inputs {
        input.hash(&mut state);
    }
    state.write_usize(output_kinds.len());
    for output in output_kinds {
        output.hash(&mut state);
    }
    state.finish()
}

fn should_cache_node_kind(kind: &NodeKind) -> bool {
    // Never hash-cons nodes that actually need to be distinct (such as stack slots).
    !has_build_identity(kind) &&
    // Also avoid storing nodes that are control-dependent, since it will never be possible to
    // combine them anyway. This is an optimization and is not necessary for correctness.
    !kind.has_control_flow()
}

fn has_build_identity(kind: &NodeKind) -> bool {
    // Regions and phi nodes need to be treated carefully, since they may appear identical during
    // construction (for example, when they are created with no inputs), but new inputs may be added
    // later that change their identity.
    kind.has_identity() || matches!(kind, NodeKind::Region | NodeKind::Phi)
}

#[cfg(test)]
mod tests {
    use crate::{builder::BuilderExt, node::Type};

    use super::*;

    fn check(f: impl FnOnce(&mut CachingBuilder<'_>)) {
        let mut body = FunctionBody::new_invalid();
        let mut cache = NodeCache::new();
        f(&mut CachingBuilder::new(&mut body, &mut cache));
    }

    #[test]
    fn combine_iconst() {
        check(|builder| {
            let five1 = builder.build_iconst(Type::I32, 5);
            let five2 = builder.build_iconst(Type::I32, 5);
            assert_eq!(five1, five2);
        })
    }

    #[test]
    fn dont_combine_different_iconsts() {
        check(|builder| {
            let five = builder.build_iconst(Type::I32, 5);
            let three = builder.build_iconst(Type::I32, 3);
            assert_ne!(five, three);
        })
    }

    #[test]
    fn dont_combine_different_iconst_types() {
        check(|builder| {
            let five1 = builder.build_iconst(Type::I32, 5);
            let five2 = builder.build_iconst(Type::I64, 5);
            assert_ne!(five1, five2);
        })
    }

    #[test]
    fn combine_iconst_with_large_cache() {
        check(|builder| {
            let values: Vec<_> = (0..10000)
                .map(|i| builder.build_iconst(Type::I32, i))
                .collect();

            for (i, &value) in values.iter().enumerate() {
                let value2 = builder.build_iconst(Type::I32, i as u64);
                assert_eq!(value, value2);
            }
        })
    }

    #[test]
    fn dont_combine_regions() {
        check(|builder| {
            let region1 = builder.build_region(&[]).ctrl;
            let region2 = builder.build_region(&[]).ctrl;

            assert_ne!(region1, region2);
        })
    }

    #[test]
    fn dont_combine_phi() {
        check(|builder| {
            let region = builder.build_region(&[]);
            let phi1 = builder.build_phi(Type::I32, region.phisel, &[]).output;
            let phi2 = builder.build_phi(Type::I32, region.phisel, &[]).output;
            assert_ne!(phi1, phi2);
        })
    }
}
