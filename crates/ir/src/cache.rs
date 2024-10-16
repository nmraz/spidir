use core::hash::{Hash, Hasher};

use cranelift_entity::SecondaryMap;
use fx_utils::FxHasher;
use hashbrown::HashTable;
use smallvec::SmallVec;

use crate::{
    builder::Builder,
    function::FunctionBody,
    node::{DepValueKind, NodeKind},
    valgraph::{DepValue, Node, ValGraph},
};

pub struct VacantEntry<'a> {
    cache: &'a mut NodeCache,
    hash: u32,
}

impl<'a> VacantEntry<'a> {
    pub fn insert(self, node: Node) {
        debug_assert!(self.cache.node_hashes[node] == HASH_NONE);
        self.cache.insert(node, self.hash);
    }
}

pub enum Entry<'a> {
    Occupied(Node),
    Vacant(VacantEntry<'a>),
}

#[derive(Clone)]
pub struct NodeCache {
    table: HashTable<Node>,
    node_hashes: SecondaryMap<Node, u32>,
}

impl NodeCache {
    pub fn new() -> Self {
        Self {
            table: HashTable::default(),
            node_hashes: SecondaryMap::with_default(HASH_NONE),
        }
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.node_hashes.clear();
    }

    pub fn contains_node(&self, node: Node) -> bool {
        self.node_hashes[node] != HASH_NONE
    }

    pub fn remove(&mut self, node: Node) {
        let old_hash = self.node_hashes[node];

        if old_hash != HASH_NONE {
            self.table
                .find_entry(expand_hash(old_hash), |&table_node| table_node == node)
                .unwrap()
                .remove();
        }
    }

    pub fn entry(
        &mut self,
        graph: &ValGraph,
        kind: &NodeKind,
        inputs: impl ExactSizeIterator<Item = DepValue> + Clone,
        output_kinds: impl ExactSizeIterator<Item = DepValueKind> + Clone,
    ) -> Option<Entry<'_>> {
        if !kind.is_cacheable() {
            return None;
        }

        let (hash, found) = self.find_raw(graph, kind, inputs, output_kinds);
        let entry = match found {
            Some(node) => Entry::Occupied(node),
            None => Entry::Vacant(VacantEntry { cache: self, hash }),
        };
        Some(entry)
    }

    pub fn find(
        &self,
        graph: &ValGraph,
        kind: &NodeKind,
        inputs: impl ExactSizeIterator<Item = DepValue> + Clone,
        output_kinds: impl ExactSizeIterator<Item = DepValueKind> + Clone,
    ) -> Option<Node> {
        if !kind.is_cacheable() {
            return None;
        }

        self.find_raw(graph, kind, inputs, output_kinds).1
    }

    fn insert(&mut self, node: Node, hash: u32) {
        self.node_hashes[node] = hash;
        self.table
            .insert_unique(expand_hash(hash), node, |&existing_node| {
                expand_hash(self.node_hashes[existing_node])
            });
    }

    fn find_raw(
        &self,
        graph: &ValGraph,
        kind: &NodeKind,
        inputs: impl ExactSizeIterator<Item = DepValue> + Clone,
        output_kinds: impl ExactSizeIterator<Item = DepValueKind> + Clone,
    ) -> (u32, Option<Node>) {
        let hash = hash_node(kind, inputs.clone(), output_kinds.clone());
        let found = self
            .table
            .find(expand_hash(hash), |&node| {
                existing_node_equal(graph, node, kind, inputs.clone(), output_kinds.clone())
            })
            .copied();
        (hash, found)
    }
}

impl Default for NodeCache {
    fn default() -> Self {
        Self::new()
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

        if !should_cache_built_node(&kind) {
            return graph.create_node(kind, inputs, output_kinds);
        }

        let inputs: SmallVec<[_; 4]> = inputs.into_iter().collect();
        let output_kinds: SmallVec<[_; 4]> = output_kinds.into_iter().collect();

        match self
            .cache
            .entry(
                graph,
                &kind,
                inputs.iter().copied(),
                output_kinds.iter().copied(),
            )
            .unwrap()
        {
            Entry::Occupied(node) => node,
            Entry::Vacant(entry) => {
                let node = graph.create_node(kind, inputs, output_kinds);
                entry.insert(node);
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
    new_inputs: impl ExactSizeIterator<Item = DepValue>,
    new_output_kinds: impl ExactSizeIterator<Item = DepValueKind>,
) -> bool {
    new_kind == graph.node_kind(node)
        && new_inputs.eq(graph.node_inputs(node))
        && new_output_kinds.eq(node_output_kinds(graph, node))
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
) -> u32 {
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
    let hash = state.finish() as u32;

    // Make sure we have a free sentinel value.
    if hash != HASH_NONE {
        hash
    } else {
        0
    }
}

fn should_cache_built_node(kind: &NodeKind) -> bool {
    kind.is_cacheable() && !has_build_identity(kind)
}

fn has_build_identity(kind: &NodeKind) -> bool {
    // Regions and phi nodes need to be treated carefully, since they may appear identical during
    // construction (for example, when they are created with no inputs), but new inputs may be added
    // later that change their identity.
    matches!(kind, NodeKind::Region | NodeKind::Phi)
}

const HASH_NONE: u32 = 0xffffffff;

fn expand_hash(hash: u32) -> u64 {
    const GOLDEN_RATIO_64: u64 = 0x61C8864680B583EB;
    (hash as u64).wrapping_mul(GOLDEN_RATIO_64)
}

#[cfg(test)]
mod tests {
    use crate::{
        builder::{BuilderExt, SimpleBuilder},
        node::Type,
    };

    use super::*;

    fn check_builder(f: impl FnOnce(&mut CachingBuilder<'_>)) {
        let mut body = FunctionBody::new_invalid();
        let mut cache = NodeCache::new();
        f(&mut CachingBuilder::new(&mut body, &mut cache));
    }

    #[test]
    fn combine_iconst() {
        check_builder(|builder| {
            let five1 = builder.build_iconst(Type::I32, 5);
            let five2 = builder.build_iconst(Type::I32, 5);
            assert_eq!(five1, five2);
        })
    }

    #[test]
    fn dont_combine_different_iconsts() {
        check_builder(|builder| {
            let five = builder.build_iconst(Type::I32, 5);
            let three = builder.build_iconst(Type::I32, 3);
            assert_ne!(five, three);
        })
    }

    #[test]
    fn dont_combine_different_iconst_types() {
        check_builder(|builder| {
            let five1 = builder.build_iconst(Type::I32, 5);
            let five2 = builder.build_iconst(Type::I64, 5);
            assert_ne!(five1, five2);
        })
    }

    #[test]
    fn combine_iconst_with_large_cache() {
        check_builder(|builder| {
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
        check_builder(|builder| {
            let region1 = builder.build_region(&[]).ctrl;
            let region2 = builder.build_region(&[]).ctrl;

            assert_ne!(region1, region2);
        })
    }

    #[test]
    fn dont_combine_phi() {
        check_builder(|builder| {
            let region = builder.build_region(&[]);
            let phi1 = builder.build_phi(Type::I32, region.phisel, &[]).output;
            let phi2 = builder.build_phi(Type::I32, region.phisel, &[]).output;
            assert_ne!(phi1, phi2);
        })
    }

    #[test]
    fn contains_node() {
        let mut cache = NodeCache::new();
        let mut body = FunctionBody::new_invalid();
        let five = CachingBuilder::new(&mut body, &mut cache).build_iconst(Type::I32, 5);
        assert!(cache.contains_node(body.graph.value_def(five).0));
    }

    #[test]
    fn doesnt_contain_node() {
        let cache = NodeCache::new();
        let mut body = FunctionBody::new_invalid();
        let five = SimpleBuilder(&mut body).build_iconst(Type::I32, 5);
        assert!(!cache.contains_node(body.graph.value_def(five).0));
    }

    #[test]
    fn contains_node_clear() {
        let mut cache = NodeCache::new();
        let mut body = FunctionBody::new_invalid();
        let five = CachingBuilder::new(&mut body, &mut cache).build_iconst(Type::I32, 5);
        assert!(cache.contains_node(body.graph.value_def(five).0));
        cache.clear();
        assert!(!cache.contains_node(body.graph.value_def(five).0));
    }

    #[test]
    fn remove() {
        let mut cache = NodeCache::new();
        let mut body = FunctionBody::new_invalid();

        let iconst = CachingBuilder::new(&mut body, &mut cache).build_iconst(Type::I32, 5);
        let iconst_node = body.graph.value_def(iconst).0;

        let iconst2 = CachingBuilder::new(&mut body, &mut cache).build_iconst(Type::I32, 5);
        assert_eq!(iconst2, iconst);

        cache.remove(iconst_node);

        let iconst3 = CachingBuilder::new(&mut body, &mut cache).build_iconst(Type::I32, 5);
        assert_ne!(iconst3, iconst);
    }

    #[test]
    fn remove_uncached() {
        let mut cache = NodeCache::new();
        let mut body = FunctionBody::new_invalid();

        let iconst = SimpleBuilder(&mut body).build_iconst(Type::I32, 5);
        cache.remove(body.graph.value_def(iconst).0);
    }
}
