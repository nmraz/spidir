use core::ops::Index;

use cranelift_entity::{packed_option::PackedOption, EntityList, ListPool, PrimaryMap};
use smallvec::SmallVec;

use crate::entities::{DepValue, Node, Use};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    F64,
    Ptr,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeKind {
    Entry,
    End,
    Region,
    Phi,
    IConst(i64),
    FConst(f64),
    Iadd,
    Load,
    Store,
    Br,
    BrCond,
    Call,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepValueKind {
    /// An "ordinary" value representing a computation.
    Value(Type),
    /// Indicates a control flow dependency between nodes. Every region takes in a number of control
    /// values indicating the predecessors of the region, while every branch produces a number of
    /// control values that are then consumed by the regions to which they branch.
    Control,
    /// A value that constrains ordering between nodes because they may have additional side
    /// effects. Every node with potential side effects should both consume and produce an effect
    /// value, and effect values should be passed/merged via region parameters when appropriate.
    /// Essentially, every function should behave as if it has a single implicit "effect" variable
    /// that is both used and defined by every effectful instruction,
    Effect,
    /// Special value produced only by region instructions to attach their phi nodes.
    PhiSelector,
}

pub struct Inputs<'a> {
    graph: &'a Graph,
    use_list: &'a [Use],
}

impl<'a> Index<usize> for Inputs<'a> {
    type Output = DepValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.graph.uses[self.use_list[index]].value
    }
}

impl<'a> Inputs<'a> {
    pub fn len(&self) -> usize {
        self.use_list.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = DepValue> + 'a {
        let graph = self.graph;
        self.use_list
            .iter()
            .map(move |&input_use| graph.uses[input_use].value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Outputs<'a>(&'a [DepValue]);

impl<'a> Index<usize> for Outputs<'a> {
    type Output = DepValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'a> Outputs<'a> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = DepValue> + 'a {
        self.0.iter().copied()
    }
}

type DepValueList = EntityList<DepValue>;
type UseList = EntityList<Use>;

#[derive(Debug, Clone, Copy)]
struct NodeData {
    kind: NodeKind,
    inputs: UseList,
    outputs: DepValueList,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DepValueData {
    kind: DepValueKind,
    source: Node,
    output_index: u32,
    first_use: PackedOption<Use>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UseData {
    prev: PackedOption<Use>,
    next: PackedOption<Use>,
    value: DepValue,
    node: Node,
    use_index: u32,
}

#[derive(Clone)]
pub struct Graph {
    nodes: PrimaryMap<Node, NodeData>,
    values: PrimaryMap<DepValue, DepValueData>,
    uses: PrimaryMap<Use, UseData>,
    value_pool: ListPool<DepValue>,
    use_pool: ListPool<Use>,
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            nodes: PrimaryMap::new(),
            values: PrimaryMap::new(),
            uses: PrimaryMap::new(),
            value_pool: ListPool::new(),
            use_pool: ListPool::new(),
        }
    }

    pub fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: &[DepValue],
        output_kinds: &[DepValueKind],
    ) -> Node {
        let node = self.nodes.push(NodeData {
            kind,
            inputs: UseList::new(),
            outputs: DepValueList::new(),
        });

        let input_uses: SmallVec<[Use; 2]> = inputs
            .iter()
            .enumerate()
            .map(|(index, &value)| {
                self.uses.push(UseData {
                    prev: None.into(),
                    next: None.into(),
                    value,
                    node,
                    use_index: index as u32,
                })
            })
            .collect();

        for (&input, &input_use) in inputs.iter().zip(&input_uses) {
            self.link_use(input, input_use);
        }

        let outputs = output_kinds.iter().enumerate().map(|(index, &kind)| {
            self.values.push(DepValueData {
                kind,
                source: node,
                output_index: index as u32,
                first_use: None.into(),
            })
        });

        self.nodes[node].inputs = UseList::from_iter(input_uses, &mut self.use_pool);
        self.nodes[node].outputs = DepValueList::from_iter(outputs, &mut self.value_pool);
        node
    }

    pub fn node_kind(&self, node: Node) -> &NodeKind {
        &self.nodes[node].kind
    }

    pub fn node_kind_mut(&mut self, node: Node) -> &mut NodeKind {
        &mut self.nodes[node].kind
    }

    pub fn node_inputs(&self, node: Node) -> Inputs<'_> {
        Inputs {
            graph: self,
            use_list: self.nodes[node].inputs.as_slice(&self.use_pool),
        }
    }

    pub fn node_outputs(&self, node: Node) -> Outputs<'_> {
        Outputs(self.nodes[node].outputs.as_slice(&self.value_pool))
    }

    pub fn value_kind(&self, value: DepValue) -> DepValueKind {
        self.values[value].kind
    }

    pub fn value_def(&self, value: DepValue) -> (Node, u32) {
        let data = &self.values[value];
        (data.source, data.output_index)
    }

    fn link_use(&mut self, value: DepValue, value_use: Use) {
        let use_data = self.uses[value_use];
        assert!(use_data.prev.is_none() && use_data.next.is_none());

        let old_first_use = self.values[value].first_use;
        self.uses[value_use].next = old_first_use;
        if let Some(old_first_use) = old_first_use.expand() {
            self.uses[old_first_use].prev = value_use.into();
        }

        self.values[value].first_use = value_use.into();
    }
}

#[cfg(test)]
mod tests {
    use core::mem;

    use super::*;

    #[test]
    fn node_size() {
        assert_eq!(mem::size_of::<NodeData>(), 24);
    }

    #[test]
    fn dep_value_size() {
        assert_eq!(mem::size_of::<DepValueData>(), 16);
    }

    #[test]
    fn use_size() {
        assert_eq!(mem::size_of::<UseData>(), 20);
    }
}
