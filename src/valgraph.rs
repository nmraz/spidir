use core::{ops::Index, slice};

use cranelift_entity::{entity_impl, EntityList, ListPool, PrimaryMap};
use smallvec::SmallVec;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node(u32);
entity_impl!(Node, "node");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DepValue(u32);
entity_impl!(DepValue, "dv");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Use(u32);
entity_impl!(Use, "use");

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
    Return,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepValueKind {
    /// An "ordinary" value representing a computation.
    Value(Type),
    /// Indicates a control flow dependency between nodes. Every region takes in a number of control
    /// values indicating the predecessors of the region, while every branch produces a number of
    /// control values that are then consumed by the regions to which they branch.
    Control,
    /// Special value produced only by region instructions to attach their phi nodes.
    PhiSelector,
}

#[derive(Clone, Copy)]
pub struct Inputs<'a> {
    graph: &'a ValGraph,
    use_list: &'a [Use],
}

impl<'a> Inputs<'a> {
    pub fn len(&self) -> usize {
        self.use_list.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'a> IntoIterator for Inputs<'a> {
    type Item = DepValue;
    type IntoIter = InputIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        InputIter {
            graph: self.graph,
            iter: self.use_list.iter(),
        }
    }
}

impl<'a> Index<usize> for Inputs<'a> {
    type Output = DepValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.graph.uses[self.use_list[index]].value
    }
}

#[derive(Clone)]
pub struct InputIter<'a> {
    graph: &'a ValGraph,
    iter: slice::Iter<'a, Use>,
}

impl<'a> Iterator for InputIter<'a> {
    type Item = DepValue;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.graph.uses[*self.iter.next()?].value)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a> DoubleEndedIterator for InputIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        Some(self.graph.uses[*self.iter.next_back()?].value)
    }
}

impl<'a> ExactSizeIterator for InputIter<'a> {}

#[derive(Clone, Copy)]
pub struct Outputs<'a>(&'a [DepValue]);

impl<'a> Outputs<'a> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'a> IntoIterator for Outputs<'a> {
    type Item = DepValue;
    type IntoIter = OutputIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        OutputIter(self.0.iter())
    }
}

impl<'a> Index<usize> for Outputs<'a> {
    type Output = DepValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

pub struct OutputIter<'a>(slice::Iter<'a, DepValue>);

impl<'a> Iterator for OutputIter<'a> {
    type Item = DepValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().copied()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a> DoubleEndedIterator for OutputIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().copied()
    }
}

impl<'a> ExactSizeIterator for OutputIter<'a> {}

pub struct UseIter<'a> {
    graph: &'a ValGraph,
    iter: slice::Iter<'a, Use>,
}

impl<'a> Iterator for UseIter<'a> {
    type Item = (Node, u32);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|&use_id| {
            let use_data = &self.graph.uses[use_id];
            (use_data.user, use_data.input_index)
        })
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
    uses: UseList,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UseData {
    value: DepValue,
    user: Node,
    input_index: u32,
}

#[derive(Clone)]
pub struct ValGraph {
    nodes: PrimaryMap<Node, NodeData>,
    values: PrimaryMap<DepValue, DepValueData>,
    uses: PrimaryMap<Use, UseData>,
    value_pool: ListPool<DepValue>,
    use_pool: ListPool<Use>,
}

impl Default for ValGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl ValGraph {
    pub fn new() -> Self {
        ValGraph {
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
                    value,
                    user: node,
                    input_index: index as u32,
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
                uses: UseList::new(),
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

    pub fn value_uses(&self, value: DepValue) -> UseIter<'_> {
        UseIter {
            graph: self,
            iter: self.values[value].uses.as_slice(&self.use_pool).iter(),
        }
    }

    pub fn has_one_use(&self, value: DepValue) -> bool {
        let mut uses = self.value_uses(value);
        uses.next().is_some() && uses.next().is_none()
    }

    pub fn replace_all_uses(&mut self, old: DepValue, new: DepValue) {
        let mut old_uses = self.values[old].uses.take();

        for &value_use in old_uses.as_slice(&self.use_pool) {
            self.uses[value_use].value = new;
        }

        let new_uses = &mut self.values[new].uses;
        let orig_new_use_len = new_uses.len(&self.use_pool);
        let old_use_len = old_uses.len(&self.use_pool);

        new_uses.grow_at(orig_new_use_len, old_use_len, &mut self.use_pool);

        // Effectively `new_uses.extend` but without borrowing issues.
        for i in 0..old_use_len {
            let old_use = old_uses.as_slice(&self.use_pool)[i];
            new_uses.as_mut_slice(&mut self.use_pool)[orig_new_use_len + i] = old_use;
        }

        old_uses.clear(&mut self.use_pool);
    }

    fn link_use(&mut self, value: DepValue, value_use: Use) {
        self.values[value].uses.push(value_use, &mut self.use_pool);
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
        assert_eq!(mem::size_of::<UseData>(), 12);
    }

    #[test]
    fn create_single_node() {
        let mut graph = ValGraph::new();
        let node = graph.create_node(NodeKind::IConst(5), &[], &[DepValueKind::Value(Type::I32)]);
        assert_eq!(graph.node_kind(node), &NodeKind::IConst(5));
        assert_eq!(Vec::from_iter(graph.node_inputs(node)), vec![]);
        let outputs = graph.node_outputs(node);
        let output_kinds: Vec<_> = outputs
            .into_iter()
            .map(|value| graph.value_kind(value))
            .collect();
        assert_eq!(output_kinds, vec![DepValueKind::Value(Type::I32)]);
        assert_eq!(graph.value_def(outputs[0]), (node, 0));
    }

    #[test]
    fn create_multi_node() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            &[],
            &[
                DepValueKind::Control,
                DepValueKind::Value(Type::I32),
                DepValueKind::Value(Type::I32),
            ],
        );
        let entry_outputs = graph.node_outputs(entry);
        let control_value = entry_outputs[0];
        let param1 = entry_outputs[1];
        let param2 = entry_outputs[2];

        let add = graph.create_node(
            NodeKind::Iadd,
            &[param1, param2],
            &[DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = graph.create_node(NodeKind::Return, &[control_value, add_res], &[]);

        assert_eq!(graph.value_def(param1), (entry, 1));
        assert_eq!(graph.value_def(param2), (entry, 2));
        assert_eq!(Vec::from_iter(graph.node_inputs(add)), vec![param1, param2]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(control_value)),
            vec![(ret, 0)]
        );
        assert_eq!(Vec::from_iter(graph.value_uses(param1)), vec![(add, 0)]);
        assert_eq!(Vec::from_iter(graph.value_uses(param2)), vec![(add, 1)]);
    }

    #[test]
    fn multi_use() {
        let mut graph = ValGraph::new();
        let five: Node =
            graph.create_node(NodeKind::IConst(5), &[], &[DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let three = graph.create_node(NodeKind::IConst(3), &[], &[DepValueKind::Value(Type::I32)]);
        let three_val = graph.node_outputs(three)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            &[five_val, five_val],
            &[DepValueKind::Value(Type::I32)],
        );
        let add2 = graph.create_node(
            NodeKind::Iadd,
            &[five_val, three_val],
            &[DepValueKind::Value(Type::I32)],
        );

        assert!(!graph.has_one_use(five_val));
        assert!(graph.has_one_use(three_val));

        assert_eq!(
            Vec::from_iter(graph.value_uses(five_val)),
            vec![(add, 0), (add, 1), (add2, 0)]
        );
        assert_eq!(Vec::from_iter(graph.value_uses(three_val)), vec![(add2, 1)]);
    }

    #[test]
    fn replace_all_uses() {
        let mut graph = ValGraph::new();
        let five = graph.create_node(NodeKind::IConst(5), &[], &[DepValueKind::Value(Type::I32)]);
        let five_val = graph.node_outputs(five)[0];
        let three = graph.create_node(NodeKind::IConst(3), &[], &[DepValueKind::Value(Type::I32)]);
        let three_val = graph.node_outputs(three)[0];
        let add = graph.create_node(
            NodeKind::Iadd,
            &[five_val, five_val],
            &[DepValueKind::Value(Type::I32)],
        );
        let add2 = graph.create_node(
            NodeKind::Iadd,
            &[five_val, three_val],
            &[DepValueKind::Value(Type::I32)],
        );

        let seven = graph.create_node(NodeKind::IConst(7), &[], &[DepValueKind::Value(Type::I32)]);
        let seven_val = graph.node_outputs(seven)[0];
        graph.replace_all_uses(five_val, seven_val);

        assert_eq!(graph.value_uses(five_val).count(), 0);
        assert_eq!(
            Vec::from_iter(graph.value_uses(seven_val)),
            vec![(add, 0), (add, 1), (add2, 0)]
        );
        assert_eq!(
            graph
                .node_inputs(add)
                .into_iter()
                .map(|input| graph.value_def(input))
                .collect::<Vec<_>>(),
            vec![(seven, 0), (seven, 0)]
        );
        assert_eq!(
            graph
                .node_inputs(add2)
                .into_iter()
                .map(|input| graph.value_def(input))
                .collect::<Vec<_>>(),
            vec![(seven, 0), (three, 0)]
        );
    }
}
