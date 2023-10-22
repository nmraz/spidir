use core::{ops::Index, slice};

use cranelift_entity::{
    entity_impl, packed_option::PackedOption, EntityList, ListPool, PrimaryMap,
};
use smallvec::SmallVec;

use crate::node::{DepValueKind, NodeKind};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node(u32);
entity_impl!(Node, "node");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DepValue(u32);
entity_impl!(DepValue, "%");

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Use(u32);
entity_impl!(Use, "use");

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

    pub fn get(&self, index: usize) -> Option<&DepValue> {
        Some(&self.graph.uses[*self.use_list.get(index)?].value)
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
        self.get(index).expect("input index out of bounds")
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

    pub fn get(&self, index: usize) -> Option<&DepValue> {
        self.0.get(index)
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
    cur_use: Option<Use>,
}

impl<'a> Iterator for UseIter<'a> {
    type Item = (Node, u32);

    fn next(&mut self) -> Option<Self::Item> {
        let use_data = &self.graph.uses[self.cur_use?];
        self.cur_use = use_data.next.expand();
        Some((use_data.user, use_data.input_index))
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
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        let node = self.nodes.push(NodeData {
            kind,
            inputs: UseList::new(),
            outputs: DepValueList::new(),
        });

        let input_uses: SmallVec<[Use; 2]> = inputs
            .into_iter()
            .enumerate()
            .map(|(index, value)| {
                self.uses.push(UseData {
                    prev: None.into(),
                    next: None.into(),
                    value,
                    user: node,
                    input_index: index as u32,
                })
            })
            .collect();

        for &input_use in &input_uses {
            self.link_use(input_use);
        }

        let outputs = output_kinds.into_iter().enumerate().map(|(index, kind)| {
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

    #[inline]
    pub fn node_kind(&self, node: Node) -> &NodeKind {
        &self.nodes[node].kind
    }

    #[inline]
    pub fn node_kind_mut(&mut self, node: Node) -> &mut NodeKind {
        &mut self.nodes[node].kind
    }

    #[inline]
    pub fn node_inputs(&self, node: Node) -> Inputs<'_> {
        Inputs {
            graph: self,
            use_list: self.nodes[node].inputs.as_slice(&self.use_pool),
        }
    }

    pub fn add_node_input(&mut self, node: Node, input: DepValue) {
        let input_index = self.nodes[node].inputs.len(&self.use_pool) as u32;
        let input_use = self.uses.push(UseData {
            prev: None.into(),
            next: None.into(),
            value: input,
            user: node,
            input_index,
        });
        self.nodes[node].inputs.push(input_use, &mut self.use_pool);
        self.link_use(input_use);
    }

    pub fn remove_node_input(&mut self, node: Node, index: u32) {
        let index = index as usize;
        let inputs = &mut self.nodes[node].inputs;
        let input_use = inputs.as_slice(&self.use_pool)[index];

        inputs.remove(index, &mut self.use_pool);
        // Adjust input indices for any of the remaining inputs
        for &input in &inputs.as_slice(&self.use_pool)[index..] {
            self.uses[input].input_index -= 1;
        }

        self.unlink_use(input_use);
    }

    #[inline]
    pub fn node_outputs(&self, node: Node) -> Outputs<'_> {
        Outputs(self.nodes[node].outputs.as_slice(&self.value_pool))
    }

    #[inline]
    pub fn value_kind(&self, value: DepValue) -> DepValueKind {
        self.values[value].kind
    }

    #[inline]
    pub fn value_def(&self, value: DepValue) -> (Node, u32) {
        let data = &self.values[value];
        (data.source, data.output_index)
    }

    #[inline]
    pub fn value_uses(&self, value: DepValue) -> UseIter<'_> {
        UseIter {
            graph: self,
            cur_use: self.values[value].first_use.expand(),
        }
    }

    #[inline]
    pub fn has_one_use(&self, value: DepValue) -> bool {
        let mut uses = self.value_uses(value);
        uses.next().is_some() && uses.next().is_none()
    }

    pub fn replace_all_uses(&mut self, old: DepValue, new: DepValue) {
        let first_use = self.values[old].first_use.take();

        let mut cursor = first_use;
        let mut last_use = None;
        while let Some(cur_use) = cursor {
            last_use = Some(cur_use);
            self.uses[cur_use].value = new;
            cursor = self.uses[cur_use].next.expand();
        }

        if let Some(last_use) = last_use {
            let orig_new_first_use = self.values[new].first_use;
            self.values[new].first_use = first_use.into();
            self.uses[last_use].next = orig_new_first_use;
            if let Some(orig_new_first_use) = orig_new_first_use.expand() {
                self.uses[orig_new_first_use].prev = last_use.into();
            }
        }
    }

    fn link_use(&mut self, value_use: Use) {
        assert!(self.uses[value_use].next.is_none());
        assert!(self.uses[value_use].prev.is_none());

        let value = self.uses[value_use].value;
        let next_use = self.values[value].first_use;
        self.uses[value_use].next = next_use;
        if let Some(next_use) = next_use.expand() {
            self.uses[next_use].prev = value_use.into();
        }
        self.values[value].first_use = value_use.into();
    }

    fn unlink_use(&mut self, value_use: Use) {
        let use_data = self.uses[value_use];
        let value = use_data.value;
        if self.values[value].first_use.expand() == Some(value_use) {
            self.values[value].first_use = use_data.next;
        }

        if let Some(prev) = use_data.prev.expand() {
            self.uses[prev].next = use_data.next;
        }

        if let Some(next) = use_data.next.expand() {
            self.uses[next].prev = use_data.prev;
        }
    }
}

#[cfg(test)]
mod tests {
    use core::mem;

    use crate::{
        node::Type,
        test_utils::{create_const32, create_entry, create_region, create_return},
    };

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

    #[test]
    fn create_single_node() {
        let mut graph = ValGraph::new();
        let node = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(Type::I32)]);
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

        let (entry, control_value, [param1, param2]) =
            create_entry(&mut graph, [Type::I32, Type::I32]);

        let add = graph.create_node(
            NodeKind::Iadd,
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        let ret = create_return(&mut graph, [control_value, add_res]);

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

        let const1 = create_const32(&mut graph);
        let const2 = create_const32(&mut graph);

        let add = graph.create_node(
            NodeKind::Iadd,
            [const1, const1],
            [DepValueKind::Value(Type::I32)],
        );
        let add2 = graph.create_node(
            NodeKind::Iadd,
            [const1, const2],
            [DepValueKind::Value(Type::I32)],
        );

        assert!(!graph.has_one_use(const1));
        assert!(graph.has_one_use(const2));

        assert_eq!(
            Vec::from_iter(graph.value_uses(const1)),
            vec![(add2, 0), (add, 1), (add, 0)]
        );
        assert_eq!(Vec::from_iter(graph.value_uses(const2)), vec![(add2, 1)]);
    }

    #[test]
    fn replace_all_uses() {
        let mut graph = ValGraph::new();

        let old_const_val = create_const32(&mut graph);

        let three = graph.create_node(NodeKind::IConst(3), [], [DepValueKind::Value(Type::I32)]);
        let three_val = graph.node_outputs(three)[0];

        let add = graph.create_node(
            NodeKind::Iadd,
            [old_const_val, old_const_val],
            [DepValueKind::Value(Type::I32)],
        );
        let add2 = graph.create_node(
            NodeKind::Iadd,
            [old_const_val, three_val],
            [DepValueKind::Value(Type::I32)],
        );

        let seven = graph.create_node(NodeKind::IConst(7), [], [DepValueKind::Value(Type::I32)]);
        let seven_val = graph.node_outputs(seven)[0];

        graph.replace_all_uses(old_const_val, seven_val);

        assert_eq!(Vec::from_iter(graph.value_uses(old_const_val)), vec![]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(seven_val)),
            vec![(add2, 0), (add, 1), (add, 0)]
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

    #[test]
    fn remove_node_input_after_replace_all_uses() {
        let mut graph = ValGraph::new();

        let const1 = create_const32(&mut graph);
        let const2 = create_const32(&mut graph);
        let const3 = create_const32(&mut graph);
        let const4 = create_const32(&mut graph);

        let add1 = graph.create_node(
            NodeKind::Iadd,
            [const1, const2],
            [DepValueKind::Value(Type::I32)],
        );
        let add2 = graph.create_node(
            NodeKind::Iadd,
            [const1, const3],
            [DepValueKind::Value(Type::I32)],
        );
        let add3 = graph.create_node(
            NodeKind::Iadd,
            [const2, const3],
            [DepValueKind::Value(Type::I32)],
        );
        let add4 = graph.create_node(
            NodeKind::Iadd,
            [const1, const4],
            [DepValueKind::Value(Type::I32)],
        );

        graph.replace_all_uses(const1, const2);
        assert_eq!(Vec::from_iter(graph.value_uses(const1)), vec![]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const2)),
            vec![(add4, 0), (add2, 0), (add1, 0), (add3, 0), (add1, 1)]
        );

        graph.remove_node_input(add2, 0);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const2)),
            vec![(add4, 0), (add1, 0), (add3, 0), (add1, 1)]
        );
    }

    #[test]
    fn replace_all_uses_with_self() {
        let mut graph = ValGraph::new();

        let const1 = create_const32(&mut graph);
        let const2 = create_const32(&mut graph);
        let const3 = create_const32(&mut graph);
        let const4 = create_const32(&mut graph);

        let add1 = graph.create_node(
            NodeKind::Iadd,
            [const1, const2],
            [DepValueKind::Value(Type::I32)],
        );
        let add2 = graph.create_node(
            NodeKind::Iadd,
            [const1, const3],
            [DepValueKind::Value(Type::I32)],
        );
        let add3 = graph.create_node(
            NodeKind::Iadd,
            [const4, const1],
            [DepValueKind::Value(Type::I32)],
        );

        graph.replace_all_uses(const1, const1);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const1)),
            vec![(add3, 1), (add2, 0), (add1, 0)]
        );

        graph.remove_node_input(add1, 0);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const1)),
            vec![(add3, 1), (add2, 0)]
        );
    }

    #[test]
    fn replace_all_uses_with_self_reused_uses() {
        let mut graph = ValGraph::new();
        let const1 = create_const32(&mut graph);
        let const2 = create_const32(&mut graph);
        let add1 = graph.create_node(
            NodeKind::Iadd,
            [const1, const2],
            [DepValueKind::Value(Type::I32)],
        );
        graph.replace_all_uses(const1, const1);
        assert_eq!(Vec::from_iter(graph.value_uses(const1)), vec![(add1, 0)]);

        let const3 = create_const32(&mut graph);
        let add2 = graph.create_node(
            NodeKind::Iadd,
            [const2, const3],
            [DepValueKind::Value(Type::I32)],
        );
        assert_eq!(Vec::from_iter(graph.value_uses(const1)), vec![(add1, 0)]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const2)),
            vec![(add2, 0), (add1, 1)]
        );
    }

    #[test]
    fn replace_all_uses_unused() {
        let mut graph = ValGraph::new();
        let const1 = create_const32(&mut graph);
        let const2 = create_const32(&mut graph);
        let add = graph.create_node(
            NodeKind::Iadd,
            [const2, const2],
            [DepValueKind::Value(Type::I32)],
        );
        graph.replace_all_uses(const1, const2);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const2)),
            vec![(add, 1), (add, 0)]
        );
    }

    #[test]
    fn replace_all_uses_fresh() {
        let mut graph = ValGraph::new();
        let const1 = create_const32(&mut graph);
        let add = graph.create_node(
            NodeKind::Iadd,
            [const1, const1],
            [DepValueKind::Value(Type::I32)],
        );

        let const2 = create_const32(&mut graph);
        graph.replace_all_uses(const1, const2);
        assert_eq!(Vec::from_iter(graph.value_uses(const1)), vec![]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(const2)),
            vec![(add, 1), (add, 0)]
        );
    }

    #[test]
    fn add_node_input() {
        let mut graph = ValGraph::new();
        let (_, _, [param1, param2]) = create_entry(&mut graph, [Type::I32, Type::I32]);

        let add = graph.create_node(NodeKind::Iadd, [param1], [DepValueKind::Value(Type::I32)]);
        graph.add_node_input(add, param2);

        assert_eq!(Vec::from_iter(graph.node_inputs(add)), vec![param1, param2]);
        assert_eq!(Vec::from_iter(graph.value_uses(param1)), vec![(add, 0)]);
        assert_eq!(Vec::from_iter(graph.value_uses(param2)), vec![(add, 1)]);
    }

    #[test]
    fn remove_node_input() {
        let mut graph = ValGraph::new();
        let (_, entry_control, []) = create_entry(&mut graph, []);

        let dead_region1_control = create_region(&mut graph, []);
        let dead_region2_control = create_region(&mut graph, []);

        let exit_region = graph.create_node(
            NodeKind::Region,
            [entry_control, dead_region1_control, dead_region2_control],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );

        graph.remove_node_input(exit_region, 1);
        assert_eq!(
            Vec::from_iter(graph.node_inputs(exit_region)),
            vec![entry_control, dead_region2_control]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(entry_control)),
            vec![(exit_region, 0)]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(dead_region1_control)),
            vec![]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(dead_region2_control)),
            vec![(exit_region, 1)]
        );

        graph.remove_node_input(exit_region, 1);
        assert_eq!(
            Vec::from_iter(graph.node_inputs(exit_region)),
            vec![entry_control]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(entry_control)),
            vec![(exit_region, 0)]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(dead_region1_control)),
            vec![]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(dead_region2_control)),
            vec![]
        );
    }

    #[test]
    fn remove_node_input_multi_use() {
        let mut graph = ValGraph::new();

        let a = create_const32(&mut graph);
        let b = create_const32(&mut graph);
        let c = create_const32(&mut graph);

        // Not really valid add nodes, but we don't care about that in this test.
        let ab = graph.create_node(NodeKind::Iadd, [a, b], []);
        let bc = graph.create_node(NodeKind::Iadd, [b, c], []);
        let ac = graph.create_node(NodeKind::Iadd, [a, c], []);
        let abc = graph.create_node(NodeKind::Iadd, [a, b, c], []);

        assert_eq!(
            Vec::from_iter(graph.value_uses(a)),
            vec![(abc, 0), (ac, 0), (ab, 0)]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(b)),
            vec![(abc, 1), (bc, 0), (ab, 1)]
        );
        assert_eq!(
            Vec::from_iter(graph.value_uses(c)),
            vec![(abc, 2), (ac, 1), (bc, 1)]
        );

        // Start by removing a use from the middle of the use list
        graph.remove_node_input(ac, 0);
        assert_eq!(Vec::from_iter(graph.node_inputs(ac)), vec![c]);
        assert_eq!(Vec::from_iter(graph.value_uses(a)), vec![(abc, 0), (ab, 0)]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(c)),
            vec![(abc, 2), (ac, 0), (bc, 1)]
        );

        // Remove a use from the end of the use list
        graph.remove_node_input(abc, 0);
        assert_eq!(Vec::from_iter(graph.node_inputs(abc)), vec![b, c]);
        assert_eq!(Vec::from_iter(graph.value_uses(a)), vec![(ab, 0)]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(b)),
            vec![(abc, 0), (bc, 0), (ab, 1)]
        );

        // Remove a use from the end of the use list
        graph.remove_node_input(abc, 0);
        assert_eq!(Vec::from_iter(graph.node_inputs(abc)), vec![c]);
        assert_eq!(Vec::from_iter(graph.value_uses(b)), vec![(bc, 0), (ab, 1)]);
        assert_eq!(
            Vec::from_iter(graph.value_uses(c)),
            vec![(abc, 0), (ac, 0), (bc, 1)]
        );

        // Remove a use from the beginning of the use list
        graph.remove_node_input(ab, 1);
        assert_eq!(Vec::from_iter(graph.node_inputs(ab)), vec![a]);
        assert_eq!(Vec::from_iter(graph.value_uses(b)), vec![(bc, 0)]);
    }

    #[test]
    fn remove_only_input() {
        let mut graph = ValGraph::new();

        let incoming_ctrl = create_region(&mut graph, []);

        let node = graph.create_node(NodeKind::Region, [incoming_ctrl], [DepValueKind::Control]);
        graph.remove_node_input(node, 0);

        assert!(graph.node_inputs(node).is_empty());
        assert!(graph.value_uses(incoming_ctrl).count() == 0);
    }
}
