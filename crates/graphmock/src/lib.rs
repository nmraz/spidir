use std::ops::ControlFlow;

use cranelift_entity::{entity_impl, PrimaryMap};
use fx_utils::FxHashMap;
use graphwalk::{GraphRef, PredGraphRef};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node(u32);
entity_impl!(Node);

struct NodeData {
    name: String,
    preds: Vec<Node>,
    succs: Vec<Node>,
}

pub struct Graph {
    nodes: PrimaryMap<Node, NodeData>,
    nodes_by_name: FxHashMap<String, Node>,
}

impl Graph {
    pub fn entry(&self) -> Node {
        Node(0)
    }

    pub fn node(&self, name: &str) -> Node {
        self.nodes_by_name[name]
    }

    pub fn name(&self, node: Node) -> &str {
        &self.nodes[node].name
    }

    fn get_or_create(&mut self, name: &str) -> Node {
        if let Some(&node) = self.nodes_by_name.get(name) {
            return node;
        }

        let node = self.nodes.push(NodeData {
            name: name.to_owned(),
            preds: Vec::new(),
            succs: Vec::new(),
        });
        self.nodes_by_name.insert(name.to_owned(), node);
        node
    }

    fn add_succ(&mut self, node: Node, succ: Node) {
        self.nodes[node].succs.push(succ);
        self.nodes[succ].preds.push(node);
    }
}

pub fn graph(input: &str) -> Graph {
    let mut graph = Graph {
        nodes: PrimaryMap::new(),
        nodes_by_name: FxHashMap::default(),
    };

    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }

        let [preds, succs]: [&str; 2] = line.split("->").collect::<Vec<_>>().try_into().unwrap();
        let preds = preds.split(',').map(|pred| pred.trim());
        let succs: Vec<_> = succs.split(',').map(|succ| succ.trim()).collect();

        for pred in preds {
            let pred = graph.get_or_create(pred);
            for succ in &succs {
                let succ = graph.get_or_create(succ);
                graph.add_succ(pred, succ);
            }
        }
    }

    graph
}

impl GraphRef for &'_ Graph {
    type Node = Node;

    fn successors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        self.nodes[node].succs.iter().copied().try_for_each(f)
    }
}

impl PredGraphRef for &'_ Graph {
    fn predecessors(
        &self,
        node: Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<()>,
    ) -> ControlFlow<()> {
        self.nodes[node].preds.iter().copied().try_for_each(f)
    }
}

#[cfg(test)]
mod tests {
    use crate::graph;

    #[test]
    fn simple_graph() {
        graph(
            "
            a -> b
            b -> c
            c -> d
        ",
        );
    }

    #[test]
    fn diamond() {
        graph(
            "
            a -> b, c
            b, c -> d
        ",
        );
    }

    #[test]
    fn loop_grpah() {
        graph(
            "
            a -> b
            b -> c
            c -> b
            c -> d
        ",
        );
    }
}
