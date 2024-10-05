use alloc::collections::VecDeque;
use core::array;

use fx_utils::FxHashSet;

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    function::FunctionBody,
    node::{NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

pub fn canonicalize(body: &mut FunctionBody) {
    let mut worklist = Worklist::new();
    for root in body.compute_live_nodes().reverse_postorder(&body.graph) {
        worklist.enqueue(root);
    }

    while let Some(node) = worklist.dequeue() {
        canonicalize_node(body, &mut worklist, node);
    }
}

macro_rules! fold_constant {
    ($body:expr, $worklist:expr, $output:expr, $a:expr, $b:expr, $func:ident) => {{
        let ty = $body.graph.value_kind($output).as_value().unwrap();
        let combined = if ty == Type::I32 {
            (($a as u32).$func($b as u32)) as u64
        } else {
            $a.$func($b)
        };

        let new_output = SimpleBuilder($body).build_iconst(ty, combined);
        replace_value(&mut $body.graph, $worklist, $output, new_output);
    }};
}

fn canonicalize_node(body: &mut FunctionBody, worklist: &mut Worklist, node: Node) {
    let graph = &body.graph;

    match graph.node_kind(node) {
        NodeKind::Iadd => {
            let [a, b] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(body, worklist, output, a, b, wrapping_add),

                (Some(0), _) => replace_value(&mut body.graph, worklist, output, b),
                (_, Some(0)) => replace_value(&mut body.graph, worklist, output, a),

                (Some(_), None) => {
                    let new_output = SimpleBuilder(body).build_iadd(b, a);
                    replace_value(&mut body.graph, worklist, output, new_output);
                }

                _ => {}
            }
        }
        NodeKind::Isub => {
            let [a, b] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(body, worklist, output, a, b, wrapping_sub),
                (_, Some(0)) => replace_value(&mut body.graph, worklist, output, a),
                _ => {}
            }
        }
        NodeKind::Imul => {
            let [a, b] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            let ty = graph.value_kind(output).as_value().unwrap();

            match (match_iconst(graph, a), match_iconst(graph, b)) {
                (Some(a), Some(b)) => fold_constant!(body, worklist, output, a, b, wrapping_mul),
                (Some(0), _) | (_, Some(0)) => {
                    let zero = SimpleBuilder(body).build_iconst(ty, 0);
                    replace_value(&mut body.graph, worklist, output, zero);
                }

                (Some(1), _) => replace_value(&mut body.graph, worklist, output, b),
                (_, Some(1)) => replace_value(&mut body.graph, worklist, output, a),

                (Some(_), None) => {
                    let new_output = SimpleBuilder(body).build_imul(b, a);
                    replace_value(&mut body.graph, worklist, output, new_output);
                }

                _ => {}
            }
        }
        NodeKind::Iext => {
            let [input] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            if let Some(value) = match_iconst(graph, input) {
                let new_output = SimpleBuilder(body).build_iconst(Type::I64, value);
                replace_value(&mut body.graph, worklist, output, new_output);
            }
        }
        NodeKind::Itrunc => {
            let [input] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            if let Some(value) = match_iconst(graph, input) {
                let new_output = SimpleBuilder(body).build_iconst(Type::I32, value as u32 as u64);
                replace_value(&mut body.graph, worklist, output, new_output);
            }
        }
        &NodeKind::Sfill(width) => {
            let [input] = node_inputs_exact(graph, node);
            let output = graph.node_outputs(node)[0];
            let ty = graph.value_kind(output).as_value().unwrap();

            if let Some(value) = match_iconst(graph, input) {
                let shift_width = 64 - width;

                let mut new_value = (((value << shift_width) as i64) >> shift_width) as u64;
                if ty == Type::I32 {
                    new_value = new_value as u32 as u64;
                }

                let new_output = SimpleBuilder(body).build_iconst(ty, new_value);
                replace_value(&mut body.graph, worklist, output, new_output);
            }
        }
        _ => {}
    }
}

struct Worklist {
    queue: VecDeque<Node>,
    enqueued: FxHashSet<Node>,
}

impl Worklist {
    fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            enqueued: FxHashSet::default(),
        }
    }

    fn enqueue(&mut self, node: Node) {
        if !self.enqueued.contains(&node) {
            self.queue.push_back(node);
        }
    }

    fn dequeue(&mut self) -> Option<Node> {
        let node = self.queue.pop_front()?;
        self.enqueued.remove(&node);
        Some(node)
    }
}

fn replace_value(
    graph: &mut ValGraph,
    worklist: &mut Worklist,
    old_value: DepValue,
    new_value: DepValue,
) {
    let mut cursor = graph.value_use_cursor(old_value);
    while let Some((node, _)) = cursor.current() {
        worklist.enqueue(node);
        cursor.replace_current_with(new_value);
    }
}

fn match_iconst(graph: &ValGraph, value: DepValue) -> Option<u64> {
    if let (node, 0) = graph.value_def(value) {
        if let &NodeKind::IConst(value) = graph.node_kind(node) {
            return Some(value);
        }
    }

    None
}

fn node_inputs_exact<const N: usize>(graph: &ValGraph, node: Node) -> [DepValue; N] {
    let inputs = graph.node_inputs(node);
    assert!(inputs.len() == N);
    array::from_fn(|i| inputs[i])
}
