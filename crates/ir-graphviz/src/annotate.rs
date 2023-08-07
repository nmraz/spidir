use alloc::{
    borrow::ToOwned,
    string::{String, ToString},
    vec::Vec,
};

use fx_utils::FxHashMap;

use ir::{
    node::DepValueKind,
    valgraph::{Node, ValGraph},
    verify::GraphVerifierError,
};
use itertools::Itertools;

pub type DotAttributes = FxHashMap<String, String>;

pub trait Annotate {
    fn annotate_node(&mut self, _graph: &ValGraph, _node: Node) -> DotAttributes {
        Default::default()
    }

    fn annotate_edge(
        &mut self,
        _graph: &ValGraph,
        _node: Node,
        _input_idx: usize,
    ) -> DotAttributes {
        Default::default()
    }
}

pub struct PlainAnnotator;

impl Annotate for PlainAnnotator {}

pub fn colored_edge_attrs(graph: &ValGraph, node: Node, input_idx: usize) -> DotAttributes {
    match graph.value_kind(graph.node_inputs(node)[input_idx]) {
        DepValueKind::Control => DotAttributes::from_iter([
            ("penwidth".to_owned(), "2".to_owned()),
            ("color".to_owned(), "#0000ff".to_owned()),
        ]),
        DepValueKind::Value(_) => {
            DotAttributes::from_iter([("color".to_owned(), "#d36805".to_owned())])
        }
        DepValueKind::PhiSelector => {
            DotAttributes::from_iter([("color".to_owned(), "#4e4e4e".to_owned())])
        }
    }
}

pub fn colored_node_attrs(graph: &ValGraph, node: Node) -> DotAttributes {
    let value_kinds = &mut graph
        .node_inputs(node)
        .into_iter()
        .chain(graph.node_outputs(node))
        .map(|value| graph.value_kind(value));

    let mut has_ctrl = false;
    let mut has_value = false;
    let mut has_phisel = false;

    for value_kind in value_kinds {
        match value_kind {
            DepValueKind::Control => has_ctrl = true,
            DepValueKind::Value(_) => has_value = true,
            DepValueKind::PhiSelector => has_phisel = true,
        }
    }

    let fill_color = if has_ctrl {
        if has_value {
            "#ffd3e4"
        } else {
            "#c2c2ff"
        }
    } else if has_phisel {
        "#dbdbdb"
    } else {
        "#ffee9b"
    };

    DotAttributes::from_iter([
        ("style".to_owned(), "filled".to_owned()),
        ("fillcolor".to_owned(), fill_color.to_owned()),
    ])
}

pub struct ColoredAnnotator;

impl Annotate for ColoredAnnotator {
    fn annotate_node(&mut self, graph: &ValGraph, node: Node) -> DotAttributes {
        colored_node_attrs(graph, node)
    }

    fn annotate_edge(&mut self, graph: &ValGraph, node: Node, input_idx: usize) -> DotAttributes {
        colored_edge_attrs(graph, node, input_idx)
    }
}
