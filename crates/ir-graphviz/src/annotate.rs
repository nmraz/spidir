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
    fn annotate_node(&mut self, _graph: &ValGraph, _node: Node, _attrs: &mut DotAttributes) {}

    fn annotate_edge(
        &mut self,
        _graph: &ValGraph,
        _node: Node,
        _input_idx: usize,
        _attrs: &mut DotAttributes,
    ) {
    }
}

pub fn colored_edge_attrs(
    graph: &ValGraph,
    node: Node,
    input_idx: usize,
    attrs: &mut DotAttributes,
) {
    match graph.value_kind(graph.node_inputs(node)[input_idx]) {
        DepValueKind::Control => attrs.extend([
            ("penwidth".to_owned(), "2".to_owned()),
            ("color".to_owned(), "#0000ff".to_owned()),
        ]),
        DepValueKind::Value(_) => attrs.extend([("color".to_owned(), "#d36805".to_owned())]),
        DepValueKind::PhiSelector => attrs.extend([("color".to_owned(), "#4e4e4e".to_owned())]),
    }
}

pub fn colored_node_attrs(graph: &ValGraph, node: Node, attrs: &mut DotAttributes) {
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

    attrs.extend([
        ("style".to_owned(), "filled".to_owned()),
        ("fillcolor".to_owned(), fill_color.to_owned()),
    ]);
}

pub struct ColoredAnnotator;

impl Annotate for ColoredAnnotator {
    fn annotate_node(&mut self, graph: &ValGraph, node: Node, attrs: &mut DotAttributes) {
        colored_node_attrs(graph, node, attrs)
    }

    fn annotate_edge(
        &mut self,
        graph: &ValGraph,
        node: Node,
        input_idx: usize,
        attrs: &mut DotAttributes,
    ) {
        colored_edge_attrs(graph, node, input_idx, attrs)
    }
}

pub struct ErrorAnnotator<'a> {
    pub(crate) node_errors: FxHashMap<Node, Vec<&'a GraphVerifierError>>,
    pub(crate) edge_errors: FxHashMap<(Node, u32), Vec<&'a GraphVerifierError>>,
}

impl<'a> ErrorAnnotator<'a> {
    pub fn new(graph: &ValGraph, errors: &'a [GraphVerifierError]) -> Self {
        let mut node_errors: FxHashMap<_, Vec<_>> = FxHashMap::default();
        let mut edge_errors: FxHashMap<_, Vec<_>> = FxHashMap::default();

        for error in errors {
            if let Some(edge) = error.node_input() {
                edge_errors.entry(edge).or_default().push(error);
            } else {
                node_errors
                    .entry(error.node(graph))
                    .or_default()
                    .push(error);
            }
        }

        Self {
            node_errors,
            edge_errors,
        }
    }
}

impl<'a> Annotate for ErrorAnnotator<'a> {
    fn annotate_node(&mut self, graph: &ValGraph, node: Node, attrs: &mut DotAttributes) {
        if let Some(errors) = self.node_errors.get(&node) {
            error_attrs(errors, graph, attrs);
        }
    }

    fn annotate_edge(
        &mut self,
        graph: &ValGraph,
        node: Node,
        input_idx: usize,
        attrs: &mut DotAttributes,
    ) {
        if let Some(errors) = self.edge_errors.get(&(node, input_idx as u32)) {
            error_attrs(errors, graph, attrs);
        }
    }
}

fn error_attrs(errors: &[&GraphVerifierError], graph: &ValGraph, attrs: &mut DotAttributes) {
    attrs.extend([
        ("color".to_owned(), "#ff0000".to_owned()),
        ("penwidth".to_owned(), "2".to_owned()),
        ("tooltip".to_owned(), format_verifier_errors(errors, graph)),
    ]);
}

fn format_verifier_errors(errors: &[&GraphVerifierError], graph: &ValGraph) -> String {
    errors
        .iter()
        .format_with("&#10;", |error, f| f(&error.display(graph)))
        .to_string()
}
