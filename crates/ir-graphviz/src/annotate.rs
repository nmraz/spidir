use alloc::{
    borrow::ToOwned,
    format,
    string::{String, ToString},
    vec::Vec,
};

use fx_utils::FxHashMap;

use ir::{
    domtree::DomTree,
    loop_forest::LoopForest,
    node::DepValueKind,
    valgraph::{Node, ValGraph},
    verify::GraphVerifierError,
};
use itertools::Itertools;

pub type DotAttributes = FxHashMap<String, String>;

pub struct StructuralEdge {
    pub from: Node,
    pub to: Node,
    pub attrs: DotAttributes,
}

pub trait Annotate {
    fn structural_edges(&mut self, _graph: &ValGraph, _entry: Node) -> Vec<StructuralEdge> {
        Vec::new()
    }

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
    node_errors: FxHashMap<Node, Vec<&'a GraphVerifierError>>,
    edge_errors: FxHashMap<(Node, u32), Vec<&'a GraphVerifierError>>,
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

pub struct DomTreeAnnotator<'a> {
    domtree: &'a DomTree,
}

impl<'a> DomTreeAnnotator<'a> {
    pub fn new(domtree: &'a DomTree) -> Self {
        Self { domtree }
    }
}

impl<'a> Annotate for DomTreeAnnotator<'a> {
    fn structural_edges(&mut self, _graph: &ValGraph, _entry: Node) -> Vec<StructuralEdge> {
        self.domtree
            .preorder()
            .filter_map(|tree_node| {
                self.domtree.idom(tree_node).map(|idom| {
                    let cfg_node = self.domtree.get_cfg_node(tree_node);
                    let idom = self.domtree.get_cfg_node(idom);
                    StructuralEdge {
                        from: idom,
                        to: cfg_node,
                        attrs: DotAttributes::from_iter([
                            ("penwidth".to_owned(), "2".to_owned()),
                            ("style".to_owned(), "dashed".to_owned()),
                            ("color".to_owned(), "#a1a1a1".to_owned()),
                        ]),
                    }
                })
            })
            .collect()
    }
}

pub struct LoopAnnotator<'a> {
    domtree: &'a DomTree,
    loop_forest: &'a LoopForest,
}

impl<'a> LoopAnnotator<'a> {
    pub fn new(domtree: &'a DomTree, loop_forest: &'a LoopForest) -> Self {
        Self {
            domtree,
            loop_forest,
        }
    }
}

impl<'a> Annotate for LoopAnnotator<'a> {
    fn annotate_node(&mut self, _graph: &ValGraph, node: Node, attrs: &mut DotAttributes) {
        let Some(domtre_node) = self.domtree.get_tree_node(node) else {
            return;
        };

        let Some(loop_node) = self.loop_forest.containing_loop(domtre_node) else {
            return;
        };

        let loop_id = loop_node.as_u32();
        attrs.insert("color".to_owned(), get_loop_color(loop_id));
        attrs.extend([("penwidth".to_owned(), "2".to_owned())]);

        let tooltip = if domtre_node == self.loop_forest.loop_header(loop_node) {
            format!("loop {loop_id} header")
        } else {
            attrs.insert("style".to_owned(), "filled,dashed".to_owned());
            format!("loop {loop_id}")
        };

        attrs.insert("tooltip".to_owned(), tooltip);
    }

    fn annotate_edge(
        &mut self,
        graph: &ValGraph,
        node: Node,
        input_idx: usize,
        attrs: &mut DotAttributes,
    ) {
        // Check if this input is a loop backedge and annotate accordingly if so.

        let input = graph.node_inputs(node)[input_idx];
        if !graph.value_kind(input).is_control() {
            return;
        }

        // Is `node` a loop header?
        let Some(target_tree_node) = self.domtree.get_tree_node(node) else {
            return;
        };
        let Some(loop_node) = self.loop_forest.containing_loop(target_tree_node) else {
            return;
        };
        if target_tree_node != self.loop_forest.loop_header(loop_node) {
            return;
        }

        // Does the input source belong to the same loop?
        let Some(src_tree_node) = self.domtree.get_tree_node(graph.value_def(input).0) else {
            return;
        };
        if self.loop_forest.containing_loop(src_tree_node) != Some(loop_node) {
            return;
        }

        let loop_id = loop_node.as_u32();
        attrs.extend([
            ("color".to_owned(), get_loop_color(loop_id)),
            ("tooltip".to_owned(), format!("loop {loop_id} backedge")),
        ]);
    }
}

fn get_loop_color(loop_id: u32) -> String {
    static LOOP_COLORS: &[&str] = &[
        "#167288", "#8cdaec", "#b45248", "#d48c84", "#a89a49", "#d6cfa2", "#3cb464", "#9bddb1",
        "#643c6a", "#836394",
    ];
    LOOP_COLORS[loop_id as usize % LOOP_COLORS.len()].to_owned()
}
