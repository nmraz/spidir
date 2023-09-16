#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::string::String;
use core::fmt;

use itertools::Itertools;

use ir::{
    module::Module,
    node::NodeKind,
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
    write::write_node_kind,
};

use crate::annotate::{Annotate, DotAttributes};

pub mod annotate;

pub struct StructuralEdge {
    pub from: Node,
    pub to: Node,
    pub attrs: DotAttributes,
}

pub fn write_graphviz(
    w: &mut dyn fmt::Write,
    annotator: &mut dyn Annotate,
    module: &Module,
    graph: &ValGraph,
    entry: Node,
    structural_edges: &[StructuralEdge],
) -> fmt::Result {
    let live_info = &LiveNodeInfo::compute(graph, entry);
    let rpo = live_info.reverse_postorder(graph);

    writeln!(w, "digraph {{")?;

    // First pass: write all nodes.
    for &node in &rpo {
        write!(w, r#"    {node} [shape=Mrecord, label="{{"#)?;

        let inputs = graph.node_inputs(node);
        let outputs = graph.node_outputs(node);

        if !inputs.is_empty() {
            write!(w, "{{")?;
            let mut first = true;
            for i in 0..inputs.len() {
                if !first {
                    write!(w, " | ")?;
                }
                first = false;
                write!(w, "<i{i}>")?;
            }
            write!(w, "}} | ")?;
        }

        write!(
            w,
            "{}",
            stringify_dot_node_kind(module, graph.node_kind(node))
        )?;

        if !outputs.is_empty() {
            write!(w, " | {{")?;
            let mut first = true;
            for (i, val) in outputs.into_iter().enumerate() {
                if !first {
                    write!(w, " | ")?;
                }
                first = false;
                write!(w, "<o{i}> {}", graph.value_kind(val))?;
            }
            write!(w, "}}")?;
        }

        write!(w, r#"}}""#)?;

        let attrs = annotator.annotate_node(graph, node);
        if !attrs.is_empty() {
            write!(w, ", {}", format_dot_attributes(&attrs))?;
        }

        writeln!(w, "]")?;
    }

    // Second pass: add edges.
    for &node in &rpo {
        // Note: the edges must be printed in input order in the actual graph, so that the
        // `ordering=in` attribute specified on all the nodes can actually guarantee that input
        // edges show up in the right order.
        for (input_idx, input) in graph.node_inputs(node).into_iter().enumerate() {
            let (def_node, def_idx) = graph.value_def(input);
            write!(w, "    {def_node}:o{def_idx} -> {node}:i{input_idx}")?;

            let attrs = annotator.annotate_edge(graph, node, input_idx);
            if !attrs.is_empty() {
                write!(w, " [{}]", format_dot_attributes(&attrs))?;
            }

            writeln!(w)?;
        }
    }

    for structural_edge in structural_edges {
        write!(w, "    {} -> {}", structural_edge.from, structural_edge.to)?;
        if !structural_edge.attrs.is_empty() {
            write!(w, "[{}]", format_dot_attributes(&structural_edge.attrs))?;
        }
    }

    writeln!(w, "}}")?;

    Ok(())
}

fn format_dot_attributes(attrs: &DotAttributes) -> impl fmt::Display + '_ {
    attrs.iter().format_with(", ", |(name, value), f| {
        f(&format_args!(
            r#"{}="{}""#,
            name,
            escape_dot_attr_value(value)
        ))
    })
}

fn stringify_dot_node_kind(module: &Module, node_kind: &NodeKind) -> String {
    let mut s = String::new();
    write_node_kind(&mut s, module, node_kind).unwrap();
    escape_dot_attr_value(&s)
}

fn escape_dot_attr_value(value: &str) -> String {
    // In DOT attributes, everything else (including a plain `\`) is treated verbatim.
    value.replace('\"', "\\\"")
}
