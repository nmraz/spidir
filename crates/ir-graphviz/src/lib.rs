#![cfg_attr(not(test), no_std)]

extern crate alloc;

use alloc::{borrow::ToOwned, string::String};
use core::fmt;
use fx_utils::FxHashMap;

use itertools::Itertools;

use ir::{
    module::Module,
    node::DepValueKind,
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
    write::write_node_kind,
};

pub type DotAttributes = FxHashMap<String, String>;

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

pub struct ColoredAnnotator;
impl Annotate for ColoredAnnotator {
    fn annotate_node(&mut self, graph: &ValGraph, node: Node) -> DotAttributes {
        colored_node_attrs(graph, node)
    }

    fn annotate_edge(&mut self, graph: &ValGraph, node: Node, input_idx: usize) -> DotAttributes {
        colored_edge_attrs(graph, node, input_idx)
    }
}

pub fn write_graphviz(
    w: &mut dyn fmt::Write,
    annotator: &mut dyn Annotate,
    module: &Module,
    graph: &ValGraph,
    entry: Node,
) -> fmt::Result {
    let rpo = LiveNodeInfo::compute(graph, entry).reverse_postorder(graph);

    writeln!(w, "digraph {{")?;

    // First pass: write all nodes.
    for &node in &rpo {
        write!(w, r#"    {node} [shape=Mrecord, ordering=in, label="{{"#)?;

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

        write_node_kind(w, module, graph.node_kind(node))?;

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
            write!(w, ", {}", format_dot_attributes(attrs))?;
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
                write!(w, " [{}]", format_dot_attributes(attrs))?;
            }

            writeln!(w)?;
        }
    }

    writeln!(w, "}}")?;

    Ok(())
}

fn format_dot_attributes(attrs: DotAttributes) -> impl fmt::Display {
    attrs.into_iter().format_with(", ", |(name, value), f| {
        f(&format_args!(
            r#"{}="{}""#,
            name,
            escape_dot_attr_value(&value)
        ))
    })
}

fn escape_dot_attr_value(value: &str) -> String {
    // In DOT attributes, everything else (including a plain `\`) is treated verbatim.
    value.replace('\"', "\\\"")
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use ir::module::FunctionData;
    use parser::parse_module;

    use super::*;

    fn graphviz_with_annotator(
        annotator: &mut dyn Annotate,
        module: &Module,
        func: &FunctionData,
    ) -> String {
        let mut graphviz = String::new();
        write_graphviz(&mut graphviz, annotator, module, &func.graph, func.entry).unwrap();
        graphviz
    }

    #[track_caller]
    fn check_dump_graphviz(input: &str, expected_plain: Expect, expected_colored: Expect) {
        let module = parse_module(input).unwrap();
        let func = module.functions.values().next().unwrap();
        expected_plain.assert_eq(&graphviz_with_annotator(&mut PlainAnnotator, &module, func));
        expected_colored.assert_eq(&graphviz_with_annotator(
            &mut ColoredAnnotator,
            &module,
            func,
        ));
    }

    #[test]
    fn dump_simple_graph() {
        check_dump_graphviz(
            "
            extfunc @my_ext_func:i32(i64)
            func @my_func:i32(i64) {
                %0:ctrl, %1:i64 = entry
                %2:ctrl, %3:i32 = call @my_ext_func %0, %1
                return %2, %3
            }",
            expect![[r#"
                digraph {
                    node0 [shape=Mrecord, ordering=in, label="{entry | {<o0> ctrl | <o1> i64}}"]
                    node1 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | call @my_ext_func | {<o0> ctrl | <o1> i32}}"]
                    node2 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | return}"]
                    node0:o0 -> node1:i0
                    node0:o1 -> node1:i1
                    node1:o0 -> node2:i0
                    node1:o1 -> node2:i1
                }
            "#]],
            expect![[r##"
                digraph {
                    node0 [shape=Mrecord, ordering=in, label="{entry | {<o0> ctrl | <o1> i64}}", fillcolor="#ffd3e4", style="filled"]
                    node1 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | call @my_ext_func | {<o0> ctrl | <o1> i32}}", fillcolor="#ffd3e4", style="filled"]
                    node2 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | return}", fillcolor="#ffd3e4", style="filled"]
                    node0:o0 -> node1:i0 [color="#0000ff", penwidth="2"]
                    node0:o1 -> node1:i1 [color="#d36805"]
                    node1:o0 -> node2:i0 [color="#0000ff", penwidth="2"]
                    node1:o1 -> node2:i1 [color="#d36805"]
                }
            "##]],
        );
    }

    #[test]
    fn dump_iota_graph() {
        check_dump_graphviz(
            "
        func @iota(ptr, i64) {
            %entry_ctrl:ctrl, %arr:ptr, %n:i64 = entry
            %zero:i64 = iconst 0
            %zerocmp:i32 = icmp eq %n, %zero
            %iszero:ctrl, %isnonzero:ctrl = brcond %entry_ctrl, %zerocmp
            %loopbody:ctrl, %loopphi:phisel = region %isnonzero, %looplatch
            %i:i64 = phi %loopphi, %zero, %inext
            %three:i64 = iconst 3
            %off:i64 = shl %i, %three
            %ptr:ptr = ptroff %arr, %off
            %poststore:ctrl = store %loopbody, %i, %ptr
            %one:i64 = iconst 1
            %inext:i64 = iadd %i, %one
            %donecmp:i32 = icmp eq %inext, %n
            %loopdone:ctrl, %looplatch:ctrl = brcond %poststore, %donecmp
            %exit:ctrl, %exitphi:phisel = region %iszero, %loopdone
            return %exit
        }
        ",
            expect![[r#"
                digraph {
                    node0 [shape=Mrecord, ordering=in, label="{entry | {<o0> ctrl | <o1> ptr | <o2> i64}}"]
                    node10 [shape=Mrecord, ordering=in, label="{iconst 1 | {<o0> i64}}"]
                    node1 [shape=Mrecord, ordering=in, label="{iconst 0 | {<o0> i64}}"]
                    node2 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}"]
                    node3 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}"]
                    node6 [shape=Mrecord, ordering=in, label="{iconst 3 | {<o0> i64}}"]
                    node7 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | shl | {<o0> i64}}"]
                    node8 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | ptroff | {<o0> ptr}}"]
                    node9 [shape=Mrecord, ordering=in, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}"]
                    node13 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}"]
                    node14 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}"]
                    node15 [shape=Mrecord, ordering=in, label="{{<i0>} | return}"]
                    node4 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}"]
                    node5 [shape=Mrecord, ordering=in, label="{{<i0> | <i1> | <i2>} | phi | {<o0> i64}}"]
                    node11 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | iadd | {<o0> i64}}"]
                    node12 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}"]
                    node0:o2 -> node2:i0
                    node1:o0 -> node2:i1
                    node0:o0 -> node3:i0
                    node2:o0 -> node3:i1
                    node5:o0 -> node7:i0
                    node6:o0 -> node7:i1
                    node0:o1 -> node8:i0
                    node7:o0 -> node8:i1
                    node4:o0 -> node9:i0
                    node5:o0 -> node9:i1
                    node8:o0 -> node9:i2
                    node9:o0 -> node13:i0
                    node12:o0 -> node13:i1
                    node3:o0 -> node14:i0
                    node13:o0 -> node14:i1
                    node14:o0 -> node15:i0
                    node3:o1 -> node4:i0
                    node13:o1 -> node4:i1
                    node4:o1 -> node5:i0
                    node1:o0 -> node5:i1
                    node11:o0 -> node5:i2
                    node5:o0 -> node11:i0
                    node10:o0 -> node11:i1
                    node11:o0 -> node12:i0
                    node0:o2 -> node12:i1
                }
            "#]],
            expect![[r##"
                digraph {
                    node0 [shape=Mrecord, ordering=in, label="{entry | {<o0> ctrl | <o1> ptr | <o2> i64}}", fillcolor="#ffd3e4", style="filled"]
                    node10 [shape=Mrecord, ordering=in, label="{iconst 1 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                    node1 [shape=Mrecord, ordering=in, label="{iconst 0 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                    node2 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                    node3 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                    node6 [shape=Mrecord, ordering=in, label="{iconst 3 | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                    node7 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | shl | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                    node8 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | ptroff | {<o0> ptr}}", fillcolor="#ffee9b", style="filled"]
                    node9 [shape=Mrecord, ordering=in, label="{{<i0> | <i1> | <i2>} | store | {<o0> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                    node13 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | brcond | {<o0> ctrl | <o1> ctrl}}", fillcolor="#ffd3e4", style="filled"]
                    node14 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                    node15 [shape=Mrecord, ordering=in, label="{{<i0>} | return}", fillcolor="#c2c2ff", style="filled"]
                    node4 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | region | {<o0> ctrl | <o1> phisel}}", fillcolor="#c2c2ff", style="filled"]
                    node5 [shape=Mrecord, ordering=in, label="{{<i0> | <i1> | <i2>} | phi | {<o0> i64}}", fillcolor="#dbdbdb", style="filled"]
                    node11 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | iadd | {<o0> i64}}", fillcolor="#ffee9b", style="filled"]
                    node12 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | icmp eq | {<o0> i32}}", fillcolor="#ffee9b", style="filled"]
                    node0:o2 -> node2:i0 [color="#d36805"]
                    node1:o0 -> node2:i1 [color="#d36805"]
                    node0:o0 -> node3:i0 [color="#0000ff", penwidth="2"]
                    node2:o0 -> node3:i1 [color="#d36805"]
                    node5:o0 -> node7:i0 [color="#d36805"]
                    node6:o0 -> node7:i1 [color="#d36805"]
                    node0:o1 -> node8:i0 [color="#d36805"]
                    node7:o0 -> node8:i1 [color="#d36805"]
                    node4:o0 -> node9:i0 [color="#0000ff", penwidth="2"]
                    node5:o0 -> node9:i1 [color="#d36805"]
                    node8:o0 -> node9:i2 [color="#d36805"]
                    node9:o0 -> node13:i0 [color="#0000ff", penwidth="2"]
                    node12:o0 -> node13:i1 [color="#d36805"]
                    node3:o0 -> node14:i0 [color="#0000ff", penwidth="2"]
                    node13:o0 -> node14:i1 [color="#0000ff", penwidth="2"]
                    node14:o0 -> node15:i0 [color="#0000ff", penwidth="2"]
                    node3:o1 -> node4:i0 [color="#0000ff", penwidth="2"]
                    node13:o1 -> node4:i1 [color="#0000ff", penwidth="2"]
                    node4:o1 -> node5:i0 [color="#4e4e4e"]
                    node1:o0 -> node5:i1 [color="#d36805"]
                    node11:o0 -> node5:i2 [color="#d36805"]
                    node5:o0 -> node11:i0 [color="#d36805"]
                    node10:o0 -> node11:i1 [color="#d36805"]
                    node11:o0 -> node12:i0 [color="#d36805"]
                    node0:o2 -> node12:i1 [color="#d36805"]
                }
            "##]],
        )
    }
}
