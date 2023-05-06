use alloc::vec::Vec;
use core::fmt;

use crate::{
    valgraph::{Node, NodeKind, ValGraph},
    valwalk::PostOrder,
};

pub fn write_graph(w: &mut dyn fmt::Write, graph: &ValGraph, entry: Node) -> fmt::Result {
    let mut rpo: Vec<_> = PostOrder::with_entry(graph, entry).collect();
    rpo.reverse();

    for node in rpo {
        write_node(w, graph, node)?;
    }

    Ok(())
}

pub fn write_node(w: &mut dyn fmt::Write, graph: &ValGraph, node: Node) -> fmt::Result {
    let outputs = graph.node_outputs(node);

    if !outputs.is_empty() {
        let mut first = true;
        for output in outputs {
            if !first {
                w.write_str(", ")?;
            }
            first = false;
            write!(w, "%{}:{}", output.as_u32(), graph.value_kind(output))?;
        }
        w.write_str(" = ")?;
    }

    match graph.node_kind(node) {
        NodeKind::Entry => w.write_str("entry")?,
        NodeKind::Return => w.write_str("return")?,
        NodeKind::Region => w.write_str("region")?,
        NodeKind::Phi => w.write_str("phi")?,
        NodeKind::IConst(val) => write!(w, "iconst {val}")?,
        NodeKind::FConst(val) => write!(w, "fconst {val}")?,
        NodeKind::Iadd => w.write_str("iadd")?,
        NodeKind::Load => w.write_str("load")?,
        NodeKind::Store => w.write_str("store")?,
        NodeKind::BrCond => w.write_str("brcond")?,
        NodeKind::Call => w.write_str("call")?,
    };

    let mut first = true;
    for input in graph.node_inputs(node) {
        if first {
            w.write_str(" ")?;
        } else {
            w.write_str(", ")?;
        }
        first = false;
        write!(w, "%{}", input.as_u32())?;
    }
    writeln!(w)?;

    Ok(())
}

#[cfg(test)]
mod tests {

    use expect_test::{expect, Expect};

    use crate::valgraph::{DepValueKind, Type};

    use super::*;

    fn check_graph_write(graph: &ValGraph, entry: Node, expected: Expect) {
        let mut output = String::new();
        write_graph(&mut output, graph, entry).expect("failed to display graph");
        expected.assert_eq(&output);
    }

    #[test]
    fn write_add_params() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [
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
            [param1, param2],
            [DepValueKind::Value(Type::I32)],
        );
        let add_res = graph.node_outputs(add)[0];
        graph.create_node(NodeKind::Return, [control_value, add_res], []);

        check_graph_write(&graph, entry, expect![[r#"
            %0:ctrl, %1:val(i32), %2:val(i32) = entry
            %3:val(i32) = iadd %1, %2
            return %0, %3
        "#]]);
    }
}
