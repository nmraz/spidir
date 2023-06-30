#![cfg_attr(not(test), no_std)]

use core::fmt;

use ir::{
    module::Module,
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
    write::write_node_kind,
};

pub fn write_graphviz(
    w: &mut dyn fmt::Write,
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

        writeln!(w, r#"}}"]"#)?;
    }

    // Second pass: add edges.
    for &node in &rpo {
        // Note: the edges must be printed in input order in the actual graph, so that the
        // `ordering=in` attribute specified on all the nodes can actually guarantee that input
        // edges show up in the right order.
        for (input_idx, input) in graph.node_inputs(node).into_iter().enumerate() {
            let (def_node, def_idx) = graph.value_def(input);
            writeln!(w, "    {def_node}:o{def_idx} -> {node}:i{input_idx}")?;
        }
    }

    writeln!(w, "}}")?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use expect_test::{expect, Expect};
    use ir::{
        module::{ExternFunctionData, FunctionData, Signature},
        node::{DepValueKind, FunctionRef, NodeKind, Type},
    };

    use super::*;

    #[track_caller]
    fn check_dump_graphviz(module: &Module, graph: &ValGraph, entry: Node, expected: Expect) {
        let mut graphviz = String::new();
        write_graphviz(&mut graphviz, module, graph, entry).expect("failed to format IR graph");
        expected.assert_eq(&graphviz);
    }

    #[test]
    fn dump_simple_graph() {
        let mut module = Module::new();
        let func = module.functions.push(FunctionData::new(
            "my_func".to_owned(),
            Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        ));
        let extfunc = module.extern_functions.push(ExternFunctionData {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                param_types: vec![Type::I64],
            },
        });

        let func_entry = module.functions[func].entry_node;
        let func_graph = &mut module.functions[func].valgraph;

        let func_entry_outputs = func_graph.node_outputs(func_entry);
        let func_entry_ctrl = func_entry_outputs[0];
        let func_param1 = func_entry_outputs[1];

        let call = func_graph.create_node(
            NodeKind::Call(FunctionRef::External(extfunc)),
            [func_entry_ctrl, func_param1],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );

        let call_outputs = func_graph.node_outputs(call);
        let call_ctrl = call_outputs[0];
        let call_retval = call_outputs[1];

        func_graph.create_node(NodeKind::Return, [call_ctrl, call_retval], []);

        check_dump_graphviz(
            &module,
            &module.functions[func].valgraph,
            func_entry,
            expect![[r#"
                digraph {
                    node0 [shape=Mrecord, ordering=in, label="{entry | {<o0> ctrl | <o1> i64}}"]
                    node1 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | call extfunc @my_ext_func | {<o0> ctrl | <o1> i32}}"]
                    node2 [shape=Mrecord, ordering=in, label="{{<i0> | <i1>} | return}"]
                    node0:o0 -> node1:i0
                    node0:o1 -> node1:i1
                    node1:o0 -> node2:i0
                    node1:o1 -> node2:i1
                }
            "#]],
        );
    }
}
