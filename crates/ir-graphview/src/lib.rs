#![cfg_attr(not(test), no_std)]

use core::fmt;

use ir::{
    module::Module,
    valgraph::{Node, ValGraph},
    valwalk::LiveNodeInfo,
    write::write_node_kind,
};

pub fn dump_ir_graphviz(
    w: &mut dyn fmt::Write,
    module: &Module,
    graph: &ValGraph,
    entry: Node,
) -> fmt::Result {
    let rpo = LiveNodeInfo::compute(graph, entry).reverse_postorder(graph);

    writeln!(w, "digraph {{")?;

    // First pass: write all nodes.
    for &node in &rpo {
        write!(w, r#"    {node} [shape=Mrecord, ordering=in, label="<l> "#)?;
        write_node_kind(w, module, graph.node_kind(node))?;

        for (i, val) in graph.node_outputs(node).into_iter().enumerate() {
            write!(w, " | <v{i}> {}", graph.value_kind(val))?;
        }

        writeln!(w, r#""]"#)?;
    }

    // Second pass: add edges.
    for &node in &rpo {
        // Note: the edges must be printed in input order in the actual graph, so that the
        // `ordering=in` attribute specified on all the nodes can actually guarantee that input
        // edges show up in the right order.
        for input in graph.node_inputs(node) {
            let (def_node, def_idx) = graph.value_def(input);
            writeln!(w, "    {def_node}:v{def_idx} -> {node}:l")?;
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
        dump_ir_graphviz(&mut graphviz, module, graph, entry).expect("failed to format IR graph");
        expected.assert_eq(&graphviz);
    }

    #[test]
    fn dump_simple_graph() {
        let mut module = Module::new();
        let func = module.functions.push(FunctionData::new(
            "my_func".to_owned(),
            Signature {
                ret_type: Some(Type::I32),
                arg_types: vec![Type::I64],
            },
        ));
        let extfunc = module.extern_functions.push(ExternFunctionData {
            name: "my_ext_func".to_owned(),
            sig: Signature {
                ret_type: Some(Type::I32),
                arg_types: vec![Type::I64],
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
                    node0 [shape=Mrecord, ordering=in, label="<l> entry | <v0> ctrl | <v1> val(i64)"]
                    node1 [shape=Mrecord, ordering=in, label="<l> call extfunc @my_ext_func | <v0> ctrl | <v1> val(i32)"]
                    node2 [shape=Mrecord, ordering=in, label="<l> return"]
                    node0:v0 -> node1:l
                    node0:v1 -> node1:l
                    node1:v0 -> node2:l
                    node1:v1 -> node2:l
                }
            "#]],
        );
    }
}
