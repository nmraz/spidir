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
        NodeKind::Isub => w.write_str("isub")?,
        NodeKind::Icmp(kind) => write!(w, "icmp {kind}")?,
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

    use crate::valgraph::{DepValueKind, IcmpKind, Type};

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

        check_graph_write(
            &graph,
            entry,
            expect![[r#"
            %0:ctrl, %1:val(i32), %2:val(i32) = entry
            %3:val(i32) = iadd %1, %2
            return %0, %3
        "#]],
        );
    }

    #[test]
    fn write_loop() {
        let mut graph = ValGraph::new();
        let entry = graph.create_node(
            NodeKind::Entry,
            [],
            [DepValueKind::Control, DepValueKind::Value(Type::I32)],
        );
        let entry_outputs = graph.node_outputs(entry);
        let entry_control = entry_outputs[0];
        let param1 = entry_outputs[1];

        // Loop preheader: skip loop if value is 0
        let zero = graph.create_node(NodeKind::IConst(0), [], [DepValueKind::Value(Type::I32)]);
        let zero_val = graph.node_outputs(zero)[0];
        let pre_loop_cmp = graph.create_node(
            NodeKind::Icmp(IcmpKind::Eq),
            [param1, zero_val],
            [DepValueKind::Value(Type::I32)],
        );
        let pre_loop_cmp_val = graph.node_outputs(pre_loop_cmp)[0];

        let pre_loop_branch = graph.create_node(
            NodeKind::BrCond,
            [entry_control, pre_loop_cmp_val],
            [DepValueKind::Control, DepValueKind::Control],
        );
        let pre_loop_branch_outputs = graph.node_outputs(pre_loop_branch);
        let loop_nontaken_ctrl = pre_loop_branch_outputs[0];
        let loop_taken_ctrl = pre_loop_branch_outputs[1];

        // Loop body: add current induction variable value to running sum, decrement by 1
        let loop_header = graph.create_node(
            NodeKind::Region,
            [loop_taken_ctrl],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let loop_header_outputs = graph.node_outputs(loop_header);
        let loop_ctrl = loop_header_outputs[0];
        let loop_phisel = loop_header_outputs[1];

        let indvar_phi = graph.create_node(
            NodeKind::Phi,
            [loop_phisel, param1],
            [DepValueKind::Value(Type::I32)],
        );
        let indvar_phi_val = graph.node_outputs(indvar_phi)[0];
        let sum_phi = graph.create_node(
            NodeKind::Phi,
            [loop_phisel, zero_val],
            [DepValueKind::Value(Type::I32)],
        );
        let sum_phi_val = graph.node_outputs(sum_phi)[0];

        let one = graph.create_node(NodeKind::IConst(1), [], [DepValueKind::Value(Type::I32)]);
        let one_val = graph.node_outputs(one)[0];
        let next_indvar = graph.create_node(
            NodeKind::Isub,
            [indvar_phi_val, one_val],
            [DepValueKind::Value(Type::I32)],
        );
        let next_indvar_val = graph.node_outputs(next_indvar)[0];

        let next_sum = graph.create_node(
            NodeKind::Iadd,
            [sum_phi_val, indvar_phi_val],
            [DepValueKind::Value(Type::I32)],
        );
        let next_sum_val = graph.node_outputs(next_sum)[0];

        // Loop latch: check if induction variable is 0, branch back to loop if not
        let latch_cmp = graph.create_node(
            NodeKind::Icmp(IcmpKind::Eq),
            [next_indvar_val, zero_val],
            [DepValueKind::Value(Type::I32)],
        );
        let latch_cmp_val = graph.node_outputs(latch_cmp)[0];

        let loop_latch_branch = graph.create_node(
            NodeKind::BrCond,
            [loop_ctrl, latch_cmp_val],
            [DepValueKind::Control, DepValueKind::Control],
        );
        let loop_latch_branch_outputs = graph.node_outputs(loop_latch_branch);
        let loop_exit_ctrl = loop_latch_branch_outputs[0];
        let loop_backedge_ctrl = loop_latch_branch_outputs[1];

        let exit_region = graph.create_node(
            NodeKind::Region,
            [loop_nontaken_ctrl, loop_exit_ctrl],
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let exit_region_outputs = graph.node_outputs(exit_region);
        let exit_region_ctrl = exit_region_outputs[0];
        let exit_region_phisel = exit_region_outputs[1];

        let ret_phi = graph.create_node(
            NodeKind::Phi,
            [exit_region_phisel, zero_val, next_sum_val],
            [DepValueKind::Value(Type::I32)],
        );
        let ret_phi_val = graph.node_outputs(ret_phi)[0];
        graph.create_node(NodeKind::Return, [exit_region_ctrl, ret_phi_val], []);

        // Hook up the backedge/phis
        graph.add_node_input(loop_header, loop_backedge_ctrl);
        graph.add_node_input(indvar_phi, next_indvar_val);
        graph.add_node_input(sum_phi, next_sum_val);

        check_graph_write(
            &graph,
            entry,
            expect![[r#"
                %0:ctrl, %1:val(i32) = entry
                %10:val(i32) = iconst 1
                %2:val(i32) = iconst 0
                %3:val(i32) = icmp eq %1, %2
                %4:ctrl, %5:ctrl = brcond %0, %3
                %13:val(i32) = icmp eq %11, %2
                %14:ctrl, %15:ctrl = brcond %6, %13
                %16:ctrl, %17:phisel = region %4, %14
                %6:ctrl, %7:phisel = region %5, %15
                %8:val(i32) = phi %7, %1, %11
                %11:val(i32) = isub %8, %10
                %9:val(i32) = phi %7, %2, %12
                %12:val(i32) = iadd %9, %8
                %18:val(i32) = phi %17, %2, %12
                return %16, %18
            "#]],
        );
    }
}
