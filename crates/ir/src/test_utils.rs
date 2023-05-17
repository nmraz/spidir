use crate::{
    node::{DepValueKind, IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

pub fn create_const_typed(graph: &mut ValGraph, ty: Type) -> DepValue {
    let const_node = graph.create_node(NodeKind::IConst(5), [], [DepValueKind::Value(ty)]);
    graph.node_outputs(const_node)[0]
}

pub fn create_const32(graph: &mut ValGraph) -> DepValue {
    create_const_typed(graph, Type::I32)
}

pub fn create_const64(graph: &mut ValGraph) -> DepValue {
    create_const_typed(graph, Type::I64)
}

pub fn create_region<const N: usize>(graph: &mut ValGraph, inputs: [DepValue; N]) -> DepValue {
    let region = graph.create_node(
        NodeKind::Region,
        inputs,
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    graph.node_outputs(region)[0]
}

pub fn create_loop_graph() -> (ValGraph, Node) {
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

    (graph, entry)
}
