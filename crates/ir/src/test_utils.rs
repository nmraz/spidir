use core::{array, iter};

use crate::{
    builder::{BuilderExt, SimpleBuilder},
    function::FunctionBody,
    node::{DepValueKind, IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

pub fn create_const_typed(graph: &mut ValGraph, ty: Type) -> DepValue {
    let node = graph.create_node(NodeKind::Iconst(5), [], [DepValueKind::Value(ty)]);
    graph.node_outputs(node)[0]
}

pub fn create_const32(graph: &mut ValGraph) -> DepValue {
    create_const_typed(graph, Type::I32)
}

pub fn create_entry<const N: usize>(
    graph: &mut ValGraph,
    input_types: [Type; N],
) -> (Node, DepValue, [DepValue; N]) {
    let node = graph.create_node(
        NodeKind::Entry,
        [],
        iter::once(DepValueKind::Control)
            .chain(input_types.iter().map(|&ty| DepValueKind::Value(ty))),
    );
    let entry_outputs = graph.node_outputs(node);

    (
        node,
        entry_outputs[0],
        array::from_fn(|i| entry_outputs[i + 1]),
    )
}

pub fn create_region<const N: usize>(graph: &mut ValGraph, inputs: [DepValue; N]) -> DepValue {
    let node = graph.create_node(
        NodeKind::Region,
        inputs,
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    graph.node_outputs(node)[0]
}

pub fn create_brcond(graph: &mut ValGraph, ctrl: DepValue, cond: DepValue) -> (DepValue, DepValue) {
    let node = graph.create_node(
        NodeKind::BrCond,
        [ctrl, cond],
        [DepValueKind::Control, DepValueKind::Control],
    );
    let outputs = graph.node_outputs(node);
    (outputs[0], outputs[1])
}

pub fn create_return<const N: usize>(graph: &mut ValGraph, inputs: [DepValue; N]) -> Node {
    graph.create_node(NodeKind::Return, inputs, [])
}

pub fn create_loop_body() -> FunctionBody {
    let mut body = FunctionBody::new(&[Type::I32]);
    let entry_ctrl = body.entry_ctrl();
    let param1 = body.param_value(0);

    let mut builder = SimpleBuilder(&mut body);

    // Loop preheader: skip loop if value is 0
    let zero = builder.build_iconst(Type::I32, 0);
    let pre_loop_cmp = builder.build_icmp(IcmpKind::Eq, Type::I32, param1, zero);

    let pre_loop_branch = builder.build_brcond(entry_ctrl, pre_loop_cmp);
    let loop_nontaken_ctrl = pre_loop_branch.true_ctrl;
    let loop_taken_ctrl = pre_loop_branch.false_ctrl;

    let loop_header = builder.build_region(&[loop_taken_ctrl]);
    let indvar_phi = builder.build_phi(Type::I32, loop_header.phisel, &[param1]);
    let sum_phi = builder.build_phi(Type::I32, loop_header.phisel, &[zero]);

    let one = builder.build_iconst(Type::I32, 1);
    let next_indvar = builder.build_isub(indvar_phi.output, one);
    let next_sum = builder.build_iadd(sum_phi.output, indvar_phi.output);

    // Loop latch: check if induction variable is 0, branch back to loop if not
    let latch_cmp = builder.build_icmp(IcmpKind::Eq, Type::I32, next_indvar, zero);
    let loop_latch_branch = builder.build_brcond(loop_header.ctrl, latch_cmp);
    let loop_exit_ctrl = loop_latch_branch.true_ctrl;
    let loop_backedge_ctrl = loop_latch_branch.false_ctrl;

    let exit_region = builder.build_region(&[loop_nontaken_ctrl, loop_exit_ctrl]);
    let ret_phi = builder.build_phi(Type::I32, exit_region.phisel, &[zero, next_sum]);
    builder.build_return(exit_region.ctrl, Some(ret_phi.output));

    // Hook up the backedge/phis
    let graph = &mut body.graph;
    graph.add_node_input(loop_header.node, loop_backedge_ctrl);
    graph.add_node_input(indvar_phi.node, next_indvar);
    graph.add_node_input(sum_phi.node, next_sum);

    body
}
