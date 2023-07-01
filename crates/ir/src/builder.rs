use core::iter;

use crate::{
    node::{DepValueKind, FunctionRef, IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

pub struct BuiltEntry {
    pub node: Node,
    pub ctrl: DepValue,
}

pub struct BuiltRegion {
    pub node: Node,
    pub ctrl: DepValue,
    pub phisel: DepValue,
}

pub struct BuiltBrcond {
    pub true_ctrl: DepValue,
    pub false_ctrl: DepValue,
}

pub struct BuiltCall {
    pub ctrl: DepValue,
    pub retval: Option<DepValue>,
}

pub struct BuiltPhi {
    pub node: Node,
    pub output: DepValue,
}

pub struct BuiltEffectful {
    pub ctrl: DepValue,
    pub output: DepValue,
}

pub fn build_entry(graph: &mut ValGraph, types: &[Type]) -> BuiltEntry {
    let entry = graph.create_node(
        NodeKind::Entry,
        [],
        iter::once(DepValueKind::Control).chain(types.iter().map(|&ty| DepValueKind::Value(ty))),
    );

    BuiltEntry {
        node: entry,
        ctrl: graph.node_outputs(entry)[0],
    }
}

pub fn build_return(graph: &mut ValGraph, ctrl: DepValue, value: Option<DepValue>) {
    graph.create_node(NodeKind::Return, iter::once(ctrl).chain(value), []);
}

pub fn build_region(graph: &mut ValGraph, incoming: &[DepValue]) -> BuiltRegion {
    let region = graph.create_node(
        NodeKind::Region,
        incoming.iter().copied(),
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    let outputs = graph.node_outputs(region);

    BuiltRegion {
        node: region,
        ctrl: outputs[0],
        phisel: outputs[1],
    }
}

pub fn build_brcond(graph: &mut ValGraph, ctrl: DepValue, cond: DepValue) -> BuiltBrcond {
    let brcond = graph.create_node(
        NodeKind::BrCond,
        [ctrl, cond],
        [DepValueKind::Control, DepValueKind::Control],
    );
    let outputs = graph.node_outputs(brcond);
    BuiltBrcond {
        true_ctrl: outputs[0],
        false_ctrl: outputs[1],
    }
}

pub fn build_call(
    graph: &mut ValGraph,
    ret_ty: Option<Type>,
    func: FunctionRef,
    ctrl: DepValue,
    args: &[DepValue],
) -> BuiltCall {
    let call = graph.create_node(
        NodeKind::Call(func),
        iter::once(ctrl).chain(args.iter().copied()),
        ret_ty.into_iter().map(DepValueKind::Value),
    );
    let outputs = graph.node_outputs(call);
    BuiltCall {
        ctrl: outputs[0],
        retval: outputs.get(1).copied(),
    }
}

pub fn build_phi(
    graph: &mut ValGraph,
    ty: Type,
    selector: DepValue,
    incoming_values: &[DepValue],
) -> BuiltPhi {
    let phi = graph.create_node(
        NodeKind::Phi,
        iter::once(selector).chain(incoming_values.iter().copied()),
        [DepValueKind::Value(ty)],
    );
    BuiltPhi {
        node: phi,
        output: graph.node_outputs(phi)[0],
    }
}

pub fn build_iconst(graph: &mut ValGraph, ty: Type, value: u64) -> DepValue {
    build_single_output_pure(graph, NodeKind::IConst(value), [], ty)
}
pub fn build_fconst(graph: &mut ValGraph, ty: Type, value: f64) -> DepValue {
    build_single_output_pure(graph, NodeKind::FConst(value), [], ty)
}

pub fn build_iadd(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Iadd, [a, b], ty)
}
pub fn build_isub(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Isub, [a, b], ty)
}
pub fn build_and(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::And, [a, b], ty)
}
pub fn build_or(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Or, [a, b], ty)
}
pub fn build_xor(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Xor, [a, b], ty)
}
pub fn build_shl(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Shl, [a, b], ty)
}
pub fn build_lshr(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Lshr, [a, b], ty)
}
pub fn build_ashr(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Ashr, [a, b], ty)
}
pub fn build_imul(graph: &mut ValGraph, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::Imul, [a, b], ty)
}
pub fn build_icmp(
    graph: &mut ValGraph,
    kind: IcmpKind,
    output_ty: Type,
    a: DepValue,
    b: DepValue,
) -> DepValue {
    build_single_output_pure(graph, NodeKind::Icmp(kind), [a, b], output_ty)
}

pub fn build_sdiv(
    graph: &mut ValGraph,
    ty: Type,
    ctrl: DepValue,
    a: DepValue,
    b: DepValue,
) -> BuiltEffectful {
    build_int_div(graph, NodeKind::Sdiv, ctrl, a, b, ty)
}
pub fn build_udiv(
    graph: &mut ValGraph,
    ty: Type,
    ctrl: DepValue,
    a: DepValue,
    b: DepValue,
) -> BuiltEffectful {
    build_int_div(graph, NodeKind::Udiv, ctrl, a, b, ty)
}

pub fn build_ptroff(graph: &mut ValGraph, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(graph, NodeKind::PtrOff, [a, b], Type::Ptr)
}

pub fn build_load(graph: &mut ValGraph, ty: Type, ctrl: DepValue, ptr: DepValue) -> BuiltEffectful {
    let load = graph.create_node(
        NodeKind::Load,
        [ctrl, ptr],
        [DepValueKind::Control, DepValueKind::Value(ty)],
    );
    let outputs = graph.node_outputs(load);
    BuiltEffectful {
        ctrl: outputs[0],
        output: outputs[1],
    }
}
pub fn build_store(
    graph: &mut ValGraph,
    ctrl: DepValue,
    data: DepValue,
    ptr: DepValue,
) -> DepValue {
    let store = graph.create_node(NodeKind::Store, [ctrl, data, ptr], [DepValueKind::Control]);
    graph.node_outputs(store)[0]
}

fn build_int_div(
    graph: &mut ValGraph,
    kind: NodeKind,
    ctrl: DepValue,
    a: DepValue,
    b: DepValue,
    output_ty: Type,
) -> BuiltEffectful {
    let node = graph.create_node(
        kind,
        [ctrl, a, b],
        [DepValueKind::Control, DepValueKind::Value(output_ty)],
    );
    let outputs = graph.node_outputs(node);
    BuiltEffectful {
        ctrl: outputs[0],
        output: outputs[1],
    }
}

fn build_single_output_pure(
    graph: &mut ValGraph,
    kind: NodeKind,
    inputs: impl IntoIterator<Item = DepValue>,
    output_ty: Type,
) -> DepValue {
    let node = graph.create_node(kind, inputs, [DepValueKind::Value(output_ty)]);
    graph.node_outputs(node)[0]
}
