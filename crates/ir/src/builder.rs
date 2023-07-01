use core::iter;

use crate::{
    node::{DepValueKind, FunctionRef, IcmpKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
};

pub trait NodeFactory {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node;
    fn graph(&self) -> &ValGraph;
}

impl NodeFactory for ValGraph {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        self.create_node(kind, inputs, output_kinds)
    }

    fn graph(&self) -> &ValGraph {
        self
    }
}

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

pub fn build_entry(factory: &mut impl NodeFactory, types: &[Type]) -> BuiltEntry {
    let entry = factory.create_node(
        NodeKind::Entry,
        [],
        iter::once(DepValueKind::Control).chain(types.iter().map(|&ty| DepValueKind::Value(ty))),
    );

    BuiltEntry {
        node: entry,
        ctrl: factory.graph().node_outputs(entry)[0],
    }
}

pub fn build_return(factory: &mut impl NodeFactory, ctrl: DepValue, value: Option<DepValue>) {
    factory.create_node(NodeKind::Return, iter::once(ctrl).chain(value), []);
}

pub fn build_region(factory: &mut impl NodeFactory, incoming: &[DepValue]) -> BuiltRegion {
    let region = factory.create_node(
        NodeKind::Region,
        incoming.iter().copied(),
        [DepValueKind::Control, DepValueKind::PhiSelector],
    );
    let outputs = factory.graph().node_outputs(region);

    BuiltRegion {
        node: region,
        ctrl: outputs[0],
        phisel: outputs[1],
    }
}

pub fn build_brcond(factory: &mut impl NodeFactory, ctrl: DepValue, cond: DepValue) -> BuiltBrcond {
    let brcond = factory.create_node(
        NodeKind::BrCond,
        [ctrl, cond],
        [DepValueKind::Control, DepValueKind::Control],
    );
    let outputs = factory.graph().node_outputs(brcond);
    BuiltBrcond {
        true_ctrl: outputs[0],
        false_ctrl: outputs[1],
    }
}

pub fn build_call(
    factory: &mut impl NodeFactory,
    ret_ty: Option<Type>,
    func: FunctionRef,
    ctrl: DepValue,
    args: &[DepValue],
) -> BuiltCall {
    let call = factory.create_node(
        NodeKind::Call(func),
        iter::once(ctrl).chain(args.iter().copied()),
        ret_ty.into_iter().map(DepValueKind::Value),
    );
    let outputs = factory.graph().node_outputs(call);
    BuiltCall {
        ctrl: outputs[0],
        retval: outputs.get(1).copied(),
    }
}

pub fn build_phi(
    factory: &mut impl NodeFactory,
    ty: Type,
    selector: DepValue,
    incoming_values: &[DepValue],
) -> BuiltPhi {
    let phi = factory.create_node(
        NodeKind::Phi,
        iter::once(selector).chain(incoming_values.iter().copied()),
        [DepValueKind::Value(ty)],
    );
    BuiltPhi {
        node: phi,
        output: factory.graph().node_outputs(phi)[0],
    }
}

pub fn build_iconst(factory: &mut impl NodeFactory, ty: Type, value: u64) -> DepValue {
    build_single_output_pure(factory, NodeKind::IConst(value), [], ty)
}
pub fn build_fconst(factory: &mut impl NodeFactory, ty: Type, value: f64) -> DepValue {
    build_single_output_pure(factory, NodeKind::FConst(value), [], ty)
}

pub fn build_iadd(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Iadd, [a, b], ty)
}
pub fn build_isub(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Isub, [a, b], ty)
}
pub fn build_and(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::And, [a, b], ty)
}
pub fn build_or(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Or, [a, b], ty)
}
pub fn build_xor(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Xor, [a, b], ty)
}
pub fn build_shl(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Shl, [a, b], ty)
}
pub fn build_lshr(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Lshr, [a, b], ty)
}
pub fn build_ashr(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Ashr, [a, b], ty)
}
pub fn build_imul(factory: &mut impl NodeFactory, ty: Type, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::Imul, [a, b], ty)
}
pub fn build_icmp(
    factory: &mut impl NodeFactory,
    kind: IcmpKind,
    output_ty: Type,
    a: DepValue,
    b: DepValue,
) -> DepValue {
    build_single_output_pure(factory, NodeKind::Icmp(kind), [a, b], output_ty)
}

pub fn build_sdiv(
    factory: &mut impl NodeFactory,
    ty: Type,
    ctrl: DepValue,
    a: DepValue,
    b: DepValue,
) -> BuiltEffectful {
    build_int_div(factory, NodeKind::Sdiv, ctrl, a, b, ty)
}
pub fn build_udiv(
    factory: &mut impl NodeFactory,
    ty: Type,
    ctrl: DepValue,
    a: DepValue,
    b: DepValue,
) -> BuiltEffectful {
    build_int_div(factory, NodeKind::Udiv, ctrl, a, b, ty)
}

pub fn build_ptroff(factory: &mut impl NodeFactory, a: DepValue, b: DepValue) -> DepValue {
    build_single_output_pure(factory, NodeKind::PtrOff, [a, b], Type::Ptr)
}

pub fn build_load(
    factory: &mut impl NodeFactory,
    ty: Type,
    ctrl: DepValue,
    ptr: DepValue,
) -> BuiltEffectful {
    let load = factory.create_node(
        NodeKind::Load,
        [ctrl, ptr],
        [DepValueKind::Control, DepValueKind::Value(ty)],
    );
    let outputs = factory.graph().node_outputs(load);
    BuiltEffectful {
        ctrl: outputs[0],
        output: outputs[1],
    }
}
pub fn build_store(
    factory: &mut impl NodeFactory,
    ctrl: DepValue,
    data: DepValue,
    ptr: DepValue,
) -> DepValue {
    let store = factory.create_node(NodeKind::Store, [ctrl, data, ptr], [DepValueKind::Control]);
    factory.graph().node_outputs(store)[0]
}

fn build_int_div(
    factory: &mut impl NodeFactory,
    kind: NodeKind,
    ctrl: DepValue,
    a: DepValue,
    b: DepValue,
    output_ty: Type,
) -> BuiltEffectful {
    let node = factory.create_node(
        kind,
        [ctrl, a, b],
        [DepValueKind::Control, DepValueKind::Value(output_ty)],
    );
    let outputs = factory.graph().node_outputs(node);
    BuiltEffectful {
        ctrl: outputs[0],
        output: outputs[1],
    }
}

fn build_single_output_pure(
    factory: &mut impl NodeFactory,
    kind: NodeKind,
    inputs: impl IntoIterator<Item = DepValue>,
    output_ty: Type,
) -> DepValue {
    let node = factory.create_node(kind, inputs, [DepValueKind::Value(output_ty)]);
    factory.graph().node_outputs(node)[0]
}
