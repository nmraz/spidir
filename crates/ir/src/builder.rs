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

pub trait NodeFactoryExt: NodeFactory {
    fn build_entry(&mut self, types: &[Type]) -> BuiltEntry {
        let entry = self.create_node(
            NodeKind::Entry,
            [],
            iter::once(DepValueKind::Control)
                .chain(types.iter().map(|&ty| DepValueKind::Value(ty))),
        );

        BuiltEntry {
            node: entry,
            ctrl: self.graph().node_outputs(entry)[0],
        }
    }

    fn build_return(&mut self, ctrl: DepValue, value: Option<DepValue>) {
        self.create_node(NodeKind::Return, iter::once(ctrl).chain(value), []);
    }

    fn build_region(&mut self, incoming: &[DepValue]) -> BuiltRegion {
        let region = self.create_node(
            NodeKind::Region,
            incoming.iter().copied(),
            [DepValueKind::Control, DepValueKind::PhiSelector],
        );
        let outputs = self.graph().node_outputs(region);

        BuiltRegion {
            node: region,
            ctrl: outputs[0],
            phisel: outputs[1],
        }
    }

    fn build_brcond(&mut self, ctrl: DepValue, cond: DepValue) -> BuiltBrcond {
        let brcond = self.create_node(
            NodeKind::BrCond,
            [ctrl, cond],
            [DepValueKind::Control, DepValueKind::Control],
        );
        let outputs = self.graph().node_outputs(brcond);
        BuiltBrcond {
            true_ctrl: outputs[0],
            false_ctrl: outputs[1],
        }
    }

    fn build_call(
        &mut self,
        ret_ty: Option<Type>,
        func: FunctionRef,
        ctrl: DepValue,
        args: &[DepValue],
    ) -> BuiltCall {
        let call = self.create_node(
            NodeKind::Call(func),
            iter::once(ctrl).chain(args.iter().copied()),
            ret_ty.into_iter().map(DepValueKind::Value),
        );
        let outputs = self.graph().node_outputs(call);
        BuiltCall {
            ctrl: outputs[0],
            retval: outputs.get(1).copied(),
        }
    }

    fn build_phi(
        &mut self,
        ty: Type,
        selector: DepValue,
        incoming_values: &[DepValue],
    ) -> BuiltPhi {
        let phi = self.create_node(
            NodeKind::Phi,
            iter::once(selector).chain(incoming_values.iter().copied()),
            [DepValueKind::Value(ty)],
        );
        BuiltPhi {
            node: phi,
            output: self.graph().node_outputs(phi)[0],
        }
    }

    fn build_iconst(&mut self, ty: Type, value: u64) -> DepValue {
        build_single_output_pure(self, NodeKind::IConst(value), [], ty)
    }
    fn build_fconst(&mut self, ty: Type, value: f64) -> DepValue {
        build_single_output_pure(self, NodeKind::FConst(value), [], ty)
    }

    fn build_iadd(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Iadd, [a, b], ty)
    }
    fn build_isub(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Isub, [a, b], ty)
    }
    fn build_and(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::And, [a, b], ty)
    }
    fn build_or(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Or, [a, b], ty)
    }
    fn build_xor(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Xor, [a, b], ty)
    }
    fn build_shl(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Shl, [a, b], ty)
    }
    fn build_lshr(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Lshr, [a, b], ty)
    }
    fn build_ashr(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Ashr, [a, b], ty)
    }
    fn build_imul(&mut self, ty: Type, a: DepValue, b: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Imul, [a, b], ty)
    }
    fn build_icmp(
        &mut self,
        kind: IcmpKind,
        output_ty: Type,
        a: DepValue,
        b: DepValue,
    ) -> DepValue {
        build_single_output_pure(self, NodeKind::Icmp(kind), [a, b], output_ty)
    }

    fn build_sdiv(&mut self, ty: Type, ctrl: DepValue, a: DepValue, b: DepValue) -> BuiltEffectful {
        build_int_div(self, NodeKind::Sdiv, ctrl, a, b, ty)
    }
    fn build_udiv(&mut self, ty: Type, ctrl: DepValue, a: DepValue, b: DepValue) -> BuiltEffectful {
        build_int_div(self, NodeKind::Udiv, ctrl, a, b, ty)
    }

    fn build_ptroff(&mut self, ptr: DepValue, off: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::PtrOff, [ptr, off], Type::Ptr)
    }

    fn build_load(&mut self, ty: Type, ctrl: DepValue, ptr: DepValue) -> BuiltEffectful {
        let load = self.create_node(
            NodeKind::Load,
            [ctrl, ptr],
            [DepValueKind::Control, DepValueKind::Value(ty)],
        );
        let outputs = self.graph().node_outputs(load);
        BuiltEffectful {
            ctrl: outputs[0],
            output: outputs[1],
        }
    }
    fn build_store(&mut self, ctrl: DepValue, data: DepValue, ptr: DepValue) -> DepValue {
        let store = self.create_node(NodeKind::Store, [ctrl, data, ptr], [DepValueKind::Control]);
        self.graph().node_outputs(store)[0]
    }
}

impl<F: NodeFactory> NodeFactoryExt for F {}

fn build_int_div(
    factory: &mut (impl NodeFactory + ?Sized),
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
    factory: &mut (impl NodeFactory + ?Sized),
    kind: NodeKind,
    inputs: impl IntoIterator<Item = DepValue>,
    output_ty: Type,
) -> DepValue {
    let node = factory.create_node(kind, inputs, [DepValueKind::Value(output_ty)]);
    factory.graph().node_outputs(node)[0]
}
