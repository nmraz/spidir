use core::iter;

use crate::{
    node::{BitwiseF64, DepValueKind, FunctionRef, IcmpKind, MemSize, NodeKind, Type},
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

pub trait Builder {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node;
    fn graph(&self) -> &ValGraph;
}

pub struct SimpleBuilder<'a>(pub &'a mut ValGraph);

impl<'a> Builder for SimpleBuilder<'a> {
    fn create_node(
        &mut self,
        kind: NodeKind,
        inputs: impl IntoIterator<Item = DepValue>,
        output_kinds: impl IntoIterator<Item = DepValueKind>,
    ) -> Node {
        self.0.create_node(kind, inputs, output_kinds)
    }

    fn graph(&self) -> &ValGraph {
        self.0
    }
}

pub trait BuilderExt: Builder {
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

    fn build_unreachable(&mut self, ctrl: DepValue) {
        self.create_node(NodeKind::Unreachable, [ctrl], []);
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
            iter::once(DepValueKind::Control).chain(ret_ty.into_iter().map(DepValueKind::Value)),
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
    fn build_fconst(&mut self, value: f64) -> DepValue {
        build_single_output_pure(self, NodeKind::FConst(BitwiseF64(value)), [], Type::F64)
    }

    fn build_iadd(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Iadd, lhs, rhs)
    }
    fn build_isub(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Isub, lhs, rhs)
    }
    fn build_and(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::And, lhs, rhs)
    }
    fn build_or(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Or, lhs, rhs)
    }
    fn build_xor(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Xor, lhs, rhs)
    }
    fn build_shl(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Shl, lhs, rhs)
    }
    fn build_lshr(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Lshr, lhs, rhs)
    }
    fn build_ashr(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Ashr, lhs, rhs)
    }
    fn build_imul(&mut self, lhs: DepValue, rhs: DepValue) -> DepValue {
        build_binop_with_lhs_type(self, NodeKind::Imul, lhs, rhs)
    }

    fn build_sdiv(&mut self, ctrl: DepValue, lhs: DepValue, rhs: DepValue) -> BuiltEffectful {
        build_int_div(self, NodeKind::Sdiv, ctrl, lhs, rhs)
    }
    fn build_udiv(&mut self, ctrl: DepValue, lhs: DepValue, rhs: DepValue) -> BuiltEffectful {
        build_int_div(self, NodeKind::Udiv, ctrl, lhs, rhs)
    }

    fn build_iext(&mut self, value: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Iext, [value], Type::I64)
    }
    fn build_itrunc(&mut self, value: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::Itrunc, [value], Type::I32)
    }

    fn build_sfill(&mut self, width: u8, value: DepValue) -> DepValue {
        let output_ty = get_input_ty(self, value);
        build_single_output_pure(self, NodeKind::Sfill(width), [value], output_ty)
    }

    fn build_icmp(
        &mut self,
        kind: IcmpKind,
        output_ty: Type,
        lhs: DepValue,
        rhs: DepValue,
    ) -> DepValue {
        build_single_output_pure(self, NodeKind::Icmp(kind), [lhs, rhs], output_ty)
    }

    fn build_ptroff(&mut self, ptr: DepValue, off: DepValue) -> DepValue {
        build_single_output_pure(self, NodeKind::PtrOff, [ptr, off], Type::Ptr)
    }

    fn build_load(
        &mut self,
        size: MemSize,
        ty: Type,
        ctrl: DepValue,
        ptr: DepValue,
    ) -> BuiltEffectful {
        let load = self.create_node(
            NodeKind::Load(size),
            [ctrl, ptr],
            [DepValueKind::Control, DepValueKind::Value(ty)],
        );
        let outputs = self.graph().node_outputs(load);
        BuiltEffectful {
            ctrl: outputs[0],
            output: outputs[1],
        }
    }
    fn build_store(
        &mut self,
        size: MemSize,
        ctrl: DepValue,
        data: DepValue,
        ptr: DepValue,
    ) -> DepValue {
        let store = self.create_node(
            NodeKind::Store(size),
            [ctrl, data, ptr],
            [DepValueKind::Control],
        );
        self.graph().node_outputs(store)[0]
    }

    fn build_stackslot(&mut self, size: u32, align: u32) -> DepValue {
        let addr = self.create_node(
            NodeKind::StackSlot { size, align },
            [],
            [DepValueKind::Value(Type::Ptr)],
        );
        self.graph().node_outputs(addr)[0]
    }
}

impl<F: Builder> BuilderExt for F {}

fn build_int_div(
    builder: &mut (impl Builder + ?Sized),
    kind: NodeKind,
    ctrl: DepValue,
    lhs: DepValue,
    rhs: DepValue,
) -> BuiltEffectful {
    let ty = get_input_ty(builder, lhs);
    let node = builder.create_node(
        kind,
        [ctrl, lhs, rhs],
        [DepValueKind::Control, DepValueKind::Value(ty)],
    );
    let outputs = builder.graph().node_outputs(node);
    BuiltEffectful {
        ctrl: outputs[0],
        output: outputs[1],
    }
}

fn build_binop_with_lhs_type(
    builder: &mut (impl Builder + ?Sized),
    kind: NodeKind,
    lhs: DepValue,
    rhs: DepValue,
) -> DepValue {
    let ty = get_input_ty(builder, lhs);
    build_single_output_pure(builder, kind, [lhs, rhs], ty)
}

fn build_single_output_pure(
    builder: &mut (impl Builder + ?Sized),
    kind: NodeKind,
    inputs: impl IntoIterator<Item = DepValue>,
    output_ty: Type,
) -> DepValue {
    let node = builder.create_node(kind, inputs, [DepValueKind::Value(output_ty)]);
    builder.graph().node_outputs(node)[0]
}

fn get_input_ty(builder: &mut (impl Builder + ?Sized), lhs: DepValue) -> Type {
    let ty = builder
        .graph()
        .value_kind(lhs)
        .as_value()
        .expect("input should be a value");
    ty
}
