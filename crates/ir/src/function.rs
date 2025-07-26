use alloc::{string::String, vec::Vec};
use core::{fmt, iter};

use cranelift_entity::{PrimaryMap, entity_impl, packed_option::ReservedValue};

use crate::{
    cache::NodeCache,
    node::{DepValueKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{CfgPreorderInfo, GraphWalkInfo},
    write::write_function_metadata,
};

#[derive(Debug, Clone)]
pub struct Signature {
    pub ret_type: Option<Type>,
    pub param_types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionMetadata {
    pub name: String,
    pub sig: Signature,
}

impl fmt::Display for FunctionMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_function_metadata(f, self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SignatureRef(u32);
entity_impl!(SignatureRef, "sig");

#[derive(Clone)]
pub struct FunctionBody {
    pub graph: ValGraph,
    pub entry: Node,
    pub call_ind_sigs: PrimaryMap<SignatureRef, Signature>,
}

impl FunctionBody {
    pub fn new_invalid() -> Self {
        Self {
            graph: ValGraph::new(),
            entry: Node::reserved_value(),
            call_ind_sigs: PrimaryMap::new(),
        }
    }

    pub fn new(param_types: &[Type]) -> Self {
        let mut body = Self::new_invalid();

        let entry = body.graph.create_node(
            NodeKind::Entry,
            [],
            iter::once(DepValueKind::Control)
                .chain(param_types.iter().map(|&ty| DepValueKind::Value(ty))),
        );
        body.entry = entry;

        body
    }

    pub fn entry_ctrl(&self) -> DepValue {
        self.graph.node_outputs(self.entry)[0]
    }

    pub fn param_value(&self, index: u32) -> DepValue {
        self.graph.node_outputs(self.entry)[index as usize + 1]
    }

    pub fn compute_full_walk_info(&self) -> GraphWalkInfo {
        GraphWalkInfo::compute_full(&self.graph, self.entry)
    }

    pub fn compute_cfg_live_walk_info(&self, cfg_preorder: &CfgPreorderInfo) -> GraphWalkInfo {
        GraphWalkInfo::compute_cfg_live(&self.graph, cfg_preorder)
    }

    pub fn compute_cfg_preorder_info(&self) -> CfgPreorderInfo {
        CfgPreorderInfo::compute(&self.graph, self.entry)
    }
}

#[derive(Clone)]
pub struct FunctionData {
    pub body: FunctionBody,
    pub node_cache: NodeCache,
}

impl FunctionData {
    pub fn new_invalid() -> Self {
        Self::from_body(FunctionBody::new_invalid())
    }

    pub fn new(param_types: &[Type]) -> Self {
        Self::from_body(FunctionBody::new(param_types))
    }

    pub fn from_body(body: FunctionBody) -> Self {
        Self {
            body,
            node_cache: NodeCache::new(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct FunctionBorrow<'a> {
    pub data: &'a FunctionData,
    pub metadata: &'a FunctionMetadata,
}

impl<'a> FunctionBorrow<'a> {
    #[inline]
    pub fn body(&self) -> &'a FunctionBody {
        &self.data.body
    }
}
