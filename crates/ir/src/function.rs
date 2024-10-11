use alloc::{string::String, vec::Vec};
use core::{fmt, iter};

use cranelift_entity::{entity_impl, packed_option::ReservedValue, PrimaryMap};

use crate::{
    cache::NodeCache,
    node::{DepValueKind, NodeKind, Type},
    valgraph::{DepValue, Node, ValGraph},
    valwalk::{cfg_preorder, CfgPreorder, LiveNodeInfo},
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

    pub fn compute_live_nodes(&self) -> LiveNodeInfo {
        LiveNodeInfo::compute(&self.graph, self.entry)
    }

    pub fn cfg_preorder(&self) -> CfgPreorder<'_> {
        cfg_preorder(&self.graph, self.entry)
    }
}

#[derive(Clone)]
pub struct FunctionData {
    pub metadata: FunctionMetadata,
    pub body: FunctionBody,
    pub node_cache: NodeCache,
}

impl FunctionData {
    pub fn new(name: String, sig: Signature) -> Self {
        let body = FunctionBody::new(&sig.param_types);
        Self::from_metadata_body(FunctionMetadata { name, sig }, body)
    }

    pub fn from_metadata_body(metadata: FunctionMetadata, body: FunctionBody) -> Self {
        Self {
            metadata,
            body,
            node_cache: NodeCache::new(),
        }
    }
}
