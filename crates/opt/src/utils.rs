use ir::{
    builder::BuilderExt,
    node::NodeKind,
    valgraph::{DepValue, ValGraph},
};
use valmatch::match_value;

use crate::state::EditContext;

pub fn match_iconst(graph: &ValGraph, value: DepValue) -> Option<u64> {
    match_value! {
        if let &NodeKind::Iconst(val) = graph, value {
            return Some(val);
        }
    }
    None
}

pub fn replace_with_iconst(ctx: &mut EditContext<'_>, value: DepValue, iconst: u64) {
    let ty = ctx.graph().value_kind(value).as_value().unwrap();
    let iconst = ctx.build_iconst(ty, iconst);
    ctx.replace_value(value, iconst);
}
