use ir::{builder::BuilderExt, valgraph::DepValue};

use crate::state::EditContext;

pub fn replace_with_iconst(ctx: &mut EditContext<'_>, value: DepValue, iconst: u64) {
    let ty = ctx.graph().value_kind(value).as_value().unwrap();
    let iconst = ctx.build_iconst(ty, iconst);
    ctx.replace_value(value, iconst);
}
