use entity_utils::{set::DenseEntitySet, worklist::Worklist};
use ir::builder::Builder;

use crate::state::EditContext;

pub fn do_sccp(ctx: &mut EditContext) {}

enum ConstantState {
    SomeConstant,
    Constant(u64),
    NotConstant,
}
