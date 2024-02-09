// TODO: Remove this once enough of the allocator is implemented.
#![allow(dead_code)]

use crate::{cfg::CfgContext, lir::Lir, machine::MachineCore};

use self::context::RegAllocContext;

mod coalesce;
mod context;
mod liveness;
mod types;

pub fn run<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext) {
    let mut ctx = RegAllocContext::new(lir, cfg_ctx);
    ctx.compute_liveness();
    ctx.coalesce_live_sets();
    ctx.dump();
}
