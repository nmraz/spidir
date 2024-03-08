use crate::{cfg::CfgContext, lir::Lir, machine::MachineCore};

use self::context::RegAllocContext;

mod conflict;
mod context;
mod core_loop;
mod live_set;
mod liveness;
mod types;
mod utils;

pub fn run<M: MachineCore>(lir: &Lir<M>, cfg_ctx: &CfgContext, machine: &M) {
    let mut ctx = RegAllocContext::new(lir, cfg_ctx, machine);
    ctx.compute_liveness();
    ctx.build_live_sets();
    ctx.run_core_loop();
    ctx.dump();
}
