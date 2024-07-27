use std::fmt::Write;

use anyhow::{anyhow, Result};
use codegen::{
    cfg::CfgContext, frame::FrameLayout, isel::select_instrs, machine::MachineCore, regalloc,
    schedule::Schedule, target::x86_64::X64Machine,
};
use ir::{module::Module, valwalk::cfg_preorder, write::display_node};
use itertools::Itertools;

use crate::utils::sanitize_raw_output;

use super::{update_per_func_output, TestProvider, Updater};

pub struct IselRegallocProvider;

impl TestProvider for IselRegallocProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.name).unwrap();

            let cfg_preorder: Vec<_> = cfg_preorder(&func.graph, func.entry).collect();
            let (cfg_ctx, block_map) = CfgContext::compute_for_valgraph(&func.graph, &cfg_preorder);
            let schedule = Schedule::compute(&func.graph, &cfg_preorder, &cfg_ctx, &block_map);
            let machine = X64Machine;
            let lir = select_instrs(module, func, &schedule, &cfg_ctx, &block_map, &machine)
                .map_err(|err| {
                    anyhow!(
                        "failed to select `{}`: `{}`",
                        func.name,
                        display_node(module, &func.graph, err.node)
                    )
                })?;
            let assignment = regalloc::run(&lir, &cfg_ctx, &machine)
                .map_err(|err| anyhow!("register allocation failed: {err:?}"))?;

            writeln!(
                output,
                "clobbers: {}",
                assignment
                    .compute_global_clobbers(&lir)
                    .iter()
                    .map(X64Machine::reg_name)
                    .format(", ")
            )
            .unwrap();

            let frame_layout = FrameLayout::compute(&lir, &assignment);
            writeln!(
                output,
                "frame: size {}, align {}",
                frame_layout.full_layout.size, frame_layout.full_layout.align
            )
            .unwrap();
            for (slot, &offset) in frame_layout.stack_slot_offsets.iter() {
                writeln!(output, "    {slot}: {offset}").unwrap();
            }
            for (spill, &offset) in frame_layout.spill_slot_offsets.iter() {
                writeln!(output, "    {spill}: {offset}").unwrap();
            }

            write!(output, "{}", assignment.display(&cfg_ctx.block_order, &lir)).unwrap();
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}
