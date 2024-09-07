use std::fmt::Write;

use anyhow::{anyhow, Result};
use codegen::{
    api::lower_func, frame::FrameLayout, machine::MachineCore, regalloc, target::x64::X64Machine,
};
use ir::{module::Module, write::display_node};
use itertools::Itertools;

use crate::utils::sanitize_raw_output;

use super::{update_per_func_output, TestProvider, Updater};

pub struct IselRegallocProvider;

impl TestProvider for IselRegallocProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let machine = X64Machine;

            let (cfg_ctx, lir) = lower_func(module, func, &machine).map_err(|err| {
                anyhow!(
                    "failed to select `{}`: `{}`",
                    func.metadata.name,
                    display_node(module, &func.body, err.node)
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
