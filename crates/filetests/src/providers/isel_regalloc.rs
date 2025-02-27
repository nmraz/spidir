use std::fmt::Write;

use anyhow::{Result, anyhow};
use codegen::{
    api::lower_func, frame::FrameLayout, machine::MachineCore, regalloc, target::x64::X64Machine,
};
use ir::module::Module;
use itertools::Itertools;

use crate::{
    provider::{SimpleTestProvider, Updater, update_per_func_output},
    utils::sanitize_raw_output,
};

use super::x64::create_x64_machine;

pub struct IselRegallocProvider {
    machine: X64Machine,
    verify_regalloc: bool,
}

impl IselRegallocProvider {
    pub fn new(machine: X64Machine, verify_regalloc: bool) -> Self {
        Self {
            machine,
            verify_regalloc,
        }
    }

    pub fn from_params(mut params: &[&str]) -> Result<Self> {
        let mut verify_regalloc = true;
        if let ["no-verify-regalloc", rest @ ..] = params {
            verify_regalloc = false;
            params = rest;
        }

        let machine = create_x64_machine(params)?;
        Ok(Self::new(machine, verify_regalloc))
    }
}

impl SimpleTestProvider for IselRegallocProvider {
    fn output_for(&self, module: Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.metadata.name).unwrap();

            let (cfg_ctx, lir) = lower_func(&module, func, &self.machine).map_err(|e| {
                anyhow!(
                    "isel failed for `{}`: {}",
                    func.metadata.name,
                    e.display(&module, &func.body)
                )
            })?;
            let assignment = regalloc::run(&lir, &cfg_ctx, &self.machine)
                .map_err(|err| anyhow!("regalloc failed for `{}`: {err}", func.metadata.name))?;

            if self.verify_regalloc {
                regalloc::verify(&lir, &cfg_ctx, &assignment).map_err(|err| {
                    anyhow!(
                        "regalloc invalid for `{}`: {}",
                        func.metadata.name,
                        err.display(&lir, &assignment)
                    )
                })?;
            }

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

    fn update(&self, updater: &mut Updater<'_>, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}
