use std::fmt::Write;

use anyhow::{anyhow, Context, Result};
use capstone::{
    arch::{
        x86::{ArchMode, ArchSyntax},
        BuildsCapstone, BuildsCapstoneSyntax,
    },
    Capstone,
};
use codegen::{
    cfg::CfgContext, emit::emit_code, isel::select_instrs, regalloc, schedule::Schedule,
    target::x86_64::X64Machine,
};
use ir::{module::Module, valwalk::cfg_preorder, write::display_node};

use crate::utils::sanitize_raw_output;

use super::{update_per_func_output, TestProvider, Updater};

pub struct CodegenProvider;

impl TestProvider for CodegenProvider {
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

            let code = emit_code(&lir, &cfg_ctx, &assignment, &machine);
            disasm_code(&code, &mut output)?;
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}

fn disasm_code(code: &[u8], output: &mut String) -> Result<()> {
    let cs = Capstone::new()
        .x86()
        .mode(ArchMode::Mode64)
        .syntax(ArchSyntax::Intel)
        .build()?;

    let insns = cs.disasm_all(code, 0).context("failed to disassemble")?;
    for insn in insns.as_ref() {
        let line = format!(
            "{:#06x}: {} {}",
            insn.address(),
            insn.mnemonic().unwrap(),
            insn.op_str().unwrap()
        );
        writeln!(output, "{}", line.trim()).unwrap();
    }

    Ok(())
}
