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
    cfg::CfgContext,
    emit::{emit_code, CodeBlob},
    isel::select_instrs,
    regalloc,
    schedule::Schedule,
    target::x86_64::X64Machine,
};
use ir::{
    module::Module,
    valwalk::cfg_preorder,
    write::{display_node, quote_ident},
};

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
            disasm_code(module, &code, &mut output)?;
        }

        Ok(output)
    }

    fn update(&self, updater: &mut Updater<'_>, _module: &Module, output_str: &str) -> Result<()> {
        update_per_func_output(updater, &sanitize_raw_output(output_str))
    }
}

fn disasm_code(module: &Module, code: &CodeBlob, output: &mut String) -> Result<()> {
    let cs = Capstone::new()
        .x86()
        .mode(ArchMode::Mode64)
        .syntax(ArchSyntax::Intel)
        .build()?;

    let insns = cs
        .disasm_all(code.code(), 0)
        .context("failed to disassemble")?;
    let mut relocs = code.relocs();

    for insn in insns.as_ref() {
        let insn_start = insn.address();
        let insn_end = insn.address() + insn.len() as u64;

        let line = format!(
            "{:#06x}: {} {}",
            insn_start,
            insn.mnemonic().unwrap(),
            insn.op_str().unwrap()
        );
        write!(output, "{}", line.trim()).unwrap();

        // Note: this assumes there is at most one reloc per instruction.
        if let Some(reloc) = relocs.first() {
            if (reloc.offset as u64) < insn_end {
                write!(
                    output,
                    "  # reloc <{}> -> @{} + {}",
                    reloc.kind.0,
                    quote_ident(module.resolve_funcref(reloc.target).name),
                    reloc.addend
                )
                .unwrap();
            }
        }

        writeln!(output).unwrap();

        // Skip past any relocs ending now.
        while relocs
            .first()
            .is_some_and(|reloc| (reloc.offset as u64) < insn_end)
        {
            relocs = &relocs[1..];
        }
    }

    Ok(())
}
