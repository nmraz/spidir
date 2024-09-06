use std::fmt::Write;

use anyhow::{anyhow, Context, Result};
use capstone::{
    arch::{
        x86::{ArchMode, ArchSyntax},
        BuildsCapstone, BuildsCapstoneSyntax,
    },
    Capstone,
};
use codegen::{api::codegen_func, emit::CodeBlob, target::x64::X64Machine};
use ir::{module::Module, write::quote_ident};

use crate::utils::sanitize_raw_output;

use super::{update_per_func_output, TestProvider, Updater};

pub struct CodegenProvider;

impl TestProvider for CodegenProvider {
    fn output_for(&self, module: &Module) -> Result<String> {
        let mut output = String::new();

        for func in module.functions.values() {
            writeln!(output, "function `{}`:", func.name).unwrap();

            let code = codegen_func(module, func, &X64Machine)
                .map_err(|err| anyhow!("codegen failed: {:?}", err))?;
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
