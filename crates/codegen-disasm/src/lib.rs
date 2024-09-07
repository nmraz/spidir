use std::fmt::Write;

use anyhow::{Context, Result};
use capstone::{
    arch::{
        x86::{ArchMode, ArchSyntax},
        BuildsCapstone, BuildsCapstoneSyntax,
    },
    Capstone,
};
use codegen::emit::CodeBlob;
use ir::{module::Module, write::quote_ident};

pub fn disasm_code(module: &Module, code: &CodeBlob, output: &mut String) -> Result<()> {
    let cs = Capstone::new()
        .x86()
        .mode(ArchMode::Mode64)
        .syntax(ArchSyntax::Intel)
        .build()?;

    let insns = cs
        .disasm_all(&code.code, 0)
        .context("failed to disassemble")?;
    let mut relocs = &code.relocs[..];

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
                    quote_ident(&module.resolve_funcref(reloc.target).name),
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
