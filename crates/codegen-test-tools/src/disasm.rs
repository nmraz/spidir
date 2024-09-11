use std::fmt::Write;

use anyhow::{Context, Result};
use capstone::{
    arch::{
        x86::{ArchMode, ArchSyntax},
        BuildsCapstone, BuildsCapstoneSyntax,
    },
    Capstone,
};
use codegen::{
    emit::{CodeBlob, RelocKind},
    target::x64::{RELOC_ABS64, RELOC_PC32},
};
use ir::{module::Module, write::quote_ident};
use itertools::Itertools;

pub fn disasm_code(
    module: &Module,
    code: &CodeBlob,
    indent: usize,
    output: &mut String,
) -> Result<()> {
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

        let insn_code = &code.code[insn_start as usize..insn_end as usize];
        let code_bytes = format!("{:02x}", insn_code.iter().format(" "));

        let line = format!(
            "{:<31} {} {}",
            code_bytes,
            insn.mnemonic().unwrap(),
            insn.op_str().unwrap()
        );
        write!(output, "{:indent$}{:06x}: {}", "", insn_start, line.trim()).unwrap();

        // Note: this assumes there is at most one reloc per instruction.
        if let Some(reloc) = relocs.first() {
            if (reloc.offset as u64) < insn_end {
                write!(
                    output,
                    "  # {} -> @{} + {}",
                    reloc_name(reloc.kind),
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

pub(crate) fn reloc_name(reloc: RelocKind) -> String {
    match reloc {
        RELOC_PC32 => "RELOC_PC32".to_owned(),
        RELOC_ABS64 => "RELOC_ABS64".to_owned(),
        _ => format!("RELOC_{}", reloc.0),
    }
}
