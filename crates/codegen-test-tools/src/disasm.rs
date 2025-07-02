use std::fmt::Write;

use anyhow::{Context, Result};
use capstone::{
    Capstone,
    arch::{
        BuildsCapstone, BuildsCapstoneSyntax,
        x86::{ArchMode, ArchSyntax},
    },
};
use codegen::{
    code_buffer::{CodeBlob, RelocKind, RelocTarget},
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
        let code_bytes = format_bytes(insn_code);

        let line = format!(
            "{:<31} {} {}",
            code_bytes,
            insn.mnemonic().unwrap(),
            insn.op_str().unwrap()
        );
        write!(output, "{:indent$}{:06x}: {}", "", insn_start, line.trim()).unwrap();

        // Note: this assumes there is at most one reloc per instruction.
        if let Some(reloc) = relocs.first() {
            let target_name = match reloc.target {
                RelocTarget::Function(func) => quote_ident(&module.resolve_funcref(func).name),
                RelocTarget::ConstantPool => "<CP>".into(),
            };

            if (reloc.offset as u64) < insn_end {
                write!(
                    output,
                    "  # {} -> @{} + {}",
                    reloc_name(reloc.kind),
                    target_name,
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

    if !code.constant_pool.is_empty() {
        writeln!(output, "<CP>:").unwrap();
        for (i, chunk) in code.constant_pool.chunks(CP_CHUNK_SIZE).enumerate() {
            let byte_offset = i * CP_CHUNK_SIZE;
            write!(
                output,
                "{:indent$}{byte_offset:06x}: {}",
                "",
                format_bytes(chunk)
            )
            .unwrap();
        }
    }

    Ok(())
}

fn format_bytes(bytes: &[u8]) -> String {
    format!("{:02x}", bytes.iter().format(" "))
}

fn reloc_name(reloc: RelocKind) -> String {
    match reloc {
        RELOC_PC32 => "RELOC_PC32".to_owned(),
        RELOC_ABS64 => "RELOC_ABS64".to_owned(),
        _ => format!("RELOC_{}", reloc.0),
    }
}

const CP_CHUNK_SIZE: usize = 8;
