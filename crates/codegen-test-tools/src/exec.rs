use std::mem;

use anyhow::{Context, Result, anyhow, bail, ensure};
use codegen::{
    api::{Codegen, CodegenOpts},
    code_buffer::{CodeBlob, RelocKind, RelocTarget},
    target::x64::{CodeModel, RELOC_ABS64, RELOC_PC32, X64Machine, X64MachineConfig},
};
use cranelift_entity::SecondaryMap;
use fx_utils::FxHashMap;
use ir::{
    module::{ExternFunction, Function, Module},
    node::FunctionRef,
};

use crate::jit_buf::JitBuf;

/// Generates native code for `module` and runs `entry_point` with the specified arguments.
///
/// # Safety
///
/// This function generates and runs untrusted code, which can cause arbitrary UB/memory corruption.
///
/// When `break_at_entry` is set, it will also trigger a breakpoint.
pub unsafe fn codegen_and_exec(
    module: &Module,
    entry_point: Function,
    args: &[isize],
    break_at_entry: bool,
) -> Result<isize> {
    let mut code_blobs = SecondaryMap::with_capacity(module.functions.len());

    for func in module.functions.keys() {
        code_blobs[func] = codegen_func(module, func)?;
    }

    let mut func_offsets = SecondaryMap::with_capacity(module.functions.len());
    let mut code_size = 0;
    let mut constpool_align = 1;
    let mut constpool_size = 0;

    for (func, code) in code_blobs.iter() {
        code_size = pad_code_offset(code_size).ok_or_else(|| anyhow!("code too large"))?;
        constpool_size = try_align_up(constpool_size, code.constpool_align)
            .ok_or_else(|| anyhow!("constant pool too large"))?;

        func_offsets[func] = FuncOffsets {
            code_offset: code_size,
            constpool_offset: constpool_size,
        };

        code_size = code_size
            .checked_add(code.code.len().try_into().context("code too large")?)
            .ok_or_else(|| anyhow!("code too large"))?;

        constpool_size = constpool_size
            .checked_add(
                code.constpool
                    .len()
                    .try_into()
                    .context("constant pool too large")?,
            )
            .ok_or_else(|| anyhow!("constant pool too large"))?;
        constpool_align = constpool_align.max(code.constpool_align);
    }

    let constpool_off =
        try_align_up(code_size, constpool_align).ok_or_else(|| anyhow!("code too large"))?;
    let total_size = constpool_off
        .checked_add(constpool_size)
        .ok_or_else(|| anyhow!("final blob too large"))?;

    let mut jit_buf = JitBuf::new(total_size as usize)?;
    let buf = unsafe { jit_buf.buf_mut() };

    // Fill everything with breakpoints, for two reasons:
    // * Accidental fallthroughs/bad jumps can be caught more easily.
    // * Break-at-entry can be implemented by moving the call target one byte back.
    buf.fill(0xcc);

    for func in module.functions.keys() {
        let blob = &code_blobs[func];
        let offsets = &func_offsets[func];

        let offset = offsets.code_offset as usize;
        let len = blob.code.len();
        buf[offset..offset + len].copy_from_slice(&blob.code);

        let offset = constpool_off as usize + offsets.constpool_offset as usize;
        let len = blob.constpool.len();
        buf[offset..offset + len].copy_from_slice(&blob.constpool);
    }

    let builtins = populate_builtins(module);
    relocate_buf(
        buf,
        module,
        constpool_off,
        &code_blobs,
        &func_offsets,
        &builtins,
    )?;

    jit_buf.make_exec()?;
    unsafe {
        let mut entry_func =
            jit_buf.base().as_ptr() as usize + func_offsets[entry_point].code_offset as usize;
        if break_at_entry {
            entry_func -= 1;
        }
        call_func(entry_func, args)
    }
}

#[derive(Clone, Copy, Default)]
struct FuncOffsets {
    code_offset: u32,
    constpool_offset: u32,
}

unsafe fn call_func(func: usize, args: &[isize]) -> Result<isize> {
    fn arg(args: &[isize], i: usize) -> isize {
        args.get(i).copied().unwrap_or_default()
    }

    ensure!(args.len() <= 16, "too many arguments passed to function");

    unsafe {
        // Welcome to UB land! We're using the fact that passing redundant arguments is safe in the
        // System V 64-bit ABI.
        let func: unsafe extern "sysv64" fn(
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
            isize,
        ) -> isize = mem::transmute(func);
        let ret = func(
            arg(args, 0),
            arg(args, 1),
            arg(args, 2),
            arg(args, 3),
            arg(args, 4),
            arg(args, 5),
            arg(args, 6),
            arg(args, 7),
            arg(args, 8),
            arg(args, 9),
            arg(args, 10),
            arg(args, 11),
            arg(args, 12),
            arg(args, 13),
            arg(args, 14),
            arg(args, 15),
        );
        Ok(ret)
    }
}

fn relocate_buf(
    buf: &mut [u8],
    module: &Module,
    constpool_off: u32,
    code_blobs: &SecondaryMap<Function, CodeBlob>,
    func_offsets: &SecondaryMap<Function, FuncOffsets>,
    builtins: &SecondaryMap<ExternFunction, Option<usize>>,
) -> Result<()> {
    for (func, code) in code_blobs.iter() {
        let start = func_offsets[func].code_offset as usize;
        let end = start + code.code.len();

        for reloc in &code.relocs {
            let target_abs = match reloc.target {
                RelocTarget::Function(FunctionRef::Internal(func)) => {
                    buf.as_ptr() as u64 + func_offsets[func].code_offset as u64
                }
                RelocTarget::Function(FunctionRef::External(extfunc)) => {
                    builtins[extfunc].ok_or_else(|| {
                        anyhow!(
                            "builtin `{}` not supported",
                            module.extern_functions[extfunc].name
                        )
                    })? as u64
                }
                RelocTarget::ConstantPool => {
                    buf.as_ptr() as u64
                        + constpool_off as u64
                        + func_offsets[func].constpool_offset as u64
                }
            };

            apply_reloc(
                &mut buf[start..end],
                reloc.kind,
                reloc.offset as usize,
                target_abs,
                reloc.addend,
            )?;
        }
    }

    Ok(())
}

fn apply_reloc(
    buf: &mut [u8],
    kind: RelocKind,
    offset: usize,
    target_abs: u64,
    addend: i64,
) -> Result<()> {
    let offset_abs = buf.as_ptr() as u64 + offset as u64;

    if cfg!(target_arch = "x86_64") {
        match kind {
            RELOC_PC32 => {
                // Note: sign extend to 64 bits.
                let value = (target_abs.wrapping_sub(offset_abs) as i64).wrapping_add(addend);
                let value: i32 = value.try_into().context("relocation out of range")?;
                buf[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
            }
            RELOC_ABS64 => {
                let value = target_abs.wrapping_add(addend as u64);
                buf[offset..offset + 8].copy_from_slice(&value.to_le_bytes());
            }
            _ => bail!("unknown relocation kind"),
        }
    } else {
        bail!("unsupported architecture");
    }

    Ok(())
}

fn codegen_func(module: &Module, func: Function) -> Result<CodeBlob> {
    if cfg!(target_arch = "x86_64") {
        codegen_func_with_machine(
            &X64Machine::new(X64MachineConfig {
                internal_code_model: CodeModel::SmallPic,
                extern_code_model: CodeModel::LargeAbs,
            }),
            module,
            func,
        )
    } else {
        bail!("unsupported architecture");
    }
}

fn codegen_func_with_machine(
    machine: &dyn Codegen,
    module: &Module,
    func: Function,
) -> Result<CodeBlob> {
    let func = &module.functions[func];
    machine
        .codegen_func(module, func, &CodegenOpts::default())
        .map_err(|err| anyhow!("codegen failed: {}", err.display(module, func)))
}

fn populate_builtins(module: &Module) -> SecondaryMap<ExternFunction, Option<usize>> {
    let builtins = FxHashMap::from_iter([
        ("malloc", libc::malloc as usize),
        ("free", libc::free as usize),
        ("memcpy", libc::memcpy as usize),
        ("memset", libc::memset as usize),
    ]);

    let mut mapping = SecondaryMap::new();
    for (extfunc, metadata) in module.extern_functions.iter() {
        if let Some(&builtin) = builtins.get(metadata.name.as_str()) {
            mapping[extfunc] = Some(builtin);
        }
    }

    mapping
}

fn pad_code_offset(offset: u32) -> Option<u32> {
    const FUNC_ALIGN: u32 = 0x10;
    // Note: we always want at least one byte of padding between functions. This is important for
    // break-at-entry support.
    try_align_up(offset + 1, FUNC_ALIGN)
}

fn try_align_up(offset: u32, align: u32) -> Option<u32> {
    let bumped = offset.checked_add(align - 1)?;
    Some(bumped & 0u32.wrapping_sub(align))
}
