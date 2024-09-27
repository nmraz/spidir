use std::mem;

use anyhow::{anyhow, bail, ensure, Context, Result};
use codegen::{
    api::{Codegen, CodegenOpts},
    code_buffer::{CodeBlob, RelocKind},
    target::x64::{CodeModel, X64Machine, X64MachineConfig, RELOC_ABS64, RELOC_PC32},
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
    let mut buffer_size: u32 = 0;

    for (func, code) in code_blobs.iter() {
        // Padding *before* the function is important for break-at-entry support.
        buffer_size = pad_offset(buffer_size).ok_or_else(|| anyhow!("code too large"))?;
        func_offsets[func] = buffer_size;
        buffer_size = buffer_size
            .checked_add(code.code.len().try_into().context("code too large")?)
            .ok_or_else(|| anyhow!("code too large"))?;
    }

    let mut jit_buf = JitBuf::new(buffer_size as usize)?;
    let buf = unsafe { jit_buf.buf_mut() };

    // Fill everything with breakpoints, for two reasons:
    // * Accidental fallthroughs/bad jumps can be caught more easily.
    // * Break-at-entry can be implemented by moving the call target one byte back.
    buf.fill(0xcc);

    for func in module.functions.keys() {
        let code = &code_blobs[func].code;
        let offset = func_offsets[func] as usize;
        let len = code.len();

        buf[offset..offset + len].copy_from_slice(code);
    }

    let builtins = populate_builtins(module);
    relocate_buf(buf, module, &code_blobs, &func_offsets, &builtins)?;

    jit_buf.make_exec()?;
    unsafe {
        let mut entry_func = jit_buf.base().as_ptr() as usize + func_offsets[entry_point] as usize;
        if break_at_entry {
            entry_func -= 1;
        }
        call_func(entry_func, args)
    }
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
    code_blobs: &SecondaryMap<Function, CodeBlob>,
    func_offsets: &SecondaryMap<Function, u32>,
    builtins: &SecondaryMap<ExternFunction, Option<usize>>,
) -> Result<()> {
    for (func, code) in code_blobs.iter() {
        let start = func_offsets[func] as usize;
        let end = start + code.code.len();

        for reloc in &code.relocs {
            let target_abs = match reloc.target {
                FunctionRef::Internal(func) => buf.as_ptr() as u64 + func_offsets[func] as u64,
                FunctionRef::External(extfunc) => builtins[extfunc].ok_or_else(|| {
                    anyhow!(
                        "builtin `{}` not supported",
                        module.extern_functions[extfunc].name
                    )
                })? as u64,
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

fn pad_offset(offset: u32) -> Option<u32> {
    const FUNC_ALIGN: u32 = 0x10;
    // Note: we add `FUNC_ALIGN` and not `FUNC_ALGIN - 1` so there is always at least one byte of
    // padding between functions. This is important for break-at-entry support.
    let bumped = offset.checked_add(FUNC_ALIGN)?;
    Some(bumped & 0u32.wrapping_sub(FUNC_ALIGN))
}
