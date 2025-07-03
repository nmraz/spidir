use capstone::{
    Capstone,
    arch::{
        BuildsCapstone, BuildsCapstoneSyntax,
        x86::{ArchMode, ArchSyntax},
    },
};
use expect_test::{Expect, expect};
use itertools::Itertools;

use super::*;

fn disasm_instr(code: &[u8], addr: u64) -> String {
    let cs = Capstone::new()
        .x86()
        .mode(ArchMode::Mode64)
        .syntax(ArchSyntax::Intel)
        .build()
        .expect("failed to create disassembler");

    let insns = cs.disasm_all(code, addr).expect("failed to disassemble");
    let insns = insns.as_ref();
    assert_eq!(insns.len(), 1);
    let insn = &insns[0];
    assert_eq!(insn.len(), code.len());

    let code_bytes = format!("{:02x}", code.iter().format(" "));
    format!(
        "{:<25} {} {}",
        code_bytes,
        insn.mnemonic().unwrap(),
        insn.op_str().unwrap()
    )
}

fn check_emit_instr(f: impl FnOnce(&mut CodeBuffer<X64Fixup>), expected: Expect) {
    let mut buffer = CodeBuffer::new();
    f(&mut buffer);
    expected.assert_eq(&disasm_instr(&buffer.finish().code, 0));
}

fn check_emit_addr_mode(addr: RawAddrMode, expected: Expect) {
    check_emit_instr(
        |buffer| emit_mov_rm_r(buffer, FullOperandSize::S32, RegMem::Mem(addr), REG_RAX),
        expected,
    );
}

#[test]
fn simple_addr_mode() {
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RAX),
            index: None,
            offset: 0,
        },
        expect!["89 00                     mov dword ptr [rax], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RBX),
            index: None,
            offset: 0,
        },
        expect!["89 03                     mov dword ptr [rbx], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RCX),
            index: None,
            offset: 0,
        },
        expect!["89 01                     mov dword ptr [rcx], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RDX),
            index: None,
            offset: 0,
        },
        expect!["89 02                     mov dword ptr [rdx], eax"],
    );

    // This is special because it always requires an SIB.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSP),
            index: None,
            offset: 0,
        },
        expect!["89 04 24                  mov dword ptr [rsp], eax"],
    );

    // This is special because it always requires a disp8.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RBP),
            index: None,
            offset: 0,
        },
        expect!["89 45 00                  mov dword ptr [rbp], eax"],
    );

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSI),
            index: None,
            offset: 0,
        },
        expect!["89 06                     mov dword ptr [rsi], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RDI),
            index: None,
            offset: 0,
        },
        expect!["89 07                     mov dword ptr [rdi], eax"],
    );

    // Higher half (REX.B)

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R8),
            index: None,
            offset: 0,
        },
        expect!["41 89 00                  mov dword ptr [r8], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R9),
            index: None,
            offset: 0,
        },
        expect!["41 89 01                  mov dword ptr [r9], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R10),
            index: None,
            offset: 0,
        },
        expect!["41 89 02                  mov dword ptr [r10], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R11),
            index: None,
            offset: 0,
        },
        expect!["41 89 03                  mov dword ptr [r11], eax"],
    );

    // This is special because it always requires an SIB.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R12),
            index: None,
            offset: 0,
        },
        expect!["41 89 04 24               mov dword ptr [r12], eax"],
    );

    // This is special because it always requires a disp8.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R13),
            index: None,
            offset: 0,
        },
        expect!["41 89 45 00               mov dword ptr [r13], eax"],
    );

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R14),
            index: None,
            offset: 0,
        },
        expect!["41 89 06                  mov dword ptr [r14], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R15),
            index: None,
            offset: 0,
        },
        expect!["41 89 07                  mov dword ptr [r15], eax"],
    );
}

#[test]
fn disp8_addr_mode() {
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RAX),
            index: None,
            offset: 5,
        },
        expect!["89 40 05                  mov dword ptr [rax + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RBX),
            index: None,
            offset: 5,
        },
        expect!["89 43 05                  mov dword ptr [rbx + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RCX),
            index: None,
            offset: 5,
        },
        expect!["89 41 05                  mov dword ptr [rcx + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RDX),
            index: None,
            offset: 5,
        },
        expect!["89 42 05                  mov dword ptr [rdx + 5], eax"],
    );

    // This is special because it always requires an SIB.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSP),
            index: None,
            offset: 5,
        },
        expect!["89 44 24 05               mov dword ptr [rsp + 5], eax"],
    );

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RBP),
            index: None,
            offset: 5,
        },
        expect!["89 45 05                  mov dword ptr [rbp + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSI),
            index: None,
            offset: 5,
        },
        expect!["89 46 05                  mov dword ptr [rsi + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RDI),
            index: None,
            offset: 5,
        },
        expect!["89 47 05                  mov dword ptr [rdi + 5], eax"],
    );

    // Higher half (REX.B)

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R8),
            index: None,
            offset: 5,
        },
        expect!["41 89 40 05               mov dword ptr [r8 + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R9),
            index: None,
            offset: 5,
        },
        expect!["41 89 41 05               mov dword ptr [r9 + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R10),
            index: None,
            offset: 5,
        },
        expect!["41 89 42 05               mov dword ptr [r10 + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R11),
            index: None,
            offset: 5,
        },
        expect!["41 89 43 05               mov dword ptr [r11 + 5], eax"],
    );

    // This is special because it always requires an SIB.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R12),
            index: None,
            offset: 5,
        },
        expect!["41 89 44 24 05            mov dword ptr [r12 + 5], eax"],
    );

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R13),
            index: None,
            offset: 5,
        },
        expect!["41 89 45 05               mov dword ptr [r13 + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R14),
            index: None,
            offset: 5,
        },
        expect!["41 89 46 05               mov dword ptr [r14 + 5], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R15),
            index: None,
            offset: 5,
        },
        expect!["41 89 47 05               mov dword ptr [r15 + 5], eax"],
    );
}

#[test]
fn disp32_addr_mode() {
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RAX),
            index: None,
            offset: 0x10000,
        },
        expect!["89 80 00 00 01 00         mov dword ptr [rax + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RBX),
            index: None,
            offset: 0x10000,
        },
        expect!["89 83 00 00 01 00         mov dword ptr [rbx + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RCX),
            index: None,
            offset: 0x10000,
        },
        expect!["89 81 00 00 01 00         mov dword ptr [rcx + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RDX),
            index: None,
            offset: 0x10000,
        },
        expect!["89 82 00 00 01 00         mov dword ptr [rdx + 0x10000], eax"],
    );

    // This is special because it always requires an SIB.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSP),
            index: None,
            offset: 0x10000,
        },
        expect!["89 84 24 00 00 01 00      mov dword ptr [rsp + 0x10000], eax"],
    );

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RBP),
            index: None,
            offset: 0x10000,
        },
        expect!["89 85 00 00 01 00         mov dword ptr [rbp + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RSI),
            index: None,
            offset: 0x10000,
        },
        expect!["89 86 00 00 01 00         mov dword ptr [rsi + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_RDI),
            index: None,
            offset: 0x10000,
        },
        expect!["89 87 00 00 01 00         mov dword ptr [rdi + 0x10000], eax"],
    );

    // Higher half (REX.B)

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R8),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 80 00 00 01 00      mov dword ptr [r8 + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R9),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 81 00 00 01 00      mov dword ptr [r9 + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R10),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 82 00 00 01 00      mov dword ptr [r10 + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R11),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 83 00 00 01 00      mov dword ptr [r11 + 0x10000], eax"],
    );

    // This is special because it always requires an SIB.
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R12),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 84 24 00 00 01 00   mov dword ptr [r12 + 0x10000], eax"],
    );

    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R13),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 85 00 00 01 00      mov dword ptr [r13 + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R14),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 86 00 00 01 00      mov dword ptr [r14 + 0x10000], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::BaseIndexOff {
            base: Some(REG_R15),
            index: None,
            offset: 0x10000,
        },
        expect!["41 89 87 00 00 01 00      mov dword ptr [r15 + 0x10000], eax"],
    );
}

#[test]
fn rip_off_addr_mode() {
    check_emit_addr_mode(
        RawAddrMode::RipOff { offset: 0 },
        expect!["89 05 00 00 00 00         mov dword ptr [rip], eax"],
    );
    check_emit_addr_mode(
        RawAddrMode::RipOff { offset: 4 },
        expect!["89 05 04 00 00 00         mov dword ptr [rip + 4], eax"],
    );
}

#[test]
fn movsd() {
    let check = |dest, src, expected| {
        check_emit_instr(
            |buffer| emit_movs_r_rm(buffer, SseFpuPrecision::Double, dest, src),
            expected,
        );
    };

    check(
        REG_XMM0,
        RegMem::Reg(REG_XMM1),
        expect!["f2 0f 10 c1               movsd xmm0, xmm1"],
    );
    check(
        REG_XMM10,
        RegMem::Reg(REG_XMM1),
        expect!["f2 44 0f 10 d1            movsd xmm10, xmm1"],
    );
    check(
        REG_XMM0,
        RegMem::Reg(REG_XMM10),
        expect!["f2 41 0f 10 c2            movsd xmm0, xmm10"],
    );
    check(
        REG_XMM15,
        RegMem::Reg(REG_XMM10),
        expect!["f2 45 0f 10 fa            movsd xmm15, xmm10"],
    );
    check(
        REG_XMM7,
        RegMem::Mem(RawAddrMode::BaseIndexOff {
            base: Some(REG_R8),
            index: None,
            offset: 0,
        }),
        expect!["f2 41 0f 10 38            movsd xmm7, qword ptr [r8]"],
    );
}

#[test]
fn movaps() {
    let check = |dest, src, expected| {
        check_emit_instr(|buffer| emit_movaps_r_rm(buffer, dest, src), expected);
    };

    check(
        REG_XMM0,
        RegMem::Reg(REG_XMM1),
        expect!["0f 28 c1                  movaps xmm0, xmm1"],
    );
    check(
        REG_XMM10,
        RegMem::Reg(REG_XMM1),
        expect!["44 0f 28 d1               movaps xmm10, xmm1"],
    );
    check(
        REG_XMM0,
        RegMem::Reg(REG_XMM10),
        expect!["41 0f 28 c2               movaps xmm0, xmm10"],
    );
    check(
        REG_XMM15,
        RegMem::Reg(REG_XMM10),
        expect!["45 0f 28 fa               movaps xmm15, xmm10"],
    );
    check(
        REG_XMM7,
        RegMem::Mem(RawAddrMode::BaseIndexOff {
            base: Some(REG_R8),
            index: None,
            offset: 0,
        }),
        expect!["41 0f 28 38               movaps xmm7, xmmword ptr [r8]"],
    );
}

#[test]
fn adds() {
    let check = |prec, dest, src, expected| {
        check_emit_instr(
            |buffer| emit_sse_fpu_r_rm(buffer, prec, SseFpuBinOp::Add, dest, src),
            expected,
        );
    };

    check(
        SseFpuPrecision::Single,
        REG_XMM0,
        RegMem::Reg(REG_XMM1),
        expect!["f3 0f 58 c1               addss xmm0, xmm1"],
    );

    check(
        SseFpuPrecision::Double,
        REG_XMM0,
        RegMem::Reg(REG_XMM1),
        expect!["f2 0f 58 c1               addsd xmm0, xmm1"],
    );
    check(
        SseFpuPrecision::Double,
        REG_XMM10,
        RegMem::Reg(REG_XMM1),
        expect!["f2 44 0f 58 d1            addsd xmm10, xmm1"],
    );
    check(
        SseFpuPrecision::Double,
        REG_XMM0,
        RegMem::Reg(REG_XMM10),
        expect!["f2 41 0f 58 c2            addsd xmm0, xmm10"],
    );
    check(
        SseFpuPrecision::Double,
        REG_XMM15,
        RegMem::Reg(REG_XMM10),
        expect!["f2 45 0f 58 fa            addsd xmm15, xmm10"],
    );
    check(
        SseFpuPrecision::Double,
        REG_XMM7,
        RegMem::Mem(RawAddrMode::BaseIndexOff {
            base: Some(REG_R8),
            index: None,
            offset: 0,
        }),
        expect!["f2 41 0f 58 38            addsd xmm7, qword ptr [r8]"],
    );
}
