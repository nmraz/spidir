#ifndef SPIDIR_X64_H
#define SPIDIR_X64_H

#include <spidir/codegen.h>

enum {
    /// A 32-bit relocation whose value must be calculated as `F + A - P`,
    /// where:
    /// * `P` is the run-time address of the field being relocated (the 32-bit
    ///   word at offset `offset` in the code)
    /// * `F` is the run-time address of the function referred to by the
    ///   relocation
    /// * `A` is the addend value stored in the relocation
    SPIDIR_RELOC_X64_PC32 = 0,
    /// A 64-bit relocation whose value should be the full run-time address of
    /// the function referred to by the relocation.
    SPIDIR_RELOC_X64_ABS64 = 1,
};

/// Creates a machine backend that generates code for the x64 architecture.
///
/// @return A new machine backend.
spidir_codegen_machine_handle_t spidir_codegen_create_x64_machine(void);

#endif
