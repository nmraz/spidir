/// @file x64.h
/// Machine code generation support for x64 processors.

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
    /// A 64-bit relocation whose value must be calculated as `F + A`, where:
    /// * `F` is the run-time address of the function referred to by the
    ///   relocation
    /// * `A` is the addend value stored in the relocation
    SPIDIR_RELOC_X64_ABS64 = 1,
};

/// Represents the code models for which the x64 backend can generate code.
///
/// See the `SPIDIR_X64_CM_` constants for possible values.
typedef uint8_t spidir_x64_code_model_t;

enum spidir_x64_code_model {
    /// Generates code that assumes that all call targets are within +/-2GiB of
    /// the caller. Generated calls end up with `SPIDIR_RELOC_X64_PC32`
    /// relocations.
    SPIDIR_X64_CM_SMALL_PIC = 0,
    /// Generates code that supports call targets anywhere within the 64-bit
    /// address space. Generated calls end up with `SPIDIR_RELOC_X64_ABS64`
    /// relocations.
    SPIDIR_X64_CM_LARGE_ABS = 1,
};

/// Configuration options that can be passed to the x64 backend.
typedef struct spidir_x64_machine_config {
    /// The code model to use for internal (module-local) calls.
    /// This option defaults to `SPIDIR_X64_CM_SMALL_PIC`.
    spidir_x64_code_model_t internal_code_model;
    /// The code model to use for external calls.
    /// This option defaults to `SPIDIR_X64_CM_LARGE_ABS`.
    spidir_x64_code_model_t extern_code_model;
} spidir_x64_machine_config_t;

/// Creates a machine backend that generates code for the x64 architecture using
/// the default configuration (see default values documented in
/// `spidir_x64_machine_config_t`).
///
/// @return A new machine backend.
spidir_codegen_machine_handle_t spidir_codegen_create_x64_machine(void);

/// Creates a machine backend that generates code for the x64 architecture using
/// the specified configuration.
///
/// @return A new machine backend.
spidir_codegen_machine_handle_t spidir_codegen_create_x64_machine_with_config(
    const spidir_x64_machine_config_t* config);

#endif
