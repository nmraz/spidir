/// @file codegen.h
/// Machine code generation API.

#ifndef SPIDIR_CODEGEN_H
#define SPIDIR_CODEGEN_H

#include <spidir/module.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// A status code returned by the machine code generation APIs.
///
/// See the `SPIDIR_CODEGEN_` constants for possible values.
typedef uint32_t spidir_codegen_status_t;

enum spidir_codegen_status {
    /// Code was generated successfully.
    SPIDIR_CODEGEN_OK = 0,
    /// Instruction selection failed.
    SPIDIR_CODEGEN_ERROR_ISEL = 1,
    /// Register allocation failed.
    SPIDIR_CODEGEN_ERROR_REGALLOC = 2,
};

/// Machine-agnostic configuration options to pass to the code generator.
typedef struct spidir_codegen_config {
    /// Verify the IR before performing code generation and panic if there are
    /// errors.
    bool verify_ir;
    /// Verify that register allocation has been performed correctly after
    /// performing it, and panic if anything is incorrect. This option should
    /// only be used when debugging the code generator/register allocator
    /// itself.
    bool verify_regalloc;
} spidir_codegen_config_t;

/// Represents the different types of relocations to apply to generated code.
/// See individual backend definitions for possible values.
typedef uint8_t spidir_reloc_kind_t;

enum spidir_reloc_target_kind {
    /// The relocation refers to an internal function, accessible via the
    /// `target.internal` relocation field.
    SPIDIR_RELOC_TARGET_INTERNAL_FUNCTION,
    /// The relocation refers to an internal function, accessible via the
    /// `target.external` relocation field.
    SPIDIR_RELOC_TARGET_EXTERNAL_FUNCTION,
    /// The relocation refers to the function's constant pool.
    SPIDIR_RELOC_TARGET_CONSTPOOL,
};

/// Indicates what type of entity is referenced by a relocation.
///
/// See the `SPIDIR_RELOC_TARGET_` constants for possible values.
typedef uint8_t spidir_reloc_target_kind_t;

/// Represents a relocation in generated code. Relocations can refer either to
/// other functions or to the function's constant pool.
typedef struct spidir_codegen_reloc {
    /// The addend for this relocation.
    /// The exact meaning of this field depends on the target architecture and
    /// the value of the `kind` field.
    int64_t addend;
    /// The target function of this relocation, if it refers to a function.
    union {
        spidir_function_t internal;
        spidir_extern_function_t external;
    } target;
    /// The offset in the generated code to which the relocation applies.
    uint32_t offset;
    /// The type of entity referred to by this relocation.
    spidir_reloc_target_kind_t target_kind;
    /// The kind of relocation to apply.
    /// The meaning of this field depends on the target architecture.
    spidir_reloc_kind_t kind;
} spidir_codegen_reloc_t;

/// An opaque type representing a machine backend for a specific architecture.
///
/// Every backend provides its own API for creating a `spidir_machine_handle_t`.
typedef struct spidir_codegen_machine* spidir_codegen_machine_handle_t;

/// An opaque type representing a blob of generated code with its accompanying
/// relocations.
typedef struct spidir_codegen_blob* spidir_codegen_blob_handle_t;

/// Destroys the specified machine backend.
/// The machine should no longer be used after this function returns.
///
/// @param[in] machine The machine to destroy.
void spidir_codegen_machine_destroy(spidir_codegen_machine_handle_t machine);

/// Destroys the specified code blob.
/// The blob should no longer be used after this function returns.
///
/// @param[in] blob The blob to destroy.
void spidir_codegen_blob_destroy(spidir_codegen_blob_handle_t blob);

/// Queries the size of the machine code (in bytes) held in the code blob.
///
/// @param[in] blob The blob to query.
/// @return The size of the contained code.
size_t spidir_codegen_blob_get_code_size(spidir_codegen_blob_handle_t blob);

/// Retrieves a pointer to the machine code stored in the code blob.
///
/// @param[in] blob The blob to query.
/// @return A pointer to the code stored in the blob. This buffer is owned by
///         the blob and should not be referenced after the blob is destroyed.
const void* spidir_codegen_blob_get_code(spidir_codegen_blob_handle_t blob);

/// Queries the size of the constant pool (in bytes) held in the code blob.
///
/// @param[in] blob The blob to query.
/// @return The size of the contained constant pool.
size_t
spidir_codegen_blob_get_constpool_size(spidir_codegen_blob_handle_t blob);

/// Queries the alignment of the constant pool (in bytes) held in the code blob.
///
/// @param[in] blob The blob to query.
/// @return The alignment of the contained constant pool.
size_t
spidir_codegen_blob_get_constpool_align(spidir_codegen_blob_handle_t blob);

/// Retrieves a pointer to the constant pool stored in the code blob.
///
/// @param[in] blob The blob to query.
/// @return A pointer to the constant pool stored in the blob. This buffer is
///         owned by the blob and should not be referenced after the blob is
///         destroyed.
const void*
spidir_codegen_blob_get_constpool(spidir_codegen_blob_handle_t blob);

/// Queries the number of relocations stored in the code blob.
///
/// @param[in] blob The blob to query.
/// @return The number of relocations.
size_t spidir_codegen_blob_get_reloc_count(spidir_codegen_blob_handle_t blob);

/// Retrieves a pointer to the relocations stored in the code blob.
///
/// @param[in] blob The blob to query.
/// @return A pointer to the relocations stored in the blob. These relocations
///         are owned by the blob and should not be referenced after the blob is
///         destroyed.
const spidir_codegen_reloc_t*
spidir_codegen_blob_get_relocs(spidir_codegen_blob_handle_t blob);

/// Generates native code for the specified function using the specified machine
/// backend.
///
/// @note It is safe to invoke this function concurrently on the same machine
///       and module; it does not mutate any of its parameters.
///
/// @param[in] machine   The machine backend to use for code generation. Use a
///                      backend-specific API to create a suitable machine.
/// @param[in] config    Machine-agnostic configuration to use.
/// @param[in] module    The module containing the function to be emitted.
/// @param[in] func      The function for which to generate code.
/// @param[out] out_blob An out parameter that will receive the generated code
///                      upon success. This parameter will be cleared on error.
/// @return A status indicating whether code generation was successful.
spidir_codegen_status_t spidir_codegen_emit_function(
    spidir_codegen_machine_handle_t machine,
    const spidir_codegen_config_t* config, spidir_module_handle_t module,
    spidir_function_t func, spidir_codegen_blob_handle_t* out_blob);

#endif
