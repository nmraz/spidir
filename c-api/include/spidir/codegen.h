/// @file codegen.h
/// Machine code generation API.

#ifndef SPIDIR_CODEGEN_H
#define SPIDIR_CODEGEN_H

#include <spidir/module.h>

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

/// Represents a relocation against another function (internal or external) in
/// generated code.
typedef struct spidir_codegen_reloc {
    /// The target of this relocation (either internal or external).
    spidir_function_t target;
    /// The addend for this relocation.
    /// The exact meaning of this field depends on the target archtitecture and
    /// the value of the `kind` field.
    int64_t addend;
    /// The offset in the generated code to which the relocation applies.
    uint32_t offset;
    /// The kind of relocation to apply.
    /// The meaning of this field depends on the target architecture.
    uint8_t kind;
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
/// @param[in] module    The module containing the function to be emitted.
/// @param[in] func      The function for which to generate code. This function
///                      must be internal to the module.
/// @param[out] out_blob An out parameter that will receive the generated code
///                      upon success. This parameter will be cleared on error.
/// @return A status indicating whether code generation was successful.
spidir_codegen_status_t spidir_codegen_emit_function(
    spidir_codegen_machine_handle_t machine, spidir_module_handle_t module,
    spidir_function_t func, spidir_codegen_blob_handle_t* out_blob);

#endif
