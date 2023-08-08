/// @file spidir.h
/// Core spidir definitions and APIs.

#ifndef SPIDIR_SPIDIR_H
#define SPIDIR_SPIDIR_H

#include <stddef.h>
#include <stdint.h>

/// A sentinel representing the absence of a `spidir_value_t`.
/// Unless otherwise specified, functions accepting a value will panic if they
/// are passed this value.
#define SPIDIR_VALUE_INVALID ((spidir_value_t){UINT32_MAX})

/// An opaque type representing a spidir module.
///
/// Modules are the high-level container for all spidir context, and can contain
/// multiple functions and external functions.
typedef struct spidir_module* spidir_module_handle_t;

/// An opaque type representing a function builder.
///
/// See `spidir_module_build_function` for more information.
typedef struct spidir_builder* spidir_builder_handle_t;

/// An identifier representing a function within a module.
///
/// Values of this type are only guaranteed to be unique within the context of a
/// module, and should not be mixed across different modules.
typedef struct spidir_function {
    uint32_t id;
} spidir_function_t;

/// An identifier representing an external function within a module.
///
/// Values of this type are only guaranteed to be unique within the context of a
/// module, and should not be mixed across different modules.
typedef struct spidir_extern_function {
    uint32_t id;
} spidir_extern_function_t;

/// An identifier representing a basic block during function construction.
typedef struct spidir_block {
    uint32_t id;
} spidir_block_t;

/// An identifier representing an SSA value during function construction.
typedef struct spidir_value {
    uint32_t id;
} spidir_value_t;

/// An identifier representing an SSA phi operation during function
/// construction.
typedef struct spidir_phi {
    uint32_t id;
} spidir_phi_t;

/// Represents the types that can be taken on by an SSA value.
///
/// See the `SPIDIR_TYPE_` constants for possible values.
typedef uint8_t spidir_value_type_t;

enum spidir_value_type {
    /// A 32-bit integer.
    SPIDIR_TYPE_I32 = 0,
    /// A 64-bit integer.
    SPIDIR_TYPE_I64 = 1,
    /// A 64-bit float.
    SPIDIR_TYPE_F64 = 2,
    /// A pointer that can be used with load and store instructions.
    SPIDIR_TYPE_PTR = 3,
    /// A sentinel value representing the absence of a type.
    /// Unless otherwise specified, functions accepting a type will panic if
    /// they are passed this value.
    SPIDIR_TYPE_NONE = UINT8_MAX,
};

/// Represents a type of integer comparison operation.
///
/// See the `SPIDIR_ICMP_` constants for possible values.
typedef uint8_t spidir_icmp_kind_t;

enum spidir_icmp_kind {
    /// Integer equality comparison.
    SPIDIR_ICMP_EQ = 0,
    /// Integer inequality comparison.
    SPIDIR_ICMP_NE = 1,
    /// Integer signed-less-than comparison.
    SPIDIR_ICMP_SLT = 2,
    /// Integer signed-less-than-or-equal comparison.
    SPIDIR_ICMP_SLE = 3,
    /// Integer unsigned-less-than comparison.
    SPIDIR_ICMP_ULT = 4,
    /// Integer unsigned-less-than-or-equal comparison.
    SPIDIR_ICMP_ULE = 5,
};

/// A status code that can be returned from user callbacks to
/// `spidir_module_dump`.
typedef uint8_t spidir_dump_status_t;

enum spidir_dump_status {
    /// Indicates to `spidir_module_dump` that it can continue dumping the
    /// module.
    SPIDIR_DUMP_CONTINUE = 0,
    /// Indicates to `spidir_module_dump` that it should stop duming the module
    /// and return immediately.
    SPIDIR_DUMP_STOP = 1,
};

/// A user callback function that can be passed to
/// `spidir_module_build_function`.
///
/// @param[in] builder A handle to the builder object for the function being
///                    built.
/// @param[in] ctx     An additional pointer for passing user context to the
///                    callback.
typedef void (*spidir_build_function_callback_t)(
    spidir_builder_handle_t builder, void* ctx);

/// A user callback function that can be passed to `spidir_module_dump` to
/// convert a module to its textual representation.
///
/// This callback will receive the textual data in chunks that, when
/// concatenated, yield the full text dump.
///
/// @param[in] data A pointer to the current string chunk. This data may *not*
///                 be null-terminated.
/// @param[in] size The size of the string pointed to by `data`, in bytes.
/// @param[in] ctx  An additional pointer for passing user context to the
///                 callback.
typedef spidir_dump_status_t (*spidir_dump_callback_t)(const char* data,
                                                       size_t size, void* ctx);

/// Creates a new, empty module.
///
/// @return The newly-allocated module.
spidir_module_handle_t spidir_module_create(void);

/// Destroys the specified module.
/// The module should no longer be used after this function returns.
///
/// @param[in] module The module to destroy.
void spidir_module_destroy(spidir_module_handle_t module);

/// Adds a new function with the specified parameters to the module.
///
/// The newly-added function will be malformed and should be built using
/// `spidir_module_build_function`.
///
/// @param[in] module      The module to which the function should be added.
/// @param[in] name        The name to give the function. This name should not
///                        collide with names given to other functions and
///                        external functions in the module.
/// @param[in] ret_type    The type to be returned by the function. If this
///                        value is `SPIDIR_TYPE_NONE`, the function will have
///                        no return type.
/// @param[in] param_count The number of parameters to be received by the
///                        function.
/// @param[in] param_types A pointer to an array of `param_count` types,
///                        indicating the types of parameters to be accepted by
///                        the function. This parameter may be null if
///                        `param_count` is 0.
/// @return A value identifying the newly-created function.
spidir_function_t
spidir_module_create_function(spidir_module_handle_t module, const char* name,
                              spidir_value_type_t ret_type, size_t param_count,
                              const spidir_value_type_t* param_types);

/// Adds a new external function with the specified parameters to the module.
///
/// External functions can be referred to by other parts of the module but
/// contain no code.
///
/// @param[in] module      The module to which the function should be added.
/// @param[in] name        The name to give the function. This name should not
///                        collide with names given to other functions and
///                        external functions in the module.
/// @param[in] ret_type    The type to be returned by the function. If this
///                        value is `SPIDIR_TYPE_NONE`, the function will have
///                        no return type.
/// @param[in] param_count The number of parameters to be received by the
///                        function.
/// @param[in] param_types A pointer to an array of `param_count` types,
///                        indicating the types of parameters to be accepted by
///                        the function. This parameter may be null if
///                        `param_count` is 0.
/// @return A value identifying the newly-created external function.
spidir_extern_function_t spidir_module_create_extern_function(
    spidir_module_handle_t module, const char* name,
    spidir_value_type_t ret_type, size_t param_count,
    const spidir_value_type_t* param_types);

/// Creates a builder object for adding code to `func` and invokes `callback` on
/// it.
///
/// The builder can be used to add basic blocks and instructions to the
/// function. The callback should, at the very least, call
/// `spidir_builder_set_entry_block` to set the function's first basic block.
///
/// @warning The callback should not access `module` in any way (**including**
/// by passing it to read-only functions such as `spidir_module_dump`). Doing so
/// will violate aliasing assumptions in the Rust implementation and lead to UB.
///
/// @param[in] module   The module containing the function to build.
/// @param[in] func     The function whose body should be built.
/// @param[in] callback The callback to invoke.
/// @param[in] ctx      An additional user pointer to pass to the callback.
void spidir_module_build_function(spidir_module_handle_t module,
                                  spidir_function_t func,
                                  spidir_build_function_callback_t callback,
                                  void* ctx);

/// Streams the textual representation of `module` to `callback`.
///
/// The callback will receive the textual data in chunks that, when
/// concatenated, yield the full text dump.
///
/// @param[in] module   The module to dump as text.
/// @param[in] callback The callback to invoke on each chunk of text. These
///                     chunks may *not* be null-terminated, so always consult
///                     the `size` parameter explicitly.
/// @param[in] ctx      An additional user pointer to pass to every invocation
///                     of the callback.
void spidir_module_dump(spidir_module_handle_t module,
                        spidir_dump_callback_t callback, void* ctx);

/// Creates a new basic block in the function being built by `builder`.
///
/// The newly-created block will not be attached to the CFG of the function
/// until referenced explicitly: use `spidir_builder_set_entry_block` or a
/// branch operation to do so.
///
/// @note Creating a new block with this function will not change the insertion
/// point of the builder.
///
/// @param[in] builder A handle to the function builder.
/// @return A value identifying the newly-created block. This value is only
/// meaningful when used with the same builder.
spidir_block_t spidir_builder_create_block(spidir_builder_handle_t builder);

/// Sets the current insertion point of `builder`.
///
/// Any instructions built after this call will be appended to the end of
/// `block`, until a different block is set.
///
/// It is permissible to switch away from a block and switch back to it again
/// later; appended instructions will continue to be added to the end.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] block   The block to switch to. This should have been created
///                    with a prior call to `spidir_builder_create_block`.
void spidir_builder_set_block(spidir_builder_handle_t builder,
                              spidir_block_t block);

/// Sets the entry block of the function being built by `builder`. The entry
/// block is the basic block to which control is transferred when the function
/// starts executing.
///
/// This function should be called exactly once on every function built.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] block   The block to set as the entry block.
void spidir_builder_set_entry_block(spidir_builder_handle_t builder,
                                    spidir_block_t block);

/// Creates an SSA value containing the requested function parameter.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] index   The index of the parameter to use. This value must be
///                    less than the `param_count` passed to the
///                    `spidir_module_create_function` call that was used to
///                    build the function.
/// @return An SSA value that will contain the relevant parameter at runtime.
///         The type of this value will be relevant `param_type` passed to
///         `spidir_module_create_function`.
spidir_value_t spidir_builder_build_param_ref(spidir_builder_handle_t builder,
                                              uint32_t index);

/// Builds an instruction calling the internal function `func` at the current
/// insertion point.
///
/// This function can only build calls to internal functions (functions that
/// reside in the current module, created by `spidir_module_create_function`).
/// To build a call to an external function, use
/// `spidir_builder_build_extern_call`.
///
/// @param[in] builder   A handle to the function builder.
/// @param[in] ret_type  The return type to use for the call. If this parameter
///                      is `SPIDIR_TYPE_NONE`, the call will be treated as
///                      returning no value. This type should match the return
///                      type specified when the function was created.
/// @param[in] func      The function to call.
/// @param[in] arg_count The number of arguments to pass to the function. This
///                      value should match the parameter count specified when
///                      the function was created.
/// @param[in] args      A pointer to an array of `arg_count` values, each
///                      representing an argument to pass. This parameter may be
///                      null if `arg_count` is 0.
/// @return An SSA value containing the return value of the function. If
/// `ret_type` was null (indicating a call to a function with no return value),
/// the returned value will be `SPIDIR_VALUE_INVALID`.
spidir_value_t spidir_builder_build_call(spidir_builder_handle_t builder,
                                         spidir_value_type_t ret_type,
                                         spidir_function_t func,
                                         size_t arg_count,
                                         const spidir_value_t* args);

/// Builds an instruction calling the external function `func` at the current
/// insertion point.
///
/// This function can only build calls to external functions (functions created
/// by `spidir_module_create_extern_function`). To build a call to an internal
/// function, use `spidir_builder_build_call`.
///
/// @param[in] builder   A handle to the function builder.
/// @param[in] ret_type  The return type to use for the call. If this parameter
///                      is `SPIDIR_TYPE_NONE`, the call will be treated as
///                      returning no value. This type should match the return
///                      type specified when the function was created.
/// @param[in] func      The function to call.
/// @param[in] arg_count The number of arguments to pass to the function. This
///                      value should match the parameter count specified when
///                      the function was created.
/// @param[in] args      A pointer to an array of `arg_count` values, each
///                      representing an argument to pass. This parameter may be
///                      null if `arg_count` is 0.
/// @return An SSA value containing the return value of the function. If
/// `ret_type` was null (indicating a call to a function with no return value),
/// the returned value will be `SPIDIR_VALUE_INVALID`.
spidir_value_t spidir_builder_build_extern_call(spidir_builder_handle_t builder,
                                                spidir_value_type_t ret_type,
                                                spidir_extern_function_t func,
                                                size_t arg_count,
                                                const spidir_value_t* args);

/// Builds a return instruction at the current insertion point.
///
/// After the return instruction is built, the current block will be considered
/// "terminated" and no new instructions will be allowed. Use
/// `spidir_builder_set_block` to switch to a different block if necessary.
///
/// The type of the value returned (or lack thereof) should match the return
/// type specified when the function was created.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] value   The value to pass to the return instruction. If this
///                    parameter is `SPIDIR_VALUE_INVALID`, the return
///                    instruction will not return a value.
void spidir_builder_build_return(spidir_builder_handle_t builder,
                                 spidir_value_t value);

/// Builds a branch instruction to the specified destination at the current
/// insertion point.
///
/// After the branch instruction is built, the current block will be considered
/// "terminated" and no new instructions will be allowed. Use
/// `spidir_builder_set_block` to switch to a different block if necessary.
///
/// For the purposes of phi operations, which depend on the order of incoming
/// control inputs to every block, this function behaves as if the control input
/// were appended to the end of `dest`'s control inputs.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] dest    The destination block to branch to.
void spidir_builder_build_branch(spidir_builder_handle_t builder,
                                 spidir_block_t dest);

/// Builds a conditional branch instruction at the current insertion point.
///
/// This instruction will branch to `true_dest` if `cond` is nonzero, and to
/// `false_dest` otherwise.
///
/// After the branch instruction is built, the current block will be considered
/// "terminated" and no new instructions will be allowed. Use
/// `spidir_builder_set_block` to switch to a different block if necessary.
///
/// For the purposes of phi operations, which depend on the order of incoming
/// control inputs to every block, this function behaves as if a control input
/// were appended to the end of `true_dest`'s control inputs, *and then* a
/// control input was added to `false_dest`.
///
/// @param[in] builder    A handle to the function builder.
/// @param[in] cond       An SSA value containing the condition for the branch.
///                       This value must have integer type.
/// @param[in] true_dest  The block to branch to when `cond` is nonzero.
/// @param[in] false_dest The block to branch to when `cond` is zero.
void spidir_builder_build_brcond(spidir_builder_handle_t builder,
                                 spidir_value_t cond, spidir_block_t true_dest,
                                 spidir_block_t false_dest);

/// Builds an SSA phi operation pertaining to the current block.
///
/// The phi operation should have one input for every incoming control edge to
/// the current block (in the same order), and the appropriate value will be
/// selected at runtime based on the flow of control.
///
/// To facilitate building loops and other graphs with data-flow cycles, this
/// function can optionally return a handle to the phi operation that can be
/// used to add more inputs later. For example, when building a loop, the phi
/// for the induction variable can be built with only the loop entry input, and
/// the backedge input can be added with `spidir_builder_add_phi_input` once its
/// value and control backedge have been built.
///
/// @param[in] builder         A handle to the function builder.
/// @param[in] type            The output type of the phi operation.
/// @param[in] input_count     The number of pre-built inputs to add to the phi.
/// @param[in] inputs          A pointer to an array of `input_count` values,
///                            each. This parameter may be null if `input_count`
///                            is 0.
/// @param[out] out_phi_handle An optional out parameter that will receive a
///                            handle to the built phi operation. This handle
///                            can later be passed to
///                            `spidir_builder_add_phi_input` to add new inputs
///                            to the phi (for example, when building loops).
/// @return An SSA value containing the output of the phi operation.
spidir_value_t spidir_builder_build_phi(spidir_builder_handle_t builder,
                                        spidir_value_type_t type,
                                        size_t input_count,
                                        const spidir_value_t* inputs,
                                        spidir_phi_t* out_phi_handle);

/// Adds a new input to a phi operation previously created via
/// `spidir_builder_build_phi`. The number and order of inputs in a phi
/// operation must ultimately correspond to the number and order of control
/// inputs of its parent block.
///
/// This function is useful when constructing graphs with data-flow cycles, such
/// as loops - inputs loop-carried dependencies can be added once they have been
/// built.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] phi     A handle to the phi value, as obtained by
///                    `spidir_builder_build_phi`.
/// @param[in] input   An SSA value to add as an input to the phi. This value
///                    will be appended to the end of the phi operation's
///                    inputs.
void spidir_builder_add_phi_input(spidir_builder_handle_t builder,
                                  spidir_phi_t phi, spidir_value_t input);

/// Builds an integer constant value at the current insertion point.
///
/// @note The value must be in range for the specified value type. For example,
/// an `iconst` of type `SPIDIR_TYPE_I32` should fit in 32 bits.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] type    The type of the integer value to build. This should be
///                    either `SPIDIR_TYPE_I32` or `SPIDIR_TYPE_I64`.
/// @param[in] value   The value of the constant to build.
/// @return An SSA value representing the constant. This value will have the
///         type `type`.
spidir_value_t spidir_builder_build_iconst(spidir_builder_handle_t builder,
                                           spidir_value_type_t type,
                                           uint64_t value);

/// Builds a floating-point constant value at the current insertion point.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] value   The value of the constant to build.
/// @return An SSA value representing the constant. This value will have type
///         `SPIDIR_TYPE_F64`.
spidir_value_t spidir_builder_build_fconst(spidir_builder_handle_t builder,
                                           double value);

/// Builds an integer add operation at the current insertion point. This
/// operation can be used for both signed and unsigned additions.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_iadd(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds an integer subtract operation at the current insertion point. This
/// operation can be used for both signed and unsigned subtractions.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_isub(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds a bitwise and operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_and(spidir_builder_handle_t builder,
                                        spidir_value_t lhs, spidir_value_t rhs);

/// Builds a bitwise or operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_or(spidir_builder_handle_t builder,
                                       spidir_value_t lhs, spidir_value_t rhs);

/// Builds a bitwise xor operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_xor(spidir_builder_handle_t builder,
                                        spidir_value_t lhs, spidir_value_t rhs);

/// Builds a left shift operation at the current insertion point. This
/// operation can be used for both arithmetic and logical shifts.
///
/// The values `lhs` and `rhs` must both be integers, but they need not have the
/// same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs     The left-hand input value.
/// @param[in] rhs     The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs`.
spidir_value_t spidir_builder_build_shl(spidir_builder_handle_t builder,
                                        spidir_value_t lhs, spidir_value_t rhs);

/// Builds a logical right shift operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers, but they need not have the
/// same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs     The left-hand input value.
/// @param[in] rhs     The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs`.
spidir_value_t spidir_builder_build_lshr(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds an arithmetic right shift operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers, but they need not have the
/// same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs     The left-hand input value.
/// @param[in] rhs     The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs`.
spidir_value_t spidir_builder_build_ashr(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds an integer multiply operation at the current insertion point. This
/// operation can be used for both signed and unsigned multiplication.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_imul(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds a signed division operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_sdiv(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds an unsigned division operation at the current insertion point.
///
/// The values `lhs` and `rhs` must both be integers of the same type.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] lhs       The left-hand input value.
/// @param[in] rhs       The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the same type as `lhs` and `rhs`.
spidir_value_t spidir_builder_build_udiv(spidir_builder_handle_t builder,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds an integer compare operation at the current insertion point. This
/// operation can be used to compare either integers or pointers.
///
/// The values `lhs` and `rhs` must both have the same type, which should be
/// either an integer or pointer type.
///
/// The value returned by this operation will be 1 if the comparison holds and
/// 0 otherwise.
///
/// @param[in] builder     A handle to the function builder.
/// @param[in] kind        The kind of comparison to perform. See the
///                        `SPIDIR_ICMP_` constants for the allowed operation
///                        kinds.
/// @param[in] output_type The desired output type. This must be an integer
///                        type.
/// @param[in] lhs         The left-hand input value.
/// @param[in] rhs         The right-hand input value.
/// @return An SSA value representing the result of the operation. This value
///         will have the type `output_type`.
spidir_value_t spidir_builder_build_icmp(spidir_builder_handle_t builder,
                                         spidir_icmp_kind_t icmp_kind,
                                         spidir_value_type_t output_type,
                                         spidir_value_t lhs,
                                         spidir_value_t rhs);

/// Builds a pointer offset operation at the current insertion point. This
/// operation adds an integer byte offset to an existing pointer to produce a
/// new pointer value.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] kind    The kind of comparison to perform. See the `SPIDIR_ICMP_`
///                    constants for the allowed operation kinds.
/// @param[in] type    The desired output type. This must be an integer type.
/// @param[in] ptr     The pointer value to offset.
/// @param[in] off     The byte offset to add to the pointer.
/// @return An SSA value representing the result of the operation. This value
///         will have the type `type`.
spidir_value_t spidir_builder_build_ptroff(spidir_builder_handle_t builder,
                                           spidir_value_t ptr,
                                           spidir_value_t off);

/// Builds a load operation at the current insertion point.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] type    The type of the value to load from the pointer
/// @param[in] ptr     A value containing the pointer to load from.
/// @return An SSA value representing the result of the operation. This value
/// will have the type `type`.
spidir_value_t spidir_builder_build_load(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t ptr);

/// Builds a store operation at the current insertion point.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] data    An SSA value containing the data to store to memory.
/// @param[in] ptr     A value containing the pointer to store to.
spidir_value_t spidir_builder_build_store(spidir_builder_handle_t builder,
                                          spidir_value_t data,
                                          spidir_value_t ptr);

/// Builds a "stack slot" operation.
///
/// This operation is how stack values and locations are represented in spidir -
/// every stack slot operation returns a pointer that can be used to access a
/// stack location with some fixed size and alignment.
///
/// A given stack slot operation will always yield the same location, even if
/// ostensibly "executed" multiple times. For example, a stack slot built inside
/// a loop will return the same address on every iteration of the loop.
///
/// @param[in] builder A handle to the function builder.
/// @param[in] size    The desired size of the stack slot, in bytes.
/// @param[in] align   The desired alignment in memory of the stack slot. This
///                    value must be a power of 2.
spidir_value_t spidir_builder_build_stackslot(spidir_builder_handle_t builder,
                                              uint32_t size, uint32_t align);

#endif
