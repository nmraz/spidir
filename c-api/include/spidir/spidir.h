#ifndef SPIDIR_SPIDIR_H
#define SPIDIR_SPIDIR_H

#include <stddef.h>
#include <stdint.h>

#include <cstdint>

typedef struct spidir_module* spidir_module_handle_t;
typedef struct spidir_builder* spidir_builder_handle_t;

typedef struct spidir_function {
    uint32_t id;
} spidir_function_t;

typedef struct spidir_extern_function {
    uint32_t id;
} spidir_extern_function_t;

typedef struct spidir_block {
    uint32_t id;
} spidir_block_t;

typedef struct spidir_value {
    uint32_t id;
} spidir_value_t;

typedef struct spidir_phi {
    uint32_t id;
} spidir_phi_t;

typedef uint8_t spidir_value_type_t;

enum spidir_value_type {
    SPIDIR_TYPE_I32 = 0,
    SPIDIR_TYPE_I64 = 1,
    SPIDIR_TYPE_F64 = 2,
    SPIDIR_TYPE_PTR = 3,
};

typedef uint8_t spidir_icmp_kind_t;

enum spidir_icmp_kind {
    SPIDIR_ICMP_EQ = 0,
    SPIDIR_ICMP_NE = 1,
    SPIDIR_ICMP_SLT = 2,
    SPIDIR_ICMP_SLE = 3,
    SPIDIR_ICMP_ULT = 4,
    SPIDIR_ICMP_ULE = 5,
};

typedef uint8_t spidir_dump_status_t;

enum spidir_dump_status {
    SPIDIR_DUMP_CONTINUE = 0,
    SPIDIR_DUMP_STOP = 1,
};

typedef void (*spidir_build_function_callback_t)(
    spidir_builder_handle_t builder, void* ctx);
typedef spidir_dump_status_t (*spidir_dump_callback_t)(const char* data,
                                                       size_t size, void* ctx);

spidir_module_handle_t spidir_module_create(void);
void spidir_module_destroy(spidir_module_handle_t module);

spidir_function_t
spidir_module_create_function(spidir_module_handle_t module, const char* name,
                              const spidir_value_type_t* ret_type,
                              size_t param_count,
                              const spidir_value_type_t* param_types);

spidir_extern_function_t spidir_module_create_extern_function(
    spidir_module_handle_t module, const char* name,
    const spidir_value_type_t* ret_type, size_t param_count,
    const spidir_value_type_t* param_types);

void spidir_module_build_function(spidir_module_handle_t module,
                                  spidir_function_t func,
                                  spidir_build_function_callback_t callback,
                                  void* ctx);

void spidir_module_dump(spidir_module_handle_t module,
                        spidir_dump_callback_t callback, void* ctx);

spidir_block_t spidir_builder_create_block(spidir_builder_handle_t builder);

void spidir_builder_set_block(spidir_builder_handle_t builder,
                              spidir_block_t block);
void spidir_builder_set_entry_block(spidir_builder_handle_t builder,
                                    spidir_block_t block);

spidir_value_t spidir_builder_build_param_ref(spidir_builder_handle_t builder,
                                              uint32_t index);

void spidir_builder_build_call(spidir_builder_handle_t builder,
                               const spidir_value_type_t* ret_type,
                               spidir_function_t func, size_t arg_count,
                               const spidir_value_t* args,
                               spidir_value_t* out_ret_value);

void spidir_builder_build_extern_call(spidir_builder_handle_t builder,
                                      const spidir_value_type_t* ret_type,
                                      spidir_function_t func, size_t arg_count,
                                      const spidir_value_t* args,
                                      spidir_value_t* out_ret_value);

void spidir_builder_build_return(spidir_builder_handle_t builder,
                                 const spidir_value_t* value);

void spidir_builder_build_branch(spidir_builder_handle_t builder,
                                 spidir_block_t dest);

void spidir_builder_build_brcond(spidir_builder_handle_t builder,
                                 spidir_value_t cond, spidir_block_t true_dest,
                                 spidir_block_t false_dest);

spidir_value_t spidir_builder_build_phi(spidir_builder_handle_t builder,
                                        spidir_value_type_t type,
                                        size_t input_count,
                                        const spidir_value_t* inputs,
                                        spidir_phi_t* out_phi_handle);

void spidir_builder_add_phi_input(spidir_builder_handle_t builder,
                                  spidir_phi_t phi, spidir_value_t input);

spidir_value_t spidir_builder_build_iconst(spidir_builder_handle_t builder,
                                           spidir_value_type_t type,
                                           uint64_t value);
spidir_value_t spidir_builder_build_fconst(spidir_builder_handle_t builder,
                                           double value);

spidir_value_t spidir_builder_build_iadd(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_isub(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_and(spidir_builder_handle_t builder,
                                        spidir_value_type_t type,
                                        spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_or(spidir_builder_handle_t builder,
                                       spidir_value_type_t type,
                                       spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_xor(spidir_builder_handle_t builder,
                                        spidir_value_type_t type,
                                        spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_shl(spidir_builder_handle_t builder,
                                        spidir_value_type_t type,
                                        spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_lshr(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_ashr(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_imul(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_sdiv(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);
spidir_value_t spidir_builder_build_udiv(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);

spidir_value_t spidir_builder_build_icmp(spidir_builder_handle_t builder,
                                         spidir_icmp_kind_t icmp_kind,
                                         spidir_value_type_t type,
                                         spidir_value_t a, spidir_value_t b);

spidir_value_t spidir_builder_build_ptroff(spidir_builder_handle_t builder,
                                           spidir_value_t ptr,
                                           spidir_value_t off);

spidir_value_t spidir_builder_build_load(spidir_builder_handle_t builder,
                                         spidir_value_type_t type,
                                         spidir_value_t ptr);
spidir_value_t spidir_builder_build_store(spidir_builder_handle_t builder,
                                          spidir_value_t data,
                                          spidir_value_t ptr);

spidir_value_t spidir_builder_build_stackslot(spidir_builder_handle_t builder,
                                              uint32_t size, uint32_t align);

#endif
