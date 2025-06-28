#include <spidir/codegen.h>
#include <spidir/log.h>
#include <spidir/module.h>
#include <spidir/x64.h>

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

#include "utils.h"

#define PAGE_SIZE 0x1000
#define FUNCTION_ALIGN 0x10

typedef struct {
    spidir_funcref_t inner;
} func_context_t;

size_t align_up(size_t val, size_t align) {
    return (val + align - 1) & -align;
}

void inner_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_value_t param = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t two =
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 2);
    spidir_value_t retval = spidir_builder_build_iadd(builder, param, two);
    spidir_builder_build_return(builder, retval);
}

void outer_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    func_context_t* func_ctx = ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t five =
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 5);
    spidir_value_t retval =
        spidir_builder_build_call(builder, func_ctx->inner, 1, &five);

    spidir_builder_build_return(builder, retval);
}

void apply_relocs(uintptr_t base, spidir_funcref_t inner, uintptr_t inner_addr,
                  spidir_funcref_t outer, uintptr_t outer_addr,
                  const spidir_codegen_reloc_t* relocs, size_t reloc_count,
                  spidir_reloc_kind_t expected_reloc) {
    for (size_t i = 0; i < reloc_count; i++) {
        const spidir_codegen_reloc_t* reloc = &relocs[i];
        uintptr_t patch_addr = base + reloc->offset;
        uint64_t target_value = 0;

        if (reloc->target.id == inner.id) {
            target_value = inner_addr;
        } else if (reloc->target.id == outer.id) {
            target_value = outer_addr;
        } else {
            ASSERT(!"unknown relocation target");
        }

        ASSERT(reloc->kind == expected_reloc);

        switch (reloc->kind) {
        case SPIDIR_RELOC_X64_PC32: {
            uint32_t reloc_value = target_value + reloc->addend - patch_addr;
            memcpy((void*) patch_addr, &reloc_value, sizeof(uint32_t));
            break;
        }
        case SPIDIR_RELOC_X64_ABS64: {
            uint64_t reloc_value = target_value + reloc->addend;
            memcpy((void*) patch_addr, &reloc_value, sizeof(uint64_t));
            break;
        }
        default:
            ASSERT(!"unknown relocation kind");
        }
    }
}

void map_and_run(spidir_funcref_t inner,
                 spidir_codegen_blob_handle_t inner_code,
                 spidir_funcref_t outer,
                 spidir_codegen_blob_handle_t outer_code,
                 spidir_reloc_kind_t expected_reloc) {
    size_t inner_size = spidir_codegen_blob_get_code_size(inner_code);
    size_t outer_size = spidir_codegen_blob_get_code_size(outer_code);

    size_t map_size =
        align_up(inner_size + outer_size + FUNCTION_ALIGN, PAGE_SIZE);
    uint8_t* map_base = mmap(NULL, map_size, PROT_READ | PROT_WRITE,
                             MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    ASSERT(map_base != MAP_FAILED);

    memset(map_base, 0xcc, map_size);

    uintptr_t inner_base = (uintptr_t) map_base;
    uintptr_t outer_base = align_up(inner_base + inner_size, FUNCTION_ALIGN);

    memcpy((void*) inner_base, spidir_codegen_blob_get_code(inner_code),
           inner_size);
    memcpy((void*) outer_base, spidir_codegen_blob_get_code(outer_code),
           outer_size);

    size_t inner_reloc_count = spidir_codegen_blob_get_reloc_count(inner_code);
    size_t outer_reloc_count = spidir_codegen_blob_get_reloc_count(outer_code);

    // Make sure we actually have some relocations in the emitted code.
    ASSERT(inner_reloc_count + outer_reloc_count > 0);

    apply_relocs(inner_base, inner, inner_base, outer, outer_base,
                 spidir_codegen_blob_get_relocs(inner_code), inner_reloc_count,
                 expected_reloc);
    apply_relocs(outer_base, inner, inner_base, outer, outer_base,
                 spidir_codegen_blob_get_relocs(outer_code), outer_reloc_count,
                 expected_reloc);

    ASSERT(mprotect((void*) map_base, map_size, PROT_READ | PROT_EXEC) == 0);

    int32_t retval = ((int (*)(void)) outer_base)();
    ASSERT(retval == 7);
}

void codegen_and_run(spidir_module_handle_t module, spidir_function_t inner,
                     spidir_function_t outer,
                     spidir_codegen_machine_handle_t machine,
                     bool verify_regalloc, spidir_reloc_kind_t expected_reloc) {
    spidir_codegen_blob_handle_t inner_code =
        codegen_function(machine, module, inner, verify_regalloc);
    spidir_codegen_blob_handle_t outer_code =
        codegen_function(machine, module, outer, verify_regalloc);
    map_and_run(spidir_funcref_make_internal(inner), inner_code,
                spidir_funcref_make_internal(outer), outer_code,
                expected_reloc);
    spidir_codegen_blob_destroy(inner_code);
    spidir_codegen_blob_destroy(outer_code);
}

int main(void) {
    init_stdout_spidir_log();
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_DEBUG);

    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t inner_param_types[] = {
        SPIDIR_TYPE_I32,
    };

    spidir_function_t inner = spidir_module_create_function(
        module, "inner", SPIDIR_TYPE_I32, 1, inner_param_types);
    spidir_module_build_function(module, inner, inner_builder_callback, NULL);

    func_context_t func_ctx = {
        .inner = spidir_funcref_make_internal(inner),
    };

    spidir_function_t outer = spidir_module_create_function(
        module, "outer", SPIDIR_TYPE_I32, 0, NULL);
    spidir_module_build_function(module, outer, outer_builder_callback,
                                 &func_ctx);

    spidir_codegen_machine_handle_t default_machine =
        spidir_codegen_create_x64_machine();
    codegen_and_run(module, inner, outer, default_machine, false,
                    SPIDIR_RELOC_X64_PC32);
    codegen_and_run(module, inner, outer, default_machine, true,
                    SPIDIR_RELOC_X64_PC32);
    spidir_codegen_machine_destroy(default_machine);

    spidir_codegen_machine_handle_t large_machine =
        spidir_codegen_create_x64_machine_with_config(
            &((spidir_x64_machine_config_t) {
                .internal_code_model = SPIDIR_X64_CM_LARGE_ABS,
                .extern_code_model = SPIDIR_X64_CM_LARGE_ABS,
            }));
    codegen_and_run(module, inner, outer, large_machine, false,
                    SPIDIR_RELOC_X64_ABS64);
    spidir_codegen_machine_destroy(large_machine);

    spidir_codegen_machine_handle_t small_machine =
        spidir_codegen_create_x64_machine_with_config(
            &((spidir_x64_machine_config_t) {
                .internal_code_model = SPIDIR_X64_CM_SMALL_PIC,
                .extern_code_model = SPIDIR_X64_CM_SMALL_PIC,
            }));
    codegen_and_run(module, inner, outer, small_machine, false,
                    SPIDIR_RELOC_X64_PC32);
    spidir_codegen_machine_destroy(small_machine);

    return 0;
}
