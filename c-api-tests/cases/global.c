#include <spidir/codegen.h>
#include <spidir/module.h>
#include <spidir/x64.h>

#include <stdbool.h>
#include <stddef.h>

#include "utils.h"

void get_global_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    spidir_extern_global_t global = *(spidir_extern_global_t*) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_value_t global_addr =
        spidir_builder_build_globaladdr(builder, global);
    spidir_builder_build_return(builder, global_addr);
}

void codegen_and_check(spidir_module_handle_t module,
                       spidir_function_t get_global,
                       spidir_extern_global_t global,
                       spidir_codegen_machine_handle_t machine,
                       spidir_reloc_kind_t expected_reloc_kind) {
    spidir_codegen_blob_handle_t code =
        codegen_function(machine, module, get_global, false);

    ASSERT(spidir_codegen_blob_get_reloc_count(code) == 1);

    const spidir_codegen_reloc_t* reloc = spidir_codegen_blob_get_relocs(code);
    ASSERT(reloc->kind == expected_reloc_kind);
    ASSERT(reloc->target_kind == SPIDIR_RELOC_TARGET_GLOBAL);
    ASSERT(reloc->target.global.id == global.id);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_extern_global_t global =
        spidir_module_create_extern_global(module, "global");

    spidir_function_t get_global = spidir_module_create_function(
        module, "get_global", SPIDIR_TYPE_PTR, 0, NULL);
    spidir_module_build_function(module, get_global,
                                 get_global_builder_callback, &global);

    spidir_codegen_machine_handle_t default_machine =
        spidir_codegen_create_x64_machine();
    codegen_and_check(module, get_global, global, default_machine,
                      SPIDIR_RELOC_X64_ABS64);
    spidir_codegen_machine_destroy(default_machine);

    spidir_codegen_machine_handle_t large_machine =
        spidir_codegen_create_x64_machine_with_config(
            &((spidir_x64_machine_config_t) {
                .internal_code_model = SPIDIR_X64_CM_LARGE_ABS,
                .extern_code_model = SPIDIR_X64_CM_LARGE_ABS,
            }));
    codegen_and_check(module, get_global, global, large_machine,
                      SPIDIR_RELOC_X64_ABS64);
    spidir_codegen_machine_destroy(large_machine);

    spidir_codegen_machine_handle_t small_machine =
        spidir_codegen_create_x64_machine_with_config(
            &((spidir_x64_machine_config_t) {
                .internal_code_model = SPIDIR_X64_CM_SMALL_PIC,
                .extern_code_model = SPIDIR_X64_CM_SMALL_PIC,
            }));
    codegen_and_check(module, get_global, global, small_machine,
                      SPIDIR_RELOC_X64_PC32);
    spidir_codegen_machine_destroy(small_machine);

    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
