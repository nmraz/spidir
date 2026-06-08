#include <spidir/codegen.h>
#include <spidir/module.h>
#include <spidir/x64.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t entry = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, entry);
    spidir_builder_set_block(builder, entry);
    spidir_builder_build_return(
        builder, spidir_builder_build_popcount(
                     builder, SPIDIR_TYPE_I32,
                     spidir_builder_build_param_ref(builder, 0)));
}

void check_popcnt_libcall(spidir_codegen_machine_handle_t machine,
                          spidir_value_type_t input_ty,
                          spidir_libcall_kind_t expected_libcall) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_I32, 1, (spidir_value_type_t[]) {input_ty});
    spidir_module_build_function(module, func, builder_callback, NULL);

    spidir_codegen_blob_handle_t code =
        codegen_function(machine, module, func, false);

    ASSERT(spidir_codegen_blob_get_reloc_count(code) == 1);

    const spidir_codegen_reloc_t* reloc = spidir_codegen_blob_get_relocs(code);
    ASSERT(reloc->kind == SPIDIR_RELOC_X64_PC32);
    ASSERT(reloc->target_kind == SPIDIR_RELOC_TARGET_LIBCALL);
    ASSERT(reloc->target.libcall == expected_libcall);

    spidir_module_destroy(module);
}

int main(void) {
    spidir_codegen_machine_handle_t machine =
        spidir_codegen_create_x64_machine_with_config(
            &(spidir_x64_machine_config_t) {
                .internal_code_model = SPIDIR_X64_CM_SMALL_PIC,
                .extern_code_model = SPIDIR_X64_CM_SMALL_PIC,

            });

    check_popcnt_libcall(machine, SPIDIR_TYPE_I32, SPIDIR_LIBCALL_X64_POPCNT32);
    check_popcnt_libcall(machine, SPIDIR_TYPE_I64, SPIDIR_LIBCALL_X64_POPCNT64);

    return 0;
}
