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

void check_popcnt_nolibcall(spidir_codegen_machine_handle_t machine,
                            spidir_value_type_t input_ty) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_I32, 1, (spidir_value_type_t[]) {input_ty});
    spidir_module_build_function(module, func, builder_callback, NULL);

    dump_module_to_stdout(module);

    spidir_codegen_blob_handle_t code =
        codegen_function(machine, module, func, false);

    ASSERT(spidir_codegen_blob_get_reloc_count(code) == 0);

    spidir_module_destroy(module);
}

int main(void) {
    spidir_codegen_machine_handle_t machine =
        spidir_codegen_create_x64_machine_with_config(
            &(spidir_x64_machine_config_t) {
                .internal_code_model = SPIDIR_X64_CM_SMALL_PIC,
                .extern_code_model = SPIDIR_X64_CM_SMALL_PIC,
                .cpu_features.popcnt = true,
            });

    check_popcnt_nolibcall(machine, SPIDIR_TYPE_I32);
    check_popcnt_nolibcall(machine, SPIDIR_TYPE_I64);

    return 0;
}
