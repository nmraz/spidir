#include <spidir/module.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t ptr = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t val = spidir_builder_build_param_ref(builder, 1);

    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_1, val, ptr);
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_2, val, ptr);
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_4, val, ptr);
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, val, ptr);

    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t param_types[] = {SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64};

    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_NONE, 2, param_types);
    spidir_module_build_function(module, func, builder_callback, NULL);
    dump_module_to_stdout(module);
    spidir_module_destroy(module);
    return 0;
}
