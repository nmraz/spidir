#include <spidir/module.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, block);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_build_return(builder,
                                spidir_builder_build_param_ref(builder, 0));
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();
    spidir_value_type_t param_types[] = {
        SPIDIR_TYPE_I32,
        SPIDIR_TYPE_I64,
        SPIDIR_TYPE_F64,
        SPIDIR_TYPE_PTR,
    };
    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_I32, 4, param_types);
    spidir_module_build_function(module, func, builder_callback, NULL);
    dump_module_to_stdout(module);
    spidir_module_destroy(module);
    return 0;
}
