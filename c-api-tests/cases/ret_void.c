#include <spidir/module.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();
    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_NONE, 0, NULL);
    spidir_module_build_function(module, func, builder_callback, NULL);
    dump_module_to_stdout(module);
    spidir_module_destroy(module);
    return 0;
}
