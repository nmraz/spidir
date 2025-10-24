#include <spidir/module.h>

#include "utils.h"

void builder_callback64(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t f = spidir_builder_build_fconst64(builder, 64.0);

    spidir_builder_build_return(builder, f);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_function_t func64 = spidir_module_create_function(
        module, "func64", SPIDIR_TYPE_F64, 0, NULL);
    spidir_module_build_function(module, func64, builder_callback64, NULL);

    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
