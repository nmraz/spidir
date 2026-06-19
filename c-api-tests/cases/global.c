#include <spidir/module.h>

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

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_extern_global_t global =
        spidir_module_create_extern_global(module, "global");

    spidir_function_t get_global = spidir_module_create_function(
        module, "get_global", SPIDIR_TYPE_PTR, 0, NULL);
    spidir_module_build_function(module, get_global,
                                 get_global_builder_callback, &global);

    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
