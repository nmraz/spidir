#include <spidir/module.h>

#include <stddef.h>

#include "utils.h"

void get_f_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    spidir_funcref_t f =
        spidir_funcref_make_external(*(spidir_extern_function_t*) ctx);
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_value_t f_addr = spidir_builder_build_funcaddr(builder, f);
    spidir_builder_build_return(builder, f_addr);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_extern_function_t f = spidir_module_create_extern_function(
        module, "f", SPIDIR_TYPE_NONE, 0, NULL);

    spidir_function_t get_f = spidir_module_create_function(
        module, "get_f", SPIDIR_TYPE_PTR, 0, NULL);
    spidir_module_build_function(module, get_f, get_f_builder_callback, &f);

    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
