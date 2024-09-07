#include <spidir/module.h>

#include <stddef.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t target1 = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t target2 = spidir_builder_build_param_ref(builder, 1);
    spidir_value_t target3 = spidir_builder_build_param_ref(builder, 2);

    spidir_value_type_t arg_types[] = {SPIDIR_TYPE_I32};

    spidir_value_t retval1 = spidir_builder_build_callind(
        builder, SPIDIR_TYPE_I32, 0, NULL, target1, NULL);
    spidir_value_t retval2 = spidir_builder_build_callind(
        builder, SPIDIR_TYPE_I32, 1, arg_types, target2, &retval1);
    spidir_value_t retval3 = spidir_builder_build_callind(
        builder, SPIDIR_TYPE_NONE, 1, arg_types, target3, &retval2);

    ASSERT(retval3.id == SPIDIR_VALUE_INVALID.id);

    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t param_types[] = {SPIDIR_TYPE_PTR, SPIDIR_TYPE_PTR,
                                         SPIDIR_TYPE_PTR};
    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_NONE, 3, param_types);

    spidir_module_build_function(module, func, builder_callback, NULL);

    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
