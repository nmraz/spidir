#include <spidir/module.h>

#include "utils.h"

#define BUILD_FCMP(name, kind)                                                 \
    spidir_value_t name = spidir_builder_build_fcmp(                           \
        builder, SPIDIR_FCMP_##kind, SPIDIR_TYPE_I32, a, b)

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t a = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t b = spidir_builder_build_param_ref(builder, 1);

    BUILD_FCMP(oeq, OEQ);
    BUILD_FCMP(one, ONE);
    BUILD_FCMP(olt, OLT);
    BUILD_FCMP(ole, OLE);

    BUILD_FCMP(ueq, UEQ);
    BUILD_FCMP(une, UNE);
    BUILD_FCMP(ult, ULT);
    BUILD_FCMP(ule, ULE);

    spidir_value_t oe = spidir_builder_build_or(builder, oeq, one);
    spidir_value_t ol = spidir_builder_build_or(builder, olt, ole);
    spidir_value_t o = spidir_builder_build_or(builder, oe, ol);

    spidir_value_t ue = spidir_builder_build_or(builder, ueq, une);
    spidir_value_t ul = spidir_builder_build_or(builder, ult, ule);
    spidir_value_t u = spidir_builder_build_or(builder, ue, ul);

    spidir_value_t retval = spidir_builder_build_or(builder, o, u);

    spidir_builder_build_return(builder, retval);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t param_types[] = {SPIDIR_TYPE_F64, SPIDIR_TYPE_F64};
    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_I32, 2, param_types);
    spidir_module_build_function(module, func, builder_callback, NULL);
    dump_module_to_stdout(module);
    spidir_module_destroy(module);
    return 0;
}
