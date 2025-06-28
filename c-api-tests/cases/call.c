#include <spidir/module.h>

#include "utils.h"

typedef struct {
    spidir_funcref_t extfunc;
    spidir_funcref_t extfunc2;
    spidir_funcref_t infunc;
    spidir_funcref_t infunc2;
} func_context_t;

void infunc_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}

void infunc2_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_builder_build_return(builder,
                                spidir_builder_build_param_ref(builder, 1));
}

void caller_builder_callback(spidir_builder_handle_t builder, void* ctx) {
    func_context_t* func_ctx = ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t ptr = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t ival = spidir_builder_build_param_ref(builder, 1);

    spidir_value_t args[] = {ptr, ival};

    ASSERT(spidir_funcref_get_kind(func_ctx->extfunc) ==
           SPIDIR_FUNCREF_EXTERNAL);
    ASSERT(spidir_funcref_is_external(func_ctx->extfunc));
    ASSERT(spidir_funcref_get_kind(func_ctx->infunc) ==
           SPIDIR_FUNCREF_INTERNAL);
    ASSERT(spidir_funcref_is_internal(func_ctx->infunc));

    spidir_builder_build_call(builder, func_ctx->extfunc, 2, args);
    spidir_value_t ext_ret =
        spidir_builder_build_call(builder, func_ctx->extfunc2, 2, args);
    spidir_builder_build_call(builder, func_ctx->infunc, 2, args);
    spidir_value_t in_ret =
        spidir_builder_build_call(builder, func_ctx->infunc2, 2, args);

    spidir_value_t retval = spidir_builder_build_iadd(builder, ext_ret, in_ret);
    spidir_builder_build_return(builder, retval);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t param_types[] = {
        SPIDIR_TYPE_PTR,
        SPIDIR_TYPE_I32,
    };

    spidir_extern_function_t extfunc = spidir_module_create_extern_function(
        module, "extfunc", SPIDIR_TYPE_NONE, 2, param_types);
    spidir_extern_function_t extfunc2 = spidir_module_create_extern_function(
        module, "extfunc2", SPIDIR_TYPE_I32, 2, param_types);

    spidir_function_t infunc = spidir_module_create_function(
        module, "infunc", SPIDIR_TYPE_NONE, 2, param_types);
    spidir_module_build_function(module, infunc, infunc_builder_callback, NULL);

    spidir_function_t infunc2 = spidir_module_create_function(
        module, "infunc2", SPIDIR_TYPE_I32, 2, param_types);
    spidir_module_build_function(module, infunc2, infunc2_builder_callback,
                                 NULL);

    func_context_t func_ctx = {
        .extfunc = spidir_funcref_make_external(extfunc),
        .extfunc2 = spidir_funcref_make_external(extfunc2),
        .infunc = spidir_funcref_make_internal(infunc),
        .infunc2 = spidir_funcref_make_internal(infunc2),
    };

    spidir_function_t caller = spidir_module_create_function(
        module, "caller", SPIDIR_TYPE_I32, 2, param_types);
    spidir_module_build_function(module, caller, caller_builder_callback,
                                 &func_ctx);

    dump_module_to_stdout(module);

    spidir_module_destroy(module);
    return 0;
}
