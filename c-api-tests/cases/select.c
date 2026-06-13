#include <spidir/module.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);
    spidir_builder_build_return(
        builder, spidir_builder_build_select(
                     builder, spidir_builder_build_param_ref(builder, 0),
                     spidir_builder_build_param_ref(builder, 1),
                     spidir_builder_build_param_ref(builder, 1)));
}

void build_select_func_with_ty(spidir_module_handle_t module, const char* name,
                               spidir_value_type_t type) {
    spidir_function_t func = spidir_module_create_function(
        module, name, type, 3,
        (spidir_value_type_t[]) {SPIDIR_TYPE_I32, type, type});
    spidir_module_build_function(module, func, builder_callback, NULL);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();
    build_select_func_with_ty(module, "select_i32", SPIDIR_TYPE_I32);
    build_select_func_with_ty(module, "select_i64", SPIDIR_TYPE_I64);
    build_select_func_with_ty(module, "select_ptr", SPIDIR_TYPE_PTR);
    build_select_func_with_ty(module, "select_f32", SPIDIR_TYPE_F32);
    build_select_func_with_ty(module, "select_f64", SPIDIR_TYPE_F64);
    dump_module_to_stdout(module);
    spidir_module_destroy(module);
    return 0;
}
