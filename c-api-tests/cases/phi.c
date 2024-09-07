#include <spidir/module.h>

#include "utils.h"

void select_phi_prebuilt_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t entry = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, entry);
    spidir_builder_set_block(builder, entry);

    spidir_value_t selector = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t a = spidir_builder_build_param_ref(builder, 1);
    spidir_value_t b = spidir_builder_build_param_ref(builder, 2);

    spidir_block_t exit_block = spidir_builder_create_block(builder);
    spidir_builder_build_brcond(builder, selector, exit_block, exit_block);

    spidir_builder_set_block(builder, exit_block);
    spidir_value_t phi_inputs[] = {a, b};
    spidir_value_t val =
        spidir_builder_build_phi(builder, SPIDIR_TYPE_I32, 2, phi_inputs, NULL);
    spidir_builder_build_return(builder, val);
}

void select_phi_handle_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t entry = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, entry);
    spidir_builder_set_block(builder, entry);

    spidir_value_t selector = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t a = spidir_builder_build_param_ref(builder, 1);
    spidir_value_t b = spidir_builder_build_param_ref(builder, 2);

    spidir_block_t exit_block = spidir_builder_create_block(builder);
    spidir_builder_build_brcond(builder, selector, exit_block, exit_block);

    spidir_builder_set_block(builder, exit_block);
    spidir_phi_t phi;
    spidir_value_t val =
        spidir_builder_build_phi(builder, SPIDIR_TYPE_I32, 0, NULL, &phi);

    spidir_builder_add_phi_input(builder, phi, a);
    spidir_builder_add_phi_input(builder, phi, b);

    spidir_builder_build_return(builder, val);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t ret_type = SPIDIR_TYPE_I32;
    spidir_value_type_t param_types[] = {SPIDIR_TYPE_I32, SPIDIR_TYPE_I32,
                                         SPIDIR_TYPE_I32};

    spidir_function_t phi_prebuilt_func = spidir_module_create_function(
        module, "select_phi_prebuilt", ret_type, 3, param_types);
    spidir_module_build_function(module, phi_prebuilt_func,
                                 select_phi_prebuilt_callback, NULL);

    spidir_function_t phi_handle_func = spidir_module_create_function(
        module, "select_phi_handle", ret_type, 3, param_types);
    spidir_module_build_function(module, phi_handle_func,
                                 select_phi_handle_callback, NULL);

    dump_module_to_stdout(module);

    spidir_module_destroy(module);

    return 0;
}
