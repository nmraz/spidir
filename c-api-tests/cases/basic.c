#include <spidir/spidir.h>
#include <stddef.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;

    spidir_block_t entry_block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, entry_block);
    spidir_builder_set_block(builder, entry_block);

    spidir_value_t i_slot = spidir_builder_build_stackslot(builder, 4, 4);
    spidir_value_t sum_slot = spidir_builder_build_stackslot(builder, 4, 4);

    spidir_value_t n = spidir_builder_build_param_ref(builder, 0);
    spidir_builder_build_store(builder, n, i_slot);
    spidir_builder_build_store(
        builder, spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0),
        sum_slot);

    spidir_block_t loop_header = spidir_builder_create_block(builder);
    spidir_builder_build_branch(builder, loop_header);

    spidir_builder_set_block(builder, loop_header);
    spidir_value_t loop_cmp = spidir_builder_build_icmp(
        builder, SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32,
        spidir_builder_build_load(builder, SPIDIR_TYPE_I32, i_slot),
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0));

    spidir_block_t exit_block = spidir_builder_create_block(builder);
    spidir_block_t loop_body = spidir_builder_create_block(builder);

    spidir_builder_build_brcond(builder, loop_cmp, exit_block, loop_body);

    spidir_builder_set_block(builder, loop_body);
    spidir_value_t i_val =
        spidir_builder_build_load(builder, SPIDIR_TYPE_I32, i_slot);
    spidir_value_t sum_val =
        spidir_builder_build_load(builder, SPIDIR_TYPE_I32, sum_slot);

    spidir_value_t next_i_val = spidir_builder_build_isub(
        builder, SPIDIR_TYPE_I32, i_val,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 1));
    spidir_value_t next_sum_val =
        spidir_builder_build_iadd(builder, SPIDIR_TYPE_I32, sum_val, i_val);

    spidir_builder_build_store(builder, next_i_val, i_slot);
    spidir_builder_build_store(builder, next_sum_val, sum_slot);

    spidir_builder_build_branch(builder, loop_header);

    spidir_builder_set_block(builder, exit_block);
    spidir_value_t retval =
        spidir_builder_build_load(builder, SPIDIR_TYPE_I32, sum_slot);
    spidir_builder_build_return(builder, &retval);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t type = SPIDIR_TYPE_I32;
    spidir_value_type_t params[] = {SPIDIR_TYPE_I32};
    spidir_function_t func =
        spidir_module_create_function(module, "sum", &type, 1, params);
    spidir_module_build_function(module, func, builder_callback, NULL);
    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
