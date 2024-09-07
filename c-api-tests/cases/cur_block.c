#include <spidir/module.h>

#include "utils.h"

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;
    spidir_block_t cur_block;
    ASSERT(!spidir_builder_cur_block(builder, &cur_block));

    spidir_block_t block1 = spidir_builder_create_block(builder);
    spidir_block_t block2 = spidir_builder_create_block(builder);

    spidir_builder_set_block(builder, block1);
    ASSERT(spidir_builder_cur_block(builder, &cur_block));
    ASSERT(cur_block.id == block1.id);

    spidir_builder_set_block(builder, block2);
    ASSERT(spidir_builder_cur_block(builder, &cur_block));
    ASSERT(cur_block.id == block2.id);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();
    spidir_function_t func = spidir_module_create_function(
        module, "func", SPIDIR_TYPE_NONE, 0, NULL);
    spidir_module_build_function(module, func, builder_callback, NULL);
}
