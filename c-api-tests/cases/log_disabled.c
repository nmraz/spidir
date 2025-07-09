#include <spidir/log.h>
#include <spidir/module.h>

#include <stddef.h>

#include "utils.h"

#define BUILD_BINOP(op) a = spidir_builder_build_##op(builder, a, b)

#define BUILD_ICMP(kind)                                                       \
    a = spidir_builder_build_icmp(builder, SPIDIR_ICMP_##kind,                 \
                                  SPIDIR_TYPE_I32, a, b)

static void set_log_level(spidir_log_level_t level) {
    spidir_log_set_max_level(level);
    ASSERT(spidir_log_get_max_level() == SPIDIR_LOG_LEVEL_NONE);
}

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    set_log_level(SPIDIR_LOG_LEVEL_ERROR);
    spidir_value_t a = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t b = spidir_builder_build_param_ref(builder, 0);

    BUILD_BINOP(iadd);
    BUILD_BINOP(isub);
    BUILD_BINOP(and);
    set_log_level(SPIDIR_LOG_LEVEL_WARN);
    BUILD_BINOP(or);
    BUILD_BINOP(xor);
    set_log_level(SPIDIR_LOG_LEVEL_DEBUG);
    BUILD_BINOP(shl);
    BUILD_BINOP(lshr);
    BUILD_BINOP(ashr);
    BUILD_BINOP(imul);
    set_log_level(SPIDIR_LOG_LEVEL_TRACE);
    BUILD_BINOP(sdiv);
    BUILD_BINOP(udiv);
    BUILD_ICMP(EQ);
    BUILD_ICMP(NE);
    BUILD_ICMP(SLT);
    set_log_level(SPIDIR_LOG_LEVEL_INFO);
    BUILD_ICMP(SLE);
    BUILD_ICMP(ULT);
    BUILD_ICMP(ULE);

    a = spidir_builder_build_iext(builder, a);
    set_log_level(SPIDIR_LOG_LEVEL_NONE);
    a = spidir_builder_build_itrunc(builder, a);
    a = spidir_builder_build_sfill(builder, 16, a);

    spidir_builder_build_return(builder, a);
}

int main(void) {
    init_stdout_spidir_log();

    ASSERT(spidir_log_get_max_level() == SPIDIR_LOG_LEVEL_NONE);

    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t ret_type = SPIDIR_TYPE_I32;
    spidir_value_type_t param_types[] = {SPIDIR_TYPE_I32, SPIDIR_TYPE_I32};
    spidir_function_t func = spidir_module_create_function(
        module, "arith_stuff", ret_type, 2, param_types);
    spidir_module_build_function(module, func, builder_callback, NULL);
    spidir_module_destroy(module);

    return 0;
}
