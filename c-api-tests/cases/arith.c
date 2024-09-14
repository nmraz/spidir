#include <spidir/module.h>

#include <stddef.h>

#include "utils.h"

#define BUILD_BINOP(op) a = spidir_builder_build_##op(builder, a, b)

#define BUILD_ICMP(kind)                                                       \
    a = spidir_builder_build_icmp(builder, SPIDIR_ICMP_##kind,                 \
                                  SPIDIR_TYPE_I32, a, b)

void builder_callback(spidir_builder_handle_t builder, void* ctx) {
    (void) ctx;

    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, block);
    spidir_builder_set_block(builder, block);

    spidir_value_t a = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t b = spidir_builder_build_param_ref(builder, 0);

    BUILD_BINOP(iadd);
    BUILD_BINOP(isub);
    BUILD_BINOP(and);
    BUILD_BINOP(or);
    BUILD_BINOP(xor);
    BUILD_BINOP(shl);
    BUILD_BINOP(lshr);
    BUILD_BINOP(ashr);
    BUILD_BINOP(imul);
    BUILD_BINOP(sdiv);
    BUILD_BINOP(udiv);
    BUILD_BINOP(srem);
    BUILD_BINOP(urem);
    BUILD_ICMP(EQ);
    BUILD_ICMP(NE);
    BUILD_ICMP(SLT);
    BUILD_ICMP(SLE);
    BUILD_ICMP(ULT);
    BUILD_ICMP(ULE);

    a = spidir_builder_build_iext(builder, a);
    a = spidir_builder_build_inttoptr(builder, a);
    a = spidir_builder_build_ptrtoint(builder, a);
    a = spidir_builder_build_itrunc(builder, a);
    a = spidir_builder_build_sfill(builder, 16, a);

    spidir_builder_build_return(builder, a);
}

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_value_type_t ret_type = SPIDIR_TYPE_I32;
    spidir_value_type_t param_types[] = {SPIDIR_TYPE_I32, SPIDIR_TYPE_I32};
    spidir_function_t func = spidir_module_create_function(
        module, "arith_stuff", ret_type, 2, param_types);
    spidir_module_build_function(module, func, builder_callback, NULL);

    dump_module_to_stdout(module);

    spidir_module_destroy(module);

    return 0;
}
