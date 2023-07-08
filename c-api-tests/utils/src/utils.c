#include "utils.h"

#include <spidir/spidir.h>
#include <stddef.h>
#include <stdio.h>

static spidir_dump_status_t stdout_dump_callback(const char* s, size_t size,
                                                 void* ctx) {
    (void) ctx;
    fwrite(s, 1, size, stdout);
    return SPIDIR_DUMP_CONTINUE;
}

void dump_module_to_stdout(spidir_module_handle_t module) {
    spidir_module_dump(module, stdout_dump_callback, NULL);
}
