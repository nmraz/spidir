#include <spidir/module.h>

#include <stddef.h>

#include "utils.h"

int main(void) {
    spidir_module_handle_t module = spidir_module_create();

    spidir_module_create_extern_global(module, "global");

    dump_module_to_stdout(module);
    spidir_module_destroy(module);

    return 0;
}
