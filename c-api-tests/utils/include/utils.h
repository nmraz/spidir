#ifndef SPIDIR_TESTS_UTILS_UTILS_H
#define SPIDIR_TESTS_UTILS_UTILS_H

#include <spidir/codegen.h>
#include <spidir/module.h>

#include <stdbool.h>

#define ASSERT(expr)                                                           \
    do {                                                                       \
        if (!(expr))                                                           \
            assert_failed(__FILE__, __LINE__, #expr);                          \
    } while (0)

void assert_failed(const char* file, int line, const char* expr);

void init_stdout_spidir_log(void);
void dump_module_to_stdout(spidir_module_handle_t module);

spidir_codegen_blob_handle_t
codegen_function(spidir_codegen_machine_handle_t machine,
                 spidir_module_handle_t module, spidir_function_t func,
                 bool verify_regalloc);

#endif
