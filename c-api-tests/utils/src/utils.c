#include "utils.h"

#include <spidir/codegen.h>
#include <spidir/log.h>
#include <spidir/spidir.h>
#include <spidir/x64.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

void assert_failed(const char* file, int line, const char* expr) {
    fprintf(stderr, "assertion failed at %s:%d: `%s`\n", file, line, expr);
    abort();
}

static const char* spidir_log_level_to_string(spidir_log_level_t level) {
    switch (level) {
    case SPIDIR_LOG_LEVEL_ERROR:
        return "ERROR";
    case SPIDIR_LOG_LEVEL_WARN:
        return "WARN";
    case SPIDIR_LOG_LEVEL_INFO:
        return "INFO";
    case SPIDIR_LOG_LEVEL_DEBUG:
        return "DEBUG";
    case SPIDIR_LOG_LEVEL_TRACE:
        return "TRACE";
    default:
        return "LOG";
    }
}

static void stdout_log_callback(spidir_log_level_t level, const char* module,
                                size_t module_len, const char* message,
                                size_t message_len) {
    printf("[%s %.*s] %.*s\n", spidir_log_level_to_string(level),
           (int) module_len, module, (int) message_len, message);
}

void init_stdout_spidir_log(void) {
    spidir_log_init(stdout_log_callback);
}

static spidir_dump_status_t stdout_dump_callback(const char* s, size_t size,
                                                 void* ctx) {
    (void) ctx;
    fwrite(s, 1, size, stdout);
    return SPIDIR_DUMP_CONTINUE;
}

void dump_module_to_stdout(spidir_module_handle_t module) {
    spidir_module_dump(module, stdout_dump_callback, NULL);
}

spidir_codegen_blob_handle_t codegen_function(spidir_module_handle_t module,
                                              spidir_function_t func) {
    spidir_codegen_blob_handle_t blob = NULL;

    spidir_codegen_machine_handle_t machine =
        spidir_codegen_create_x64_machine();

    ASSERT(spidir_codegen_emit_function(machine, module, func, &blob) ==
           SPIDIR_CODEGEN_OK);

    spidir_codegen_machine_destroy(machine);

    return blob;
}
