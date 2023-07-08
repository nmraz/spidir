#include <spidir/handlers.h>
#include <stdalign.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void* spidir_alloc(size_t size, size_t align) {
    if (align <= alignof(max_align_t)) {
        // Get a `realloc`-able pointer where possible
        return malloc(size);
    } else {
        return aligned_alloc(align, size);
    }
}

void spidir_free(void* ptr, size_t size, size_t align) {
    free(ptr);
}

void* spidir_realloc(void* ptr, size_t old_size, size_t align,
                     size_t new_size) {
    if (align <= alignof(max_align_t)) {
        return realloc(ptr, new_size);
    } else {
        void* new_ptr = aligned_alloc(align, new_size);
        if (!new_ptr) {
            return NULL;
        }
        memcpy(new_ptr, ptr, old_size);
        free(ptr);
        return new_ptr;
    }
}

noreturn void spidir_panic(const char* message, size_t message_len) {
    fwrite(message, 1, message_len, stderr);
    fputs("\n", stderr);
    abort();
}
