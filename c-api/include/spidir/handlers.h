#ifndef SPIDIR_HANDLERS_H
#define SPIDIR_HANDLERS_H

#include <stddef.h>
#include <stdnoreturn.h>

void* spidir_alloc(size_t size, size_t align);
void spidir_free(void* ptr, size_t size, size_t align);
void* spidir_realloc(void* ptr, size_t old_size, size_t align, size_t new_size);
noreturn void spidir_panic(const char* message, size_t message_len);

#endif
