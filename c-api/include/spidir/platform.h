/// @file platform.h
/// Platform-specific wrappers that must be provided by users of the library.
///
/// @note These functions only need to be implemented when linking against the
/// `c-api` crate as opposed to the raw `bindings` crate.

#ifndef SPIDIR_PLATFORM_H
#define SPIDIR_PLATFORM_H

#include <stddef.h>
#include <stdnoreturn.h>

/// Requests an allocation of the specified size and alignment.
///
/// @param[in] size  The desired size, in bytes.
/// @param[in] align The desired alignment. This value is guaranteed to be a
///                  power of 2.
/// @return A pointer to the newly-allocated memory, or null on failure.
void* spidir_platform_alloc(size_t size, size_t align);

/// Frees memory previously allocated by `spidir_platform_alloc` or
/// `spidir_platform_realloc`.
///
/// @param[in] ptr   A pointer to the memory region to be freed. This parameter
///                  is guaranteed to be non-null.
/// @param[in] size  The size of the region to be freed. This is guaranteed to
///                  be the size passed to the last call to
///                  `spidir_platform_alloc` or `spidir_platform_realloc` that
///                  returned `ptr`.
/// @param[in] align The alignment of the region to be freed. This is guaranteed
///                  to be the alignment value passed to the original call to
///                  `spidir_platform_alloc` and any subsequent calls to
///                  `spidir_platform_realloc`.
void spidir_platform_free(void* ptr, size_t size, size_t align);

/// Resizes a region of memory previously allocated by `spidir_platform_alloc`
/// or `spidir_platform_realloc`.
///
/// If this function returns a non-null pointer, the newly-returned pointer will
/// be used in place of `ptr` to access the region; if a new region is
/// allocated, the existing one should be deallocated and any data copied over.
///
/// If this function fails (by returning a null pointer), it should make sure
/// that the region pointed to by `ptr` is still allocated and otherwise
/// untouched.
///
/// This function will not be used to change the alignment of a region - the
/// alignment is always determined by the original call to
/// `spidir_platform_alloc`.
///
/// @param[in] ptr      A pointer to the region to resize.
/// @param[in] old_size The current size of the region. This is guaranteed to be
///                     the size passed to the last call to
///                     `spidir_platform_alloc` or `spidir_platform_realloc`
///                     that returned `ptr`.
/// @param[in] align    The alignment of the region. This is guaranteed to be
///                     the alignment value passed to the original call to
///                     `spidir_platform_alloc` and any subsequent calls to
///                     `spidir_platform_realloc`.
/// @param[in] new_size The desired new size of the region.
/// @return A pointer to the resized region on success, or null on failure. See
/// the general description for more detailed requirements on the return value.
void* spidir_platform_realloc(void* ptr, size_t old_size, size_t align,
                              size_t new_size);

/// Indicates an irrecoverable error within spidir.
///
/// This function should not return to its caller, and no spidir APIs should be
/// used after this function has been called.
///
/// @param[in] message     A string describing the reason for the panic. This
///                        string may *not* be null-terminated, so
///                        implementations should always consult `message_len`
///                        explicitly.
/// @param[in] message_len The length of the message, in bytes.
noreturn void spidir_platform_panic(const char* message, size_t message_len);

#endif
