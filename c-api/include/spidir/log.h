/// @file platform.h
/// APIs for capturing internal spidir logging. These APIs will still exist, but
/// will silently do nothing, if logging has been disabled at compile-time.

#ifndef SPIDIR_LOG_H
#define SPIDIR_LOG_H

#include <stdint.h>
#include <stdlib.h>

/// A possible severity level of a spidir log.
///
/// See the `SPIDIR_LOG_LEVEL_` constants for possible values.
typedef uint8_t spidir_log_level_t;

enum {
    SPIDIR_LOG_LEVEL_NONE = 0,
    SPIDIR_LOG_LEVEL_ERROR = 1,
    SPIDIR_LOG_LEVEL_WARN = 2,
    SPIDIR_LOG_LEVEL_INFO = 3,
    SPIDIR_LOG_LEVEL_DEBUG = 4,
    SPIDIR_LOG_LEVEL_TRACE = 5,
};

/// A user callback function that can be used to capture logs from spidir.
///
/// @param[in] level       The severity of the reported log message.
/// @param[in] module      The (internal) name of the module reporting the
///                        message. This string may *not* be null-terminated, so
///                        implementations should always consult `module_len`
///                        explicitly.
/// @param[in] module_len  The length of `module`, in bytes.
/// @param[in] message     A string containing the log message.This string may
///                        *not* be null-terminated, so implementations should
///                        always consult `message_len` explicitly.
/// @param[in] message_len The length of the message, in bytes.
typedef void (*spidir_log_callback_t)(spidir_log_level_t level,
                                      const char* module, size_t module_len,
                                      const char* message, size_t message_len);

/// Initializes the logging infrastructure with the specified message callback.
/// If logging has been disabled at compile-time, this function will do nothing.
///
/// This function will panic if called more than once.
///
/// @note The default log level is `SPIDIR_LOG_LEVEL_NONE`, so callers should
/// use `spidir_log_set_max_level` to enable messages to be delivered.
///
/// @param[in] callback The callback used to capture logs.
void spidir_log_init(spidir_log_callback_t callback);

/// Sets the maximum level for logs to be delivered; all logs at or below the
/// specified level will be reported to the registered log callback.
///
/// Note that the logging level defaults to `SPIDIR_LOG_LEVEL_NONE`, so it
/// should be overridden with this function if logs are to be received.
void spidir_log_set_max_level(spidir_log_level_t level);

#endif
