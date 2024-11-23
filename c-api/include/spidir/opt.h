/// @file opt.h
/// Spidir optimization API.

#ifndef SPIDIR_OPT_H
#define SPIDIR_OPT_H

#include <spidir/module.h>

/// Optimizes all functions defined in the specified module.
///
/// This may involve both intra-procedural and inter-procedural transformations.
///
/// @param[in] module The module to optimize.
void spidir_opt_optimize_module(spidir_module_handle_t module);

#endif
