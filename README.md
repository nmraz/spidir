# Spidir

A (WIP) JIT library written in Rust for use on freestanding targets.

Design tenets:

- Embeddability: Spidir is completely freestanding and requires only a memory allocator
- Simple C API: The API should feel natural to use directly in C code and is the intended way to consume Spidir
- Reasonable optimization quality: Higher-level language runtimes using Spidir as a backend should eventually be able to expect decent codegen

Spidir was originally created for use in [TomatoDotNet](https://github.com/TomatOrg/TomatoDotNet/), so feature development is guided primarily by the needs of that project.

## Using Spidir

There are two ways to link against Spidir in C code:

1. Using the `c-api` crate to build `libspidir.a` and implementing the platform API declared in `<spidir/platform.h>`. This method is most suitable for bare-metal users that don't want to link against any other Rust code.

2. Creating a "glue" Rust crate that depends on the lower-level `bindings` crate and making sure the Rust allocation/panic hooks are implemented. This method is better for users that need to link against other Rust code or use the hosted Rust standard library.

See the `c-api-tests` directory for programs using the former method.

All important functionality is exposed via the CLI binary `spidirtool`, which can be used to:

- Graph the IR using graphviz
- Inspect internal state
- Generate/execute code for a given IR module

See

```
cargo run -p spidirtool help
```

for more information.

## IR

Spidir uses two intermediate representations internally:

- Spidir: target-independent IR for high-level optimizations
  - SSA-based sea-of-nodes representation
  - Uses phi nodes to model dataflow merges (easier to integrate in a sea-of-nodes setup)
- LIR (linear IR): target-dependent IR used after instruction selection
  - Flat list of instructions with CFG information stored separately
  - Still in SSA form, but uses block parameters instead of phi nodes (more elegant for block-based IRs)
  - Used by register allocator and final code emission

These are abstracted away by the C construction API, which provides a simple block-based facade over the sea-of-nodes graph.

## Implementation Status

### Implemented

- Miscellaneous:

  - C bindings: IR construction/dumping/optimization/codegen
  - IR text format/parser for testing and debugging
  - IR verifier

- IR features:

  - Basic integer arithmetic/bitwise instructions
  - 64-bit floating point arithmetic
  - Loads/stores
  - Basic control flow (conditional branches)
  - Internal/external/indirect calls
  - External functions
  - Stack slots

- Codegen:

  - Basic x64 backend
  - Hopefully-intelligent register allocator
    - Live range splitting
    - Rematerialization
    - Symbolic verifier
    - Instruction commutation/3-address conversion

- Optimization:

  - Global code motion
  - Simple canonicalization/GVN

### Planned

- IR:

  - Memory operations (`memset`, `memcpy`, etc.)
  - Bit-counting operations
  - External globals
  - Cold block annotation
  - 32-bit floating point arithmetic
  - Unwinding support
  - Jump tables
  - Static branch/patchpoint support

- Codegen:

  - Improved x64 instruction selection
  - Minor register allocator improvements
    - Spill slot coloring
    - Better stack access rewrites
    - Improved redundant copy elimination?

- Optimization:
  - Improved global/local scheduling
  - SROA
  - SCCP
  - Jump threading
  - Range check elimination

## Projects Using Spidir

- [TomatoDotNet](https://github.com/TomatOrg/TomatoDotNet/), a freestanding C# runtime (original project motivation)
- [spidir-wasm](https://github.com/Itay2805/spidir-wasm/), a small WASM implementation showcasing the C API
