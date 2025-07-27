# Spidir

A JIT library written in Rust for use on freestanding targets.

Spidir is an immature hobby project and shouldn't be taken too seriously, but hey, it can generate (occasionally dumb) x64 code!

## Why?

Spidir was originally created for use in the [TomatoDotNet](https://github.com/TomatOrg/TomatoDotNet/) runtime, which needs a freestanding, optimizing JIT compiler with a simple C API. There still isn't much in the way of optimization, but Spidir does export a fairly clean C API and builds given only an allocator and panic hook.

## Using Spidir

If you just want to play around with the JIT, all important functionality is exposed via the CLI binary `spidirtool`. It can be used to:

- Display a function's IR using graphviz
- Validate a module's IR
- View internal pass output
- Generate code for and execute a given IR module

Although not much is documented, you can get a quick taste of what Spidir can do by running:

```sh
# Dump disassembled code
cargo run -p spidirtool compile crates/filetests/cases/codegen/wasm/factorial_O3.spdr

# Run it!
cargo run -p spidirtool codegen-exec crates/filetests/cases/codegen/wasm/factorial_O3.spdr func00000001 6
```

For more information about the CLI tool, see:

```sh
cargo run -p spidirtool help
```

### Linking against Spidir

The C API is the intended way to consume Spidir and will probably remain the most stable over time. The internal Rust crates can be used as well, but they tend to experience more churn as none of them are considered public API boundaries.

The simplest way to use the C API on a bare-metal target is to build `libspidir.a` from the `c-api` crate and implement the functions declared in `<spidir/platform.h>`; see the `c-api-tests` for an example of this setup.

If you're building for a hosted Rust target platform or need to link against other Rust code in your project, you'll probably want a "glue" Rust crate that depends directly on the internal `bindings` crate.

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

Spidir's roadmap is most accurately described as "whatever TomatoDotNet needs next", but here's some stuff we'll probably want eventually (in no particular order):

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

## Implementation Details

### IR

Spidir uses two intermediate representations internally:

- Spidir: target-independent IR for high-level optimizations
  - SSA-based sea-of-nodes representation
  - Uses phi nodes to model dataflow merges (easier to integrate in a sea-of-nodes setup)
- LIR (linear IR): target-dependent IR used after instruction selection
  - Flat list of instructions with CFG information stored separately
  - Still in SSA form, but uses block parameters instead of phi nodes (more elegant for block-based IRs)
  - Used by register allocator and final code emission

These are abstracted away by the C construction API, which provides a simple block-based facade over the sea-of-nodes graph.
