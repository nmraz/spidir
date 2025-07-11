#!/bin/bash

set -euo pipefail

cd $(dirname $(readlink -f "$0"))

export CMAKE_EXPORT_COMPILE_COMMANDS=ON
export CTEST_OUTPUT_ON_FAILURE=1

mkdir -p build
cmake -S . -B build
cmake --build build -j$(nproc)
cmake --build build --target test -j$(nproc)
