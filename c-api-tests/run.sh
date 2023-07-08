#!/bin/bash

cd $(dirname $(readlink -f "$0"))

export CMAKE_EXPORT_COMPILE_COMMANDS=ON
export CTEST_OUTPUT_ON_FAILURE=1

mkdir -p build &&
    cmake -S . -B build &&
    cmake --build build &&
    cmake --build build --target test
