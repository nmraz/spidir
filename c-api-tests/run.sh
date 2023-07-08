#!/bin/bash

cd $(dirname $(readlink -f "$0"))

mkdir -p build &&
    cmake -S . -B build &&
    cmake --build build &&
    CTEST_OUTPUT_ON_FAILURE=1 cmake --build build --target test
