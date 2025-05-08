cmake_minimum_required(VERSION 3.21)
project(spidir-c-tests)
enable_testing()

add_compile_options(-Wall -Wextra -Werror)

set(SPIDIR_RUSTUP_TOOLCHAIN nightly-2025-05-07)
set(SPIDIR_CARGO_TARGET x86_64-unknown-linux-none)
set(SPIDIR_CARGO_FLAGS "-Zbuild-std=core,alloc")

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/../c-api spidir)
add_subdirectory(utils)

function(spidir_test_case_inner CASE_NAME SUFFIX)
    add_executable(test-${CASE_NAME} cases/${CASE_NAME}.c)
    target_link_libraries(test-${CASE_NAME} spidir-${SUFFIX} utils-${SUFFIX})
    add_test(
        NAME ${CASE_NAME}
        COMMAND ${CMAKE_CURRENT_LIST_DIR}/run-case.sh ${CMAKE_CURRENT_LIST_DIR}/cases/${CASE_NAME}.out "$<TARGET_FILE:test-${CASE_NAME}>"
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
endfunction()

function(spidir_test_case CASE_NAME)
    spidir_test_case_inner(${CASE_NAME} unlogged)
endfunction()

function(spidir_test_case_logged CASE_NAME)
    spidir_test_case_inner(${CASE_NAME} logged)
endfunction()

spidir_test_case(basic)
spidir_test_case(ret_void)
spidir_test_case(types)
spidir_test_case(arith)
spidir_test_case(phi)
spidir_test_case(funcaddr)
spidir_test_case(call)
spidir_test_case(callind)
spidir_test_case(memsize)
spidir_test_case(nested_build)
spidir_test_case(unreachable)
spidir_test_case(cur_block)
spidir_test_case_logged(log)
spidir_test_case(log_disabled)
spidir_test_case_logged(codegen)
spidir_test_case(opt)
