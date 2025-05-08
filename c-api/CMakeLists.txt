option(SPIDIR_ENABLE_LOGGING "Enable spidir logs to be captured via logging API")
set(SPIDIR_CARGO_TARGET "" CACHE STRING "Override target passed to cargo invocations")
set(SPIDIR_CARGO_FLAGS "" CACHE STRING "Additional flags to pass to cargo")
set(SPIDIR_RUSTUP_TOOLCHAIN "" CACHE STRING "Override rust toolchain used to build spidir")

function(define_spidir_library SUFFIX CARGO_FLAGS)
    set(CRATE_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/${SUFFIX}/cargo-target)
    set(SPIDIR_LIB_PATH ${CRATE_TARGET_DIR}/${SPIDIR_CARGO_TARGET}/release/libspidir.a)

    if (SPIDIR_CARGO_TARGET)
        list(APPEND CARGO_FLAGS --target ${SPIDIR_CARGO_TARGET})
    endif()

    if (SPIDIR_RUSTUP_TOOLCHAIN)
        set(TOOLCHAIN_OVERRIDE +${SPIDIR_RUSTUP_TOOLCHAIN})
    endif()

    add_custom_target(spidir-crate-${SUFFIX}
        COMMAND cargo ${TOOLCHAIN_OVERRIDE} build --release -p c-api ${CARGO_FLAGS} ${SPIDIR_CARGO_FLAGS} --target-dir ${CRATE_TARGET_DIR}
        COMMENT "Building spidir (${SUFFIX}) library"
        BYPRODUCTS ${SPIDIR_LIB_PATH}
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
        USES_TERMINAL)
    add_library(spidir-${SUFFIX} INTERFACE)
    add_dependencies(spidir-${SUFFIX} spidir-crate-${SUFFIX})
    target_include_directories(spidir-${SUFFIX} INTERFACE ${CMAKE_CURRENT_LIST_DIR}/include)
    target_link_libraries(spidir-${SUFFIX} INTERFACE ${SPIDIR_LIB_PATH})
endfunction()

define_spidir_library(unlogged "--features;no_logging")
define_spidir_library(logged "")

add_library(spidir INTERFACE)
if (SPIDIR_ENABLE_LOGGING)
    target_link_libraries(spidir INTERFACE spidir-logged)
else()
    target_link_libraries(spidir INTERFACE spidir-unlogged)
endif()
