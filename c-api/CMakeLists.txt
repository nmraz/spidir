function(define_spidir_library SUFFIX CARGO_FEATURES)
    set(CRATE_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/${SUFFIX}/cargo-target)
    set(SPIDIR_LIB_PATH ${CRATE_TARGET_DIR}/x86_64-unknown-none/release/libspidir.a)
    add_custom_target(spidir-crate-${SUFFIX}
        COMMAND cargo build --release -p c-api --target x86_64-unknown-none --features "${CARGO_FEATURES}" --target-dir ${CRATE_TARGET_DIR}
        COMMENT "Building spidir (${SUFFIX}) library"
        BYPRODUCTS ${SPIDIR_LIB_PATH}
        WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
        USES_TERMINAL)
    add_library(spidir-${SUFFIX} INTERFACE)
    add_dependencies(spidir-${SUFFIX} spidir-crate-${SUFFIX})
    target_include_directories(spidir-${SUFFIX} INTERFACE ${CMAKE_CURRENT_LIST_DIR}/include)
    target_link_libraries(spidir-${SUFFIX} INTERFACE ${SPIDIR_LIB_PATH})
endfunction()

define_spidir_library(unlogged "no_logging")
define_spidir_library(logged "")

option(SPIDIR_ENABLE_LOGGING "Enable spidir logs to be captured via logging API")

add_library(spidir INTERFACE)
if (SPIDIR_ENABLE_LOGGING)
    target_link_libraries(spidir INTERFACE spidir-logged)
else()
    target_link_libraries(spidir INTERFACE spidir-unlogged)
endif()
