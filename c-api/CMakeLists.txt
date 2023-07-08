set(CRATE_TARGET_DIR ${CMAKE_CURRENT_BINARY_DIR}/cargo-target)
set(SPIDIR_LIB_PATH ${CRATE_TARGET_DIR}/x86_64-unknown-none/release/libspidir.a)

add_custom_target(spidir-crate
    COMMAND cargo build --release -p c-api --target x86_64-unknown-none --target-dir ${CRATE_TARGET_DIR}
    COMMENT "Building spidir library"
    BYPRODUCTS ${SPIDIR_LIB_PATH}
    WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
    USES_TERMINAL)

add_library(spidir INTERFACE)
add_dependencies(spidir spidir-crate)
target_include_directories(spidir INTERFACE ${CMAKE_CURRENT_LIST_DIR}/include)
target_link_libraries(spidir INTERFACE ${SPIDIR_LIB_PATH})
