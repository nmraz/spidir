cmake_minimum_required(VERSION 3.20)
project(spidir-c-tests)

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/../c-api spidir)
add_executable(test test.c)
target_link_libraries(test spidir)
