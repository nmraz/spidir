# Making this an object library is somewhat hacky, but lets us cleanly sidestep
# the circular dependency with spidir because of the platform implementation.
add_library(utils OBJECT src/spidir_platform.c src/utils.c)
target_include_directories(utils PUBLIC include)
target_link_libraries(utils spidir)
