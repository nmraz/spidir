# Making this an object library is somewhat hacky, but lets us cleanly sidestep
# the circular dependency with spidir because of the platform implementation.

function(define_utils_library SUFFIX)
    add_library(utils-${SUFFIX} OBJECT src/spidir_platform.c src/utils.c)
    target_include_directories(utils-${SUFFIX} PUBLIC include)
    target_link_libraries(utils-${SUFFIX} spidir-${SUFFIX})
endfunction()

define_utils_library(unlogged)
define_utils_library(logged)
