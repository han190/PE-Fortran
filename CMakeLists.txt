cmake_minimum_required(VERSION 3.4)
project(Euler-Project-with-Modern-Fortran)
enable_language(Fortran)

# if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
#     set(CMAKE_Fortran_FLAGS_DEBUG "-g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -fcheck=all -finit-real=nan -std=f2008")
#     set(CMAKE_Fortran_FLAGS_RELEASE "-pthread -O3 -mtune=native -march=native")
# endif()

include_directories(data)
file(GLOB_RECURSE sources src/euler/*.f90 src/probs/*.f90 src/utils/*.f90)
add_executable(euler ${sources})