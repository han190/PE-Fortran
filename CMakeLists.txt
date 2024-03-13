cmake_minimum_required(VERSION 3.10)
project(PE-Fortran VERSION 0.0.4 
  DESCRIPTION "Project Euler solutions written in Fortran"
  HOMEPAGE_URL https://github.com/han190/PE-Fortran
  LANGUAGES Fortran)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Debug)
  message(STATUS "Setting CMAKE_BUILD_TYPE to ${CMAKE_BUILD_TYPE}.")
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

if (CMAKE_SYSTEM_NAME STREQUAL "Linux" OR CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 12.0)
      message(FATAL_ERROR gfortran >= ${CMAKE_Fortran_COMPILER_VERSION})
    endif()
    message(STATUS "Using gfortran ${CMAKE_Fortran_COMPILER_VERSION}.")

    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
      set(CMAKE_Fortran_FLAGS "-g -O0 -Wall -Wextra -pedantic -fbounds-check \
        -fimplicit-none -fPIC -Wno-uninitialized -fcheck=all -fbacktrace \
        -ffree-form -fcheck=array-temps -Werror=implicit-interface \
        -std=f2018 -march=native")
    elseif (CMAKE_BUILD_TYPE STREQUAL "Release")
      set(CMAKE_Fortran_FLAGS "-O3 -march=native -std=f2018 -march=native")
    endif()
  elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    if (CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 2021.11)
      message(FATAL_ERROR ifx >= ${CMAKE_Fortran_COMPILER_VERSION})
    endif()
    message(STATUS "Using ifx ${CMAKE_Fortran_COMPILER_VERSION}.")
      
    if (CMAKE_BUILD_TYPE STREQUAL "Debug")
      set(CMAKE_Fortran_FLAGS "-O0 -warn all -check all,nouninit \
        -g -traceback -no-simd -stand f18 -mtune=native")
    elseif (CMAKE_BUILD_TYPE STREQUAL "Release")
      set(CMAKE_Fortran_FLAGS "-O1 -ipo -xHost -stand f18 -mtune=native")
    endif()
  endif()
else()
  message(FATAL_ERROR "Unsupported OS.")
endif()

# Build static library
file(GLOB toolkit_sources ${PROJECT_SOURCE_DIR}/src/toolkit/*.f90)
add_library(petk STATIC ${toolkit_sources})

# Build preprocess
set(data_prefix $ENV{HOME}/.local/PE-Fortran-data/)
set(include_dir ${PROJECT_BINARY_DIR}/include/)
file(GLOB preprocess_sources ${PROJECT_SOURCE_DIR}/src/preprocess/*.f90)
add_executable(PE-Preprocess ${preprocess_sources})

# Run preprocess
set(generated_files directory.inc interface.inc problemset.inc)
set(preprocess_arguments -d ${data_prefix} 
  -i ${include_dir} -r ${CMAKE_SOURCE_DIR})
add_custom_command(
  OUTPUT ${generated_files}
  COMMAND PE-Preprocess ARGS ${preprocess_arguments}
  COMMENT "Generating included files.")
add_custom_target(generate_source DEPENDS ${generated_files})

# Build main
file(GLOB problem_sources ${PROJECT_SOURCE_DIR}/src/problems/*.f90)
file(GLOB projecteuler_sources ${PROJECT_SOURCE_DIR}/src/*.f90)
set(sources ${projecteuler_sources} ${problem_sources})
add_executable(PE-Fortran ${sources})
add_dependencies(PE-Fortran generate_source)
target_include_directories(PE-Fortran PRIVATE ${include_dir})
target_link_libraries(PE-Fortran petk)

# Add a quick test
# Apparently I don't need add_test for make test to work...
add_test(TestAllProblems PE-Fortran)
add_custom_target(test 
  COMMAND PE-Fortran -d ${PROJECT_SOURCE_DIR}/data/ -a ${PROJECT_BINARY_DIR}
  DEPENDS PE-Fortran)

# Install
install(TARGETS PE-Fortran DESTINATION $ENV{HOME}/.local/bin)
file(GLOB data_files ${PROJECT_SOURCE_DIR}/data/*.txt)
install(FILES ${data_files} DESTINATION ${data_prefix})