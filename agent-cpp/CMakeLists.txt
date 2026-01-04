cmake_minimum_required(VERSION 3.14)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Cross-compilation for macOS:
#   cmake -B build -DCMAKE_OSX_ARCHITECTURES=arm64
#   cmake -B build -DCMAKE_OSX_ARCHITECTURES=x86_64

project(criterium)

# Find clang-tidy for use on our targets only (not dependencies)
find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
if(NOT CLANG_TIDY_EXE)
    # On mac, this can be installed with `brew install llvm`
    message(WARNING "clang-tidy not found")
endif()

# Debug configuration
option(DEBUG "Enable debug mode" OFF)
if(DEBUG)
    add_definitions(-DDEBUG)
endif()

# Testing option
option(BUILD_TESTS "Build tests" ON)

# Warning flags - treat warnings as errors in Release builds
set(WARNING_FLAGS -Wall -Wextra -Wpedantic)
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    list(APPEND WARNING_FLAGS -Werror)
endif()

# Optimization flags
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")
endif()

set(sources agent.cpp)

find_package(JNI REQUIRED)
include_directories(${JNI_INCLUDE_DIRS})

add_library(criterium SHARED ${sources})
target_compile_options(criterium PRIVATE ${WARNING_FLAGS})
if(CLANG_TIDY_EXE)
    set_target_properties(criterium PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")
endif()

# Testing with GoogleTest
if(BUILD_TESTS)
    include(FetchContent)
    FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.14.0
    )
    # Prevent GoogleTest from overriding compiler/linker options
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)

    enable_testing()
    add_subdirectory(test)
endif()
