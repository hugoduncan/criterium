cmake_minimum_required(VERSION 3.10)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Cross-compilation for macOS:
#   cmake -B build -DCMAKE_OSX_ARCHITECTURES=arm64
#   cmake -B build -DCMAKE_OSX_ARCHITECTURES=x86_64

find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
if(CLANG_TIDY_EXE)
    set(CMAKE_CXX_CLANG_TIDY
        ${CLANG_TIDY_EXE}
    )
else()
    # On mac, this can be installed with `brew install llvm`
    message(WARNING "clang-tidy not found")
endif()

project(criterium)

# Debug configuration
option(DEBUG "Enable debug mode" OFF)
if(DEBUG)
    add_definitions(-DDEBUG)
endif()

# Warning flags - treat warnings as errors in Release builds
set(WARNING_FLAGS -Wall -Wextra -Wpedantic)
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    list(APPEND WARNING_FLAGS -Werror)
endif()

# Optimization flags
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")
endif()

file(GLOB_RECURSE sources *.cpp *.h)

find_package(JNI REQUIRED)
include_directories(${JNI_INCLUDE_DIRS})

add_library(criterium SHARED ${sources})
target_compile_options(criterium PRIVATE ${WARNING_FLAGS})
