cmake_minimum_required(VERSION 3.10)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)

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

file(GLOB_RECURSE sources *.cpp *.h)

find_package(JNI REQUIRED)
include_directories(${JNI_INCLUDE_DIRS})

add_library(criterium SHARED ${sources})
