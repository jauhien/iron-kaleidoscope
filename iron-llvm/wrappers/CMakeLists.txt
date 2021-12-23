cmake_minimum_required(VERSION 2.8.8)
project(LLVMWrappers)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_definitions(-std=c++11 -fPIC)

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

add_library(llvm-wrappers STATIC memory-manager-wrappers.cc support-wrappers.cc)

llvm_map_components_to_libnames(llvm_libs executionengine)

# Link against LLVM libraries
target_link_libraries(llvm-wrappers ${llvm_libs})

install(TARGETS llvm-wrappers ARCHIVE DESTINATION .)
