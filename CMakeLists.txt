cmake_minimum_required(VERSION 3.27)
project(Jason)

set(CMAKE_CXX_STANDARD 20)

add_subdirectory(src/Core)
add_subdirectory(src/Calc)

add_executable(Jason src/Entry.cpp)

target_link_libraries(Jason Core)
target_link_libraries(Jason Calc)
