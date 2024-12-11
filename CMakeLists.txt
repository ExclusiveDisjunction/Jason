cmake_minimum_required(VERSION 3.27)
project(Jason)

set(CMAKE_CXX_STANDARD 20)

add_subdirectory(src/Calc)
add_subdirectory(src/Function)
add_subdirectory(src/Expressions)
add_subdirectory(src/IO)
add_subdirectory(src/Commands)
add_subdirectory(src/Core)

add_executable(Jason src/Entry.cpp
        src/IOTester.cpp
        src/Testing.cpp)

target_link_libraries(Jason Core)
target_link_libraries(Jason Calc)
target_link_libraries(Jason Function)
target_link_libraries(Jason IO)
target_link_libraries(Jason Expressions)
target_link_libraries(Jason Commands)
