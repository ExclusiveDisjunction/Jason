cmake_minimum_required(VERSION 3.27)
project(Expressions)
include(CTest)

set(CMAKE_CXX_STANDARD 20)

add_executable(Expressions 
    Entry.cpp
    Literals.cpp
    Expressions.cpp
    Tests.cpp)

add_test(NAME braces COMMAND Expressions -t braces)