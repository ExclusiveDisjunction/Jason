cmake_minimum_required(VERSION 3.29)
project(quick)

set(CMAKE_CXX_STANDARD 20)

add_executable(quick
        parsing.cpp
        ASTNode.h
        Operator.cpp
        Operator.h
        Expressions.h
        Expressions.cpp
        ParsingCond.h
        ParsingCond.cpp)
