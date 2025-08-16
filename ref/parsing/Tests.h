#ifndef TESTS_H
#define TESTS_H

#include <iostream>
#include <unordered_map>
#include <string>
#include <functional>

extern std::unordered_map<std::string, std::function<void()>> tests;

void register_tests();

void test_braces();

#endif