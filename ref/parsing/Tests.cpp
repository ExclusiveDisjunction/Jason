#include "Tests.h"

#include "Literals.h"
#include "Expressions.h"

std::unordered_map<std::string, std::function<void()>> tests;

void register_tests() {
    tests = std::unordered_map<std::string, std::function<void()>>();

    tests.emplace("braces", std::function(test_braces));
}

void test_braces() {
    std::vector<std::pair<std::string, bool>> cases = {
        std::make_pair("", true),

        std::make_pair("()", true),
        std::make_pair("[]", true),
        std::make_pair("{}", true),

        std::make_pair("(())", true),
        std::make_pair("[[]]", true),
        std::make_pair("{{}}", true),
        std::make_pair("([])", true),
        std::make_pair("({})", true),
        std::make_pair("[()]", true),
        std::make_pair("[{}]", true),
        std::make_pair("{()}", true),
        std::make_pair("{[]}", true),

        std::make_pair("(", false),
        std::make_pair("[", false),
        std::make_pair("{", false),
        std::make_pair(")", false),
        std::make_pair("]", false),
        std::make_pair("}", false),

        std::make_pair("(]", false),
        std::make_pair("(}", false),
        std::make_pair("[)", false),
        std::make_pair("[}", false),
        std::make_pair("{)", false),
        std::make_pair("{]", false),
    };

    for (const auto& [raw, passes] : cases) {
        bool result = IsBalancedString(raw);
        if (result != passes) {
            if (passes) {
                throw std::logic_error("the expression '" + raw + "' was expected to pass, but it failed");
            }
            else {
                throw std::logic_error("the expression '" + raw + "' was expected to fail, but it passed");
            }
        }
    }
}