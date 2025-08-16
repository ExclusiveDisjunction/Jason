#ifndef EXPRESSIONS_H
#define EXPRESSIONS_H

#include <iostream>
#include <string>
#include <vector>
#include <stack>
#include <queue>
#include <algorithm>
#include <math.h>

#include "Literals.h"

/// @brief An exception class that puts a format for expressions.
class FormatError : public std::logic_error
{
public:
	FormatError(const std::string& invalidExpression, const std::string& why) : std::logic_error("Invalid expression: \'" + invalidExpression + "\' due to \'" + why + '\'') {}
};

/// @brief Determines if a string has the proper number of opening to closing braces, and that they are matched.
/// @param obj The string to be evaluated.
/// @return True if all braces that were opened were closed in the correct order.
bool IsBalancedString(const std::string& obj);
/// @brief Determines if a string is only containing integer numerical digits. This disregards white space at the front and back of the string.
/// @param obj The string to be checked.
/// @return True if the string is in numerical format.
bool IsNumericString(const std::string& obj);

using ParsedExpression = std::queue<std::unique_ptr<Literal>>;
using PostfixExpression = std::queue<std::unique_ptr<NonBraceLiteral>>;

ParsedExpression parse(const std::string& infix);
PostfixExpression to_postfix(ParsedExpression& input);
void print_postfix(const PostfixExpression& s);
double evaluatePostfix(PostfixExpression& postfix);

#endif