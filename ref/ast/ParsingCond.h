//
// Created by Hollan on 12/15/24.
//

#ifndef QUICK_PARSINGCOND_H
#define QUICK_PARSINGCOND_H

#include "Operator.h"
#include "Expressions.h"
#include <optional>

bool IsBalancedString(const std::string& obj);
bool IsOperator(char obj);
Operator GetOperator(char op);

std::optional<std::shared_ptr<Constant>> IsNumericString(const std::string& obj);
std::optional<std::shared_ptr<Variable>> IsVariableString(const std::string& obj);
void FlushExpr(std::string& expr, std::vector<std::shared_ptr<ASTNode>>& list);
std::optional<std::vector<std::shared_ptr<ASTNode>>> IsMultiplyString(const std::string& obj);

#endif //QUICK_PARSINGCOND_H
