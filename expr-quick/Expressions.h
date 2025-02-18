//
// Created by Hollan on 12/15/24.
//

#ifndef QUICK_EXPRESSIONS_H
#define QUICK_EXPRESSIONS_H

#include "ASTNode.h"
#include <sstream>

class Expression : public ASTNode
{
public:
    [[nodiscard]] virtual std::string to_string() const noexcept = 0;
    void Print(std::ostream& out) const noexcept override;
    [[nodiscard]] unsigned GetWidth() const noexcept override;
};
class EvalExpr : public Expression
{
public:
    [[nodiscard]] virtual long long Evaluate(const std::vector<long long>& inputs) const = 0;
};

class RawElement : public Expression
{
public:
    explicit RawElement(std::string&& str);
    
    std::string str;
    
    [[nodiscard]] std::string to_string() const noexcept override;
};

class Constant : public EvalExpr
{
public:
    explicit Constant(long long data);
    
    long long data;
    
    [[nodiscard]] std::string to_string() const noexcept override;
    [[nodiscard]] long long Evaluate(const std::vector<long long>& inputs) const override;
};

class Variable : public Expression
{
public:
    explicit Variable(char letter);
    
    char symbol;

    [[nodiscard]] std::string to_string() const noexcept override;
};

class Function : public EvalExpr
{
private:
    void EvalHelper(const std::vector<long long>& inputs, std::stack<long long>& result_so_far, const std::shared_ptr<ASTNode>& curr) const;
    void as_inorder_helper(std::stringstream& ss, const std::shared_ptr<ASTNode>& target) const noexcept;
public:
    Function(std::string name, const std::vector<Variable>& vars, std::shared_ptr<ASTNode>&& ast);
    
    std::string name;
    [[nodiscard]] std::string to_string() const noexcept override;
    [[nodiscard]] std::string as_inorder() const noexcept;
    [[nodiscard]] long long Evaluate(const std::vector<long long>& inputs) const override;
    
    std::unordered_map<char, unsigned> vars;
    std::shared_ptr<ASTNode>& root = right;
};
class Delimiter : public Expression
{
public:
    Delimiter();
    
    [[nodiscard]] std::string to_string() const noexcept override;
};

#endif //QUICK_EXPRESSIONS_H
