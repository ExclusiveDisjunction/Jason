//
// Created by Hollan on 12/15/24.
//

#include "Expressions.h"
#include "Operator.h"

#include <ranges>
#include <sstream>

void Expression::Print(std::ostream& out) const noexcept
{
    out << this->to_string();
}
unsigned Expression::GetWidth() const noexcept
{
    return this->to_string().length();
}

// Raw Element
RawElement::RawElement(std::string&& str) : str(std::move(str))
{
    
}

std::string RawElement::to_string() const noexcept
{
    return this->str;
}

// Constant
Constant::Constant(long long data) : data(data)
{
    
}

long long Constant::Evaluate(const std::vector<long long>& inputs) const
{
    return this->data;
}
std::string Constant::to_string() const noexcept
{
    return std::to_string(this->data);
}

// Variable
Variable::Variable(char letter) : symbol(letter)
{
    
}

std::string Variable::to_string() const noexcept
{
    return std::string(1, this->symbol);
}

// Function
Function::Function(std::string name, const std::vector<Variable>& vars, std::shared_ptr<ASTNode>&& ast)
{
    this->right = std::move(ast);
    this->name = std::move(name);
    
    unsigned i = vars.size() - 1;
    for (const auto & var : std::ranges::reverse_view(vars))
    {
        this->vars[var.symbol] = i;
        i--;
    }
}

std::string Function::to_string() const noexcept
{
    std::stringstream ss;
    ss << this->name << '(';
    auto iter = this->vars.begin();
    for (unsigned i = 0; iter != this->vars.end(); iter++, i++)
    {
        ss << iter->first;
        if (i != this->vars.size() - 1)
            ss << ", ";
    }
    ss << ')';
    
    return ss.str();
}
std::string Function::as_inorder() const noexcept
{
    std::stringstream ss;
    as_inorder_helper(ss, this->root);
    
    return ss.str();
}
void Function::as_inorder_helper(std::stringstream& ss, const std::shared_ptr<ASTNode>& target) const noexcept
{
    if (!target)
        return;
    
    as_inorder_helper(ss, target->left);
    ss << ' ' << *target;
    as_inorder_helper(ss, target->right);
}
void Function::EvalHelper(const std::vector<long long>& inputs, std::stack<long long>& result_so_far, const std::shared_ptr<ASTNode>& curr) const
{
    if (!curr)
        return;
    
    EvalHelper(inputs, result_so_far, curr->left);
    EvalHelper(inputs, result_so_far, curr->right);
    
    try
    {
        //See if it is an operator
        const auto& op = dynamic_cast<const Operator&>(*curr);
        long long b = result_so_far.top();
        result_so_far.pop();
        long long a = result_so_far.top();
        result_so_far.pop();
        
        result_so_far.push(op.Apply(a, b));
    }
    catch (...)
    {
        try
        {
            //See if it is a Evaluatable Expression
            const auto& eval = dynamic_cast<const EvalExpr&>(*curr);
            result_so_far.push(eval.Evaluate(inputs));
        }
        catch (...)
        {
            try
            {
                //See if it is a variable
                const auto& var = dynamic_cast<const Variable&>(*curr);
                auto index = this->vars.find(var.symbol);
                if (index == this->vars.end())
                    throw std::exception();
                
                result_so_far.push(inputs[index->second]);
            }
            catch (...)
            {
                throw std::logic_error("could not get data out of current node");
            }
        }
    }
}
long long Function::Evaluate(const std::vector<long long>& inputs) const
{
    if (inputs.size() != vars.size())
        throw std::logic_error("too many or too few inputs for this function");
    
    std::stack<long long> result;
    EvalHelper(inputs, result, this->root);
    
    if (result.size() != 1)
        throw std::logic_error("error when parsing AST");
    
    return result.top();
}

// Delimiter
Delimiter::Delimiter() = default;

std::string Delimiter::to_string() const noexcept
{
    return ",";
}