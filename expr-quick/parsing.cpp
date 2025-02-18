#include <iostream>
#include <sstream>

#include "Operator.h"
#include "Expressions.h"



/*
std::vector<std::shared_ptr<ASTNode>> infix_to_postfix(const std::string& in)
{
    if (!IsBalancedString(in))
        return {};

    enum expr_kind {
        numeric,
        oper,
        variable,
        raw_expr,
        none
    };

    std::vector<std::shared_ptr<ASTNode>> phase_one;
    std::string expr_so_far;
    expr_kind prev_was;
    bool first = true;
    std::stack<Operator> opers;
    std::stack<bool> braces;

    for (const char& item : in)
    {
        if (isdigit(item) || isalpha(item))
        {
            if (!first && prev_was == none)
                throw std::logic_error("Format: An operator or brace was expected, but an expression was given");

            expr_so_far += item;
            prev_was = raw_expr;
            first = false;
        }
        else if (item == '(') {
            if (!first && prev_was == none)
                throw std::logic_error("format: an operator was expected, but an expression was given");
            
            if (!braces.empty()) //We dont have anything there, and we want to get rid of the first and last braces.
                expr_so_far += item;
            prev_was = raw_expr;
            first = false;
            braces.push(true);
        }
        else if (item == ')')
        {
            if (!first && prev_was == none)
                throw std::logic_error("format: an operator was expected, but an expression was given");
            
            if (braces.size() != 1)
                expr_so_far += item;
            prev_was = raw_expr;
            first = false;
            braces.pop();
        }
        else if (iswspace(item))
        {
            if (!braces.empty()) //We treat it as a raw expression
            {
                expr_so_far += item;
                prev_was = raw_expr;
                first = false;
                continue;
            }
            
            if (!expr_so_far.empty())
                phase_one.push_back(std::make_shared<RawElement>(std::move(expr_so_far)));
            
            if (prev_was != raw_expr) 
                continue; //We preserve the state unless it was a raw_expr
                
            prev_was = none;
        }
        else if (IsOperator(item))
        {
            if (first)
                throw std::logic_error("format: first character cannot be an operator");
            else if (prev_was == oper)
                throw std::logic_error("format: two operators indicated without an expression between them");
            
            if (!braces.empty()) //We just treat it as an expression unprocessed
            {
                expr_so_far += item;
                prev_was = raw_expr;
                first = false;
                continue;
            }

            Operator our_oper = GetOperator(item);
            if (our_oper == Operator('\0', 0))
                throw std::logic_error("could not get operator");

            if (!expr_so_far.empty())
                phase_one.push_back(std::make_shared<RawElement>(std::move(expr_so_far)));

            if (!opers.empty())
            {
                Operator last_oper = opers.top();

                while (!opers.empty() && last_oper != '(' &&
                       (our_oper < last_oper || (our_oper == last_oper && our_oper != '^')))
                {
                    phase_one.push_back(std::make_shared<Operator>(last_oper));
                    opers.pop();

                    if (!opers.empty())
                        last_oper = opers.top();
                }
            }

            opers.push(our_oper); //No matter what we add this in

            prev_was = oper;
            first = false;
        }        
    }
    
    if (!expr_so_far.empty())
        phase_one.push_back(std::make_shared<RawElement>(std::move(expr_so_far)));
    
    while (!opers.empty())
    {
        phase_one.push_back(std::make_shared<Operator>(std::move(opers.top())));
        opers.pop();
    }
    
    if (phase_one.empty()) //Empty string
        return { std::make_shared<Constant>(0) };
    
    std::vector<std::shared_ptr<ASTNode>> result;
    for (auto& item : phase_one)
    {
        if (!item)
            continue;
        
        try
        {
            auto& conv = dynamic_cast<RawElement&>(*item);
            auto& str = conv.str;
            if (IsNumericString(str))
            {
                long long num;
                std::stringstream ss(str);
                ss >> num;
                
                result.emplace_back(std::make_shared<Constant>(num));
            }
            else if (IsVariableString(str))
            {
                result.emplace_back(std::make_shared<Variable>(str[0]));
            }
            else if (IsMulString(str))
            {
                std::vector<std::shared_ptr<ASTNode>> expanded_result = infix_to_postfix(ExpandMulString(str));
                for (auto& sub_item : expanded_result)
                    result.emplace_back(std::move(sub_item));
            }
            else
            {
                std::vector<std::shared_ptr<ASTNode>> conv_result = conv.convert_to_postfix();
                for (auto& sub_item : conv_result)
                    result.emplace_back(std::move(sub_item));   
            }
        }
        catch (...)
        {
            result.emplace_back(std::move(item));
        }
    }
    
    return result;
}
*/

#include "ParsingCond.h"

int main()
{
    /*
    //infix_to_postfix("2+3-5+2^2");
    std::cout << std::endl;
    //infix_to_postfix("(2+3*(2+2))-4");
    std::cout << std::endl;
    auto result = infix_to_postfix("(8-3)/(4+2)");
    for (const auto& item : result)
    {
        if (item)
        {
            item->Print(std::cout);
            std::cout << ' ';
        }
    }
    std::cout << std::endl;
    //infix_to_postfix("((294 + 12) * (4 - 52 ))/ 4 ^ 2 + 67 % 12 - 867");
    //std::cout << std::endl << "294 12 + 4 52 - * 4 2 ^ / 67 12 % + 867 -" << std::endl;
    
    result = infix_to_postfix("3(x+4)y - 3y");
    for (const auto& item : result)
    {
        if (item)
        {
            item->Print(std::cout);
            std::cout << ' ';
        }
    }
     */
    
    /*
               + 
           /      \  
         *         *
       /   \     /   \
      2    3    x     y
     */
    
    std::shared_ptr<ASTNode> tree = 
            ASTNode::Join(
                ASTNode::Join(
                            ASTNode::Join(
                                    std::make_shared<Constant>(2),
                                    std::make_shared<Variable>('x'),
                                    std::make_shared<Operator>(Operator::times)
                                    ),
                            std::make_shared<Constant>(3),
                            std::make_shared<Operator>(Operator::times)
                        ),
                ASTNode::Join(
                            std::make_shared<Variable>('x'),
                            std::make_shared<Variable>('y'),
                            std::make_shared<Operator>(Operator::times)
                        ),
                std::make_shared<Operator>(Operator::plus)
            );
    
    Function f("f", std::vector<Variable>( { Variable('x'), Variable('y') } ), std::move(tree));   
    std::cout << f << " = " << f.as_inorder() << std::endl;
    std::cout << "f(1, 2) = " << f.Evaluate({1, 2}) << std::endl;
    
    std::vector<std::string> mul_strs = {
            "3xy",
            "3(x+4)y",
            "3((x))y",
            "(3x)(3y)(xy)"
    };
    for (const auto& str : mul_strs)
    {
        auto result = IsMultiplyString(str);
        if (result)
        {
            std::cout << str << " = ";
            for (const auto& item : *result)
                std::cout << *item << " ";
            std::cout << std::endl;
        }
        else
            std::cout << "got none from " << str << std::endl;
    }
}