//
// Created by Hollan on 12/15/24.
//

//
// Created by Hollan on 12/15/24.
//

#include "ParsingCond.h"

bool IsBalancedString(const std::string& obj)
{
    /*

        This function will, for each character in the string, push the character to the stack if it is an opening brace, and pop it if it is a closing.
        When popping, if the brace that gets popped is not the opening pair for the current brace, then it returns false.
        If the stack is not empty when the string is exhausted, then it returns false.
    */

    std::stack<char> st;
    for (char c : obj)
    {
        switch (c)
        {
            case '[':
            case '(':
            case '{':
                st.push(c);
                break;
            case ']':
            case ')':
            case '}':
            {
                try
                {
                    char top = st.top();
                    st.pop();

                    bool IsMatch = (top == '[' && c == ']') || (top == '(' && c == ')') || (top == '{' && c == '}');

                    if (!IsMatch)
                        return false;
                }
                catch (...)
                {
                    return false;
                }
                break;
            }
            default:
                continue;
        }
    }

    return st.empty();
}
bool IsOperator(char obj)
{
    return obj == '+' || obj == '-' || obj == '*' || obj == '/' || obj == '%' || obj == '^';
}
Operator GetOperator(char op)
{
    switch (op)
    {
        case '+':
            return Operator::plus;
        case '-':
            return Operator::minus;
        case '*':
            return Operator::times;
        case '/':
            return Operator::div;
        case '%':
            return Operator::mod;
        case '^':
            return Operator::exp;
        case '(':
            return Operator::lbrace;
        case ')':
            return Operator::rbrace;
        default:
            throw std::exception();
    }
}

std::optional<std::shared_ptr<Constant>> IsNumericString(const std::string& obj)
{
    //These states are used to determine if the string is numerical, minus the whitespace at the front and end. 
    bool lastWasNumber = false, lastWasSpace = false;
    for (char item : obj)
    {
        if (isdigit(item)) //if it is currently a digit, but the last was a space, then we know that it is not a proper numerical format. Otherwise keep looking.
        {
            if (lastWasSpace)
                return {};
            else
                lastWasNumber = true;
        }
        else if (isspace(item)) //If the currnet is a space, and the last was a space, then we keep looking. Otherwise, we set the state.
        {
            if (lastWasSpace)
                continue;
            else if (lastWasNumber)
                lastWasSpace = true;
        }
        else //If it's not a number or a space, then we know its not numerical. 
            return {};
    }

    std::stringstream ss(obj);
    long long result;
    ss >> result;

    return std::make_shared<Constant>(result);
}
std::optional<std::shared_ptr<Variable>> IsVariableString(const std::string& obj)
{
    if (obj.length() != 1 || !isalpha(obj[0]))
        return {};

    return std::make_shared<Variable>(obj[0]);
}
void FlushExpr(std::string& expr, std::vector<std::shared_ptr<ASTNode>>& list)
{
    if (auto num_str = IsNumericString(expr)){
        expr.clear();
        list.emplace_back(std::move(*num_str));
    }
    else if (auto var_str = IsVariableString(expr)) {
        expr.clear();
        list.emplace_back(std::move(*var_str));
    }
    else if (auto mul_str = IsMultiplyString(expr))
    {
        expr.clear();
        for (auto& item : *mul_str)
            list.emplace_back(std::move(item));
    }
    else
        list.emplace_back(std::make_shared<RawElement>(std::move(expr)));
}
std::optional<std::vector<std::shared_ptr<ASTNode>>> IsMultiplyString(const std::string& obj)
{
    //A string is a MUL string if it:
    //1. Contains no white space between elements
    //2. Only contains numbers and alphabetical characters
    //3. Contains no operators

    std::stringstream ss(obj);
    std::string trimmed;
    std::getline(ss, trimmed);

    {
        auto space_found = std::find_if(trimmed.begin(), trimmed.end(), [](char x) -> bool { return iswspace(x); });
        if (space_found != trimmed.end()) //Space was found
            return {};
    }

    bool last_was_num = false;
    std::string curr_expr;
    std::vector<std::shared_ptr<ASTNode>> pre_result;

    std::stack<bool> braces;

    for (const char& item : obj)
    {
        if (item == '(')
        {
            if (braces.empty())
            {
                if (!curr_expr.empty())
                    FlushExpr(curr_expr, pre_result);
                //We do not add the first and last parenthesis, because we do not want them when we parse later
            }
            else
                curr_expr += item;

            last_was_num = false;
            braces.push(true);

            continue;
        }
        else if (item == ')')
        {
            if (braces.empty())
                return {};

            if (braces.size() == 1)
            {
                if (!curr_expr.empty())
                    FlushExpr(curr_expr, pre_result);
            }
            else
                curr_expr += item;

            last_was_num = false;
            braces.pop();

            continue;
        }

        if (!braces.empty())
        {
            //No matter what just add to the curr_expr
            curr_expr += item;
            continue;
        }

        if (isdigit(item))
        {
            if (!last_was_num && !curr_expr.empty())
                FlushExpr(curr_expr, pre_result);

            curr_expr += item;
            last_was_num = true;
        }
        else if (isalpha(item))
        {
            if (last_was_num && !curr_expr.empty())
                FlushExpr(curr_expr, pre_result);

            //We treat this as a variable
            last_was_num = false;
            pre_result.emplace_back(std::make_shared<Variable>(item));
        }
        else if (item == '^') //Only operator kept
        {
            curr_expr += item;
            //We do not modify the 'last_was_num` because `^` is valid for numbers and variables.
        }
        else //Invalid
            return {};
    }

    //Now we have to add the multiply operators to it.
    //THe first two are paired, and then everything after it is preceded by a multiplication operator.

    if (pre_result.empty())
        return {};
    else if (pre_result.size() == 1)
        return pre_result;
    else
    {
        std::vector<std::shared_ptr<ASTNode>> result;
        result.emplace_back(std::move(pre_result[0]));
        result.emplace_back(std::move(pre_result[1]));
        result.emplace_back(std::make_shared<Operator>(Operator::times));

        //That added the first two, now we iterate through all remaining
        for (auto curr = pre_result.begin() + 2; curr != pre_result.end(); curr++)
        {
            auto& item = *curr;
            try
            {
                [[maybe_unused]] const auto& op = dynamic_cast<const Operator&>(*item); //We only skip if the thing is an operator.
                continue;
            }
            catch (...)
            {
                result.emplace_back(std::move(item));
                result.emplace_back(std::make_shared<Operator>(Operator::times));
            }
        }

        return result;
    }
}
