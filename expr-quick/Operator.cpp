//
// Created by Hollan on 12/15/24.
//

#include "Operator.h"

Operator::Operator(char symbol) : symbol(symbol), rank(0)
{
    auto found = lookup.find(symbol);
    if (found == lookup.end())
        throw std::logic_error("Could not find operator");
    
    this->rank = found->second;
}
Operator::Operator(const Operator& obj) noexcept : ASTNode(obj)
{
    this->symbol = obj.symbol;
    this->rank  = obj.rank;
}
Operator::Operator(Operator&& obj) noexcept 
{
    this->symbol = obj.symbol;
    this->rank = obj.rank;
}

Operator& Operator::operator=(Operator&& obj) noexcept
{
    this->symbol = obj.symbol;
    this->rank = obj.rank;
    return *this;
}

void Operator::Print(std::ostream& out) const noexcept
{ 
    out << this->symbol; 
}
unsigned Operator::GetWidth() const noexcept
{
    return 1;
}

std::strong_ordering Operator::operator<=>(const Operator& obj) const noexcept
{
    return this->rank <=> obj.rank;
}
bool Operator::operator==(const Operator& obj) const noexcept
{
    return this->operator<=>(obj) == 0;
}
bool Operator::operator==(char obj) const noexcept { return this->symbol == obj; }
bool Operator::operator!=(const Operator& obj) const noexcept
{
    return this->operator<=>(obj) != 0;
}
bool Operator::operator!=(char obj) const noexcept { return this->symbol != obj; }
bool Operator::operator<(const Operator& obj) const noexcept
{
    return this->operator<=>(obj) < 0;
}
bool Operator::operator<=(const Operator& obj) const noexcept
{
    return this->operator<=>(obj) <= 0;
}
bool Operator::operator>(const Operator& obj) const noexcept
{
    return this->operator<=>(obj) > 0;
}
bool Operator::operator>=(const Operator& obj) const noexcept
{
    return this->operator<=>(obj) >= 0;
}

long long Operator::Apply(long long a, long long b) const
{
    switch (this->symbol)
    {
        case '+':
            return a + b;
        case '-':
            return a - b;
        case '*':
            return a * b;
        case '/':
            return a / b;
        case '%':
            return a % b;
        case '^':
            return (long long)pow(a, b);
        default:
            throw std::logic_error("invalid operands");
    }
}