#include "Literals.h"
#include <iostream>

Brace::Brace(BraceKind kind, bool opening) noexcept : kind(kind), opening(opening) {}
Brace Brace::resolve(char symbol)
{
    BraceKind kind;
    bool is_open;
    switch (symbol)
    {
    case '(':
    case ')':
        kind = BraceKind::Soft;
        is_open = symbol == '(';
        break;
    case '[':
    case ']':
        kind = BraceKind::Hard;
        is_open = symbol == '[';
        break;
    case '{':
    case '}':
        kind = BraceKind::Curly;
        is_open = symbol == '{';
        break;
    default:
        throw UnresolvedBraceError(symbol);
    }

    return Brace(kind, is_open);
}
bool Brace::is_brace(char symbol) noexcept {
    return symbol == '(' || symbol == ')' || symbol == '[' || symbol == ']' || symbol == '{' || symbol == '}';
}

char Brace::symbol() const noexcept
{
    switch (this->kind)
    {
    case BraceKind::Soft:
        return opening ? '(' : ')';
    case BraceKind::Hard:
        return opening ? '[' : ']';
    case BraceKind::Curly:
        return opening ? '{' : '}';
    }
}
Brace Brace::inverse() const noexcept {
    return Brace(this->kind, !this->opening);
}

bool Brace::operator==(const Brace& rhs) const noexcept {
    return this->kind == rhs.kind && this->opening == rhs.opening;
}
bool Brace::operator!=(const Brace& rhs) const noexcept {
    return this->kind != rhs.kind || this->opening != rhs.opening;
}

Operator::Operator(char symbol, int precidence) noexcept : precidence(precidence), symbol(symbol) {}
Operator Operator::resolve(char symbol)
{
    int precidence;
    switch (symbol)
    {
    case '+':
    case '-':
        precidence = 1;
        break;
    case '*':
    case '/':
    case '%':
        precidence = 2;
        break;
    case '^':
        precidence = 3;
        break;
    default:
        throw UnresolvedOperatorError(symbol);
    }

    return Operator(symbol, precidence);
}
bool Operator::is_operator(char symbol) noexcept {
    return symbol == '+' || symbol == '-' || symbol == '*' || symbol == '/' || symbol == '%' || symbol == '^';
}

double Operator::evaluate(double lhs, double rhs) const noexcept
{
    switch (this->symbol)
    {
    case '+':
        return lhs + rhs;
    case '-':
        return lhs - rhs;
    case '*':
        return lhs * rhs;
    case '/':
        return lhs / rhs;
    case '%':
        return fmod(lhs, rhs);
    case '^':
        return pow(lhs, rhs);
    default:
        std::cerr << "Assertion failure! Operator '" << this->symbol << "' was observed, but did not resolve to a specific operator" << std::endl;
        std::exit(3);
    }
}

std::strong_ordering Operator::operator<=>(const Operator &rhs) const noexcept
{
    return this->precidence <=> rhs.precidence;
}

Number::Number(double value) noexcept : value(value) {}

std::partial_ordering Number::operator<=>(const Number &rhs) const noexcept
{
    return this->value <=> rhs.value;
}