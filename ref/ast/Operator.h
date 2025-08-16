//
// Created by Hollan on 12/15/24.
//

#ifndef QUICK_OPERATOR_H
#define QUICK_OPERATOR_H

#include "ASTNode.h"
#include <unordered_map>

class Operator : public ASTNode
{
private:
    char symbol;
    unsigned rank;
    
    static std::unordered_map<char, unsigned> lookup; //Symbol, Rank
    
public:
    explicit Operator(char symbol);
    Operator(const Operator& obj) noexcept;
    Operator(Operator&& obj) noexcept;
    
    Operator& operator=(Operator&& obj) noexcept;

    void Print(std::ostream& out) const noexcept override;
    [[nodiscard]] unsigned GetWidth() const noexcept override;

    std::strong_ordering operator<=>(const Operator& obj) const noexcept;
    bool operator==(const Operator& obj) const noexcept;
    bool operator==(char obj) const noexcept;
    bool operator!=(const Operator& obj) const noexcept;
    bool operator!=(char obj) const noexcept;
    bool operator<(const Operator& obj) const noexcept;
    bool operator<=(const Operator& obj) const noexcept;
    bool operator>(const Operator& obj) const noexcept;
    bool operator>=(const Operator& obj) const noexcept;
    
    [[nodiscard]] long long Apply(long long a, long long b) const;

    static Operator plus;
    static Operator minus;
    static Operator times;
    static Operator div;
    static Operator mod;
    static Operator exp;
    static Operator lbrace;
    static Operator rbrace;
};


#endif //QUICK_OPERATOR_H
