#ifndef LITERALS_H
#define LITERALS_H

#include <compare>
#include <string>

class Literal {
	public:
		virtual ~Literal() = default;
};

class UnresolvedBraceError : public std::logic_error {
	public:
		UnresolvedBraceError(char symbol) : std::logic_error("the symbol" + std::string(1, symbol) + "could not be interpreted as a brace") { }
};

enum class BraceKind {
	Soft,
	Hard,
	Curly
};
class Brace : public Literal {
	public:
		Brace(BraceKind kind, bool opening) noexcept;
		static Brace resolve(char symbol);
		static bool is_brace(char symbol) noexcept;

		const BraceKind kind;
		const bool opening;

		Brace inverse() const noexcept;
		char symbol() const noexcept;

		bool operator==(const Brace& rhs) const noexcept;
		bool operator!=(const Brace& rhs) const noexcept;
};

class NonBraceLiteral : Literal {

};

class UnresolvedOperatorError : public std::logic_error {
	public:
		UnresolvedOperatorError(char symbol) : std::logic_error("the symbol" + std::string(1, symbol) + "could not be resolved to an operator") { }
};

class Operator : public NonBraceLiteral {
	private: 
		Operator(char symbol, int precidence) noexcept;
	public:
		static Operator resolve(char symbol);
		static bool is_operator(char symbol) noexcept;

		const int precidence;
		const char symbol;

		double evaluate(double lhs, double rhs) const noexcept;

		std::strong_ordering operator<=>(const Operator& rhs) const noexcept;
};
class Number : public NonBraceLiteral {
	public:
		Number(double value) noexcept;

		const double value;

		std::partial_ordering operator<=>(const Number& rhs) const noexcept;
};

#endif