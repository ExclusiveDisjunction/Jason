#ifndef JASON_ERRORS_H
#define JASON_ERRORS_H

#include <iostream>
#include "Log.h"

class ErrorBase : public std::exception, public DebugPrint {
private:
    std::string contents;

protected:
    ErrorBase(std::string contents) : contents(std::move(contents)) {}
    
public:
    ErrorBase() = default;
    ErrorBase(const ErrorBase& e) = default;
    ErrorBase(ErrorBase&& e) = default;

    [[nodiscard]] const char* what() const noexcept override { return contents.c_str(); }
    void dbg_fmt(std::ostream& out) const noexcept override { out << contents; }
}; 

class ArgumentError : public ErrorBase {
public:
    ArgumentError(const std::string& name, const std::string& value);
    ArgumentError(const std::string& name, const DebugPrint& value);
};

class NullError : public ErrorBase {
public:
    NullError(const std::string& name);
};

class FormatError : public ErrorBase {
public:
    FormatError(const std::string& target, const std::string& reason);
    FormatError(const std::string& reason);
};

class RangeError : public ErrorBase {
public:
    template<typename T> 
    RangeError(const std::string& variable, T value, T min, T max) 
        : ErrorBase("the value " + std::to_string(value) + " in variable '" + variable + "' is out of range (" + std::to_string(min) + " - " + std::to_string(max) + "')")
    {
        
    }
};

class NotFoundError : public ErrorBase {
public:
    NotFoundError(const std::string& identifier);
    NotFoundError(const DebugPrint& identifier);
};

class OperatorError : public ErrorBase {
public:
    OperatorError(char oper, const std::string& operand1, const std::string& operand2, const std::string& reason = "does not exist");
    OperatorError(const DebugPrint& oper, const std::string& operand1, const std::string& operand2, const std::string& reason = "does not exist");
    OperatorError(const std::string& oper, const std::string& operand1, const std::string& operand2, const std::string& reason = "does not exist");

    OperatorError(char oper, const DebugPrint& operand1, const DebugPrint& operand2, const std::string& reason = "does not exist");
    OperatorError(const std::string& oper, const DebugPrint& operand1, const DebugPrint& operand2, const std::string& reason = "does not exist");
    OperatorError(const DebugPrint& oper, const DebugPrint& operand1, const DebugPrint& operand2, const std::string& reason = "does not exist");
};

class UnexepctedError : public ErrorBase {
public:
    UnexepctedError(const std::string& reason);
};

class OperationError : public ErrorBase {
public:
    OperationError(const std::string& action, const std::string& reason);
};

class ConversionError : public ErrorBase {
public:
    ConversionError(const std::string& action, const std::string& reason);
};

enum IOErrorKind {
    IEK_NotFound,
    IEK_Permissions,
    IEK_Invalid
};

class IOError : public ErrorBase {
public:
    IOError(const std::string& location, IOErrorKind kind);
};

/*
/// @brief A function that converts the arguments into a single string
/// @tparam ...Args The types of the arguments
/// @param ...value The values to put into a string
/// @return A full string that contains the arguments in order
template<typename... Args>
std::string reason_formatter(Args... value) noexcept {
    std::vector<void*> to_print( &(value)... );
    std::stringstream ss;
    for (const auto& item : to_print)
        ss << *item;

    return ss.str();
}
*/

#endif
