//
// Created by Hollan on 12/17/24.
//

#include "Errors.h"

ArgumentError::ArgumentError(const std::string& name, const DebugPrint& value) : ArgumentError(name, value.dbg_fmt_string())
{
    
}
ArgumentError::ArgumentError(const std::string& name, const std::string& value) : ErrorBase("the value '" + value + "' stored in '" + name + "' is invalid")
{
    
}

FormatError::FormatError(const std::string& target, const std::string& reason) : ErrorBase("the expression '" + target + "' is invalid because of '" + reason + "'")
{
    
}
FormatError::FormatError(const std::string& reason) : ErrorBase("the input was invalid because of '" + reason + "'")
{
    
}

NullError::NullError(const std::string& name) : ErrorBase(name + " was null")
{
    
}

NotFoundError::NotFoundError(const DebugPrint& identifier) : NotFoundError(identifier.dbg_fmt_string())
{
    
}
NotFoundError::NotFoundError(const std::string& identifier)  : ErrorBase(identifier + "was not found")
{
    
}

OperatorError::OperatorError(char oper, const std::string& operand1, const std::string& operand2, const std::string& reason) : OperatorError(std::string(1, oper), operand1, operand2, reason)
{
    
}
OperatorError::OperatorError(const DebugPrint& oper, const std::string& operand1, const std::string& operand2, const std::string& reason) : OperatorError(oper.dbg_fmt_string(), operand1, operand2, reason)
{
    
}
OperatorError::OperatorError(const std::string& oper, const std::string& operand1, const std::string& operand2, const std::string& reason) : ErrorBase("the operator '" + oper + "' is invalid for '" + operand1 + "' and '" + operand2 + "' because of '" + reason + "'")
{
    
}

OperatorError::OperatorError(char oper, const DebugPrint& operand1, const DebugPrint& operand2, const std::string& reason) : OperatorError(std::string(1, oper), operand1.dbg_fmt_string(), operand2.dbg_fmt_string(), reason)
{
    
}
OperatorError::OperatorError(const std::string& oper, const DebugPrint& operand1, const DebugPrint& operand2, const std::string& reason) : OperatorError(oper, operand1.dbg_fmt_string(), operand2.dbg_fmt_string(), reason)
{
    
}
OperatorError::OperatorError(const DebugPrint& oper, const DebugPrint& operand1, const DebugPrint& operand2, const std::string& reason) : OperatorError(oper.dbg_fmt_string(), operand1.dbg_fmt_string(), operand2.dbg_fmt_string())
{
    
}

UnexepctedError::UnexepctedError(const std::string& reason) : ErrorBase(reason)
{
    
}

OperationError::OperationError(const std::string& action, const std::string& reason) : ErrorBase("could not perform action '" + action + "' because of '" + reason + "'")
{
    
}

ConversionError::ConversionError(const std::string& action, const std::string& reason) : ErrorBase("the conversion '" + action + "' is invalid because of '" + reason + "'")
{
    
}

std::string io_error_string(IOErrorKind kind)
{
    switch (kind)
    {
        case IEK_Invalid:
            return "invalid file";
        case IEK_NotFound:
            return "not found";
        case IEK_Permissions:
            return "invalid permissions";
    }
}

IOError::IOError(const std::string& location, IOErrorKind kind) : ErrorBase("the resource at '" + location + "' could not be resolved because of '" + io_error_string(kind) + "'") 
{
    
}
