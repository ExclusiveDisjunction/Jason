//
// Created by exdisj on 5/26/24.
//

#ifndef JASON_VARIABLETYPE_H
#define JASON_VARIABLETYPE_H

#include <iostream>
#include <sstream>
#include <string>
#include <optional>
#include <memory>

#include "../Core/BinaryUnit.h"
#include "../Core/Log.h"

enum VariableTypes
{
    VT_Scalar = 1,
    VT_Vector = 2,
    VT_Matrix = 3,
    VT_Complex = 4,
};
std::ostream& operator<<(std::ostream& out, const VariableTypes& obj);
std::istream& operator>>(std::istream& in, VariableTypes& obj);

class VariableType : public DebugPrint, public DisplayPrint, public StringSerializable
{
public:
    [[nodiscard]] virtual VariableTypes GetType() const noexcept = 0;
    [[nodiscard]] virtual std::unique_ptr<VariableType> Clone() const noexcept = 0;
    [[nodiscard]] std::string GetTypeString() const noexcept;
    
    // [[nodiscard]] static std::unique_ptr<VariableType> ApplyOperation(const VariableType& One, const VariableType& Two, char oper);

    virtual bool operator==(const VariableType& obj) const noexcept = 0;
    virtual bool operator!=(const VariableType& obj) const noexcept = 0;
};

std::ostream& operator<<(std::ostream& out, const VariableType& Obj);

#endif //JASON_VARIABLETYPE_H
