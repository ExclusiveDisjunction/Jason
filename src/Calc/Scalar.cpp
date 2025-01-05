//
// Created by exdisj on 5/26/24.
//

#include "Scalar.h"

#include <iomanip>


void Scalar::dbg_fmt(std::ostream& out) const noexcept
{
    out <<  "(Scalar:" << this->Data << ')';
}

std::unique_ptr<VariableType> Scalar::Clone() const noexcept
{
    return std::make_unique<Scalar>(*this);
}

void Scalar::str_serialize(std::ostream& out) const noexcept
{
    out << VariableTypes::VT_Scalar << ' ' << this->Data;
}
void Scalar::str_deserialize(std::istream& in)
{
    VariableTypes type;
    in >> type;
    if (type != VT_Scalar)
        throw OperationError("deserialize", "type mismatch");
    
    in >> this->Data;
}

long long Scalar::ToLongNoRound() const
{
    long long Trunc = ToLongTrunc();
    double diff = static_cast<double>(Trunc) - this->Data;
    diff = diff < 0 ? -1 * diff : diff;

    if (diff > 5E-6)
        throw std::logic_error("The instance cannot be converted to a long long.");
    else
        return Trunc;
}
long long Scalar::ToLongTrunc() const noexcept
{
    return static_cast<long long>(this->Data);
}

bool Scalar::operator==(const VariableType& obj) const noexcept
{
    try
    {
        return dynamic_cast<const Scalar&>(obj).operator double() == this->operator double();
    }
    catch (std::bad_cast& e)
    {
        return false;
    }
}
bool Scalar::operator!=(const VariableType& obj) const noexcept
{
    return !(*this == obj);
}
bool Scalar::operator==(double obj) const noexcept
{
    return this->Data == obj;
}
bool Scalar::operator!=(double obj) const noexcept
{
    return this->Data != obj;
}

void Scalar::dsp_fmt(std::ostream& out) const noexcept
{
    out << this->Data;
}
