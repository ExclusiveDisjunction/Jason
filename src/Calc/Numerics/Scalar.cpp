//
// Created by exdisj on 5/26/24.
//

#include "Scalar.h"

#include <iomanip>

VariableTypes Scalar::GetType() const noexcept
{
    return VariableTypes::VT_Scalar;
}

size_t Scalar::RequiredUnits() const noexcept 
{
    return 1;
}
std::vector<Unit> Scalar::ToBinary() const noexcept
{
    Unit data = Unit::FromVar(this->Data);

    return {data};
}
Scalar Scalar::FromBinary(const std::vector<Unit>& in)
{
    if (in.empty())
        throw std::logic_error("No data was provided");

    return Scalar(in[0].Convert<double>());
}
std::unique_ptr<Scalar> Scalar::FromBinaryPtr(const std::vector<Unit>& in)
{
    return std::make_unique<Scalar>(std::move(FromBinary(in)));
}
std::string Scalar::GetTypeString() const noexcept
{
    return "(Scalar)";
}

std::unique_ptr<VariableType> Scalar::Clone() const noexcept
{
    return std::make_unique<Scalar>(*this);
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

void Scalar::Print(std::ostream& out) const noexcept
{
    out << this->Data;
}
