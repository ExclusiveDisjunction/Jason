#include "MathVector.h"

//Includes templated functions for MathVector.

template<std::convertible_to<double>... Args>
MathVector MathVector::FromList(Args... Value) noexcept
{
    auto ToFill = std::vector<double>({((double) Value)...});

    MathVector result;
    result.Data = ToFill;

    return result;
}

template<typename T> requires IsScalarOrDouble<T>
MathVector MathVector::operator*(const T& in) const
{
    MathVector result(*this);
    result.operator*=<T>(in);
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
MathVector MathVector::operator/(const T& in) const
{
    MathVector result(*this);
    result.operator/=<T>(in);
    return result;
}

template<typename T> requires IsScalarOrDouble<T>
MathVector& MathVector::operator*=(const T& in)
{
    if (!this->IsValid())
        throw OperatorException('*', this->GetTypeString(), "(Scalar)", "Cannot multiply an error vector.");

    auto fac = static_cast<double>(in);
    for (auto& elem : this->Data)
        elem *= fac;

    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
MathVector& MathVector::operator/=(const T& in)
{
    if (!this->IsValid())
        throw OperatorException('*', this->GetTypeString(), "(Scalar)", "Cannot divide an error vector.");

    auto fac = static_cast<double>(in);
    for (auto& elem : this->Data)
        elem /= fac;

    return *this;
}