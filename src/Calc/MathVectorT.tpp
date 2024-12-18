#include "MathVector.h"

//Includes templated functions for MathVector.

#include "../Core/Errors.h"
#include "Scalar.h"

template<std::convertible_to<double>... Args>
MathVector MathVector::FromList(Args... Value) noexcept
{
    MathVector result;
    result.Data = std::vector<double>(
        {
            static_cast<double>(Value)...
        }
    );
    
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
        throw OperatorError('*', *this, Scalar(in), "cannot multiply an error vector.");

    auto fac = static_cast<double>(in);
    for (auto& elem : this->Data)
        elem *= fac;

    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
MathVector& MathVector::operator/=(const T& in)
{
    if (!this->IsValid())
        throw OperatorError('*', *this, Scalar(in), "cannot divide an error vector.");

    auto fac = static_cast<double>(in);
    if (fac == 0)
        throw OperatorError('*', *this, Scalar(0), "cannot divide by zero");
    
    for (auto& elem : this->Data)
        elem /= fac;

    return *this;
}