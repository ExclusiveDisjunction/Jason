#include "Scalar.h"

#include "../Core/Errors.h"

//Stores templated functions for scalar.

template<typename T> requires IsScalarOrDouble<T>
Scalar::Scalar(const T& Item) : Data(static_cast<double>(Item))
{

}

template<typename T> requires IsScalarOrDouble<T>
Scalar Scalar::operator+(const T& in) const noexcept
{
    Scalar result(*this);
    result += in;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Scalar Scalar::operator-(const T& in) const noexcept
{
    Scalar result(*this);
    result -= in;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Scalar Scalar::operator*(const T& in) const noexcept
{
    Scalar result(*this);
    result *= in;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Scalar Scalar::operator/(const T& in) const
{
    Scalar result(*this);
    result /= in;
    return result;
}

template<typename T> requires IsScalarOrDouble<T>
Scalar& Scalar::operator+=(const T& in) noexcept
{
    this->Data += static_cast<double>(in);
    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
Scalar& Scalar::operator-=(const T& in) noexcept
{
    this->Data -= static_cast<double>(in);
    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
Scalar& Scalar::operator*=(const T& in) noexcept
{
    this->Data *= static_cast<double>(in);
    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
Scalar& Scalar::operator/=(const T& in)
{
    auto conv = static_cast<double>(in);
    if (conv == 0)
        throw OperatorError('/', *this, Scalar(0), "Divide by zero");

    this->Data /= conv;
    return *this;
}

template<typename T> requires IsScalarOrDouble<T>
Scalar Scalar::Pow(const T& in) const noexcept
{
    return Scalar { pow(static_cast<double>(*this), static_cast<double>(in)) };
}
