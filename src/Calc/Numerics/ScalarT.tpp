#include "Scalar.h"

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
    //FIX THIS LATER
    auto conv = static_cast<double>(in);
    if (conv == 0) //div by zero
        throw OperatorException('/', this->GetTypeString(), this->GetTypeString(), "Divide by zero");

    Scalar result(*this);
    result /= conv;
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
        throw OperatorException('/', this->GetTypeString(), this->GetTypeString(), "Divide by zero");

    this->Data /= conv;
    return *this;
}

template<typename T> requires IsScalarOrDouble<T>
[[nodiscard]] Scalar Scalar::Pow(const T& in) const noexcept
{
    return Scalar { pow(static_cast<double>(*this), static_cast<double>(in)) };
}
