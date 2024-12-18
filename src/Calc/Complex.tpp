#include "Complex.h"

#include "Scalar.h"
#include "../Core/Errors.h"

template<typename T> requires IsScalarOrDouble<T>
Complex& Complex::operator+=(const T& b) noexcept
{
    this->a += static_cast<double>(b);
}
template<typename T> requires IsScalarOrDouble<T>
Complex& Complex::operator-=(const T& b) noexcept
{
    this->a -= static_cast<double>(b);
}
template<typename T> requires IsScalarOrDouble<T>
Complex& Complex::operator*=(const T& b) noexcept
{
    auto fac = static_cast<double>(b);
    
    this->a *= fac;
    this->b *= fac;
    
    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
Complex& Complex::operator/=(const T& b)
{
    auto fac = static_cast<double>(b);
    if (fac == 0)
        throw OperatorError('/', *this, Scalar(b), "divide by zero");
    
    
}

template<typename T> requires IsScalarOrDouble<T>
Complex operator+(const Complex& a, const T& b) noexcept
{
    Complex result(a);
    result += b;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator+(const T& a, const Complex& b) noexcept
{
    return operator+(b, a);
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator-(const Complex& a, const T& b) noexcept
{
    Complex result(a);
    result -= b;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator-(const T& a, const Complex& b) noexcept
{
    return operator-(b, a);
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator*(const Complex& a, const T& b) noexcept
{
    Complex result(a);
    result *= b;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator*(const T& a, const Complex& b) noexcept
{
    return operator*(b, a);
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator/(const Complex& a, const T& b)
{
    Complex result(b);
    result /= a;
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Complex operator/(const T& a, const Complex& b)
{
    return operator/(b, a);
}
