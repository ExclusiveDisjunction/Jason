#include "Matrix.h"

#include "Scalar.h"
#include "../Core/Errors.h"

//Includes templated functions for matrix.

template<std::convertible_to<double>... args>
Matrix Matrix::FromList(size_t Rows, size_t Columns, args... vals)
{
    std::vector<double> conv = std::vector<double>({ static_cast<double>(vals)... });
    if (conv.size() != Rows * Columns)
        throw std::logic_error("Not enough or too many numbers provided.");

    Matrix result(Rows, Columns);
    auto sCurr = conv.begin();

    for (auto& row : result.Data)
    {
        for (auto& element : row)
        {
            element = *sCurr;
            sCurr++;
        }
    }

    return result;
}

template<typename T> requires IsScalarOrDouble<T>
Matrix Matrix::operator*(const T& Two) const
{
    Matrix result(*this);
    result.operator*=<T>(Two);
    return result;
}
template<typename T> requires IsScalarOrDouble<T>
Matrix Matrix::operator/(const T& Two) const
{
    Matrix result(*this);
    result.operator/=<T>(Two);
    return result;
}

template<typename T> requires IsScalarOrDouble<T>
Matrix& Matrix::operator*=(const T& Two)
{
    if (!this->IsValid())
        throw OperatorError('*', *this, Scalar(Two), "empty matrix");

    auto fac = static_cast<double>(Two);
    for (auto& row : this->Data)
        for (auto& element : row)
            element *= fac;

    return *this;
}
template<typename T> requires IsScalarOrDouble<T>
Matrix& Matrix::operator/=(const T& Two)
{
    if (!this->IsValid())
        throw OperatorError('*', *this, Scalar(Two), "empty matrix");

    auto fac = static_cast<double>(Two);
    if (fac == 0)
        throw OperatorError('*', *this, Scalar(0), "divide by zero");
    
    for (auto& row : this->Data)
        for (auto& element : row)
            element /= fac;

    return *this;
}
