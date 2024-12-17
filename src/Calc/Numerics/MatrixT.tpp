#include "Matrix.h"

//Includes templated functions for matrix.

template<std::convertible_to<double>... args>
Matrix Matrix::FromList(unsigned Rows, unsigned Columns, args... vals)
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
        throw OperatorException('*', this->GetTypeString(), "(Scalar)", "Empty Matrix");

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
        throw OperatorException('/', this->GetTypeString(), "(Scalar)", "Empty Matrix");

    auto fac = static_cast<double>(Two);
    for (auto& row : this->Data)
        for (auto& element : row)
            element /= fac;

    return *this;
}