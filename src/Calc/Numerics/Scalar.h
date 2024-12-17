//
// Created by exdisj on 5/26/24.
//

#ifndef JASON_SCALAR_H
#define JASON_SCALAR_H

#include "Constraints.h"
#include "../VariableType.h"

class MATH_LIB Scalar : public VariableType
{
public:
    Scalar() : Scalar(0) { }
    template<typename T> requires IsScalarOrDouble<T>
    explicit Scalar(const T& Item);

    double Data = 0.00;

    [[nodiscard]] VariableTypes GetType() const noexcept override;
    [[nodiscard]] std::string GetTypeString() const noexcept override;
    [[nodiscard]] size_t RequiredUnits() const noexcept override;
    [[nodiscard]] std::vector<Unit> ToBinary() const noexcept override;
    [[nodiscard]] static Scalar FromBinary(const std::vector<Unit>& in);
    [[nodiscard]] static std::unique_ptr<Scalar> FromBinaryPtr(const std::vector<Unit>& in);

    [[nodiscard]] std::unique_ptr<VariableType> Clone() const noexcept override;

    template<typename T> requires IsScalarOrDouble<T>
    Scalar operator+(const T& in) const noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Scalar operator-(const T& in) const noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Scalar operator*(const T& in) const noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Scalar operator/(const T& in) const;

    template<typename T> requires IsScalarOrDouble<T>
    Scalar& operator+=(const T& in) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Scalar& operator-=(const T& in) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Scalar& operator*=(const T& in) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Scalar& operator/=(const T& in);

    [[nodiscard]] long long ToLongNoRound() const;
    [[nodiscard]] long long ToLongTrunc() const noexcept;

    template<typename T> requires IsScalarOrDouble<T>
    [[nodiscard]] Scalar Pow(const T& in) const noexcept;

    bool operator==(const VariableType& obj) const noexcept override;
    bool operator!=(const VariableType& obj) const noexcept override;
    bool operator==(double obj) const noexcept;
    bool operator!=(double obj) const noexcept;

    void Print(std::ostream& out) const noexcept override;

    [[maybe_unused]] constexpr explicit operator double() const noexcept
    {
        return this->Data;
    }
};

#include "ScalarT.tpp"

#endif //JASON_SCALAR_H
