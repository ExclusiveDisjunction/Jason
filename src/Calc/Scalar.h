//
// Created by exdisj on 5/26/24.
//

#ifndef JASON_SCALAR_H
#define JASON_SCALAR_H

#include "Constraints.h"
#include "VariableType.h"

class Scalar : public VariableType
{
public:
    Scalar() : Scalar(0) { }
    template<typename T> requires IsScalarOrDouble<T>
    explicit Scalar(const T& Item);
    Scalar(const Scalar& obj) = default;
    Scalar(Scalar&& obj) noexcept = default;
    
    Scalar& operator=(const Scalar& obj) = default;
    Scalar& operator=(Scalar&& obj) noexcept = default;

    double Data = 0.00;

    [[nodiscard]] constexpr VariableTypes GetType() const noexcept override { return VT_Scalar; }
    void str_serialize(std::ostream& out) const noexcept override;
    void str_deserialize(std::istream& in) override;
    void dsp_fmt(std::ostream& out) const noexcept override;
    void dbg_fmt(std::ostream& out) const noexcept override;

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

    [[maybe_unused]] constexpr explicit operator double() const noexcept
    {
        return this->Data;
    }
};

#include "ScalarT.tpp"

#endif //JASON_SCALAR_H
