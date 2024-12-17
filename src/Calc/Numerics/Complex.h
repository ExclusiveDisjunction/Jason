//
// Created by Hollan on 12/17/24.
//

#ifndef JASON_COMPLEX_H
#define JASON_COMPLEX_H

#include "../VariableType.h"
#include "Constraints.h"

class Complex : public VariableType
{
private:
    
public:
    Complex() = default;
    Complex(double a, double b) noexcept;
    Complex(const Complex& obj) noexcept = default;
    Complex(Complex&& obj) noexcept = default;
    template<typename T> requires IsScalarOrDouble<T>
    Complex(T&& a, T&& b);
    
    [[nodiscard]] std::pair<double, double> ToPolar() const noexcept;
    [[nodiscard]] static Complex FromPolar(double r, double theta) noexcept;
    [[nodiscard]] static Complex FromPolar(const std::pair<double, double>& polar) noexcept;
    
    double a = 0.00;
    double b = 0.00;
    
    [[nodiscard]] constexpr VariableTypes GetType() const noexcept override { return VT_Complex; }
    [[nodiscard]] std::string GetTypeString() const noexcept override;
    [[nodiscard]] constexpr size_t RequiredUnits() const noexcept override { return 2; }
    [[nodiscard]] static Complex FromBinary(const std::vector<Unit>& in);
    [[nodiscard]] static std::unique_ptr<Complex> FromBinaryPtr(const std::vector<Unit>& in);
    
    [[nodiscard]] std::unique_ptr<VariableType> Clone() const noexcept override;

    Complex operator+(const Complex& b) const noexcept;
    Complex operator-(const Complex& b) const noexcept;
    Complex operator*(const Complex& b) const noexcept;
    Complex operator/(const Complex& b) const noexcept;
    
    Complex& operator+=(const Complex& b) noexcept;
    Complex& operator-=(const Complex& b) noexcept;
    Complex& operator*=(const Complex& b) noexcept;
    Complex& operator/=(const Complex& b) noexcept;
    
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator+=(const T& b) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator-=(const T& b) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator*=(const T& b) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator/=(const T& b) noexcept;
    
    bool operator==(const VariableType& obj) const noexcept override;
    bool operator!=(const VariableType& obj) const noexcept override;
    
    void Print(std::ostream& obj) const noexcept override;
};

template<typename T> requires IsScalarOrDouble<T>
Complex operator+(const Complex& a, const T& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator+(const T& a, const Complex& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator-(const Complex& a, const T& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator-(const T& a, const Complex& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator*(const Complex& a, const T& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator*(const T& a, const Complex& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator/(const Complex& a, const T& b) noexcept;
template<typename T> requires IsScalarOrDouble<T>
Complex operator/(const T& a, const Complex& b) noexcept;

#endif //JASON_COMPLEX_H
