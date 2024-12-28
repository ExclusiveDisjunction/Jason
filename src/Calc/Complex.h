//
// Created by Hollan on 12/17/24.
//

#ifndef JASON_COMPLEX_H
#define JASON_COMPLEX_H

#include "VariableType.h"
#include "Constraints.h"

class Complex : public VariableType
{
private:
    
public:
    Complex() noexcept;
    Complex(const Complex& obj) noexcept;
    Complex(Complex&& obj) noexcept;
    Complex(double a, double b);
    
    Complex& operator=(const Complex& obj) noexcept;
    Complex& operator=(Complex&& obj) noexcept;
    
    [[nodiscard]] std::pair<double, double> ToPolar() const noexcept;
    [[nodiscard]] static Complex FromPolar(double r, double theta) noexcept;
    [[nodiscard]] static Complex FromPolar(const std::pair<double, double>& polar) noexcept;
    
    double a = 0.00;
    double b = 0.00;
    
    [[nodiscard]] VariableTypes GetType() const noexcept override { return VT_Complex; }
    void str_serialize(std::ostream& out) const noexcept override;
    void str_deserialize(std::istream& in) override;
    void dbg_fmt(std::ostream& out) const noexcept override;
    void dsp_fmt(std::ostream& out) const noexcept override;
    
    [[nodiscard]] std::unique_ptr<VariableType> Clone() const noexcept override;

    Complex operator+(const Complex& b) const noexcept;
    Complex operator-(const Complex& b) const noexcept;
    Complex operator*(const Complex& b) const noexcept;
    Complex operator/(const Complex& b) const;
    
    Complex& operator+=(const Complex& b) noexcept;
    Complex& operator-=(const Complex& b) noexcept;
    Complex& operator*=(const Complex& b) noexcept;
    Complex& operator/=(const Complex& b);
    
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator+=(const T& b) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator-=(const T& b) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator*=(const T& b) noexcept;
    template<typename T> requires IsScalarOrDouble<T>
    Complex& operator/=(const T& b);
    
    bool operator==(const VariableType& obj) const noexcept override;
    bool operator!=(const VariableType& obj) const noexcept override;
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
Complex operator/(const Complex& a, const T& b);
template<typename T> requires IsScalarOrDouble<T>
Complex operator/(const T& a, const Complex& b);

#include "Complex.tpp"

#endif //JASON_COMPLEX_H
