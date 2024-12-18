//
// Created by Hollan on 12/17/24.
//


#include "Complex.h"
#include "Scalar.h"
#include "../Core/Errors.h"

#include <math.h>
#include <utility>

Complex::Complex() noexcept : a(0), b(0)
{
    
}
Complex::Complex(double a, double b) : a(a), b(b)
{
    
}
Complex::Complex(const Complex& obj) noexcept : a(obj.a), b(obj.b)
{
    
}
Complex::Complex(Complex&& obj) noexcept : a(std::exchange(obj.a, 0)), b(std::exchange(obj.b, 0))
{
    
}

Complex& Complex::operator=(const Complex& obj) noexcept
{
    if (this == &obj)
        return *this;
    
    this->a = obj.a;
    this->b = obj.b;
    
    return *this;
}
Complex& Complex::operator=(Complex&& obj) noexcept
{
    this->a = std::exchange(obj.a, 0);
    this->b = std::exchange(obj.b, 0);
    
    return *this;
}

std::pair<double, double> Complex::ToPolar() const noexcept
{
    return std::make_pair<double, double>(sqrt(pow(this->a, 2) + pow(this->b, 2)), atan2(this->b, this->a));
}
Complex Complex::FromPolar(double r, double theta) noexcept
{
    return Complex::FromPolar(std::make_pair(r, theta));
}
Complex Complex::FromPolar(const std::pair<double, double>& polar) noexcept
{
    double x = polar.first * cos(polar.second);
    double y = polar.first * sin(polar.second);
    
    return Complex(x, y);
}

Complex Complex::FromBinary(const std::vector<Unit>& in)
{
    if (in.size() < 2)
        throw ConversionError("from binary", "expected at least two units");
        
    Complex result;
    in[0].Convert(result.a);
    in[1].Convert(result.b);
    
    return result;
}
std::unique_ptr<Complex> Complex::FromBinaryPtr(const std::vector<Unit>& in)
{
    return std::make_unique<Complex>(Complex::FromBinary(in));
}
void Complex::dbg_fmt(std::ostream& out) const noexcept
{
    out << "(Complex:" << this->a << ", " << this->b << ")";
}
void Complex::dsp_fmt(std::ostream& out) const noexcept
{
    out << this->a;
    if (this->b != 0)
    {
        if (this->b < 0)
            out << " - " << (this->b * -1);
        else
            out << " + " << this->b;
        
        out << "i";
    }
}

std::unique_ptr<VariableType> Complex::Clone() const noexcept
{
    return std::make_unique<Complex>(*this);
}

bool Complex::operator==(const VariableType& obj) const noexcept
{
    try {
        const auto& two = dynamic_cast<const Complex&>(obj);
        return this->a == two.a && this->b == two.b;
    } catch (...) {
        return false;
    }
}
bool Complex::operator!=(const VariableType& obj) const noexcept
{
    return !(*this == obj);
}

Complex Complex::operator+(const Complex& b) const noexcept
{
    Complex result(*this);
    result += b;
    return result;
}
Complex Complex::operator-(const Complex& b) const noexcept
{
    Complex result(*this);
    result -= b;
    return result;
}
Complex Complex::operator*(const Complex& b) const noexcept
{
    Complex result(*this);
    result *= b;
    return result;
}
Complex Complex::operator/(const Complex& b) const
{
    Complex result(*this);
    result /= b;
    return result;
}

Complex& Complex::operator+=(const Complex& b) noexcept
{
    this->a += b.a;
    this->b += b.b;
    
    return *this;
}
Complex& Complex::operator-=(const Complex& b) noexcept
{
    this->a -= b.a;
    this->b -= b.b;
    
    return *this;
}
Complex& Complex::operator*=(const Complex& b) noexcept
{
    // (a+bi)(c+di) = (ac + adi + bci - bd) = (ac - bd) + (ad + bc)i
    
    this->a = this->a * b.a - this->b * b.b;
    this->b = this->a * b.b + this->b * b.a;
    
    return *this;
}
Complex& Complex::operator/=(const Complex& b)
{
    std::pair<double, double> polarA = this->ToPolar(), polarB = b.ToPolar();
    
    if (polarB.first == 0)
        throw OperatorError('/', *this, b, "magnitude of second operand is zero");
    
    std::pair<double, double> newPolar =
        std::make_pair<double, double>(
            polarA.first / polarB.first,
            polarA.second - polarB.second
        );
    
    *this = Complex::FromPolar(newPolar);
    
    return *this;
}

