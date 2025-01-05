/*
 * Created by exdisj
 */

#ifndef JASON_MATHVECTOR_H
#define JASON_MATHVECTOR_H

#include "Constraints.h"
#include "VariableType.h"
#include "../Core/Errors.h"

#include <iostream>
#include <vector>
#include <string>
#include <sstream>

class MathVector : public VariableType
{
private:
    std::vector<double> Data;

public:
    MathVector();
    explicit MathVector(size_t Dim, double Val = 0.0);
    MathVector(const MathVector &Obj) noexcept;
    MathVector(MathVector &&Obj) noexcept;

    MathVector& operator=(const MathVector &Obj) noexcept;
    MathVector& operator=(MathVector &&Obj) noexcept;
    
    [[nodiscard]] static MathVector ErrorVector();
    template<std::convertible_to<double>... Args>
    [[nodiscard]] static MathVector FromList(Args... Value) noexcept;

    double& operator[](size_t Index);
    double operator[](size_t Index) const;

    [[maybe_unused]] [[nodiscard]] double Magnitude() const;
    [[maybe_unused]] [[nodiscard]] double Angle() const;

    [[nodiscard]] std::unique_ptr<VariableType> Clone() const noexcept override;
    
    void str_serialize(std::ostream& out) const noexcept override;
    void str_deserialize(std::istream& in) override;
    
    [[nodiscard]] constexpr VariableTypes GetType() const noexcept override { return VariableTypes::VT_Vector; }
    void dsp_fmt(std::ostream& out) const noexcept override;
    void dbg_fmt(std::ostream& out) const noexcept override;
    
    [[nodiscard]] size_t Dim() const { return Data.size(); }
    [[nodiscard]] bool IsValid() const { return !Data.empty(); }

    [[maybe_unused]] [[nodiscard]] static MathVector CrossProduct(const MathVector &One, const MathVector &Two);
    [[maybe_unused]] [[nodiscard]] static double DotProduct(const MathVector &One, const MathVector &Two);

    MathVector operator+(const MathVector& in) const;
    MathVector operator-(const MathVector& in) const;
    template<typename T> requires IsScalarOrDouble<T>
    MathVector operator*(const T& in) const;
    template<typename T> requires IsScalarOrDouble<T>
    MathVector operator/(const T& in) const;

    MathVector& operator+=(const MathVector& in);
    MathVector& operator-=(const MathVector& in);
    template<typename T> requires IsScalarOrDouble<T>
    MathVector& operator*=(const T& in);
    template<typename T> requires IsScalarOrDouble<T>
    MathVector& operator/=(const T& in);

    bool operator==(const VariableType& in) const noexcept override;
    bool operator!=(const VariableType& in) const noexcept override;
    bool operator==(const MathVector& in) const noexcept;
    bool operator!=(const MathVector& in) const noexcept;
};

#include "MathVectorT.tpp"

#endif //JASON_MATHVECTOR_H
