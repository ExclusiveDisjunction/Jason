#include "MathVector.h"

#include <utility>
#include <ranges>

MathVector::MathVector() : Data()
{

}
MathVector::MathVector(size_t Dim, double Val) : MathVector()
{
    if (Dim == 0)
        return; //No allocations take place.

    Data.resize(Dim);

    for (unsigned int i = 0; i < Dim; i++)
        Data[i] = Val;
}
MathVector::MathVector(const MathVector& Obj) noexcept : Data(Obj.Data)
{

}
MathVector::MathVector(MathVector&& Obj) noexcept : Data(std::move(Obj.Data))
{

}

MathVector& MathVector::operator=(const MathVector& Obj) noexcept
{
    if (this == &Obj) //Self assignment
        return *this;

    this->Data = Obj.Data;
    return *this;
}
MathVector& MathVector::operator=(MathVector&& Obj) noexcept
{
    this->Data = std::move(Obj.Data);
    return *this;
}

MathVector MathVector::ErrorVector()
{
    return {};
}

std::unique_ptr<VariableType> MathVector::Clone() const noexcept
{
    return std::make_unique<MathVector>(*this);
}

double& MathVector::operator[](size_t Index)
{
    if (Index >= Dim())
        throw std::logic_error("The index provided is invalid.");

    return Data[Index];
}
double MathVector::operator[](size_t Index) const
{
    if (Index >= Dim())
        throw std::logic_error("The index provided is invalid.");

    return Data[Index];
}
double MathVector::Magnitude() const
{
    if (Dim() == 1)
        return Data[0];

    if (!IsValid())
        throw std::logic_error("A magnitude cannot be measured on a dimensionless object.");

    double Sum = 0;
    for (const auto& slice : Data)
        Sum += pow(slice, 2);

    return sqrt(Sum);
}
double MathVector::Angle() const
{
    if (Dim() != 2) //nonsensical answer
        throw std::logic_error("Numerical error: Taking the angle of a vector with dimension other than 2 is a nonsensical answer");

    return atan(Magnitude());
}

void MathVector::dbg_fmt(std::ostream& out) const noexcept
{
    out << "(Vector:";
    if (!this->IsValid())
        out << "Err)";
    else
        out << this->Dim();
    out << ")";
}
void MathVector::dsp_fmt(std::ostream& out) const noexcept
{
    out << "{ ";
    size_t d = this->Dim();
    for (unsigned i = 0; i < d; i++)
        out << this->Data[i] << (i == d - 1 ? " " : ", ");
    out << '}';
}
void MathVector::str_serialize(std::ostream &out) const noexcept
{
    out << VariableTypes::VT_Vector << ' ' << this->Data.size();
    for (const auto& item : this->Data)
        out << ' ' << item;
}
void MathVector::str_deserialize(std::istream &in)
{
    VariableTypes type;
    size_t dim;
    in >> type;
    if (type != VT_Vector)
        throw FormatError("expected vector type");
    
    in >> dim;
    this->Data.resize(dim);
    
    size_t i;
    auto iter = this->Data.begin(), end = this->Data.end();
    for (i = 0; i < dim && in && iter != end; i++, iter++)
        in >> *iter;
    
    if (i != dim)
        throw FormatError("not enough arguments");
}

MathVector MathVector::CrossProduct(const MathVector& One, const MathVector& Two)
{
    if (Two.Dim() != One.Dim())
        throw OperatorError('X', One, Two, "cannot cross vectors with different dimensions");

    MathVector A, B;
    switch (One.Dim())
    {
    case 2:
        A = MathVector::FromList(One[0], One[1], 0);
        B = MathVector::FromList(Two[0], Two[1], 0);
        break;
    case 3:
        A = One;
        B = Two;
        break;
    default:
        throw OperatorError('X', One, Two, "cross is only defined for d={2,3}");
    }

    return MathVector::FromList((A[1] * B[2]) - (A[2] * B[1]), (A[2] * B[0]) - (A[0] * B[2]), (A[0] * B[1]) - (A[1] * B[0])); //Uses the cross product equation.
}
double MathVector::DotProduct(const MathVector& One, const MathVector& Two)
{
    if (One.Dim() != Two.Dim())
        throw OperatorError("dot", One, Two, "dimension mismatch");
    
    double Return = 0.0;
    for (auto i = One.Data.begin(), j = Two.Data.begin(); i != One.Data.end() && j != Two.Data.end(); i++, j++)
        Return += (*i) * (*j);

    return Return;
}

MathVector MathVector::operator+(const MathVector& in) const
{
    //Because our vectors can be added with different dimensions, we cannot call +=.
    
    if (this->Dim() == in.Dim())
    {
        MathVector result(*this);
        result += in;
        return result;
    }
    else
    {
        MathVector result(this->Dim() >= in.Dim() ? *this : in);
        const MathVector& lesser = (this->Dim() < in.Dim() ? *this : in);
        
        auto result_iter = result.Data.begin();
        for (const auto& item : lesser.Data)
        {
            *result_iter += item;
            result_iter++;
        }
        
        return result;
    }
}
MathVector MathVector::operator-(const MathVector& in) const
{
    //Because our vectors can be added with different dimensions, we cannot call -=.
    
    if (this->Dim() == in.Dim())
    {
        MathVector result(*this);
        result -= in;
        return result;
    }
    else
    {
        MathVector result(this->Dim() >= in.Dim() ? *this : in);
        const MathVector& lesser = (this->Dim() < in.Dim() ? *this : in);
        
        auto result_iter = result.Data.begin();
        for (const auto& item : lesser.Data)
        {
            *result_iter -= item;
            result_iter++;
        }
        
        return result;
    }
}

MathVector& MathVector::operator+=(const MathVector& in)
{
    if (!this->IsValid() || !in.IsValid())
        throw OperatorError('+', *this, in, "cannot combine error vectors");

    if (this->Dim() != in.Dim())
        throw OperatorError('+', *this, in, "dimension mismatch");

    auto i = this->Data.begin();
    auto j = in.Data.begin();

    for (; i != this->Data.end() && j != in.Data.end(); i++, j++)
        *i += *j;

    return *this;
}
MathVector& MathVector::operator-=(const MathVector& in)
{
    if (!this->IsValid() || !in.IsValid())
        throw OperatorError('-', *this, in, "cannot combine error vectors");

    if (this->Dim() != in.Dim())
        throw OperatorError('-', *this, in, "dimension mismatch");

    auto i = this->Data.begin();
    auto j = in.Data.begin();

    for (; i != this->Data.end() && j != in.Data.end(); i++, j++)
        *i -= *j;

    return *this;
}

bool MathVector::operator==(const VariableType& in) const noexcept
{
    try
    {
        const auto& obj = dynamic_cast<const MathVector&>(in);
        return (*this == obj);
    }
    catch (std::bad_cast& e)
    {
        return false;
    }
}
bool MathVector::operator!=(const VariableType& in) const noexcept
{
    return !(*this == in);
}
bool MathVector::operator==(const MathVector& in) const noexcept
{
    if (this->Dim() != in.Dim())
        return false;

    auto i = this->Data.begin(), j = in.Data.begin();
    for (; i != this->Data.end() && j != in.Data.end(); i++, j++)
        if (*i != *j)
            return false;

    return true;
}
bool MathVector::operator!=(const MathVector& in) const noexcept
{
    return !(*this == in);
}
