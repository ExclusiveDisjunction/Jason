//
// Created by exdisj on 5/26/24.
//

#include <optional>
#include "VariableType.h"

#include "Scalar.h"
#include "Complex.h"
#include "MathVector.h"
#include "Matrix.h"

#include "../Core/Errors.h"

std::string VariableType::GetTypeString() const noexcept
{
    return static_cast<const DebugPrint&>(*this).dbg_fmt_string(); 
}

/*
std::unique_ptr<VariableType> VariableType::ApplyOperation(const VariableType& One, const VariableType& Two, char oper)
{
    try
    {
        VariableTypes t1 = One.GetType(), t2 = Two.GetType();

        if (t1 == VT_Scalar && t2 == VT_Scalar) //Trivial Case
        {
            const auto& A = dynamic_cast<const Scalar&>(One);
            const auto& B = dynamic_cast<const Scalar&>(Two);

            switch (oper)
            {
                case '+':
                    return std::make_unique<Scalar>(A + B);
                case '-':
                    return std::make_unique<Scalar>(A - B);
                case '*':
                    return std::make_unique<Scalar>(A * B);
                case '/':
                    return std::make_unique<Scalar>(A / B);
                case '%':
                {
                    try
                    {
                        long long ConvA = A.ToLongNoRound();
                        long long ConvB = B.ToLongNoRound();

                        return std::make_unique<Scalar>(ConvA % ConvB);
                    }
                    catch (std::logic_error& e)
                    {
                        throw OperatorError('%', A, B, "one or both operands are not integers");
                    }
                }
                case '^':
                    return std::make_unique<Scalar>(A.Pow(B));
                default:
                    return nullptr;
            }
        }
        else if (t1 == VT_Vector && t2 == VT_Vector)
        {
            const auto& A = dynamic_cast<const MathVector&>(One);
            const auto& B = dynamic_cast<const MathVector&>(Two);

            switch (oper)
            {
                case '+':
                    return std::make_unique<MathVector>(A + B);
                case '-':
                    return std::make_unique<MathVector>(A - B);
                default:
                    throw OperatorError(oper, A, B);
            }
        }
        else if (t1 == VT_Matrix && t2 == VT_Matrix)
        {
            const auto& A = dynamic_cast<const Matrix&>(One);
            const auto& B = dynamic_cast<const Matrix&>(Two);

            switch (oper)
            {
                case '+':
                    return std::make_unique<Matrix>(A + B);
                case '-':
                    return std::make_unique<Matrix>(A - B);
                case '*':
                    return std::make_unique<Matrix>(A * B);
                default:
                    throw OperatorError(oper, A, B);
            }
        }

        //
        // * Now that the trivial cases are out of the way, there are the more exotic cross types.
        // * These are:
        // *  1. Scalar * Vector
        // *  2. Vector * Scalar
        // *  3. Scalar * Matrix
        // *  4. Matrix * Scalar
        // *  5. Matrix * Vector (Given d == cols)
        // *  6. Matrix ^ Scalar (Given (Sca -> N || Sca == 0)  && Matrix is square if Sca >= 2)
        // *  7. Matrix + Vector (Given dims match)
        // *  8. Vector + Matrix (Given dims match)
        // *  9. Matrix - Vector (Given dims match)
        // *  10. Vector - Matrix (Given dims match)
        //
        
        switch (oper)
        {
            case '+':
            {
                if (t1 == VT_Matrix && t2 == VT_Vector)
                    return std::make_unique<Matrix>( dynamic_cast<const Matrix&>(One) + Matrix(dynamic_cast<const MathVector&>(Two)) );
                else if (t1 == VT_Vector && t2 == VT_Matrix)
                    return std::make_unique<Matrix>( dynamic_cast<const Matrix&>(Two) + Matrix(dynamic_cast<const MathVector&>(One)) );
                else
                    throw OperatorError(oper, One, Two);
            }
            case '-':
            {
                if (t1 == VT_Matrix && t2 == VT_Vector)
                    return std::make_unique<Matrix>( dynamic_cast<const Matrix&>(One) - Matrix(dynamic_cast<const MathVector&>(Two)) );
                else if (t1 == VT_Vector && t2 == VT_Matrix)
                    return std::make_unique<Matrix>( dynamic_cast<const Matrix&>(Two) - Matrix(dynamic_cast<const MathVector&>(One)) );
                else
                    throw OperatorError(oper, One, Two);
            }
            case '*':
            {
                if (t1 == VT_Scalar && t2 == VT_Vector)
                    return std::make_unique<MathVector>(dynamic_cast<const MathVector&>(Two) * dynamic_cast<const Scalar&>(One));
                else if (t1 == VT_Vector && t2 == VT_Scalar)
                    return std::make_unique<MathVector>(dynamic_cast<const MathVector&>(One) * dynamic_cast<const Scalar&>(Two));
                else if (t1 == VT_Scalar && t2 == VT_Matrix)
                    return std::make_unique<Matrix>(dynamic_cast<const Matrix&>(Two) * dynamic_cast<const Scalar&>(One));
                else if (t1 == VT_Matrix && t2 == VT_Scalar)
                    return std::make_unique<Matrix>(dynamic_cast<const Matrix&>(One) * dynamic_cast<const Scalar&>(Two));
                else if (t1 == VT_Matrix && t2 == VT_Vector)
                    return std::make_unique<Matrix>(dynamic_cast<const Matrix&>(One) * Matrix(dynamic_cast<const MathVector&>(Two)));
                else
                    throw OperatorError(oper, One, Two);
            }
            case '^':
            {
                if (t1 == VT_Matrix && t2 == VT_Scalar)
                {
                    try
                    {
                        const auto& A = dynamic_cast<const Matrix&>(One);
                        const auto& B = dynamic_cast<const Scalar&>(Two);

                        long long ConvB = B.ToLongNoRound();
                        if (ConvB < 0)
                            throw OperatorError('^', One, Two, "Cannot raise matrix to a negative power.");

                        return std::make_unique<Matrix>(A.Pow(static_cast<unsigned long long>(ConvB)));
                    }
                    catch (std::logic_error& e)
                    {
                        throw OperatorError('^', One, Two, "Cannot raise matrix to a non-integer power.");
                    }
                }

                throw OperatorError(oper, One, Two);
            }
            default:
                throw OperatorError(oper, One, Two);
        }

    }
    catch (std::bad_cast& e)
    {
        throw std::logic_error("A bad cast exception was thrown, meaning that a type lied about its ElementType");
    }
}
*/
 
std::ostream& operator<<(std::ostream& out, const VariableType& obj)
{
    return out << display_print(obj);
}

std::ostream& operator<<(std::ostream& out, const VariableTypes& obj)
{
    switch (obj)
    {
        case VT_Scalar:
            out << "SCA";
            break;
        case VT_Vector:
            out << "VEC";
            break;
        case VT_Matrix:
            out << "MAT";
            break;
        case VT_Complex:
            out << "CMP";
            break;
    }
    
    return out;
}
std::istream& operator>>(std::istream& in, VariableTypes& obj)
{
    char r_str[4] = {0};
    std::string str;
    in.read(r_str, 3);
    
    str = r_str;
    
    if (str == "SCA")
        obj = VT_Scalar;
    else if (str == "MAT")
        obj = VT_Matrix;
    else if (str == "VEC")
        obj = VT_Vector;
    else if (str == "CMP")
        obj = VT_Complex;
    else
        throw FormatError(str, "invalid option (expected SCA, MAT, VEC, or CMP)");
    
    return in;
}
