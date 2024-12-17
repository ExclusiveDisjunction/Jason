/*
 * Created by exdisj
 */

#ifndef JASON_MATRIX_H
#define JASON_MATRIX_H

#include "Constraints.h"
#include "../StdCalc.h"
#include "../VariableType.h"
#include "../OperatorException.h"

#include <vector>

class MATH_LIB MathVector;

/// <summary>
/// Represents a rectangular arrangement of numbers for calculations, given some row and column definition.
/// </summary>
class MATH_LIB Matrix : public VariableType
{
private:
    /// <summary>
    /// The data stored in the matrix.
    /// </summary>
    std::vector<std::vector<double>> Data;
    unsigned int rows;
    unsigned int cols;

    void Allocate(unsigned int NewRows, unsigned int NewColumns, double Value = 0) noexcept;

    using ColumnSchema = std::vector<std::pair<bool, unsigned long>>;
    [[nodiscard]] ColumnSchema GetColumnWidthSchematic() const noexcept;
    [[nodiscard]] bool GetRowString(std::ostream& out, unsigned row, ColumnSchema& schema, char open, char close) const;

    Matrix();

public:
    Matrix(unsigned int Rows, unsigned int Columns) noexcept;
    [[maybe_unused]] explicit Matrix(const MathVector& in);
    Matrix(const Matrix& Other) noexcept;
    Matrix(Matrix&& Other) noexcept;

    template<std::convertible_to<double>... args>
    [[nodiscard]] static Matrix FromList(unsigned Rows, unsigned Columns, args... vals);

    Matrix& operator=(const Matrix& Other) noexcept;
    Matrix& operator=(Matrix&& Other) noexcept;

    friend std::ostream& operator<<(std::ostream&, const struct MatrixSingleLinePrint&);

    [[nodiscard]] unsigned int Rows() const { return Data.size(); }
    [[nodiscard]] unsigned int Columns() const { return Data.empty() ? 0 : Data[0].size(); }
    [[nodiscard]] bool IsValid() const { return rows != 0 && cols != 0; }
    [[nodiscard]] bool IsSquare() const { return IsValid() && rows == cols; }

    [[nodiscard]] std::unique_ptr<VariableType> Clone() const noexcept override;

    [[nodiscard]] size_t RequiredUnits() const noexcept override;
    [[nodiscard]] std::vector<Unit> ToBinary() const noexcept override;
    [[nodiscard]] static Matrix FromBinary(const std::vector<Unit>& in);
    [[nodiscard]] static std::unique_ptr<Matrix> FromBinaryPtr(const std::vector<Unit>& in);

    [[nodiscard]] VariableTypes GetType() const noexcept override;
    [[nodiscard]] std::string GetTypeString() const noexcept override;
    void Print(std::ostream& out) const noexcept override;

    [[nodiscard]] static Matrix ErrorMatrix();
    [[nodiscard]] static Matrix Identity(unsigned int Size);
    [[nodiscard]] static Matrix Identity(unsigned int Rows, unsigned int Cols);
    [[nodiscard]] [[maybe_unused]] static Matrix RandomMatrix(unsigned int Rows, unsigned int Columns, bool Integers);

    [[nodiscard]] const std::vector<double>& operator[](unsigned int Row) const;
    [[nodiscard]] const double& Access(unsigned int i, unsigned int j) const;
    [[nodiscard]] double& Access(unsigned int i, unsigned int j);

    [[nodiscard]] Matrix Extract(unsigned int StartI, unsigned int StartJ, unsigned int RowCount, unsigned int ColumnCount);

    void RowSwap(unsigned int OrigRow, unsigned int NewRow);
    void RowAdd(unsigned int OrigRow, double Fac, unsigned int TargetRow);

    [[maybe_unused]] [[nodiscard]] double Determinant() const;
    [[maybe_unused]] [[nodiscard]] Matrix Invert() const;
    [[maybe_unused]] [[nodiscard]] Matrix Transpose() const;
    void TransposeInplace();

    void RowEchelonForm();
    void ReducedRowEchelonForm();

    Matrix operator|(const Matrix& Two) const;

    Matrix operator+(const Matrix& Two) const;
    Matrix operator-(const Matrix& Two) const;
    Matrix operator*(const Matrix& Two) const;

    template<typename T> requires IsScalarOrDouble<T>
    Matrix operator*(const T& Two) const;
    template<typename T> requires IsScalarOrDouble<T>
    Matrix operator/(const T& Two) const;

    Matrix& operator+=(const Matrix& Two);
    Matrix& operator-=(const Matrix& Two);
    Matrix& operator*=(const Matrix& Two);
    template<typename T> requires IsScalarOrDouble<T>
    Matrix& operator*=(const T& Two);
    template<typename T> requires IsScalarOrDouble<T>
    Matrix& operator/=(const T& Two);

    [[nodiscard]] Matrix Pow(unsigned long long Two) const;

    bool operator==(const VariableType& two) const noexcept override;
    bool operator!=(const VariableType& two) const noexcept override;
    bool operator==(const Matrix& two) const noexcept;
    bool operator!=(const Matrix& two) const noexcept;
};

struct MatrixSingleLinePrint {
    const Matrix& Target;
};
inline MatrixSingleLinePrint PrintMatrixOneLine(const Matrix& obj) noexcept
{
    return MatrixSingleLinePrint { obj };
}

std::ostream& operator<<(std::ostream& out, const MatrixSingleLinePrint& Obj);
std::istream& operator>>(std::istream& in, Matrix& obj);

#include "MatrixT.tpp"

#endif //JASON_MATRIX_H