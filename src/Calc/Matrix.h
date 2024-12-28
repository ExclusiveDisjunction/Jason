/*
 * Created by exdisj
 */

#ifndef JASON_MATRIX_H
#define JASON_MATRIX_H

#include "Constraints.h"
#include "VariableType.h"
#include "../Core/Errors.h"

#include <vector>

class MathVector;

/// <summary>
/// Represents a rectangular arrangement of numbers for calculations, given some row and column definition.
/// </summary>
class Matrix : public VariableType, public UIDisplayPrint
{
private:
    /// <summary>
    /// The data stored in the matrix.
    /// </summary>
    std::vector<std::vector<double>> Data;
    size_t rows;
    size_t cols;

    void Allocate(size_t NewRows, size_t NewColumns, double Value = 0) noexcept;

    using ColumnSchema = std::vector<std::pair<bool, unsigned long>>;
    [[nodiscard]] ColumnSchema GetColumnWidthSchematic() const noexcept;
    [[nodiscard]] bool GetRowString(std::ostream& out, unsigned row, ColumnSchema& schema, char open, char close) const;

    Matrix();

public:
    Matrix(size_t Rows, size_t Columns) noexcept;
    [[maybe_unused]] explicit Matrix(const MathVector& in);
    Matrix(const Matrix& Other) noexcept;
    Matrix(Matrix&& Other) noexcept;

    template<std::convertible_to<double>... args>
    [[nodiscard]] static Matrix FromList(size_t Rows, size_t Columns, args... vals);

    Matrix& operator=(const Matrix& Other) noexcept;
    Matrix& operator=(Matrix&& Other) noexcept;

    friend std::ostream& operator<<(std::ostream&, const struct MatrixSingleLinePrint&);

    [[nodiscard]] size_t Rows() const { return Data.size(); }
    [[nodiscard]] size_t Columns() const { return Data.empty() ? 0 : Data[0].size(); }
    [[nodiscard]] bool IsValid() const { return rows != 0 && cols != 0; }
    [[nodiscard]] bool IsSquare() const { return IsValid() && rows == cols; }

    [[nodiscard]] std::unique_ptr<VariableType> Clone() const noexcept override;
    [[nodiscard]] VariableTypes GetType() const noexcept override;

    void str_serialize(std::ostream& out) const noexcept override;
    void str_deserialize(std::istream& in) override;

    void dbg_fmt(std::ostream& out) const noexcept override;
    void dsp_fmt(std::ostream& out) const noexcept override;
    void ui_dsp_fmt(std::ostream& out) const noexcept override;

    [[nodiscard]] static Matrix ErrorMatrix();
    [[nodiscard]] static Matrix Identity(size_t Size);
    [[nodiscard]] static Matrix Identity(size_t Rows, size_t Cols);
    [[nodiscard]] [[maybe_unused]] static Matrix RandomMatrix(size_t Rows, size_t Columns, bool Integers);

    [[nodiscard]] const std::vector<double>& operator[](size_t Row) const;
    [[nodiscard]] const double& Access(size_t i, size_t j) const;
    [[nodiscard]] double& Access(size_t i, size_t j);

    [[nodiscard]] Matrix Extract(size_t StartI, size_t StartJ, size_t RowCount, size_t ColumnCount);

    void RowSwap(size_t OrigRow, size_t NewRow);
    void RowAdd(size_t OrigRow, double Fac, size_t TargetRow);

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

#include "MatrixT.tpp"

#endif //JASON_MATRIX_H
