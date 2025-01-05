#include "Matrix.h"

#include "MathVector.h"
#include "Scalar.h"

#include <random>
#include <utility>

Matrix::Matrix() : rows(0), cols(0)
{

}
Matrix::Matrix(size_t Rows, size_t Columns) noexcept : rows(Rows), cols(Columns)
{
    Allocate(Rows, Columns, 0);
}
Matrix::Matrix(const MathVector& in) : Matrix()
{
    if (in.Dim() == 0)
        return;
    else
    {
        Allocate(in.Dim(), 1);
        for (unsigned i = 0; i < this->rows; i++)
            this->Data[i][0] = in[i];
    }
}
Matrix::Matrix(const Matrix& Other) noexcept  : cols(Other.cols), rows(Other.rows), Data(Other.Data)
{

}
Matrix::Matrix(Matrix&& Other) noexcept : cols(std::exchange(Other.cols, 0)), rows(std::exchange(Other.rows, 0)), Data(std::move(Other.Data))
{

}

Matrix& Matrix::operator=(const Matrix& Other) noexcept
{
    if (this == &Other)
        return *this;

    this->Data = Other.Data;
    this->cols = Other.cols;
    this->rows = Other.rows;

    return *this;
}
Matrix& Matrix::operator=(Matrix&& Other) noexcept
{
    this->Data = std::move(Other.Data);
    this->cols = std::exchange(Other.cols, 0);
    this->rows = std::exchange(Other.rows, 0);

    return *this;
}

void Matrix::Allocate(size_t NewRows, size_t NewColumns, double Value) noexcept
{
    size_t currRows = this->Rows(), currCols = this->Columns();

    if (rows == NewRows && cols == NewColumns && rows == currRows && cols == currCols) //Already that size, just set value
    {
        for (auto& row : Data)
            for (auto& element : row)
                element = Value;

        return;
    }

    this->Data.clear();

    if (NewRows > 0 && NewColumns > 0)
    {
        rows = NewRows;
        cols = NewColumns;

        Data.resize(cols);
        for (auto& row : Data)
            row.resize(cols, Value);
    }
    else
        cols = rows = 0;
}

Matrix Matrix::ErrorMatrix()
{
    return {};
}
Matrix Matrix::Identity(size_t Size)
{
    return Identity(Size, Size);
}
Matrix Matrix::Identity(size_t Rows, size_t Cols)
{
    Matrix result(Rows, Cols);
    for (unsigned i = 0; i < Rows && i < Cols; i++)
        result.Access(i, i) = 1;

    return result;
}
Matrix Matrix::RandomMatrix(size_t Rows, size_t Columns, bool Integers)
{
    Matrix result(Rows, Columns);
    if (!result.IsValid())
        return result;

    std::random_device dev;
    std::mt19937 engine(dev());

    if (Integers)
    {
        std::uniform_int_distribution dist(-10, 10);

        for (unsigned int i = 0; i < Rows; i++)
            for (unsigned int j = 0; j < Columns; j++)
                result.Data[i][j] = dist(engine);
    }
    else
    {
        std::uniform_real_distribution dist(-10.0, 10.0);

        for (unsigned int i = 0; i < Rows; i++)
            for (unsigned int j = 0; j < Columns; j++)
                result.Data[i][j] = dist(engine);
    }

    return result;
}

std::unique_ptr<VariableType> Matrix::Clone() const noexcept
{
    return std::make_unique<Matrix>(*this);
}

const std::vector<double>& Matrix::operator[](size_t Row) const
{
    if (Row > rows)
        throw std::logic_error("Out of bounds");

    return Data[Row];
}
const double& Matrix::Access(size_t i, size_t j) const
{
    if (i >= rows || j >= cols) //Out of range
        throw std::logic_error("Out of range");

    return this->Data[i][j];
}
double& Matrix::Access(size_t i, size_t j)
{
    return const_cast<double&>( const_cast<const Matrix*>(this)->Access(i, j) );
}

VariableTypes Matrix::GetType() const noexcept
{
    return VariableTypes::VT_Matrix;
}

Matrix Matrix::Extract(size_t StartI, size_t StartJ, size_t RowCount, size_t ColumnCount)
{
    /*
     * I want to split this in two parts. This function will return a MatrixExtrusion, and then the other part will
     * return a MatrixMinor.
     */

    if (RowCount == 0 || ColumnCount == 0)
        return Matrix::ErrorMatrix();

    if ((StartI + RowCount - 1) > rows || (StartJ + ColumnCount - 1) > cols)
        throw std::logic_error("The index was out of range for that size.");

    Matrix Return(RowCount, ColumnCount);
    if (!Return.IsValid())
        return Matrix::ErrorMatrix();

    for (size_t i = StartI, ip = 0; i < StartI + RowCount - 1; i++, ip++)
        for (size_t j = StartJ, jp = 0; j < StartJ + ColumnCount - 1; j++, jp++)
            Return.Data[ip][jp] = Data[i][j];

    return Return;
}

void Matrix::RowSwap(size_t OrigRow, size_t NewRow)
{
    if (OrigRow == NewRow || OrigRow > rows || NewRow > rows)
        return;

    std::swap(Data[OrigRow], Data[NewRow]);
}
void Matrix::RowAdd(size_t OrigRow, double Fac, size_t TargetRow)
{
    if (Fac == 0)
        throw std::logic_error("The factor of multiplication cannot be zero.");

    for (unsigned int j = 0; j < cols; j++)
        Data[TargetRow][j] += Data[OrigRow][j] * Fac;
}

double Matrix::Determinant() const
{
    if (this->rows != this->cols)
        throw std::logic_error("The matrix must be square");

    throw std::logic_error("Not implemented");

    /*
     * To do the determinant, we will have to get the minors of the matrix, and therefore requires the MatrixMinor.
     * Will be done with the addition of the matrix branch.
     */
}

Matrix Matrix::Invert() const
{
    if (rows != cols)
        throw std::logic_error("The matrix must be square");

    Matrix Id = Identity(rows);
    Matrix Aug = Id | *this;
    Aug.ReducedRowEchelonForm();

    Matrix Left = Aug.Extract(0, 0, rows, rows), Right = Aug.Extract(0, 4, rows, rows);
    if (Right != Id)
        return ErrorMatrix();
    else
        return Left;
}
Matrix Matrix::Transpose() const
{
    Matrix result(*this);
    result.TransposeInplace();

    return result;
}
void Matrix::TransposeInplace()
{
    for (unsigned i = 0; i < rows; i++)
        for (unsigned j = 0; j < cols; j++)
            std::swap(Data[i][j], Data[j][i]);
}
void Matrix::RowEchelonForm()
{
    if (!this->IsValid())
        return;

    unsigned currentRow = 0;

    for (unsigned current_col = 0; current_col < cols; current_col++)
    {
        unsigned pivot_row = current_col;
        while (pivot_row < rows && this->Data[pivot_row][current_col] == 0)
            pivot_row++;

        if (pivot_row < rows)
        {
            this->RowSwap(currentRow, pivot_row);

            double pivot_value = this->Data[currentRow][current_col];
            for (unsigned col = current_col; col < cols; col++)
                this->Data[currentRow][col] /= pivot_value;

            for (unsigned row = currentRow + 1; row < rows; row++)
            {
                double mul = this->Data[row][current_col];
                for (unsigned col = current_col; col < cols; col++)
                    this->Data[row][col] -= mul * this->Data[currentRow][col];
            }

            currentRow++;
        }
    }
}
void Matrix::ReducedRowEchelonForm()
{
    this->RowEchelonForm();

    unsigned lead = 0;

    for (unsigned r = 0; r < cols; r++)
    {
        if (lead >= cols)
            break;

        unsigned i = r;
        while (this->Data[i][lead] == 0)
        {
            i++;
            if (i == rows)
            {
                i = r;
                lead++;
                if (lead == cols)
                    break;
            }
        }

        if (lead < cols)
        {
            this->RowSwap(i, r);
            double divisor = this->Data[r][lead];
            if (divisor == 0)
            {
                for (unsigned j = 0; j < cols; j++)
                    this->Data[r][j] /= divisor;
            }

            for (unsigned k = 0; k < rows; k++)
            {
                if (k != r)
                {
                    double factor = this->Data[k][lead];
                    for (unsigned j = 0; j < cols; j++)
                        this->Data[k][j] = factor * this->Data[r][j];
                }
            }
        }

        lead++;
    }

    size_t Lead = 0;
    size_t Rows = this->rows, Columns = this->cols;
    for (unsigned int r = 0; r < Rows; r++)
    {
        if (Columns < Lead)
            return;
        unsigned i = r;
        while (Data[i][Lead] == 0)
        {
            i++;
            Lead++;
            if (Columns == Lead)
                return;
        }

        if (i != r)
            RowSwap(i, r);

        for (unsigned int j = 0; j < Columns; j++)
            Data[r][j] /= Data[r][Lead];

        for (unsigned int j = 0; j < Rows; j++)
        {
            if (j != r)
                RowAdd(r, -Data[j][Lead], j);
        }
        Lead++;
    }
}

bool Matrix::operator==(const VariableType& two) const noexcept
{
    try
    {
        const auto& conv = dynamic_cast<const Matrix&>(two);
        return *this == conv;
    }
    catch (std::bad_cast& e)
    {
        return false;
    }
}
bool Matrix::operator!=(const VariableType& two) const noexcept
{
    try
    {
        const auto& conv = dynamic_cast<const Matrix&>(two);
        return *this != conv;
    }
    catch (std::bad_cast& e)
    {
        return true;
    }
}
bool Matrix::operator==(const Matrix& two) const noexcept
{
    return this->rows == two.rows && this->cols == two.cols && this->Data == two.Data;
}
bool Matrix::operator!=(const Matrix& two) const noexcept
{
    return !(*this == two);
}

Matrix::ColumnSchema Matrix::GetColumnWidthSchematic() const noexcept
{
    /*
        This algorithm will find the largest width of each column, and store the result in the result value.

        For instance:

        [4 3 1 2]
        [33 4 1 -2]
        [5 -44.3 2 1]

        Which evaluates to:
        [(false, 2), (true, 5), (false, 1), (true, 2)]

        Which can be used to re-format the matrix when printing to be:
        [4   3    1  2]
        [33  4    1 -2]
        [5  -44.3 2  1]
    */
    ColumnSchema result;

    for (unsigned j = 0; j < this->cols; j++)
    {
        bool negative_found = false;
        unsigned long largest_num = 0;
        for (unsigned i = 0; i < this->rows; i++)
        {
            const double& curr = this->Data[i][j];
            if (curr < 0)
                negative_found = true;

            unsigned long as_str_l;
            {
                std::stringstream temp;
                temp << curr;
                as_str_l = static_cast<unsigned long>(temp.str().length());

                if (curr < 0) //Removes negative from the computation.
                    as_str_l--;
            }

            largest_num = std::max(as_str_l, largest_num);
        }

        result.emplace_back(negative_found, largest_num);
    }

    return result;
}
bool Matrix::GetRowString(std::ostream& out, unsigned row, Matrix::ColumnSchema& schema, char open, char close) const
{
    if (row > this->rows)
        return false;

    /*
        Convert the number to a string
        If the length of that string is less than the maximum of the column, then add extra spaces to the end till it meets the maximum.
        If there is a negative in that column, and the current number is not negative, add one to the beginning of the string, and remove one from the end. If it is negative, do not add any extra spaces.
        Repeat this for each number for each column and row, and put a space between columns.
    */

    out << open << ' ';
    const auto& host = this->Data[row];
    for (unsigned i = 0; i < this->cols; i++)
    {
        const auto&[has_negative, width] = schema[i];

        double curr = host[i];
        if (curr >= 0 && has_negative)
            out << ' ';

        std::string curr_str;
        {
            std::stringstream temp;
            temp << curr;
            curr_str = temp.str();
        }


        if (((has_negative && curr_str.length() < width + 1) || curr_str.length() < width))
        {
            unsigned long diff = width - (curr_str.length()) + (curr < 0 ? 1 : 0);
            std::string space_str;
            //if (has_negative && i != this->cols - 1) //If not in last row
            //   diff += (curr < 0 ? 1 : 0); //If negative, add one extra space after.

            for (unsigned t = 0; t < diff; t++)
                space_str += ' ';

            curr_str += space_str;
        }
        out << curr_str << ' ';
    }

    out << close;
    return out.good(); //If out.bad(), this returns false.
}
void Matrix::ui_dsp_fmt(std::ostream& out) const noexcept
{
    if (!this->IsValid())
    {
        out << "[ ]";
        return;
    }

    auto schema = this->GetColumnWidthSchematic();

    if (this->rows == 1)
    {
        if (!this->GetRowString(out, 0, schema, '[', ']'))
            std::cerr << "FAILED TO PRINT LINE 0" << std::endl;
        else
            out << '\n';
    }
    else
    {
        // ⌊ ⌋ ⌈ ⌉ |

        for (unsigned i = 0; i < rows; i++)
        {
            char open, close;
            if (i == 0 || i == rows - 1) //First and last rows
            {
                open = '[';
                close = ']';
            }
            else
                open = close = '|';

            if (!this->GetRowString(out, i, schema, open, close)) //Returns false if could not print properly.
                std::cerr << "FAILED TO PRINT LINE " << i << std::endl;
            else
                out << '\n';
        }
    }


}
void Matrix::dbg_fmt(std::ostream& out) const noexcept
{
    out << "(Matrix:" << rows << "x" << cols << ")";
}
void Matrix::dsp_fmt(std::ostream& out) const noexcept
{
    if (!this->IsValid())
        out << "[empty matrix]";
    else
    {
        out << "[ ";
        for (const auto& row : this->Data)
        {
            for (const auto& item : row)
                out << ' ' << item;
            out << ";";
        }
        out << " ]";
    }
}

void Matrix::str_serialize(std::ostream &out) const noexcept
{
    out << VariableTypes::VT_Matrix << ' ' << this->rows << ' ' << this->cols;
    for (const auto& rows : this->Data)
        for (const auto& item : rows)
            out << ' ' << item;
}
void Matrix::str_deserialize(std::istream &in)
{
    VariableTypes type;
    size_t rows, cols;
    in >> type;
    if (type != VT_Matrix)
        throw FormatError("expected matrix type");
    
    in >> rows >> cols;
    if (rows == 0 || cols == 0)
    {
        *this = Matrix();
        return;
    }
    
    this->Data.resize(rows, std::vector<double>(cols));
    size_t count = 0;
    for (auto& row : this->Data)
    {
        for (auto& element : row)
        {
            in >> element;
            count++;
        }
    }
    
    if (count != rows * cols)
        throw FormatError("not enough elements provided");
}

Matrix Matrix::operator|(const Matrix& Two) const
{
    if (!IsValid() || !Two.IsValid())
        throw OperatorError('|', *this, Two, "one or both is empty");

    if (this->rows != Two.rows)
        throw OperatorError('|', *this, Two, "row size mismatch");

    auto OneRows = rows, OneColumns = cols, TwoColumns = Two.cols;
    Matrix Return(OneRows, OneColumns + TwoColumns);

    if (!Return.IsValid())
        throw std::exception(); //Not supposed to happen, just in case though.

    unsigned i = 0;
    for (auto& resultRow : Return.Data)
    {
        unsigned int j = 0;
        auto currElem = resultRow.begin(), end = resultRow.end();
        for (; j < OneColumns && currElem != end; j++, currElem++)
            *currElem = this->Data[i][j];
        for (j = 0; j < TwoColumns && currElem != end; j++, currElem++)
            *currElem = Two.Data[i][j];

        i++;
    }

    return Return;
}

Matrix Matrix::operator+(const Matrix& Two) const
{
    Matrix result(*this);
    result += Two;
    return result;
}
Matrix Matrix::operator-(const Matrix& Two) const
{
    Matrix result(*this);
    result -= Two;
    return result;
}
Matrix Matrix::operator*(const Matrix& Two) const
{
    Matrix result(*this);
    result.operator*=(Two);
    return result;
}

Matrix& Matrix::operator+=(const Matrix& Two)
{
    if (!this->IsValid() || !Two.IsValid())
        throw OperatorError('+', *this, Two, "empty matrix");

    if (this->rows != Two.rows || this->cols != Two.cols)
        throw OperatorError('+', this->GetTypeString(), Two.GetTypeString(), "dimension mismatch");

    for (unsigned i = 0; i < this->rows; i++)
        for (unsigned j = 0; j < this->cols; j++)
            this->Data[i][j] += Two.Data[i][j];

    return *this;
}
Matrix& Matrix::operator-=(const Matrix& Two)
{
    if (!this->IsValid() || !Two.IsValid())
        throw OperatorError('-', *this, Two, "empty matrix");

    if (this->rows != Two.rows || this->cols != Two.cols)
        throw OperatorError('-', this->GetTypeString(), Two.GetTypeString(), "dimension mismatch");

    for (unsigned i = 0; i < this->rows; i++)
        for (unsigned j = 0; j < this->cols; j++)
            this->Data[i][j] -= Two.Data[i][j];

    return *this;
}
Matrix& Matrix::operator*=(const Matrix& Two)
{
    if (!this->IsValid() || !Two.IsValid())
        throw OperatorError('*', *this, Two, "empty matrix");

    if (this->rows != Two.rows || this->cols != Two.cols)
        throw OperatorError('*', this->GetTypeString(), Two.GetTypeString(), "dimension mismatch");

    auto r = this->rows, c = Two.cols;

    for (unsigned i = 0; i < r; i++)
    {
        for (unsigned j = 0; j < c; j++)
        {
            double calc = 0; //Since this is a matrix multiplication, I cannot reset the value at Data[i][j] since it will interfere with the calculation.
            for (unsigned k = 0; k < Two.rows; k++)
                calc += this->Data[i][k] * Two.Data[k][j];

            this->Data[i][j] = calc;
        }
    }

    return *this;
}

Matrix Matrix::Pow(unsigned long long Two) const
{
    if (!this->IsValid())
        throw OperatorError('^', this->GetTypeString(), "(Scalar:" + std::to_string(Two) + ")", "Empty Matrix");

    if (Two == 0)
        return Matrix::Identity(this->rows, this->cols);
    else if (Two == 1)
        return *this;
    else
    {
        if (!this->IsSquare())
            throw OperatorError('^', this->GetTypeString(), "(Scalar:" + std::to_string(Two) + ")", "Non-square matrix does not support powers greater than 1");

        Matrix result(*this);
        for (unsigned long long i = 1; i < Two; i++)
            result *= *this;

        return result;
    }
}
