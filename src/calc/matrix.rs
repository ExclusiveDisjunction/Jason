use super::variable_type::*;
use super::scalar::{Scalar, ScalarLike};
use super::vector::MathVector;
use super::calc_error::{CalcError, CalcResult, DimensionError, DimensionKind, IndexOutOfRangeError, OperationError};
use std::ops::{Add, Sub, Mul, Div, Index, IndexMut, Neg, Range};
use std::fmt::{Display, Debug, Formatter};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq)]
pub struct MatrixDimension {
    rows: usize,
    cols: usize
}
impl MatrixDimension {
    pub fn new(a: usize, b: usize) -> Self {
        Self {
            rows: a,
            cols: b
        }
    }

    pub fn is_square(&self) -> bool {
        self.rows == self.cols
    }
    pub fn is_empty(&self) -> bool {
        self.rows == 0 || self.cols == 0
    }
}
impl PartialEq<(usize, usize)> for MatrixDimension {
    fn eq(&self, other: &(usize, usize)) -> bool {
        self.rows == other.0 && self.cols == other.1
    }
}
impl Debug for MatrixDimension {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}x{}", self.rows, self.cols)
    }
}
impl Display for MatrixDimension {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Debug).fmt(f)
    }
}
impl DimensionKind for MatrixDimension { }

pub struct MatrixExtraction<'a> {
    target: &'a Matrix,
    rows: Vec<usize>,
    cols: Vec<usize>
}
impl<'a> MatrixExtraction<'a> {
    fn new(target: &'a Matrix, rows: Vec<usize>, cols: Vec<usize>) -> Self {
        Self {
            target,
            rows,
            cols
        }
    }
    fn new_range(target: &'a Matrix, rows: Range<usize>, cols: Range<usize>) -> Self {
        Self {
            target,
            rows: rows.collect(),
            cols: cols.collect()
        }
    }

    pub fn dim(&self) -> MatrixDimension {
        MatrixDimension::new(self.rows.len(), self.cols.len())
    }
    
    pub fn determinant(&self) -> Option<f64> {
        if !self.dim().is_square() {
            return None;
        }

        if self.dim() == (2, 2) {
            Some(
                self[(0, 0)] * self[(1, 1)] - self[(0, 1)] * self[(1, 0)]
            )
        }
        else {
            let mut result: f64 = 0.0;
            let first_rows_removed: Vec<usize> = self.rows.clone().into_iter().skip(1).collect();
            for i in self.cols.clone() {
                let our_cols: Vec<usize> = self.cols.clone().into_iter().filter(|x| *x != i).collect();

                let minor: MatrixExtraction<'a> = MatrixExtraction::new(
                    &self.target,
                    first_rows_removed.clone(),
                    our_cols
                );
                let minors_det = minor.determinant()?;

                let fac: f64 = if i % 2 == 0 {
                    -1.0
                } else {
                    1.0
                };

                result += fac * self[(0, i)] * minors_det;
            }

            Some(result)
        }
    }

    pub fn extract<U>(&self, rows: U, cols: U) -> MatrixExtraction<'a> where U: Iterator<Item = usize> {
        let extracted_rows: Vec<usize> = rows.map( |x| self.rows[x] ).collect();
        let extracted_cols: Vec<usize> = cols.map( |x| self.rows[x] ).collect();

        return MatrixExtraction::new(&self.target, extracted_rows, extracted_cols);
    }
}
impl<'a> Into<Matrix> for MatrixExtraction<'a> {
    fn into(self) -> Matrix {
        let mut result = Matrix::with_capacity(self.rows.len(), self.cols.len(), 0);

        let mut ourI: usize = 0;
        let mut ourJ: usize;

        for i in &self.rows {
            ourJ = 0;
            for j in &self.cols {
                result[(ourI, ourJ)] = self.target[(*i, *j)];
                ourJ += 1;
            }
            ourI += 1;
        }

        result
    }
}
impl<'a, 'b> PartialEq for MatrixExtraction<'a> {
    fn eq(&self, other: &MatrixExtraction<'a>) -> bool {
        if self.dim() != other.dim() { return false; }

        for i in 0..self.rows.len() {
            for j in 0..self.cols.len() {
                if self[(i, j)] != other[(i, j)] {
                    return false;
                }
            }
        }

        true
    }
}
impl<'a> PartialEq<Matrix> for MatrixExtraction<'a> {
    fn eq(&self, other: &Matrix) -> bool {
        if self.dim() != other.dims() { return false; }

        for i in 0..self.rows.len() {
            for j in 0..self.cols.len() {
                if self[(i, j)] != other[(i, j)] {
                    return false;
                }
            }
        }

        true
    }
}
impl<'a> PartialEq<MatrixExtraction<'a>> for Matrix {
    fn eq(&self, other: &MatrixExtraction<'a>) -> bool {
        other == self
    }
}
impl<'a> Index<(usize, usize)> for MatrixExtraction<'a> {
    type Output = f64;
    fn index(&self, index: (usize, usize)) -> &Self::Output {
        let row_offset = self.rows[index.0];
        let col_offset = self.cols[index.1];

        self.target.index((row_offset, col_offset))
    }
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct Matrix {
    data: Vec<Vec<f64>>
}
impl Index<(usize, usize)> for Matrix {
    type Output = f64;
    fn index(&self, index: (usize, usize)) -> &Self::Output {
        let (row, col) = index;
        if row >= self.rows() || col >= self.columns() {
            panic!("invalid row or column index")
        }

        &self.data[row][col]
    }
}
impl IndexMut<(usize, usize)> for Matrix {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        let (row, col) = index;
        if row >= self.rows() || col >= self.columns() {
            panic!("invalid row or column index")
        }

        &mut self.data[row][col]
    }
}

impl Debug for Matrix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let rows_str: Vec<String> = self.data.iter().map(
            |x| {
                x.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ")
            }
        ).collect();

        write!(
            f, 
            "[ {} ]", 
            rows_str.join("; ")
        )
    }
}
impl Display for Matrix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Debug).fmt(f)
    }
}

impl VariableData for Matrix {
    fn get_type() -> VariableType {
        VariableType::Matrix
    }
}

impl TryFrom<Vec<Vec<f64>>> for Matrix {
    type Error = DimensionError<usize>;
    fn try_from(value: Vec<Vec<f64>>) -> Result<Self, Self::Error> {
        if value.is_empty() {
            Ok(
                Self {
                    data: Vec::new()
                }
            )
        }
        else {
            let row_size: usize = value[0].len();
            for row in &value {
                if row.len() != row_size {
                    return Err(DimensionError::new(row_size, row.len()));
                }
            }

            Ok(
                Self {
                    data: value
                }
            )
        }
    }
}
impl Matrix {
    pub fn with_capacity<T>(rows: usize, cols: usize, val: T) -> Self where T: ScalarLike {
        let mut tmp = Self { data: vec![] };
        tmp.allocate(rows, cols, val);

        tmp
    }

    pub fn identity(dim: usize) -> Matrix {
        let mut result = Matrix::with_capacity(dim, dim, 0);
        for i in 0..dim {
            result[(i, i)] = 1.0;
        }

        result
    }

    fn allocate<T>(&mut self, rows: usize, cols: usize, val: T) where T: ScalarLike {
        let f = val.as_scalar();
        let currs = (self.rows(), self.columns());

        if currs == (rows, cols) {
            for row in &mut self.data {
                for item in row {
                    *item = f;
                }
            }
        }
        else {
            if rows == 0 || cols == 0 {
                self.data = vec![];
            } 
            else {
                self.data = vec![vec![f; cols]; rows];
            }
        }

    }
    
    pub fn rows(&self) -> usize {
        self.data.len()
    }
    pub fn columns(&self) -> usize {
        if self.rows() == 0 {
            0
        }
        else {
            self.data[0].len()
        }
    }
    pub fn dims(&self) -> MatrixDimension {
        MatrixDimension::new(self.rows(), self.columns())
    }
    pub fn is_square(&self) -> bool {
        self.dims().is_square()
    }
    pub fn is_valid(&self) -> bool {
        self.rows() != 0 && self.columns() != 0
    }

    pub fn extract<'a>(&'a self, rows: Range<usize>, cols: Range<usize>) -> MatrixExtraction<'a> {
        MatrixExtraction::new_range(self, rows, cols)
    }
    pub fn extract_specific<'a>(&'a self, rows: Vec<usize>, cols: Vec<usize>) -> MatrixExtraction<'a> {
        MatrixExtraction::new(self, rows, cols)
    }
    pub fn as_extraction<'a>(&'a self) -> MatrixExtraction<'a> {
        MatrixExtraction::new_range(self, 0..self.rows(), 0..self.columns())
    }

    pub fn determinant(&self) -> Option<f64> {
        self.as_extraction().determinant()
    }
    pub fn inverse(&self) -> Option<Matrix> {
        if !self.is_square() { return None; }
        if !self.is_valid() { return Some(Matrix::default()); }

        let identity = Matrix::identity(self.rows());
        let augmented = match Matrix::augment(&identity, self) {
            Ok(m) => m,
            Err(_) => return None
        };

        let cols_index = 0..self.rows();
        let left_index = 0..self.rows();
        let right_index = self.rows()..(2 * self.rows());

        let left = augmented.extract(left_index, cols_index.clone());
        let right = augmented.extract(right_index, cols_index);

        if right != identity {
            None
        }
        else {
            let as_mat: Matrix = left.into();
            return Some(as_mat);
        }
    }
    pub fn transpose(&self) -> Matrix {
        let mut result = Matrix::with_capacity(self.columns(), self.rows(), 0);

        for i in 0..self.rows() {
            for j in 0..self.columns() {
                result.data[j][i] = self.data[i][j];
            }
        }

        result
    }
    pub fn transpose_inplace(&mut self) {
        let rows = self.rows();
        let cols = self.columns();

        let mut old_data: Vec<Vec<f64>> = vec![vec![]; rows];
        self.data.swap_with_slice(&mut old_data);
        self.allocate(cols, rows, 0);

        for i in 0..rows {
            for j in 0..cols {
                self.data[j][i] = old_data[i][j];
            }
        }
    }

    pub fn row_swap(&mut self, orig: usize, dest: usize) -> Result<(), IndexOutOfRangeError<usize>> {
        if orig >= self.rows() {
            return Err(IndexOutOfRangeError::new(orig))
        }
        else if dest >= self.rows() {
            return Err(IndexOutOfRangeError::new(dest))
        }

        if orig != dest {
            self.data.swap(orig, dest);
        }

        Ok(())
    }
    pub fn row_add<T>(&mut self, orig: usize, fac: T, dest: usize) -> Result<(), IndexOutOfRangeError<usize>> where T: ScalarLike{
        if orig >= self.rows() {
            return Err(IndexOutOfRangeError::new(orig))
        }
        else if dest >= self.rows() {
            return Err(IndexOutOfRangeError::new(dest))
        }

        let fac = fac.as_scalar();
        for j in 0..self.columns() {
            self.data[dest][j] += fac * self.data[orig][j];
        }

        Ok(())
    }
    pub fn row_echelon_form(&mut self) -> Result<(), IndexOutOfRangeError<usize>> {
        if !self.is_valid() {
            return Ok(());
        }

        let mut current_row: usize = 0;
        let rows = self.rows();
        let columns = self.columns();
        for current_col in 0..columns {
            let mut pivot_row = current_col;
            while pivot_row < rows && self.data[pivot_row][current_col] == 0.0 {
                pivot_row += 1;
            }

            if pivot_row < rows {
                self.row_swap(current_row, pivot_row)?;

                let pivot_value = self.data[current_row][current_col];
                for col in current_col..columns {
                    self.data[current_row][col] /= pivot_value;
                }

                for row in current_row+1..rows {
                    let mul = self.data[row][current_col];
                    self.row_add(row, -mul, current_row)?;
                }
            }

            current_row += 1;
        }

        Ok(())
    }
    pub fn reduced_row_echelon_form(&mut self) -> Result<(), IndexOutOfRangeError<usize>>{
        if !self.is_valid() {
            return Ok(());
        }

        self.row_echelon_form()?;

        let rows = self.rows();
        let cols = self.columns();

        for i in (0..rows).rev() {
            let mut pivot_col = None;
            for j in 0..cols {
                if self.data[i][j] != 0.0 {
                    pivot_col = Some(j);
                    break;
                }
            }

            if let Some(col) = pivot_col {
                let pivot_value = self.data[i][col];
                if pivot_value != 1.0 {
                    let scale_factor = 1.0 / pivot_value;
                    for j in 0..cols {
                        self.data[i][j] /= scale_factor;
                    }
                }

                //Elminate the pivot column above the current row
                for k in 0..i {
                    let factor = self.data[k][col];
                    if factor != 0.0 {
                        self.row_add(k, -factor, i)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn augment(lhs: &Self, rhs: &Self) -> Result<Self, DimensionError<usize>> {
        if lhs.rows() != rhs.rows() {
            return Err(DimensionError::new(lhs.rows(), rhs.rows()))
        }

        let one_rows = lhs.rows();
        let one_cols = lhs.columns();
        let two_cols = rhs.columns();

        let mut result = Matrix::with_capacity(one_rows, one_cols + two_cols, 0);
        assert!(result.is_valid());

        for i in 0..one_rows {
            let mut j: usize = 0;
            for sources_col in &lhs.data[i] {
                result.data[i][j] = *sources_col;
                j += 1;
            }

            for sources_col in &rhs.data[i] {
                result.data[i][j] = *sources_col;
                j += 1;
            }
        }
        
        Ok(result)
    }
}

impl Add for Matrix {
    type Output = Result<Matrix, DimensionError<MatrixDimension>>;
    fn add(self, rhs: Self) -> Self::Output {
        if self.dims() != rhs.dims() { return Err(DimensionError::new(self.dims(), rhs.dims())); } 

        let mut result = Matrix::with_capacity(self.rows(), self.columns(), 0.0);
        for i in 0..self.rows() {
            for j in 0..self.columns() {
                result.data[i][j] = self.data[i][j] + rhs.data[i][j];
            }
        }
        
        Ok(result)
    }
}
impl Sub for Matrix {
    type Output = Result<Matrix, DimensionError<MatrixDimension>>;
    fn sub(self, rhs: Self) -> Self::Output {
        if self.dims() != rhs.dims() { return Err(DimensionError::new(self.dims(), rhs.dims())); } 

        let mut result = Matrix::with_capacity(self.rows(), self.columns(), 0.0);
        for i in 0..self.rows() {
            for j in 0..self.columns() {
                result.data[i][j] = self.data[i][j] - rhs.data[i][j];
            }
        }
        
        Ok(result)
    }
}
impl Mul for Matrix {
    type Output = Result<Matrix, DimensionError<usize>>;
    fn mul(self, rhs: Self) -> Self::Output {
        if self.columns() != rhs.rows() { 
            return Err(DimensionError::new(self.columns(), rhs.rows()));
        }

        let r = self.rows();
        let c = rhs.columns();
        let mut result = Matrix::with_capacity(r, c, 0.0);

        for i in 0..r {
            for j in 0..c {
                let calc = &mut result.data[i][j]; //Note that we dont have to set to zero, because the matrix is filled out to zero.
                for k in 0..rhs.rows() {
                    *calc += self.data[i][k] * rhs.data[k][j];
                }
            }
        }

        Ok(result)
    }
}
impl<T> Mul<T> for Matrix where T: ScalarLike {
    type Output = Matrix;
    fn mul(self, rhs: T) -> Self::Output {
        let rhs = rhs.as_scalar();
        let result: Vec<Vec<f64>> = self.data.clone().into_iter().map(|x| 
            x.into_iter().map(|x| x * rhs).collect()
        ).collect();

        Matrix::try_from(result).unwrap() //We made this list, and we know it is "perfect", so we can safely unwrap. 
    }
}
impl Mul<Matrix> for Scalar {
    type Output = Matrix;
    fn mul(self, rhs: Matrix) -> Self::Output {
        rhs * self
    }
}
impl Mul<Matrix> for MathVector {
    type Output = Result<MathVector, DimensionError<usize>>;
    fn mul(self, rhs: Matrix) -> Self::Output {
        if self.dim() != rhs.columns() { 
            return Err(DimensionError::new(self.dim(), rhs.columns()));
        }

        let mut result = MathVector::with_capacity(rhs.rows());
        for i in 0..rhs.rows() {
            for j in 0..rhs.columns() {
                result[i] += self[i] * rhs.data[i][j];
            }
        }

        Ok(result)
    }
}
impl Mul<MathVector> for Matrix {
    type Output = Result<MathVector, DimensionError<usize>>;
    fn mul(self, rhs: MathVector) -> Self::Output {
        rhs * self
    }
}
impl<T> Div<T> for Matrix where T: ScalarLike {
    type Output = Matrix;
    fn div(self, rhs: T) -> Self::Output {
        let rhs = rhs.as_scalar();
        let result: Vec<Vec<f64>> = self.data.clone().into_iter().map(|x| 
            x.into_iter().map(|x| x / rhs).collect()
        ).collect();

        Matrix::try_from(result).unwrap() //We made this list, and we know it is "perfect", so we can safely unwrap. 
    }
}

impl Neg for Matrix {
    type Output = Self;
    fn neg(self) -> Self::Output {
        let mut result = self.clone();
        for row in &mut result.data {
            for element in row {
                *element *= -1.0;
            }
        }

        result
    }
}
