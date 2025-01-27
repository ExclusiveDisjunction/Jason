use super::variable_type::*;
use super::scalar::{Scalar, ScalarLike};
use super::vector::MathVector;
use super::calc_error::{DimensionError, DimensionKind, IndexOutOfRangeError};
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

#[derive(Clone)]
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
    fn new_iter<T1, T2>(target: &'a Matrix, rows: T1, cols: T2) -> Self where T1: Iterator<Item = usize>, T2: Iterator<Item = usize> {
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
                    self.target,
                    first_rows_removed.clone(),
                    our_cols
                );
                let minors_det = minor.determinant()?;

                let fac: f64 = if (i + 1) % 2 == 0 {
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

        MatrixExtraction::new(self.target, extracted_rows, extracted_cols)
    }
}
impl Debug for MatrixExtraction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let rows_str: Vec<String> = self.target.data.iter().map(
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
impl Display for MatrixExtraction<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Debug).fmt(f)
    }
}
impl PartialEq for MatrixExtraction<'_> {
    fn eq(&self, other: &MatrixExtraction<'_>) -> bool {
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
impl PartialEq<Matrix> for MatrixExtraction<'_> {
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
impl PartialEq<MatrixExtraction<'_>> for Matrix {
    fn eq(&self, other: &MatrixExtraction<'_>) -> bool {
        other == self
    }
}
impl Index<(usize, usize)> for MatrixExtraction<'_> {
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
        (&self.as_extraction() as &dyn Debug).fmt(f)
    }
}
impl Display for Matrix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (&self.as_extraction() as &dyn Display).fmt(f)
    }
}

impl VariableData for Matrix {
    fn get_type(&self) -> VariableType {
        VariableType::Matrix
    }
}

impl<T> TryFrom<Vec<Vec<T>>> for Matrix where T: ScalarLike {
    type Error = DimensionError<usize>;
    fn try_from(value: Vec<Vec<T>>) -> Result<Self, Self::Error> {
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
                    data: value.into_iter().map(|x| x.into_iter().map(|x| x.as_scalar()).collect() ).collect()
                }
            )
        }
    }
}
impl From<MatrixExtraction<'_>> for Matrix {
    fn from(value: MatrixExtraction<'_>) -> Self {
        let mut result = Matrix::with_capacity(value.rows.len(), value.cols.len(), 0);

        for (our_i, i) in value.rows.iter().enumerate() {
            for (our_j, j) in value.cols.iter().enumerate() {
                result[(our_i, our_j)] = value.target[(*i, *j)];
            }
        }

        result
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
        else if rows == 0 || cols == 0 {
            self.data = vec![];
        } 
        else {
            self.data = vec![vec![f; cols]; rows];
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

    pub fn extract(&self, rows: Range<usize>, cols: Range<usize>) -> MatrixExtraction<'_> {
        MatrixExtraction::new_iter(self, rows, cols)
    }
    pub fn extract_specific(&self, rows: Vec<usize>, cols: Vec<usize>) -> MatrixExtraction<'_> {
        MatrixExtraction::new(self, rows, cols)
    }
    pub fn as_extraction(&self) -> MatrixExtraction<'_> {
        MatrixExtraction::new_iter(self, 0..self.rows(), 0..self.columns())
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
            Some(as_mat)
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

        for (i, row) in old_data.into_iter().enumerate(){
            for (j, element) in row.into_iter().enumerate() {
                self.data[j][i] = element;
            }
        }
    }

    fn rows_oob(&self, a: usize, b: usize) -> Result<(), IndexOutOfRangeError<usize>> {
        if a >= self.rows() {
            return Err(IndexOutOfRangeError::new(a))
        }
        else if b >= self.rows() {
            return Err(IndexOutOfRangeError::new(b))
        }

        Ok(())
    }
    pub fn row_swap(&mut self, orig: usize, dest: usize) -> Result<(), IndexOutOfRangeError<usize>> {
        self.rows_oob(orig, dest)?;

        if orig != dest {
            self.data.swap(orig, dest);
        }

        Ok(())
    }
    pub fn row_add<T>(&mut self, orig: usize, fac: T, dest: usize) -> Result<(), IndexOutOfRangeError<usize>> where T: ScalarLike{
        self.rows_oob(orig, dest)?;

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

        let rows = self.rows();
        let cols = self.columns();

        for i in 0..rows {
            let mut pivot_col = None;
            for col in 0..cols {
                if self.data[i][col] != 0.0 {
                    pivot_col = Some(col);
                    break;
                }
            }

            if let Some(p) = pivot_col {
                if self.data[i][p] != 1.0 {
                    let fac = self.data[i][p]; //We need to reduce the pivot to one.
                    for k in 0..cols {
                        self.data[i][k] /= fac;
                    }
                }

                for below_row in (i+1)..rows { //Every row under our row needs to have the 'p' value eliminated.
                    let fac = -self.data[below_row][p];
                    if fac == 0.0 || fac == -0.0 {
                        continue;
                    }

                    self.row_add(p, fac, below_row)?;
                }
            }
        }

        let mut rows: usize = rows;

        let mut i: usize = 0;
        loop {
            if i >= rows || i >= cols {
                break;
            }

            let mut pivot = None;
            for j in 0..cols {
                if self.data[i][j] != 0.0 {
                    pivot = Some(j);
                    break;
                }
            }

            let switch_with_last: bool;

            if let Some(p) = pivot {
                if p < rows && p > i {
                    self.row_swap(p, i)?;
                    i += 1;
                    switch_with_last = false;
                }
                else if p >= rows && p < cols {
                    switch_with_last = true;
                }
                else {
                    switch_with_last = false;
                    i += 1;
                }
                
            }
            else {
                switch_with_last = true;
            }

            if switch_with_last {
                let with = rows - 1;
                if with != i && with < rows {
                    self.row_swap(with, i)?;
                    rows -= 1; //This shrinks the rows, so that we dont double count that row.
                }
                else {
                    i += 1; //We increment when we dont swap with a zero row. 
                }
            }
        }

        Ok(())
    }
    pub fn reduced_row_echelon_form(&mut self) -> Result<(), IndexOutOfRangeError<usize>>{
        if !self.is_valid() {
            return Ok(());
        }

        self.row_echelon_form()?;

        /*

            The row echelon form provides the following functionalities:
                1. All pivots are one.
                2. All values under a pivot are zero.
                3. All zeroes rows are at the bottom.
                4. All pivots are in decending order. 

            Therefore, to convert this into reduced row echelon form:
                1. All values above a pivot must be zero.
         */

        let rows = self.rows();
        let cols = self.columns();

        for i in 0..rows {
            //Pivot is a column position
            let mut pivot = None;
            for j in 0..cols {
                if self.data[i][j] != 0.0 {
                    pivot = Some(j);
                    break;
                }
            }

            if let Some(p) = pivot {
                //We need to go above this pivot
                for row in 0..p {
                    let fac = -self.data[row][p];
                    self.row_add(i, fac, row)?;
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

        /*
              [ 1 ]   [ 4 -1 2 ]
            = [ 2 ] * [ 1  3 0 ] 
              [ 3 ]   [ 0 -1 4 ] 

                  [ 4 ]       [ -1 ]       [ 2 ]
            = 1 * [ 1 ] + 2 * [  3 ] + 3 * [ 0 ]
                  [ 0 ]       [ -1 ]       [ 4 ]

              [ 4 - 2 + 6  ]   
            = [ 1 + 3 + 0  ]
              [ 0 - 2 + 12 ]
            
              [ 8  ]
            = [ 4  ]
              [ 10 ]
         */

        let mut result = MathVector::with_capacity(rhs.rows());
        for i in 0..rhs.rows() {
            for j in 0..rhs.columns() {
                result[i] += self[j] * rhs.data[i][j];
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

#[test]
fn matrix_dimension_tester() {
    let a = MatrixDimension::new(1, 4);
    let b = MatrixDimension::new(3, 1);
    assert_ne!(a, b);
    assert_eq!(format!("{}", a), String::from("1x4"));
    assert_eq!(b, (3, 1));
    assert!(!a.is_empty());
    assert!(!a.is_square());
    assert!(MatrixDimension::new(1, 1).is_square());
}

#[test]
fn matrix_extraction_tester() {
    let mat = Matrix::identity(3);
    let extr = mat.as_extraction();
    assert_eq!(extr[(0, 0)], mat[(0, 0)]);
    assert_eq!(extr, mat);

    let sub_extr = extr.extract(0..1, 0..1);
    assert_eq!(sub_extr, mat.extract(0..1, 0..1));

    let sub_mat: Matrix = sub_extr.clone().into();
    assert_eq!(sub_mat, sub_extr);
    assert_ne!(sub_extr, extr);

    let det = extr.determinant();
    assert_eq!(det, Some(1.0));
}

#[test]
fn matrix_tester() {
    let a = Matrix::try_from(vec![vec![1, 3, 6], vec![-1, 4, 1], vec![6, 2, 4]]).unwrap();
    let b = Matrix::try_from(vec![vec![0, 4, 6], vec![-1, -2, -1], vec![1, -3, 6]]).unwrap();
    let c = Matrix::identity(2);
    let d = Matrix::try_from(vec![vec![1, 3], vec![-4, 1], vec![7, 6]]).unwrap();
    let e = Matrix::try_from(vec![vec![4, 1, 9], vec![7, -2, 1]]).unwrap();
    let f = Matrix::try_from(vec![vec![2, 6], vec![1, 4]]).unwrap();
    let v = MathVector::from(vec![1, 2, 3]);
    let s = Scalar::new(4);

    assert!(a.is_square() && a.is_valid());
    assert!(!Matrix::default().is_valid());

    assert_eq!(a.determinant(), Some(-112.0));
    assert_eq!(b.determinant(), Some(50.0));
    assert_eq!(c.determinant(), Some(1.0));
    assert_eq!(d.determinant(), None);

    assert_eq!((a.clone() + b).ok(), Matrix::try_from(vec![vec![1, 7, 12], vec![-2, 2, 0], vec![7, -1, 10]]).ok());

    assert_eq!(d * e, Matrix::try_from(vec![vec![25, -5, 12], vec![-9, -6, -35], vec![70, -5, 69]]));
    assert_eq!(c.clone() * f.clone(), Ok(f));
    assert_eq!(v * a, Ok(MathVector::from(vec![25, 10, 22])));
    assert_eq!(c.clone() * s, s * c.clone());
    assert_eq!(c * s, Matrix::try_from(vec![vec![4, 0], vec![0, 4]]).unwrap());
}

#[test]
fn matrix_rref_tester() {
    let mut l = Matrix::try_from(vec![vec![1, 4, 9], vec![-2, 1, 0], vec![0, -3, -6]]).unwrap();
    l.row_echelon_form().unwrap();
    let as_ref =Matrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![0.0, 1.0, 2.0], vec![0.0, 0.0, 0.0]]).unwrap();
    assert_eq!(l, as_ref);

    let mut m = Matrix::try_from(vec![vec![1.0, 4.0, 9.0], vec![0.0, 0.0, 0.0], vec![0.0, 1.0, 2.0]]).unwrap();
    m.row_echelon_form().unwrap();
    assert_eq!(m, as_ref);

    let mut r = Matrix::try_from(vec![vec![0, 0, 0], vec![4, 8, 16], vec![0, 0, 0]]).unwrap();
    r.row_echelon_form().unwrap();
    assert_eq!(r, Matrix::try_from(vec![vec![1, 2, 4], vec![0, 0, 0], vec![0, 0, 0]]).unwrap());

    let mut t = Matrix::try_from(vec![vec![1, 0, 0, 0, 0], vec![0, 0, 0, 0, 1], vec![0, 1, 0, 0, 0]]).unwrap();
    t.row_echelon_form().unwrap();
    assert_eq!(t, Matrix::try_from(vec![vec![1, 0, 0, 0, 0], vec![0, 1, 0, 0, 0], vec![0, 0, 0, 0, 1]]).unwrap());

    l.reduced_row_echelon_form().unwrap();
    let rref = Matrix::try_from(vec![vec![1, 0, 1], vec![0, 1, 2], vec![0, 0, 0]]).unwrap();
    assert_eq!(l, rref);
}