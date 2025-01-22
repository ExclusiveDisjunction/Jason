use super::variable_type::*;
use super::scalar::{Scalar, ScalarLike};
use super::calc_error::{DimensionError, OperationError, CalcError, CalcResult, DimensionKind};
use std::ops::{Add, Sub, Mul, Div, Index, IndexMut, Neg, Range};
use std::fmt::{Display, Debug, Formatter};
use std::iter::zip;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq)]
pub struct MatrixDimension<T> where T: DimensionKind {
    rows: T,
    cols: T
}
impl<T> MatrixDimension<T> where T: DimensionKind {
    pub fn new(a: T, b: T) -> Self {
        Self {
            rows: a,
            cols: b
        }
    }

    pub fn is_square(&self) -> bool {
        self.rows == self.cols
    }
}
impl<T, U> PartialEq<(U, U)> for MatrixDimension<T> where T: DimensionKind, U: DimensionKind, T: PartialEq<U> {
    fn eq(&self, other: &(U, U)) -> bool {
        self.rows == other.0 && self.cols == other.1
    }
}


pub struct MatrixExtraction<'a, T> {
    target: &'a Matrix,
    rows: T,
    cols: T
}
impl<'a, T> MatrixExtraction<'a, T> {
    fn new(target: &'a Matrix, rows: T, cols: T) -> Self {
        Self {
            target,
            rows,
            cols
        }
    }
}
impl<'a, T> Into<Matrix> for MatrixExtraction<'a, T> where T: Iterator<Item = usize> + Clone {
    fn into(self) -> Matrix {
        let mut result = Matrix::with_capacity(self.rows.clone().count(), self.cols.clone().count(), 0);

        let mut ourI: usize = 0;
        let mut ourJ: usize;

        for i in self.rows {
            ourJ = 0;
            for j in self.cols.clone() {
                result[(ourI, ourJ)] = self.target[(i, j)];
                ourJ += 1;
            }
            ourI += 1;
        }

        result
    }
}
impl<'a, T> MatrixExtraction<'a, T> where T: Iterator<Item = usize> + Clone {
    pub fn dim(&self) -> MatrixDimension<usize> {
        MatrixDimension::new(self.rows.clone().count(), self.cols.clone().count())
    }
    
    pub fn determinant(&self) -> Option<f64> {
        if !self.dim().is_square() {
            return None;
        }

        if self.dim() == (2, 2) {
            
        }
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

    fn allocate<T>(&mut self, rows: usize, cols: usize, val: T) where T: ScalarLike {
        let f = val.as_scalar();


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

    pub fn extract<'a>(&'a self, rows: Range<usize>, cols: Range<usize>) -> MatrixExtraction<'a, Range<usize>> {
        MatrixExtraction::new(self, rows, cols)
    }
}
