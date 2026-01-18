//! Contains primatives for the matrix structures and derived types.
//! This contains the errors, traits, and functions used by the rest of the module.

/*
    Copyright 2025 Hollan Sellars, Dr. Dipali Swain

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

use std::fmt::{Display, Debug};
use std::iter::zip;
use std::ops::{Index, Neg, Add, Sub, Mul};

use crate::calc::prelude::DimensionKind;

use super::super::num::NullIdentity;
use serde::{Deserialize, Serialize};

/// A simple structure representing mxn dimension of a matrix. 
/// This value can be constructed from a `(usize, usize)`, and compared with a `(usize, usize)` for simplicity. 
#[derive(Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct MatrixDimension {
    rows: usize,
    cols: usize
}
impl PartialEq<(usize, usize)> for MatrixDimension {
    fn eq(&self, other: &(usize, usize)) -> bool {
        self.rows == other.0 && self.cols == other.1
    }
}
impl Debug for MatrixDimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(rows: {}, cols: {})", self.rows, self.cols)
    }
}
impl Display for MatrixDimension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}x{}", self.rows, self.cols)
    }
}
impl From<(usize, usize)> for MatrixDimension {
    fn from(value: (usize, usize)) -> Self {
        Self::new(value.0, value.1)
    }
}
impl DimensionKind for MatrixDimension { }
impl MatrixDimension {
    /// Constructs a new `MatrixDimension` object containing the specificed rows and columns.
    pub fn new(a: usize, b: usize) -> Self {
        if a == 0 || b == 0 {
            Self {
                rows: 0,
                cols: 0
            }
        }
        else {
            Self {
                rows: a,
                cols: b
            }
        }
    }

    /// Returns true if the number of rows and colums are equal.
    pub fn is_square(&self) -> bool {
        self.rows == self.cols
    }
    /// Returns true if the number of rows or columns is zero.
    pub fn is_empty(&self) -> bool {
        self.rows == 0 || self.cols == 0
    }

    /// Returns the total number of elements stored by this matrix.
    pub fn elements_count(&self) -> usize {
        self.rows * self.cols
    }
}

/// A construct that rows of a matrix must conform to. Specifically, this allows for indexing to get individual elements of the matrix, and allows for iteration over the row. 
pub trait MatrixRowStorage<'a, T> : Index<usize, Output = T> where T: 'a {
    /// The iterator type used by this storage.
    type Iter: Iterator<Item=&'a T>;

    /// Obtains the iterator over the elements of this row.
    fn iter(&'a self) -> Self::Iter;
}
impl<'a, T> MatrixRowStorage<'a, T> for [T] where T: 'a {
    type Iter = std::slice::Iter<'a, T>;
    fn iter(&'a self) -> Self::Iter {
        self.iter()
    }
}

/// Represents a structure that has the shape, general actions, and common functionality of a 2d grid, or matrix. 
/// It includes functionality to look at the structure,  extract sub matricies, and compute the determinant (if the storing data allows for it)
pub trait MatrixLike<'a> : Index<usize, Output= Self::RowStorage> where Self::Storage: 'a, Self::RowStorage: 'a {
    /// The information that this matrix will store.
    type Storage;
    /// The row storage, or object that will allow for accessing a row as one object. 
    type RowStorage: MatrixRowStorage<'a, Self::Storage> + ?Sized ;
    /// An iterator over the rows.
    type Iter: Iterator<Item = &'a Self::RowStorage> where Self::RowStorage: 'a;
    /// The data type representing a sub matrix of this object.
    type Extraction: MatrixLike<'a, Storage = Self::Storage>;

    /// The number of rows in the data structure.
    fn rows(&self) -> usize;
    /// The number of columns in the data structure.
    fn cols(&self) -> usize;
    /// Returns a `MatrixDimension` encoding the rows and columns of this matrix.
    fn dimension(&self) -> MatrixDimension {
        MatrixDimension::new(self.rows(), self.cols())
    }
    /// Prints (Matrix:[`self.rows()`] x [`self.cols()`]), displaying a general shape of the matrix.
    fn shorthand(&self) -> String {
        format!("(Matrix:{})", self.dimension())
    }
    /// Returns true if the number of rows equals the number of columns.
    fn is_square(&self) -> bool {
        self.dimension().is_square()
    }
    /// Returns true if the matrix has no element (zero dimension). 
    fn is_empty(&self) -> bool {
        self.dimension().is_empty()
    }

    /// Returns an iterator over the matrices' rows.
    fn iter(&'a self) -> Self::Iter;

    /// Gets a subset of the matrix to view as a slice, or minor of the matrix.
    fn extract<A, B>(&'a self, rows: A, cols: B) -> Self::Extraction where A: IntoIterator<Item = usize>, B: IntoIterator<Item = usize>;
    /// Returns the matrix as an extraction itself.
    fn as_extraction(&'a self) -> Self::Extraction {
        self.extract(0..self.rows(), 0..self.cols())
    }

    /// When allowed (`Self::Storage` is `DeterminantComputable`), computes the determinant of the matrix.
    /// For this to be computed, the matrix must have the same storage across all extractions, the extraction must be matrix like, and the extraction's extraction must be the same as the top extraction.
    /// Essentially, the matrix extraction must be consistent with itself & cannot change type or storage from the origional matrix.
    fn determinant(&self) -> Option<Self::Storage> where Self::Storage: DeterminantComputable;
}

/// Represents a data type that can be used to compute the determinant of a Matrix. 
/// This is a composite trait that ensures that the type supports all functionality required for determinants.
/// Any type that matches it's criteria automatically implements it.
pub trait DeterminantComputable: 
    Sized + 
    NullIdentity + 
    Clone + 
    Mul<Self, Output=Self> + 
    Add<Self, Output=Self> + 
    Sub<Self, Output=Self> + 
    Neg<Output=Self> { }

impl<T> DeterminantComputable for T 
    where T: Sized + NullIdentity + Clone + Mul<Output=T> + Add<Output=T> + Sub<Output=T> + Neg<Output=T> { }

/// When allowed (`T::Storage` is `DeterminantComputable`), computes the determinant of the matrix.
/// For this to be computed, the matrix must have the same storage across all extractions, the extraction must be matrix like, and the extraction's extraction must be the same as the top extraction.
/// Essentially, the matrix extraction must be consistent with itself & cannot change type or storage from the origional matrix.
pub fn matrix_determinant<'a, T>(target: &'a T) -> Option<T::Storage> 
where 
    T: MatrixLike<'a>,
    T::Extraction: MatrixLike<'a, Storage = T::Storage, Extraction = T::Extraction>,
    T::Storage: DeterminantComputable {

    let dim = target.dimension();
    if !dim.is_square() || dim.is_empty() {
        return None;
    }

    if dim == (1, 1) {
       Some( target[0][0].clone() )
    }
    else if dim == (2, 2) {
        let a = target[0][0].clone();
        let b = target[0][1].clone();
        let c = target[1][0].clone();
        let d = target[1][1].clone();

        Some(
            a * d - b * c
        )
    }
    else {
        let mut result = T::Storage::null_id();
        let cols_range: Vec<usize> = (0..target.cols()).collect();
        // All rows but the first one, so the minors can be computed.
        let first_rows_remove: Vec<usize> = (1..target.rows()).collect();
        for i in &cols_range {
            let i = *i;
            // Removes the current column, to get a specific range of columns for the minor.
            let our_cols: Vec<usize> = cols_range.iter().filter(|x| **x != i).cloned().collect();

            // Obtain the minor, and compute the determinant.
            let minor = target.extract(first_rows_remove.clone().into_iter(), our_cols.into_iter());
            let minors_det = minor.determinant()?;

            // Computes the current value, and negates it if the index is odd.
            let mut pre_result = target[0][i].clone() * minors_det;
            if (i + 2) % 2 == 1 {
                pre_result = pre_result.neg()
            }

            result = result.clone() + pre_result;
        }

        Some(result)
    }
}

/// Determines if two different matricies are equal by value. This does not care which kind of matrices are placed in it, only that they are both `MatrixLike`, and the storage in matrix `lhs` must be equatable to `rhs`'s elements. 
pub fn matrix_eq<'a, 'b, T1, T2>(lhs: &'a T1, rhs: &'b T2) -> bool where T1: MatrixLike<'a>, T2: MatrixLike<'b>, T1::Storage: PartialEq<T2::Storage> {
    if lhs.dimension() != rhs.dimension() { return false; }

    zip(lhs.iter(), rhs.iter())
        .all(|(a, b)| -> bool {
            zip(a.iter(), b.iter())
                .all(|x| x.0 == x.1)
        })
}

/// Prints the matrix as a series of lines, one for each row. Rows are ended with ';', and the entire data set is surrounded with '[ ]'. This is Inline Printing.
pub fn print_matrix<'a, T>(f: &mut std::fmt::Formatter<'_>, target: &'a T) -> std::fmt::Result where T: MatrixLike<'a>, T::Storage: Display {
    f.write_str("[ ")?;
    let mut is_first = true;
    for row in target.iter() {
        if is_first {
            f.write_str("[ ")?;
            is_first = false;
        }
        else {
            f.write_str("; [ ")?;
        }
        let mut row_iter = row.iter();

        //Print the first element
        if let Some(first) = row_iter.next() {  
            f.write_fmt(format_args!("{first}"))?;
        }
        else {
            continue;
        }

        for item in row_iter {
            f.write_fmt(format_args!(", {item}"))?;
        }

        f.write_str(" ]")?;
    }

    f.write_str(" ]")
}

#[test]
fn test_matrix_print() {
    use super::mat::Matrix;
    let mat = Matrix::try_from(
        vec![
            vec![1, 2, 3],
            vec![4, 5, 6],
            vec![7, 8, 9]
        ]
    ).unwrap();

    println!("{}", &mat);
}

#[test]
fn dimension_tester() {
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
fn determinant_tester() {
    use super::mat::Matrix;

    let a = Matrix::try_from(vec![vec![1, 2], vec![2, 1]]).unwrap();
    let b = Matrix::try_from(vec![vec![1, 0], vec![1, 0]]).unwrap();

    assert_eq!(a.determinant(), Some(1 * 1 - 2 * 2));
    assert_eq!(b.determinant(), Some(0));

    let c = Matrix::try_from(vec![vec![1, 4, 6], vec![-1, 2, 4], vec![3, 1, 2]]).unwrap();

    assert_eq!(c.determinant(), Some(14))

}