//! Includes a series of utilities to store an extraction (minor) of a matrix. 
//! This provides the [`MatrixRef`] structure, which holds a reference to an owning matrix. 
//! If required, future versions will include operations on these extractions.

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

use std::fmt::{Display, Debug, Formatter};
use std::ops::Index;
use std::rc::Rc;

use super::mat::Matrix;
use super::prelude::{matrix_determinant, matrix_eq, print_matrix, MatrixLike, MatrixRowStorage, DeterminantComputable};

/// An iterator over a specific [`MatrixRef`], used to extract row information.
/// Once `Self::next` returns `None`, it will never return `Some(_)` again.
#[derive(Debug, Clone)]
pub struct MatrixRefIter<'a, T> where T: 'a {
    host: &'a [T],
    inner: std::slice::Iter<'a, usize>
}
impl<'a, T> Iterator for MatrixRefIter<'a, T> where T: 'a {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let next = *self.inner.next()?;

        Some( &self.host[next] )
    }
}

/// A specific row of a [`MatrixRef`] extraction.
/// This provides an abstraction over the extaction.
#[derive(Debug, PartialEq, Eq)]
pub struct MatrixRefRow<'a, T> {
    /// The target full row used.
    over: &'a [T],
    /// The shared columns used by the overall extraction.
    cols: Rc<Vec<usize>>
}
impl<T> Clone for MatrixRefRow<'_, T> {
    fn clone(&self) -> Self {
        Self {
            over: self.over,
            cols: Rc::clone(&self.cols)
        }
    }
}
impl<T> Index<usize> for MatrixRefRow<'_, T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        let mapped_col = self.cols[index];

        &self.over[mapped_col]
    }
}
impl<'a, T> MatrixRowStorage<'a, T> for MatrixRefRow<'a, T> where T: 'a {
    type Iter = MatrixRefIter<'a, T>;
    fn iter(&'a self) -> Self::Iter {
         MatrixRefIter {
            host: self.over,
            inner: self.cols.iter()
        }
    }
}
impl<'a, T> MatrixRefRow<'a, T> {
    /// Constructs a new row given some initial values.
    pub(super) fn new(over: &'a [T], cols: Rc<Vec<usize>>) -> Self {
        Self {
            over,
            cols
        }
    }
}

/// An extraction of a `Matrix`. This extraction, in many cases, can be treated like a matrix. 
/// It does not support arethmatic, but can be converted to a matrix directly.
/// This extraction allows viewing parts of a `Matrix` without making new heap allocations.
/// This does not allow for element modification, only viewing. 
/// This also holds a lock over the matrix, so no modifications can exist while the `MatrixRef` exists.
/// ```
/// let mat = Matrix::<i32>::try_from(vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]).unwrap();
/// let extract = mat.extract(0..2, 0..2);
/// assert_eq!(extract, Matrix::try_from(vec![1, 2], vec![4, 5]).unwrap())
/// ```
#[derive(Clone, Debug)]
pub struct MatrixRef<'a, T> where T: 'a {
    /// The rows included in this extraction
    rows: Vec<MatrixRefRow<'a, T>>,
    /// A shared access to the columns of the extraction 
    cols: Rc<Vec<usize>>
}
impl<'a, T> Index<usize> for MatrixRef<'a, T> where T: 'a {
    type Output = MatrixRefRow<'a, T>;
    fn index(&self, index: usize) -> &Self::Output {
        &self.rows[index]
    }
}
impl<T> PartialEq for MatrixRef<'_, T> where T: PartialEq {
    fn eq(&self, other: &Self) -> bool {
        matrix_eq(self, other)
    }
}
impl<T> Eq for MatrixRef<'_, T> where T: Eq { }
impl<T> Display for MatrixRef<'_, T> where T: Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_matrix(f, self)
    }
}
impl<'a, T> MatrixLike<'a> for MatrixRef<'a, T> where T: 'a {
    type Storage = T;
    type RowStorage = MatrixRefRow<'a, T>;
    type Extraction = Self;
    type Iter = std::slice::Iter<'a, MatrixRefRow<'a, T>>;

    fn extract<A, B>(&self, rows: A, cols: B) -> Self::Extraction where A: IntoIterator<Item = usize>, B: IntoIterator<Item = usize> {
        let mut new_rows: Vec<MatrixRefRow<'a, T>> = rows.into_iter().filter_map( |x| self.rows.get(x)).cloned().collect();
        let cols: Vec<usize> = cols.into_iter().collect();

        let rc = Rc::new(cols);
        for row  in &mut new_rows {
            row.cols = Rc::clone(&rc)
        };

        Self {
            rows: new_rows,
            cols: rc
        }
    }
    
    fn rows(&self) -> usize {
        self.rows.len()
    }
    fn cols(&self) -> usize {
        self.cols.len()
    }
    
    fn iter(&'a self) -> Self::Iter {
        self.rows.iter()
    }

    fn determinant(&self) -> Option<Self::Storage> where Self::Storage: DeterminantComputable {
        matrix_determinant(self)
    }
}
impl<'a, T> MatrixRef<'a, T> where T: 'a {
    /// Constructs a new extraction assuming that the rows and columns are within range for the matrix.
    pub(super) fn new<T1, T2>(target: &'a Matrix<T>, rows: T1, cols: T2) -> Self where T1: IntoIterator<Item=usize>, T2: IntoIterator<Item=usize> {
        let mapped_rows = rows.into_iter().filter_map(|x| target.get_row(x));
        let cols = Rc::new(cols.into_iter().collect());

        let total_rows = mapped_rows.map(|x| 
            MatrixRefRow::new(
                x,
                Rc::clone(&cols)
            )
        ).collect();

        Self {
            rows: total_rows,
            cols
        }
    }
}

impl<'a, 'b, T> IntoIterator for &'b MatrixRef<'a, T> where 'b: 'a {
    type IntoIter = std::slice::Iter<'a, MatrixRefRow<'a, T>>;
    type Item = &'a MatrixRefRow<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[test]
fn extraction_tester() {
    let mat: Matrix<i8> = Matrix::identity(3);
    let extr = mat.as_extraction();
    assert_eq!(extr[0][0], mat[0][0]);
    assert_eq!(extr, mat);

    let sub_extr = extr.extract(0..1, 0..1);
    assert_eq!(sub_extr, mat.extract(0..1, 0..1));

    let sub_mat: Matrix<_> = sub_extr.clone().into();
    assert_eq!(sub_mat, sub_extr);
    assert_ne!(sub_extr, extr);

    let det = extr.determinant();
    assert_eq!(det, Some(1));
}