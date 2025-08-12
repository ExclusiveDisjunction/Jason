use std::fmt::{Display, Debug, Formatter};
use std::ops::Index;
use std::rc::Rc;
use crate::calc::num::DeterminantComputable;

use super::mat::Matrix;
use super::base::{matrix_determinant, matrix_eq, print_matrix, MatrixLike, MatrixRowStorage};

/// An iterator over a specific `MatrixRef`, used to extract row information.
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

/// A specific row of a `MatrixRef` extraction.
/// This provides an abstraction over the extaction.
#[derive(Debug, PartialEq, Eq)]
pub struct MatrixRefRow<'a, T> {
    over: &'a [T],
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
    rows: Vec<MatrixRefRow<'a, T>>,
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
impl<T> Eq for MatrixRef<'_, T> where T: PartialEq + Eq { }
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

        let rc = Rc::new(cols.clone());
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