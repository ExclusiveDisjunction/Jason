use crate::calc::err::DimensionError;
use crate::calc::num::NullIdentity;
use crate::calc::MathVector;
use super::base::{matrix_eq, MatrixDimension, MatrixLike};
use super::mat::Matrix;
use super::extract::MatrixRef;

use std::ops::{Neg, Add, Sub, Mul, Div};
use std::iter::zip;

impl<T> Eq for Matrix<T> where T: Eq { } 

impl<T> PartialEq<MatrixRef<'_, T>> for Matrix<T> where T: PartialEq {
    fn eq(&self, other: &MatrixRef<'_, T>) -> bool {
        matrix_eq(self, other)        
    }
}
impl<T> PartialEq<Matrix<T>> for MatrixRef<'_, T> where T: PartialEq {
    fn eq(&self, other: &Matrix<T>) -> bool {
        matrix_eq(self, other)        
    }
}

// PURE
impl<T> Neg for Matrix<T> where T: Neg<Output=T> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        let result = self.into_iter()
            .map(|x| 
                x.into_iter()
                .map(|y| y.neg()) 
                .collect()
            ).collect();

        Self::direct(result)
    }
}
impl<T1, T2> Add<Matrix<T2>> for Matrix<T1> where T1: Add<T2> {
    type Output = Result<Matrix<<T1 as Add<T2>>::Output>, DimensionError<MatrixDimension>>;
    fn add(self, rhs: Matrix<T2>) -> Self::Output {
        if self.dimension() != rhs.dimension() { return Err(DimensionError::new(self.dimension(), rhs.dimension())); }

        let result_data = zip(self, rhs)
            .map(|x| {
                zip(x.0, x.1)
                    .map(|(a, b)| a + b)
                    .collect()
            })
            .collect();

        Ok( Matrix::direct(result_data) )
    }
}
impl<T1, T2> Sub<Matrix<T2>> for Matrix<T1> where T1: Sub<T2> {
    type Output = Result<Matrix<<T1 as Sub<T2>>::Output>, DimensionError<MatrixDimension>>;
    fn sub(self, rhs: Matrix<T2>) -> Self::Output {
        if self.dimension() != rhs.dimension() { return Err(DimensionError::new(self.dimension(), rhs.dimension())); }

        let result_data = zip(self, rhs)
            .map(|x| {
                zip(x.0, x.1)
                    .map(|(a, b)| a - b)
                    .collect()
            })
            .collect();

        Ok( Matrix::direct(result_data) )
    }
}
impl<T> Mul for Matrix<T> where T:  Clone + NullIdentity + Add<Output=T> + Mul<Output=T> {
    type Output = Result<Self, DimensionError<usize>>;
    fn mul(self, rhs: Self) -> Result<Self, DimensionError<usize>> {
        (&self).mul(&rhs)
    }
} 

// REFERENCE
impl<T> Neg for &Matrix<T> where T: Clone + Neg {
    type Output = Matrix<<T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        let result = self.iter()
            .map(|x| 
                x.iter  ()
                .map(|y| y.clone().neg()) 
                .collect()
            ).collect();

        Matrix::direct(result)
    }
}
impl<'a, A, B> Add<&'a Matrix<B>> for &Matrix<A> where A: Clone + Add<B>, B: Clone {
    type Output = Result<Matrix<<A as Add<B>>::Output>, DimensionError<MatrixDimension>>;
    fn add(self, rhs: &'a Matrix<B>) -> Self::Output {
        if self.dimension() != rhs.dimension() { 
            return Err(DimensionError::new(self.dimension(), rhs.dimension())); 
        }
        if self.is_empty() || rhs.is_empty() {
            return Ok( Matrix::default() )
        }

        let result_data = zip(self.iter(), rhs.iter())
            .map(|x| {
                zip(x.0.iter(), x.1.iter())
                    .map(|(a, b)| a.clone() + b.clone())
                    .collect()
            })
            .collect();

        Ok(
            Matrix::direct(result_data)
        )
    }
}
impl<'a, A, B> Sub<&'a Matrix<B>> for &Matrix<A> where A: Clone + Sub<B>, B: Clone {
    type Output = Result<Matrix<<A as Sub<B>>::Output>, DimensionError<MatrixDimension>>;
    fn sub(self, rhs: &'a Matrix<B>) -> Self::Output {
        if self.dimension() != rhs.dimension() { 
            return Err(DimensionError::new(self.dimension(), rhs.dimension())); 
        }
        if self.is_empty() || rhs.is_empty() {
            return Ok( Matrix::default() )
        }

        let result_data = zip(self.iter(), rhs.iter())
            .map(|x| {
                zip(x.0.iter(), x.1.iter())
                    .map(|(a, b)| a.clone() - b.clone())
                    .collect()
            })
            .collect();

        Ok(
            Matrix::direct(result_data)
        )
    }
}
impl<'a, T> Mul<&'a Matrix<T>> for &Matrix<T> where T: Clone + NullIdentity + Add<Output=T> + Mul<Output=T> {
    type Output = Result<Matrix<T>, DimensionError<usize>>;

    fn mul(self, rhs: &'a Matrix<T>) -> Self::Output {
        if self.cols() != rhs.rows() {
            return Err(DimensionError::new(self.cols(), rhs.rows()));
        }
        if self.is_empty() || rhs.is_empty() {
            return Ok( Matrix::default() )
        }

        let r = self.rows();
        let c = rhs.cols();
        let mut result = Matrix::allocate(r, c, T::null_id());

        for i in 0..r {
            for j in 0..c {
                let calc = &mut result[i][j]; //Note that we don't have to set to zero, because the matrix is filled out to zero.
                for k in 0..rhs.rows() {
                    let result = self[i][k].clone() * rhs[k][j].clone() + calc.clone();
                    *calc = result;
                }
            }
        }

        Ok(result)
    }
}

impl<T> Mul<T> for Matrix<T> where T: Mul + Clone {
    type Output = Matrix<<T as Mul>::Output>;
    fn mul(self, rhs: T) -> Self::Output {
        let result = self.into_iter()
            .map(|x| 
                x.into_iter()
                .map(|y| y * rhs.clone() )
                .collect()
            )
            .collect();

        Matrix::direct(result)
    }
}
impl<T> Mul<T> for &Matrix<T> where T: Mul + Clone {
    type Output = Matrix<<T as Mul>::Output>;
    fn mul(self, rhs: T) -> Self::Output {
        let result = self.iter()
            .map(|x| 
                x.iter()
                .map(|y| y.clone() * rhs.clone() )
                .collect()
            )
            .collect();

        Matrix::direct(result)
    }
}

impl<T> Div<T> for Matrix<T> where T: Div + Clone {
    type Output = Matrix<<T as Div>::Output>;
    fn div(self, rhs: T) -> Self::Output {
        let result = self.into_iter()
            .map(|x| 
                x.into_iter()
                .map(|y| y / rhs.clone() )
                .collect()
            )
            .collect();

        Matrix::direct(result)
    }
}
impl<T> Div<T> for &Matrix<T> where T: Div + Clone {
    type Output = Matrix<<T as Div>::Output>;
    fn div(self, rhs: T) -> Self::Output {
        let result = self.iter()
            .map(|x| 
                x.iter()
                .map(|y| y.clone() / rhs.clone() )
                .collect()
            )
            .collect();

        Matrix::direct(result)
    }
}

impl<T> Mul<MathVector<T>> for Matrix<T> where T: Mul + Clone, <T as Mul>::Output: Add<Output=<T as Mul>::Output> + Clone + NullIdentity {
    type Output = Result<MathVector<<T as Mul>::Output>, DimensionError<usize>>;
    fn mul(self, rhs: MathVector<T>) -> Self::Output {
        (&self).mul(&rhs)
    }
}
impl<'a, T> Mul<&'a MathVector<T>> for &Matrix<T> where T: Mul + Clone, <T as Mul>::Output: Add<Output=<T as Mul>::Output> + Clone + NullIdentity {
    type Output = Result<MathVector<<T as Mul>::Output>, DimensionError<usize>>;
    fn mul(self, rhs: &'a MathVector<T>) -> Self::Output {
        if self.cols() != rhs.dim() {
            return Err(DimensionError::new(self.cols(), rhs.dim()));
        }

        let mut result = MathVector::allocate(self.rows(), <T as Mul>::Output::null_id());
        for (result_elem, row) in zip(result.iter_mut(), self.iter()) {
            for (j, element) in row.iter().enumerate() {
                let curr = result_elem.clone() + rhs[j].clone() * element.clone();
                *result_elem = curr;
            }
        }

        Ok(result)
    }
}