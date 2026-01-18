//! Contains the operators implemented for the matrix structure
//! This is put in a separate module to help separate out code and organize it.
//! 
//! Matrices support operations on both the owned structure, and a reference to it. Therefore, this code is valid:
//! ```
//! let a = Matrix::identity(2);
//! let b = Matrix::try_from(
//!     vec![
//!         vec![1, 2],
//!         vec![3, 4]
//!     ]   
//! );
//! 
//! let add_result = Matrix::try_from(
//!     vec![
//!         vec![2, 2],
//!         vec![3, 5]
//!     ]   
//! );
//! assert_eq!(&a + &b, Ok( add_result.clone() ));
//! assert_eq!(a + b, Ok( add_result ));
//! ```
//! 
//! Here is a summary of the operators provided to [`Matrix`] depending on what `T` supports:
//! 
//! |   Support  |                    Statement                     |                                   Requirements                                      |           Failures         | 
//! | ---------- | ------------------------------------------------ | ----------------------------------------------------------------------------------- | ---------------------------|
//! | [`Neg`]    | -[`Matrix<T>`] -> [`Matrix<B>`]                  | [`Neg<Output = B>`]                                                                 |                            |
//! | [`Add`]    | [`Matrix<A>`] + [`Matrix<B>`] -> [`Matrix<C>`]   | `A`: [`Add<B, Output = C>`]                                                         | [`Dimension`]              |
//! | [`Sub`]    | [`Matrix<A>`] - [`Matrix<B>`] -> [`Matrix<C>`]   | `A`: [`Sub<B, Output = C>`]                                                         | [`Dimension`]              |
//! | [`Mul`]    | [`Matrix<T>`] * `T` -> [`Matrix<C>`]             | `T`: [`Mul<T, Output = C>`] + [`Clone`]                                             |                            |
//! | [`Mul`]    | [`Matrix<T>`] * [`Matrix<T>`] -> [`Matrix<T>`]   | `T`: [`Add<T, Output = T>`] + [`Mul<T, Output = T>`] + [`Clone`] + [`NullIdentity`] | [`Dimension`]              |
//! | [`Div`]    | [`Matrix<T>`] / `T` -> [`Matrix<C>`]             | `T`: [`Div<T, Output = C>`] + [`Clone`]                                             |                            |
//! |   REF      | REF([`Matrix<T>`]) -> [`Matrix<T>`]              | See [`Matrix::row_echelon_form`]                                                    |                            |
//! |  RREF      | RREF([`Matrix<T>`]) -> [`Matrix<T>`]             | See [`Matrix::reduced_row_echelon_form`]                                            |                            |
//! | [`BitOr`]  | [`Matrix<T>`] | [`Matrix<T>`] -> [`Matrix<T>`]   |                                                                                     | [`Dimension`]              |
//! | [`BitOr`]  | &[`Matrix<T>`] | &[`Matrix<T>`] -> [`Matrix<T>`] | `T`: Copy                                                                           | [`Dimension`]              |
//! | powi, powf | POW{F/I}([`Matrix<T>`]) -> [`Matrix<T>`]         | `T`: Clone + NullIdentity + UnitIdentity + Add<Output=T> + Mul<Output=T>            | [`NonSquare`]/[`PowError`] |
//! 
//! [`NonSquare`]: `NonSquareError`
//! [`PowError`]: `MatrixPowError`
//! [`Dimension`]: `DimensionError`

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

use std::ops::{Add, BitOr, Div, Mul, Neg, Sub};
use std::iter::zip;
use std::fmt::Debug;

use super::super::num::{Incrementable, NullIdentity, UnitIdentity};
use super::prelude::{matrix_eq, MatrixDimension, MatrixLike};
use super::err::{NonSquareError, MatrixPowError, DimensionError};
use super::mat::Matrix;
use super::extract::MatrixRef;

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
impl<T> Neg for Matrix<T> where T: Neg {
    type Output = Matrix<<T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        let result: Vec<Vec<<T as Neg>::Output>> = self.into_iter()
            .map(|x| 
                x.into_iter()
                .map(|y| y.neg()) 
                .collect()
            ).collect();

        Matrix::direct(result)
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
impl<T> BitOr for Matrix<T> {
    type Output = Result<Self, DimensionError<usize>>;

    /// Combines two same-row matricies (but not same column) together into one matrix. The resultant matrix will have:
    /// 1) the same rows as both `self` and `rhs`.
    /// 2) The sum of `self.cols() + rhs.cols()` for its cols.
    /// ```
    /// let lhs = Matrix::try_from(vec![vec![1, 2], vec![3, 4]]).unwrap();
    /// let rhs = Matrix::try_from(vec![vec![5], vec![6]]).unwrap();
    /// 
    /// let aug = lhs.agument(&rhs).unwrap();
    /// assert_eq!(aug, Matrix::try_from(vec![vec![1, 2, 5], vec![3, 4, 6]]).unwrap());
    /// assert_eq!(aug.rows(), lhs.rows());
    /// assert_eq!(aug.cols(), lhs.cols() + rhs.cols());
    /// ```
    /// This will fail if and only if:
    /// 1) `self` or `rhs` is empty
    /// 2) `self.rows() != rhs.rows()`
    fn bitor(self, rhs: Self) -> Self::Output {
        if self.rows() != rhs.rows() || self.is_empty() || rhs.is_empty() {
            return Err(DimensionError::new(self.rows(), rhs.rows()))
        }

        let new_list: Vec<Vec<T>> = zip(self, rhs)
            .map(|(a, b)| 
                a.into_iter()
                    .chain(b)
                    .collect()
            )
            .collect();

        Ok( Matrix::direct(new_list) )
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
impl<'a, T> BitOr<&'a Matrix<T>> for &Matrix<T> where T: Clone {
    type Output = Result<Matrix<T>, DimensionError<usize>>;

    /// Combines two same-row matricies (but not same column) together into one matrix. The resultant matrix will have:
    /// 1) the same rows as both `self` and `rhs`.
    /// 2) The sum of `self.cols() + rhs.cols()` for its cols.
    /// ```
    /// let lhs = Matrix::try_from(vec![vec![1, 2], vec![3, 4]]).unwrap();
    /// let rhs = Matrix::try_from(vec![vec![5], vec![6]]).unwrap();
    /// 
    /// let aug = lhs.agument(&rhs).unwrap();
    /// assert_eq!(aug, Matrix::try_from(vec![vec![1, 2, 5], vec![3, 4, 6]]).unwrap());
    /// assert_eq!(aug.rows(), lhs.rows());
    /// assert_eq!(aug.cols(), lhs.cols() + rhs.cols());
    /// ```
    /// This will fail if and only if:
    /// 1) `self` or `rhs` is empty
    /// 2) `self.rows() != rhs.rows()`
    /// 
    /// Since the elements are stored behind a reference, the elements of both `self` and `rhs` are cloned into the new matrix. 
    /// If cloning is not cheap, do not use this function.
    fn bitor(self, rhs: &'a Matrix<T>) -> Self::Output {
        let lhs = self;
        if lhs.rows() != rhs.rows() || lhs.is_empty() || rhs.is_empty() {
            return Err(DimensionError::new(lhs.rows(), rhs.rows()))
        }

        let one_rows = lhs.rows();
        let one_cols = lhs.cols();
        let two_cols = rhs.cols();

        let mut result = Matrix::allocate(one_rows, one_cols + two_cols, lhs[0][0].clone());
        assert!(!result.is_empty());

        for i in 0..one_rows {
            let mut j: usize = 0;
            for sources_col in &lhs[i] {
                result[i][j] = sources_col.clone();
                j += 1;
            }

            for sources_col in &rhs[i] {
                result[i][j] = sources_col.clone();
                j += 1;
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

impl<T> Matrix<T> where T: Clone + PartialEq + UnitIdentity + NullIdentity + Neg<Output = T> + Add<Output=T> + Mul<Output=T> + Div<Output=T> {
    /// Puts the matrix into reduced row echelon form. This should not return Err. If it does, please report this issue.
    pub fn row_echelon_form(&mut self) {
        if self.is_empty() {
            return;
        }

        let rows = self.rows();
        let cols = self.cols();

        for i in 0..rows {
            let mut pivot_col = None;
            for col in 0..cols {
                if self[i][col] != T::null_id() {
                    pivot_col = Some(col);
                    break;
                }
            }

            if let Some(p) = pivot_col {
                if self[i][p] != T::unit_id() {
                    let fac = self[i][p].clone(); //We need to reduce the pivot to one.
                    for k in 0..cols {
                        let result = self[i][k].clone() / fac.clone();
                        self[i][k] = result;
                    }
                }

                for below_row in (i+1)..rows { //Every row under our row needs to have the 'p' value eliminated.
                    let fac = -self[below_row][p].clone();
                    if fac == T::null_id() || fac == -T::null_id(){
                        continue;
                    }

                    self.row_fac_add(p, fac, below_row).unwrap();
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
                if self[i][j] != T::null_id() {
                    pivot = Some(j);
                    break;
                }
            }

            let switch_with_last: bool;

            if let Some(p) = pivot {
                if p < rows && p > i {
                    self.row_swap(p, i).unwrap();
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
                    self.row_swap(with, i).unwrap();
                    rows -= 1; //This shrinks the rows, so that we dont double count that row.
                }
                else {
                    i += 1; //We increment when we dont swap with a zero row. 
                }
            }
        }
    }
    /// Puts the matrix into reduced row echelon form. This should not return an error, and if it does, please report it.
    pub fn reduced_row_echelon_form(&mut self) {
        if self.is_empty() {
            return;
        }

        self.row_echelon_form();

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
        let cols = self.cols();

        for i in 0..rows {
            //Pivot is a column position
            let mut pivot = None;
            for j in 0..cols {
                if self[i][j] != T::null_id() {
                    pivot = Some(j);
                    break;
                }
            }

            if let Some(p) = pivot {
                //We need to go above this pivot
                for row in 0..p {
                    let fac = -self[row][p].clone();
                    self.row_fac_add(i, fac, row).unwrap();
                }
            }
        }
    }

    /// Using the reduced row echelon form, computes the inverse of this matrix. 
    /// If the current matrix is `A`, then this finds `A^-1`, such that `A * A^-1 == A^-1 * A == I_n`, where `n` is the dimension of the matrix.
    /// This will return `None` if:
    /// 1. The matrix is not square
    /// 2. The determinant is zero
    /// 
    /// Note that this does not compute the determinant. It uses the reduced row echelon form for computation.
    /// To compute this value, the augment of `self` and `I_n` must be created. This will clone all elements of the current matrix,
    /// and will clone `T::null_id()`, `T::unit_id()` `n x n` times.
    pub fn inverse(&self) -> Option<Self> where T: PartialEq {
        if !self.is_square() { return None; }
        if self.is_empty() { return Some(Self::default()); }

        let identity = Self::identity(self.rows());
        let mut augmented = match &identity | self {
            Ok(m) => m,
            Err(_) => return None
        };

        augmented.reduced_row_echelon_form();

        let cols_index = 0..self.rows();
        let left_index = 0..self.rows();
        let right_index = self.rows()..(2 * self.rows());

        let left = augmented.extract(left_index, cols_index.clone());
        let right = augmented.extract(right_index, cols_index);

        if !matrix_eq(&right, &identity) {
            None
        }
        else {
            Some( left.into() )
        }
    }
}

impl<T> Matrix<T> where T: Clone + NullIdentity + UnitIdentity + Add<Output=T> + Mul<Output=T> {
    /// Applies the pow operation on the specified matrix until a counter (starting at `P::default()`) reaches `amount`.
    fn pow_base<P>(mut on: Matrix<T>, amount: P) -> Result<Self, NonSquareError> where P : Default + Incrementable + PartialOrd {
        if !on.is_square() {
            return Err(NonSquareError)
        }
        if on.is_empty() {
            return Ok(on)
        }

        let mut countdown = P::default();

        if amount == countdown {
            return Ok( Self::identity(on.rows() ) )
        }

        countdown.increment(); //If we want to raise to the 2nd power, we apply this loop once. 
        //So, for any given `amount`, we run this `amount - 1` times.
        while countdown < amount {
            on = (&on * &on).unwrap(); //Since matrix multiplcation can only fail IF the matrix is not of the right dimensions, but square matricies can always be multiplied... this is redundant. 
            countdown.increment();
        }

        Ok( on )
    }

    /// Computes the result of multiplying `self` `amount` times.
    pub fn powi<P>(&self, amount: P) -> Result<Self, NonSquareError> where P : Default + Incrementable + PartialOrd {
        Self::pow_base(self.clone(), amount)
    }

    /// Computes the result of multiplying `self` `amount` times, considering if `amount == P::default()`, `amount == P::unit_id()` or if `amount < P::default()`.
    /// If `amount < P::default()`, then the inverse of the matrix will be computed instead, and the operation placed on it. 
    /// Due to this, the value of `T` must support inverse operations. Please see `Matrix<T>::inverse()` for more info.
    pub fn powf<P>(&self, mut amount: P) -> Result<Self, MatrixPowError> 
    where P : Default + Incrementable + PartialOrd + UnitIdentity + Neg<Output=P> + Debug,
    T: Div<Output=T> + Neg<Output=T> + PartialEq {
        if !self.is_square() {
            return Err(MatrixPowError::NonSquare)
        }

        let acting: Self;

        if amount == P::default() {
            return Ok( Matrix::identity(self.rows()) );
        }
        else if amount == P::unit_id() {
            return Ok( self.clone() );
        }
        else if amount < P::default() {
            amount = amount.neg();
            acting = self.inverse().ok_or(MatrixPowError::NonInvertable)?;
        }
        else {
            acting = self.clone();
        }

        Ok( Self::pow_base(acting, amount).unwrap() ) //Since pow_base will return NonSquareError, and we already checked if our matrix is square, this will never fail.
    }
}