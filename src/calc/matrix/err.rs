//! All errors returned by [`Matrix`] and supporting structures.
//! 
//! [`Matrix`]: `super::mat::Matrix`

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
use std::error::Error as StdError;

/// An error that occurs when a matrix could not be constructed from a `Vec<Vec<T>>`.
/// This happens if one row has a different column size than the rest. 
/// Note that the first row is assumed to be the column size for the entire matrix. 
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct MatrixConversionError {
    /// The expected column size
    pub expected: usize,
    /// The found column size
    pub found: usize
}
impl Display for MatrixConversionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "unable to construct a matrix out of the list provided. all rows were expected to be {} item(s) long, but one row was {} item(s) long", self.expected, self.found)
    }
}
impl std::error::Error for MatrixConversionError { }
impl MatrixConversionError {
    /// Constructs a new instance of this error.
    pub fn new(expected: usize, found: usize) -> Self {
        Self {
            expected,
            found
        }
    }
}

pub use crate::calc::err::{OutOfRangeError, DimensionError};

/// An error that occurs when the matrix provided is non-square, but the operation needs it to be square.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct NonSquareError;
impl Display for NonSquareError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("the matrix is not square")
    }
}
impl std::error::Error for NonSquareError { }

/// All errors that occur with raising a matrix to some power, using float power.
/// Like [`NonSquareError`], this includes a case for when the matrix is not square.
/// Additionally, [`MatrixPowError::NonInvertable`] can only occur if the power operand is negative. 
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum MatrixPowError {
    /// Indicates that the matrix is not square. 
    NonSquare,
    /// Indicates that the power operand is negative, but the inverse of the matrix could not be found.
    NonInvertable
}
impl Display for MatrixPowError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::NonInvertable => "the matrix is non-invertable, but the power provided is negative",
                Self::NonSquare => "the matrix is not square"
            }
        )
    }
}
impl std::error::Error for MatrixPowError { }

/// The error returned when a matrix is unable to be parsed from a [`str`]. 
/// This is returned by the [`FromStr`] trait for [`Matrix`].
/// See each case to determine the cause of the error.
/// 
/// [`FromStr`]: `std::str::FromStr`
/// [`Matrix`]: `crate::matrix::Matrix`
#[derive(Debug, PartialEq, Clone, Eq)]
pub enum MatrixParseError<E> {
    /// The string within the opening braces is empty, or the input string is empty.
    /// ```
    /// use std::str::FromStr as _;
    /// assert_eq!(Matrix::<u8>::from_str(""), Err(MatrixParseError::EmptyMatrix));
    /// assert_eq!(Matrix::<u8>::from_str("[]"), Err(MatrixParseError::EmptyMatrix));
    /// ```
    EmptyMatrix,
    /// The string does not include an opening/closing pair of brackets for the matrix.
    /// ```
    /// use std::str::FromStr as _;
    /// assert_eq!(Matrix::<u8>::from_str("["), Err(MatrixParseError::NonOpenedMatrix));
    /// assert_eq!(Matrix::<u8>::from_str("]"), Err(MatrixParseError::NonOpenedMatrix));
    /// ```
    NonOpenedMatrix,
    ///
    /// ```
    /// use std::str::FromStr as _;
    /// 
    /// ```
    ValueOutsideOfRow,
    ///
    /// ```
    /// use std::str::FromStr as _;
    /// 
    /// ```
    MissingDelimeter,
    /// Occurs when a set is opened with in a row.
    /// Specifically, this means that there is another '[' within a row. 
    /// ```
    /// use std::str::FromStr as _;
    /// assert_eq!(Matrix::<u8>::from_str("[[[1, 2]]]"), Err(MatrixParseError::OpeningSetInSet))
    /// ```
    OpeningSetInSet,
    ///
    /// ```
    /// use std::str::FromStr as _;
    /// 
    /// ```
    ClosingSetWithoutOpeningSet,
    ///
    /// ```
    /// use std::str::FromStr as _;
    /// 
    /// ```
    EmptyValue,
    /// Occurs when the number of columns are inconsisent across rows. 
    /// ```
    /// use std::str::FromStr as _;
    /// assert!(matches!(Matrix::<u8>::from_str("[[1, 1], [2]]"), MatrixParseError::Conv(_)));
    /// 
    /// ```
    Conv(MatrixConversionError),
    /// Occurs when the inner `T` of the matrix cannot be parsed from a value it extracts.
    /// ```
    /// use std::str::FromStr as _;
    /// assert!(matches!(Matrix::<u8>::from_str("[[a]]"), Err( MatrixParseError::Inner(_) ) ));
    /// 
    /// ```
    Inner(E)
}
impl<E> Display for MatrixParseError<E> where E: Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyMatrix => write!(f, "empty matrix string"),
            Self::NonOpenedMatrix => write!(f, "matrix was not surrounded by [] pair"),
            Self::MissingDelimeter => write!(f, "matrix rows are missing a delimiter ',' between them"),
            Self::ValueOutsideOfRow => write!(f, "a value was written outside of a row set"),
            Self::OpeningSetInSet  => write!(f, "a row set was opened before closing the previous set"),
            Self::ClosingSetWithoutOpeningSet => write!(f, "a row set was closed before an opening set was found"),
            Self::EmptyValue => write!(f, "a value in the matrix was empty"),
            Self::Conv(c) => (c as &dyn Display).fmt(f),
            Self::Inner(e) => (e as &dyn Display).fmt(f)
        }
    }
}
impl<E> StdError for MatrixParseError<E> where E: Display + Debug { }