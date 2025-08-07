use std::fmt::Display;
use std::error::Error;

use crate::calc::calc_error::{DimensionError, OutOfRangeError};
use crate::calc::matrix::mat::MatrixPowError;
use crate::calc::matrix::MatrixDimension;
use crate::prelude::FlatType;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct UndefinedUniOperation(&'static str, FlatType);
impl Display for UndefinedUniOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the operation {} cannot be applied to type {}", self.0, self.1)
    }
}
impl Error for UndefinedUniOperation { }
impl UndefinedUniOperation {
    pub fn new(oper: &'static str, ty: FlatType) -> Self {
        Self(oper, ty)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct UndefinedBiOperation(&'static str, FlatType, FlatType);
impl Display for UndefinedBiOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the operation {} cannot be applied to {} and {}", self.0, self.1, self.2)
    }
}
impl Error for UndefinedBiOperation { }
impl UndefinedBiOperation {
    pub fn new(oper: &'static str, a: FlatType, b: FlatType) -> Self {
        Self(oper, a, b)
    }

    pub fn rename(self, new: &'static str) -> Self {
        Self::new(new, self.1, self.2)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum BiOperationError {
    Undefined(UndefinedBiOperation),
    Dim(DimensionError<usize>),
    MatDim(DimensionError<MatrixDimension>),
    Index(OutOfRangeError<usize>)
}
impl From<UndefinedBiOperation> for BiOperationError {
    fn from(value: UndefinedBiOperation) -> Self {
        Self::Undefined(value)
    }
}
impl From<DimensionError<usize>> for BiOperationError {
    fn from(value: DimensionError<usize>) -> Self {
        Self::Dim(value)
    }
}
impl From<DimensionError<MatrixDimension>> for BiOperationError {
    fn from(value: DimensionError<MatrixDimension>) -> Self {
        Self::MatDim(value)
    }
}
impl From<OutOfRangeError<usize>> for BiOperationError {
    fn from(value: OutOfRangeError<usize>) -> Self {
        Self::Index(value)
    }
}
impl Display for BiOperationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let target: &dyn Display = match self {
            Self::Undefined(u) => u,
            Self::Dim(d) => d,
            Self::MatDim(d) => d,
            Self::Index(i) => i,
        };
        target.fmt(f)
    }
}
impl std::error::Error for BiOperationError { }
impl BiOperationError {
    pub fn new_undef(oper: &'static str, lhs: FlatType, rhs: FlatType) -> Self {
        Self::Undefined(
            UndefinedBiOperation::new(oper, lhs, rhs)
        )
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum PowError {
    Mat(MatrixPowError),
    Undef(UndefinedBiOperation)
}
impl From<MatrixPowError> for PowError {
    fn from(value: MatrixPowError) -> Self {
        Self::Mat(value)
    }
}
impl From<UndefinedBiOperation> for PowError {
    fn from(value: UndefinedBiOperation) -> Self {
        Self::Undef(value)
    }
}
impl Display for PowError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Mat(m) => m,
            Self::Undef(u) => u
        };

        x.fmt(f)
    }
}
impl PowError {
    pub fn new_undef(lhs: FlatType, rhs: FlatType) -> Self {
        Self::Undef(UndefinedBiOperation::new("^", lhs, rhs))
    }
}