use std::fmt::{Display, Debug, Formatter};
use std::error::Error as StdError;

use crate::calc::matrix::{MatrixPowError, MatrixDimension};
use crate::prelude::FlatType;
use super::prelude::DimensionKind;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct OutOfRangeError<T> where T: DimensionKind {
    index: T
}
impl<T> OutOfRangeError<T> where T: DimensionKind {
    pub fn new(index: T) -> Self {
        Self {
            index
        }
    }
}
impl<T> Display for OutOfRangeError<T> where T: DimensionKind + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "the index {} is out of range", self.index)
    }
}
impl<T> StdError for OutOfRangeError<T> where T: DimensionKind + Display + Debug { }

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DimensionError<T> where T: DimensionKind {
    a: T,
    b: T
}
impl<T> DimensionError<T> where T: DimensionKind {
    pub fn new(a: T, b: T) -> Self {
        Self {
            a,
            b
        }
    }
}
impl<T> Display for DimensionError<T> where T: DimensionKind + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "dimension mismatch between {} and {} (they must be equal)", self.a, self.b)
    }
}
impl<T> StdError for DimensionError<T> where T: DimensionKind + Display + Debug { }

/*
These errors will be better adapted to math functions, and will be corrected later on. 
I want the format to be "function _ expected arguments (_, ...), but got (_, ...) instead"
"function _ expected argument 'x' to be of type '_', but got type '_'"

/// Describes a specific amount of arguments, and how many were supplied.
#[derive(Clone, PartialEq, Eq)]
pub struct ArgCountError {
    expected: usize,
    got: usize
}
impl Display for ArgCountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected {} arguments, got {}", self.expected, self.got)
    }
}
impl Debug for ArgCountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl ArgCountError {
    pub fn new(expected: usize, got: usize) -> Self {
        Self {
            expected,
            got
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct ArgTypeError {
    expected: VariableType,
    got: VariableType,
    target: String
}
impl Display for ArgTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected an argument of type {} for '{}', got an argument of {}", &self.expected, &self.target, &self.got)
    }
}
impl Debug for ArgTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl ArgTypeError {
    pub fn new<S>(expected: VariableType, got: VariableType, target: S) -> Self where S: ToString{
        Self {
            expected,
            got,
            target: target.to_string() 
        }
    }
}
 */

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct UndefinedUniOperation(&'static str, FlatType);
impl Display for UndefinedUniOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the operation {} cannot be applied to type {}", self.0, self.1)
    }
}
impl StdError for UndefinedUniOperation { }
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
impl StdError for UndefinedBiOperation { }
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

pub enum OperationError {
    UniUndef(UndefinedUniOperation),
    BiUndef(UndefinedBiOperation),
    Dim(DimensionError<usize>),
    MatDim(DimensionError<MatrixDimension>),
    Index(OutOfRangeError<usize>),
    MatrixPow(MatrixPowError)
}
impl From<UndefinedUniOperation> for OperationError {
    fn from(value: UndefinedUniOperation) -> Self {
        Self::UniUndef(value)
    }
}
impl From<UndefinedBiOperation> for OperationError {
    fn from(value: UndefinedBiOperation) -> Self {
        Self::BiUndef(value)
    }
}
impl From<BiOperationError> for OperationError {
    fn from(value: BiOperationError) -> Self {
        use OperationError::*;
        match value {
            BiOperationError::Undefined(u) => BiUndef(u),
            BiOperationError::Dim(d) => Dim(d),
            BiOperationError::MatDim(d) => MatDim(d),
            BiOperationError::Index(i) => Index(i)
        }
    }
}
impl From<PowError> for OperationError {
    fn from(value: PowError) -> Self {
        match value {
            PowError::Mat(m) => Self::MatrixPow(m),
            PowError::Undef(u) => Self::BiUndef(u)
        }
    }
}
impl From<MatrixPowError> for OperationError {
    fn from(value: MatrixPowError) -> Self {
        Self::MatrixPow(value)
    }
}