use std::fmt::{Display, Debug, Formatter};

use super::{matrix::MatrixDimension, VariableType};

pub trait DimensionKind : Clone + Copy {}
impl DimensionKind for usize { }
impl DimensionKind for i32 {}
impl DimensionKind for i64 {}
impl DimensionKind for u32 {}
impl DimensionKind for u64 {}

#[derive(Clone)]
pub struct IndexOutOfRangeError<T> where T: DimensionKind {
    index: T
}
impl<T> IndexOutOfRangeError<T> where T: DimensionKind {
    pub fn new(index: T) -> Self {
        Self {
            index
        }
    }
}
impl<T> PartialEq for IndexOutOfRangeError<T> where T: DimensionKind + PartialEq {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl<T> Display for IndexOutOfRangeError<T> where T: DimensionKind + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "the index {} is out of range", self.index)
    }
}
impl<T> Debug for IndexOutOfRangeError<T> where T: DimensionKind + Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "the index {:?} is out of range", self.index)
    }
}

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
impl<T> PartialEq for DimensionError<T> where T: DimensionKind + PartialEq {
    fn eq(&self, other: &Self) -> bool {
        self.a == other.a && self.b == other.b
    }
}
impl<T> Display for DimensionError<T> where T: DimensionKind + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "dimension mismatch between {} and {} (they must be equal)", self.a, self.b)
    }
}
impl<T> Debug for DimensionError<T> where T: DimensionKind + Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "dimension mismatch between {:?} and {:?} (they must be equal)", self.a, self.b)
    }
}

#[derive(PartialEq)]
pub struct OperationError {
    operation: String,
    on: (String, String),
    reason: Option<String>
}
impl OperationError {
    pub fn new(operation: &str, a: String, b: String, reason: Option<&str>) -> Self {
        Self {
            operation: operation.to_string(),
            on: (a, b),
            reason: reason.map(|r| r.to_string())
        }
    }
    pub fn new_fmt<T1, T2>(operation: &str, a: &T1, b: &T2, reason: Option<&str>) -> Self where T1: Debug, T2: Debug {
        Self {
            operation: operation.to_string(),
            on: (format!("{:?}", a), format!("{:?}", b)),
            reason: reason.map(|r| r.to_string())
        }
    }
}
impl Display for OperationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(reason) = self.reason.as_deref() {
            write!(f, "the operation {} is not defined for {} and {} because of '{}'", &self.operation, &self.on.0, &self.on.1, reason)
        }
        else {
            write!(f, "the operation {} is not defined for {} and {}", &self.operation, &self.on.0, &self.on.1)
        }
    }
}
impl Debug for OperationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}

/// Describes a specific amount of arguments, and how many were supplied.
#[derive(Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
pub struct ArgTypeError {
    expected: VariableType,
    got: VariableType
}
impl Display for ArgTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "expected an argument of type {}, got an argument of {}", &self.expected, &self.got)
    }
}
impl Debug for ArgTypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl ArgTypeError {
    pub fn new(expected: VariableType, got: VariableType) -> Self{
        Self {
            expected,
            got
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum CalcError {
    Index(IndexOutOfRangeError<usize>),
    MatDim(DimensionError<MatrixDimension>),
    Dim(DimensionError<usize>),
    Oper(OperationError),
    ArgCount(ArgCountError),
    ArgType(ArgTypeError)
}

impl From<IndexOutOfRangeError<usize>> for CalcError {
    fn from(value: IndexOutOfRangeError<usize>) -> Self {
        Self::Index(value)
    }
}
impl From<DimensionError<MatrixDimension>> for CalcError {
    fn from(value: DimensionError<MatrixDimension>) -> Self {
        Self::MatDim(value)
    }
}
impl From<DimensionError<usize>> for CalcError {
    fn from(value: DimensionError<usize>) -> Self {
        Self::Dim(value)
    }
}
impl From<OperationError> for CalcError {
    fn from(value: OperationError) -> Self {
        Self::Oper(value)
    }
}
impl From<ArgCountError> for CalcError {
    fn from(value: ArgCountError) -> Self {
        Self::ArgCount(value)
    }
}
impl From<ArgTypeError> for CalcError {
    fn from(value: ArgTypeError) -> Self {
        Self::ArgType(value)
    }
}

impl Display for CalcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Index(a) => a as &dyn Display,
            Self::Dim(a) => a as &dyn Display,
            Self::MatDim(a) => a as &dyn Display,
            Self::Oper(a) => a as &dyn Display,
            Self::ArgCount(a) => a as &dyn Display,
            Self::ArgType(a) => a as &dyn Display
        }.fmt(f)
    }
}

pub type CalcResult<T> = Result<T, CalcError>;