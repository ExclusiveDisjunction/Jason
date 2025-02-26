use std::fmt::{Display, Debug, Formatter};

use super::{matrix::MatrixDimension, VariableType};

pub trait DimensionKind : Clone + Copy + PartialEq + Eq {}
impl DimensionKind for usize { }
impl DimensionKind for i32 {}
impl DimensionKind for i64 {}
impl DimensionKind for u32 {}
impl DimensionKind for u64 {}

#[derive(Clone, Eq)]
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

#[derive(Clone)]
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
impl<T> Eq for DimensionError<T> where T: DimensionKind + PartialEq + Eq { }
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

#[derive(PartialEq, Eq, Clone)]
pub struct OperationError {
    operation: String,
    on: (String, String),
    reason: Option<String>
}
impl OperationError {
    pub fn new<S>(operation: S, a: String, b: String, reason: Option<&str>) -> Self where S: ToString {
        Self {
            operation: operation.to_string(),
            on: (a, b),
            reason: reason.map(|r| r.to_string())
        }
    }
    pub fn new_fmt<S, T1, T2>(operation: S, a: &T1, b: &T2, reason: Option<&str>) -> Self where S: ToString, T1: Debug, T2: Debug {
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

#[derive(Clone, PartialEq, Eq)]
pub struct UndefinedError {
    reason: String
}
impl Display for UndefinedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "the operation is undefined because of '{}'", &self.reason)
    }
}
impl Debug for UndefinedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl UndefinedError {
    pub fn new<T>(reason: T) -> Self where T: ToString{
        Self {
            reason: reason.to_string()
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FeatureErrKind {
    /// Will be released later
    Future,
    /// No proper decision has been made
    NotPlanned,
    /// This feature will not be made
    Never
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FeatureReason {
    Complexity,
    Planning,
    Deadline
}
impl Display for FeatureReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Complexity => "computational complexity",
                Self::Planning => "need for planning",
                Self::Deadline => "deadline constraint(s)"
            }
        )
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct FeatureError {
    kind: FeatureErrKind,
    name: String,
    reason: FeatureReason
}
impl Display for FeatureError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f, 
            "the feature '{}' is not availiable, due to {}. This feature {}",
            &self.name,
            &self.reason,
            match self.kind {
                FeatureErrKind::Future => "will be released later.",
                FeatureErrKind::NotPlanned => "may or may not be released later.",
                FeatureErrKind::Never => "will not be released."
            })
    }
}
impl FeatureError {
    pub fn new<T>(name: T, reason: FeatureReason, kind: FeatureErrKind) -> Self where T: ToString {
        Self {
            name: name.to_string(),
            kind,
            reason
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum CalcError {
    Index(IndexOutOfRangeError<usize>),
    MatDim(DimensionError<MatrixDimension>),
    Dim(DimensionError<usize>),
    Oper(OperationError),
    ArgCount(ArgCountError),
    ArgType(ArgTypeError),
    Undefined(UndefinedError),
    Feature(FeatureError)
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
impl From<UndefinedError> for CalcError {
    fn from(value: UndefinedError) -> Self {
        Self::Undefined(value)
    }
}
impl From<FeatureError> for CalcError {
    fn from(value: FeatureError) -> Self {
        Self::Feature(value)
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
            Self::ArgType(a) => a as &dyn Display,
            Self::Undefined(a) => a as &dyn Display,
            Self::Feature(a) => a as &dyn Display

        }.fmt(f)
    }
}
impl Debug for CalcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}

pub type CalcResult<T> = Result<T, CalcError>;