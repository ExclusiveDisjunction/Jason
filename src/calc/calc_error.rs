use std::fmt::{Display, Debug, Formatter};

pub trait DimensionKind : Clone + Copy {}
impl DimensionKind for usize { }
impl DimensionKind for i32 {}
impl DimensionKind for i64 {}
impl DimensionKind for u32 {}
impl DimensionKind for u64 {}

pub enum CalcError<T> where T: DimensionKind {
    Index(IndexOutOfRangeError<T>),
    Dim(DimensionError<T>),
    Oper(OperationError)
}
impl<T> Debug for CalcError<T> where T: DimensionKind + Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Index(a) => (a as &dyn Debug).fmt(f),
            Self::Dim(a) => (a as &dyn Debug).fmt(f),
            Self::Oper(a) => (a as &dyn Debug).fmt(f)
        }
    }
}
impl<T> Display for CalcError<T> where T: DimensionKind + Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Index(a) => (a as &dyn Display).fmt(f),
            Self::Dim(a) => (a as &dyn Display).fmt(f),
            Self::Oper(a) => (a as &dyn Display).fmt(f)
        }
    }
}
impl<T> PartialEq for CalcError<T> where T: DimensionKind + PartialEq {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Index(a), Self::Index(b)) => a == b,
            (Self::Dim(a), Self::Dim(b)) => a == b,
            (Self::Oper(a), Self::Oper(b)) => a == b,
            (_, _) => false
        }
    }
}


pub type CalcResult<T, U> = Result<T, CalcError<U>>;

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
impl<T> Debug for DimensionError<T> where T: DimensionKind + Debug{
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
