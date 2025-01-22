use std::fmt::{Display, Debug, Formatter};

pub trait DimensionKind : Display + Debug  {}
impl DimensionKind for usize { }
impl DimensionKind for i32 {}
impl DimensionKind for i64 {}
impl DimensionKind for u32 {}
impl DimensionKind for u64 {}

pub enum CalcError<T> where T: DimensionKind {
    index(IndexOutOfRangeError<T>),
    dimension(DimensionError<T>),
    operation(OperationError)
}

pub type CalcResult<T, U> where U: DimensionKind = Result<T, CalcError<U>>;

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
impl<T> Display for IndexOutOfRangeError<T> where T: DimensionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "the index {} is out of range", self.index)
    }
}
impl<T> Debug for IndexOutOfRangeError<T> where T: DimensionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
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
impl<T> Display for DimensionError<T> where T: DimensionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "dimension mismatch between {} and {} (they must be equal)", self.a, self.b)
    }
}
impl<T> Debug for DimensionError<T> where T: DimensionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}

pub struct OperationError {
    operation: String,
    on: (String, String),
    reason: Option<String>
}
impl OperationError {
    pub fn new(operation: String, a: String, b: String, reason: Option<String>) -> Self {
        Self {
            operation,
            on: (a, b),
            reason
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
