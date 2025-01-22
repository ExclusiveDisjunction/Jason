use std::fmt::{Display, Debug, Formatter};

pub trait DimensionKind : Display + Debug + Clone + Copy + PartialEq {}
impl DimensionKind for usize { }
impl DimensionKind for i32 {}
impl DimensionKind for i64 {}
impl DimensionKind for u32 {}
impl DimensionKind for u64 {}

#[derive(Debug)]
pub enum CalcError<T> where T: DimensionKind {
    index(IndexOutOfRangeError<T>),
    dimension(DimensionError<T>),
    operation(OperationError)
}

pub type CalcResult<T, U> = Result<T, CalcError<U>>;

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
    pub fn new(operation: &str, a: String, b: String, reason: Option<&str>) -> Self {
        Self {
            operation: operation.to_string(),
            on: (a, b),
            reason: if let Some(r) = reason {
                Some(r.to_string())
            } else {
                None
            }  
        }
    }
    pub fn new_fmt<T1, T2>(operation: &str, a: &T1, b: &T2, reason: Option<&str>) -> Self where T1: Debug, T2: Debug {
        Self {
            operation: operation.to_string(),
            on: (format!("{:?}", a), format!("{:?}", b)),
            reason: if let Some(r) = reason {
                Some(r.to_string())
            } else {
                None
            }  
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
