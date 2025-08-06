use std::fmt::Display;
use std::error::Error;

use crate::expr::raw::types::FlatType;

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
}