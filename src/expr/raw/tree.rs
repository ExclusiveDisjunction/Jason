use super::ops::BinaryOperator;

use crate::calc::err::DimensionError;
use crate::prelude::{Name, NameRef};
use crate::calc::{Scalar, Complex, Boolean, FloatVector, FloatMatrix, Literal};

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperation<T> {
    x: BinaryOperator,
    lhs: Box<T>,
    rhs: Box<T>
}
impl<T> BinaryOperation<T> {
    pub fn new(x: BinaryOperator, lhs: T, rhs: T) -> Self {
        Self::new_boxed(x, Box::new(lhs), Box::new(rhs))
    }
    pub fn new_boxed(x: BinaryOperator, lhs: Box<T>, rhs: Box<T>) -> Self {
        Self {
            x,
            lhs,
            rhs
        }
    }

    pub fn oper(&self) -> BinaryOperator {
        self.x
    }
    pub fn lhs(&self) -> &T {
        &self.lhs
    }
    pub fn rhs(&self) -> &T {
        &self.rhs
    }
}
/*
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperation<T> {
    x: UnaryOperator,
    lhs: Box<T>
}
impl<T> UnaryOperation<T> {
    pub fn new(x: UnaryOperator, lhs: T) -> Self {
        Self::new_boxed(x, Box::new(lhs))
    }
    pub fn new_boxed(x: UnaryOperator, lhs: Box<T>) -> Self {
        Self {
            x,
            lhs
        }
    }

    pub fn oper(&self) -> UnaryOperator {
        self.x
    }
    pub fn lhs(&self) -> &T {
        &self.lhs
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct FnCallRef<'a, T> {
    name: NameRef<'a>,
    args: Vec<T>
}
impl<'a, T> FnCallRef<'a, T> {
    pub fn new<I>(name: NameRef<'a>, args: I) -> Self where I: IntoIterator<Item = T> {
        Self {
            name,
            args: args.into_iter().collect()
        }
    }
    
    pub fn name(&self) -> NameRef<'a> {
        self.name.clone()
    }
    pub fn args(&self) -> &[T] {
        &self.args
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct FnCall<T> {
    name: Name,
    args: Vec<T>
}
impl<T> FnCall<T> {
    pub fn new<I>(name: Name, args: I) -> Self where I: IntoIterator<Item = T> {
        Self {
            name,
            args: args.into_iter().collect()
        }
    }
    
    pub fn name(&self) -> NameRef<'_> {
        self.name.as_name_ref()
    }
    pub fn args(&self) -> &[T] {
        &self.args
    }
} 
*/

pub enum RawExpression {
    Scalar(f64),                     // 3.4
    Vector(Vec<f64>),                // [3.1, 2.4]
    Matrix(Vec<Vec<f64>>),           // [[1, 0], [0, 1]]
    Complex(f64),                    // 1.3i or 1.3j or j1.2 or i1.2
    Bool(bool),
    BinOper(BinaryOperation<Self>),  // 2 + 4
    Wrapped(Box<Self>),              // (3+4) <- The wrap is the parenthisis 
}

pub enum ExpressionElement {
    Literal(Literal),
    Operator(BinaryOperator),
    LBrace,
    RBrace
}
impl<T> From<T> for ExpressionElement where T: Into<Literal> {
    fn from(value: T) -> Self {
        Self::Literal(value.into())
    }
}