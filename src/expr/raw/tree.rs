use std::fmt::Display;

use super::id::IdentifierRef;
use super::ops::{BinaryOperator, UnaryOperator};

use crate::calc::err::DimensionError;
use crate::prelude::{Name, NameRef};
use crate::calc::{Scalar, Complex, Boolean, FloatVector, FloatMatrix, VariableUnion};

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

pub enum RawExpression<'a> {
    Scalar(f64),                     // 3.4
    Vector(Vec<f64>),                // [3.1, 2.4]
    Matrix(Vec<Vec<f64>>),           // [[1, 0], [0, 1]]
    Complex(f64),                    // 1.3i or 1.3j or j1.2 or i1.2
    Bool(bool),
    BinOper(BinaryOperation<Self>),  // 2 + 4
    UniOper(UnaryOperation<Self>),   // -1, -(1+4), !true, !(1 == 4)
    Wrapped(Box<Self>),              // (3+4) <- The wrap is the parenthisis 
    FnCall(FnCallRef<'a, Self>),
    Identifier(IdentifierRef<'a>)    //_::$x, $func()
}

pub enum VerifiedRawExpression<'a> {
    Literal(VariableUnion),
    BinOper(BinaryOperation<Self>),
    UniOper(UnaryOperation<Self>),
    Wrapped(Box<Self>),
    FnCall(FnCallRef<'a, Self>),
    Identifier(IdentifierRef<'a>)
}
impl<'a> TryFrom<RawExpression<'a>> for VerifiedRawExpression<'a> {
    type Error = DimensionError<usize>;
    fn try_from(value: RawExpression<'a>) -> Result<Self, Self::Error> {
        match value {
            RawExpression::Scalar(x) => Ok( Self::Literal(Scalar::new(x).into()) ),
            RawExpression::Complex(x) => Ok( Self::Literal(Complex::new(0.0, x).into()) ),
            RawExpression::Bool(x) => Ok( Self::Literal(Boolean::from(x).into()) ),
            RawExpression::Vector(x) => Ok( Self::Literal(FloatVector::from(x).into()) ),
            RawExpression::Matrix(x) => Ok( Self::Literal(FloatMatrix::try_from(x)?.into()) )
        }
    }
}

pub enum SemiResolvedExpression {
    Literal(VariableUnion),
    Wrapped(Box<SemiResolvedExpression>),
    Identifier(Name),
    Binary {
        x: BinaryOperator,
        lhs: Box<SemiResolvedExpression>,
        rhs: Box<SemiResolvedExpression>
    },
    Unary {
        x: UnaryOperator,
        expr: Box<SemiResolvedExpression>
    },
    FnCall {
        name: Name,
        args: Vec<SemiResolvedExpression>
    }
}

/*

pub enum OwnedRawExpression {
    Scalar(f64),                     // 3.4
    Vector(Vec<f64>),                // [3.1, 2.4]
    Matrix(Vec<Vec<f64>>),           // [[1, 0], [0, 1]]
    Complex(f64),                    // 1.3i or 1.3j or j1.2 or i1.2
    BinOperation {                   // 2 + 4
        x: BinaryOperator, 
        lhs: Box<OwnedRawExpression>,
        rhs: Box<OwnedRawExpression>
    },
    UniOperation {                   // -1, -(1+4), !true, !(1 == 4)
        x: UnaryOperator,
        expr: Box<OwnedRawExpression>
    },
    Wrapped(Box<OwnedRawExpression>), // (3+4) <- The wrap is the parenthisis 
    FnCall {
        name: Name,
        args: Vec<OwnedRawExpression>
    },
    Identifier(Identifier)       //_::$x, $func()
} */