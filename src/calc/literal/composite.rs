
use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use serde::{Deserialize, Serialize};

use super::numeric::Numeric;
use crate::calc::err::BiOperationError;
use crate::calc::{MathVector, Matrix};
use crate::prelude::FlatType;

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum Composite {
    Vec(MathVector<Numeric>),
    Mat(Matrix<Numeric>)
}
impl From<MathVector<Numeric>> for Composite {
    fn from(value: MathVector<Numeric>) -> Self {
        Self::Vec(value)
    }
}
impl From<Matrix<Numeric>> for Composite {
    fn from(value: Matrix<Numeric>) -> Self {
        Self::Mat(value)
    }
}
impl Display for Composite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Vec(v) => v,
            Self::Mat(v) => v
        };

        x.fmt(f)
    }
}
impl Composite {
    pub fn flat_type(&self) -> FlatType {
        match self {
            Self::Vec(_) => FlatType::Vector,
            Self::Mat(_) => FlatType::Matrix
        }
    }

    pub fn get_ref<'a>(&'a self) -> CompositeRef<'a> {
        match self {
            Self::Vec(v) => CompositeRef::Vec(v),
            Self::Mat(v) => CompositeRef::Mat(v)
        }
    }
}
impl Neg for Composite {
    type Output = Composite;

    fn neg(self) -> Self::Output {
        match self {
            Self::Vec(v) => Self::Vec(-v),
            Self::Mat(v) => Self::Mat(-v)
        }
    }
}
impl<T> Add<T> for Composite where T: Into<Composite> {
    type Output = Result<Composite, BiOperationError>;

    fn add(self, rhs: T) -> Self::Output {
        match (self, rhs.into()) {
            (Self::Vec(lhs), Self::Vec(rhs)) => Ok( Self::Vec(lhs + rhs) ),
            (Self::Mat(lhs), Self::Mat(rhs)) => (lhs + rhs).map(Self::from).map_err(BiOperationError::from),
            (a, b) => Err( BiOperationError::new_undef(
                "+",
                a.flat_type(),
                b.flat_type()
            ))
        }
    }
}
impl<T> Sub<T> for Composite where T: Into<Composite> {
    type Output = Result<Composite, BiOperationError>;

    fn sub(self, rhs: T) -> Self::Output {
        match (self, rhs.into()) {
            (Self::Vec(lhs), Self::Vec(rhs)) => Ok( Self::Vec(lhs - rhs) ),
            (Self::Mat(lhs), Self::Mat(rhs)) => (lhs - rhs).map(Self::from).map_err(BiOperationError::from),
            (a, b) => Err( BiOperationError::new_undef(
                "-",
                a.flat_type(),
                b.flat_type()
            ))
        }
    }
}
impl<T> Mul<T> for Composite where T: Into<Composite> {
    type Output = Result<Composite, BiOperationError>;

    fn mul(self, rhs: T) -> Self::Output {
        match (self, rhs.into()) {
            (Self::Mat(lhs), Self::Mat(rhs)) => (lhs * rhs).map(Self::from).map_err(BiOperationError::from),
            (a, b) => Err( BiOperationError::new_undef(
                "*",
                a.flat_type(),
                b.flat_type()
            ))
        }
    }
}
impl Mul<Numeric> for Composite {
    type Output = Composite;

    fn mul(self, rhs: Numeric) -> Self::Output {
        match self {
            Self::Vec(v) => Self::Vec( v * rhs ),
            Self::Mat(v) => Self::Mat(v * rhs)
        }
    }
}
impl Div<Numeric> for Composite {
    type Output = Composite;

    fn div(self, rhs: Numeric) -> Self::Output {
        match self {
            Self::Vec(v) => Self::Vec( v / rhs ),
            Self::Mat(v) => Self::Mat(v / rhs)
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug, Serialize)]
pub enum CompositeRef<'a> {
    Vec(&'a MathVector<Numeric>),
    Mat(&'a Matrix<Numeric>)
}
impl CompositeRef<'_> {
    pub fn to_composite(self) -> Composite {
        match self {
            Self::Vec(v) => Composite::Vec(v.clone()),
            Self::Mat(v) => Composite::Mat(v.clone())
        }
    }

    pub fn flat_type(&self) -> FlatType {
        match self {
            Self::Vec(_) => FlatType::Vector,
            Self::Mat(_) => FlatType::Matrix
        }
    }
}
impl Display for CompositeRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Vec(v) => v,
            Self::Mat(v) => v
        };

        x.fmt(f)
    }
}

impl Neg for CompositeRef<'_> {
    type Output = Composite;

    fn neg(self) -> Self::Output {
        match self {
            Self::Vec(v) => Composite::Vec(-v),
            Self::Mat(v) => Composite::Mat(-v)
        }
    }
}
impl Add for CompositeRef<'_> {
    type Output = Result<Composite, BiOperationError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Vec(lhs), Self::Vec(rhs)) => Ok( Composite::Vec(lhs + rhs) ),
            (Self::Mat(lhs), Self::Mat(rhs)) => (lhs + rhs).map(Composite::from).map_err(BiOperationError::from),
            (a, b) => Err( BiOperationError::new_undef(
                "+",
                a.flat_type(),
                b.flat_type()
            ))
        }
    }
}
impl Sub for CompositeRef<'_> {
    type Output = Result<Composite, BiOperationError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Vec(lhs), Self::Vec(rhs)) => Ok( Composite::Vec(lhs - rhs) ),
            (Self::Mat(lhs), Self::Mat(rhs)) => (lhs - rhs).map(Composite::from).map_err(BiOperationError::from),
            (a, b) => Err( BiOperationError::new_undef(
                "-",
                a.flat_type(),
                b.flat_type()
            ))
        }
    }
}
impl Mul for CompositeRef<'_> {
    type Output = Result<Composite, BiOperationError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Mat(lhs), Self::Mat(rhs)) => (lhs * rhs).map(Composite::from).map_err(BiOperationError::from),
            (a, b) => Err( BiOperationError::new_undef(
                "*",
                a.flat_type(),
                b.flat_type()
            ))
        }
    }
}
impl Mul<Numeric> for CompositeRef<'_>  {
    type Output = Composite;

    fn mul(self, rhs: Numeric) -> Self::Output {
        match self {
            Self::Vec(v) => Composite::Vec( v * rhs ),
            Self::Mat(v) => Composite::Mat(v * rhs)
        }
    }
}
impl Div<Numeric> for CompositeRef<'_>  {
    type Output = Composite;

    fn div(self, rhs: Numeric) -> Self::Output {
        match self {
            Self::Vec(v) => Composite::Vec( v / rhs ),
            Self::Mat(v) => Composite::Mat(v / rhs)
        }
    }
}

