use super::variable_type::*;
use std::ops::{Add, Sub, Mul, Div};
use std::fmt::{Display, Debug, Formatter};
use serde::{Deserialize, Serialize};

pub trait ScalarLike {
    fn as_scalar(&self) -> f64;
}
impl ScalarLike for f64 {
    fn as_scalar(&self) -> f64 {
        *self
    }
}
impl ScalarLike for f32 {
    fn as_scalar(&self) -> f64 {
        *self as f64
    }
}
impl ScalarLike for i32 {
    fn as_scalar(&self) -> f64 {
        *self as f64
    }
}
impl ScalarLike for i64 {
    fn as_scalar(&self) -> f64 {
        *self as f64
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Default, PartialOrd)]
pub struct Scalar {
    a: f64
}
impl ScalarLike for Scalar {
    fn as_scalar(&self) -> f64 {
        self.a
    }
}
impl Display for Scalar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.a)
    }
}
impl Debug for Scalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl VariableData for Scalar {
    fn get_type() -> VariableType {
        VariableType::Scalar
    }
}

impl Scalar {
    pub fn new(a: f64) -> Self {
        Self {
            a
        }
    }
}

impl<T> PartialEq<T> for Scalar where T: ScalarLike {
    fn eq(&self, other: &T) -> bool {
        self.a == other.as_scalar()
    }
}

impl<T> Add<T> for Scalar where T: ScalarLike {
    type Output = Scalar;
    fn add(self, rhs: T) -> Self::Output {
        Scalar { a: self.a + rhs.as_scalar() }
    }
}
impl<T> Sub<T> for Scalar where T: ScalarLike {
    type Output = Scalar;
    fn sub(self, rhs: T) -> Self::Output {
        Scalar { a: self.a - rhs.as_scalar() }
    }
}
impl<T> Mul<T> for Scalar where T: ScalarLike {
    type Output = Scalar;
    fn mul(self, rhs: T) -> Self::Output {
        Scalar { a: self.a * rhs.as_scalar() }
    }
}
impl<T> Div<T> for Scalar where T: ScalarLike {
    type Output = Scalar;
    fn div(self, rhs: T) -> Self::Output {
        Scalar { a: self.a / rhs.as_scalar() }
    }
}

#[test]
fn test_scalar() {
    let a = Scalar::new(1.0);
    let b = Scalar::new(2.0);
    let c = Scalar::new(0.0);

    assert_eq!(a + b, 3.0);
    assert_eq!(a - b, -1.0);
    assert_eq!(a * b, 2.0);
    assert_eq!(a * c, 0.0);
    assert_eq!(a / c, f64::INFINITY)
}