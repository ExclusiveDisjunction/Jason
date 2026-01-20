use std::fmt::Display;
use std::ops::{Add, Sub, Mul, Div, Neg};

use serde::{Deserialize, Serialize};

use crate::calc::Complex;
use crate::calc::err::UndefinedBiOperation;
use crate::calc::literal::prelude::LogicalCmp;
use crate::calc::num::{NullIdentity, UnitIdentity};
use crate::prelude::FlatType;

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize, Debug)]
pub enum Numeric {
    Integer(i64),
    Real(f64),
    Complex(Complex)
}
impl Default for Numeric {
    fn default() -> Self {
        Self::Integer(0)
    }
}
impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Integer(i) => i,
            Self::Real(i) => i,
            Self::Complex(i) => i
        };

        x.fmt(f)
    }
}
impl From<i8> for Numeric {
    fn from(value: i8) -> Self {
        Self::Integer( value as i64 ) 
    }
}
impl From<i16> for Numeric {
    fn from(value: i16) -> Self {
        Self::Integer( value as i64 )
    }
}
impl From<i32> for Numeric {
    fn from(value: i32) -> Self {
        Self::Integer( value as i64 )
    }
}
impl From<i64> for Numeric {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}
impl From<f32> for Numeric {
    fn from(value: f32) -> Self {
        Self::Real( value as f64 )
    }
}
impl From<f64> for Numeric {
    fn from(value: f64) -> Self {
        Self::Real(value)
    }
}
impl From<Complex> for Numeric {
    fn from(value: Complex) -> Self {
        Self::Complex(value)
    }
}
impl From<Numeric> for Complex {
    fn from(value: Numeric) -> Self {
        match value {
            Numeric::Complex(i) => i,
            Numeric::Integer(i) => Complex::new(i as f64, 0.0),
            Numeric::Real(i) => Complex::new(i, 0.0)
        }
    }
}

impl NullIdentity for Numeric {
    #[inline(always)]
    fn null_id() -> Self {
        Self::Integer(0)
    }
}
impl UnitIdentity for Numeric {
    #[inline(always)]
    fn unit_id() -> Self {
        Self::Integer(1)
    }
}

impl LogicalCmp for Numeric {
    fn oper_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok( a == b ),
            (Self::Integer(a), Self::Real(b)) | (Self::Real(b), Self::Integer(a)) => Ok(*a as f64 == *b),
            (Self::Real(a), Self::Real(b)) => Ok( a == b ),
            (a, b) => Err( UndefinedBiOperation::new("==", a.flat_type(), b.flat_type() ) )
        }
    }
    fn oper_greater(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok( a > b ),
            (Self::Integer(a), Self::Real(b)) | (Self::Real(b), Self::Integer(a)) => Ok(*a as f64 > *b),
            (Self::Real(a), Self::Real(b)) => Ok( a > b ),
            (a, b) => Err( UndefinedBiOperation::new(">", a.flat_type(), b.flat_type() ) )
        }
    }
    fn oper_greater_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok( a >= b ),
            (Self::Integer(a), Self::Real(b)) | (Self::Real(b), Self::Integer(a)) => Ok(*a as f64 >= *b),
            (Self::Real(a), Self::Real(b)) => Ok( a >= b ),
            (a, b) => Err( UndefinedBiOperation::new(">=", a.flat_type(), b.flat_type() ) )
        }
    }
    fn oper_less(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok( a < b ),
            (Self::Integer(a), Self::Real(b)) | (Self::Real(b), Self::Integer(a)) => Ok((*a as f64) < *b),
            (Self::Real(a), Self::Real(b)) => Ok( a < b ),
            (a, b) => Err( UndefinedBiOperation::new("<", a.flat_type(), b.flat_type() ) )
        }
    }
    fn oper_less_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok( a <= b ),
            (Self::Integer(a), Self::Real(b)) | (Self::Real(b), Self::Integer(a)) => Ok(*a as f64 <= *b),
            (Self::Real(a), Self::Real(b)) => Ok( a <= b ),
            (a, b) => Err( UndefinedBiOperation::new("<=", a.flat_type(), b.flat_type() ) )
        }
    }
}

impl Numeric {
    /// The largest possible power that an integer can be raised to before it becomes a float.
    const INT_POW_LIMIT: i64 = u32::MAX as i64;

    pub fn pow<R>(self, rhs: R) -> Self where R: Into<Self> {
        let rhs = rhs.into();

        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => {
                if b < 0 || b >= Self::INT_POW_LIMIT{
                    Self::Real( (a as f64).powf(b as f64) )
                }
                else {
                    Self::Integer( a.pow(b as u32) )
                }
            },
            (Self::Integer(a), Self::Real(b)) => Self::Real( (a as f64).powf(b) ),
            (Self::Integer(a), Self::Complex(b)) => Self::Complex( Complex::from(a).pow(b) ),

            (Self::Real(a), Self::Integer(b)) => Self::Real( a.powf(b as f64) ),
            (Self::Real(a), Self::Real(b)) => Self::Real( a.powf(b) ),
            (Self::Real(a), Self::Complex(b) ) => Self::Complex( Complex::from(a).pow(b) ),

            (Self::Complex(a), Self::Integer(b) ) => Self::Complex( a.pow_sca(b as f64) ),
            (Self::Complex(a), Self::Real(b) ) => Self::Complex( a.pow_sca(b) ),
            (Self::Complex(a), Self::Complex(b) ) => Self::Complex( a.pow(b) )
        }
    }

    pub fn flat_type(&self) -> FlatType {
        todo!()
    }
}

impl Neg for Numeric {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(a) => Self::Integer(-a),
            Self::Real(a) => Self::Real(-a),
            Self::Complex(a) => Self::Complex(-a)
        }
    }
}
impl<R> Add<R> for Numeric where R: Into<Numeric> {
    type Output = Self;

    fn add(self, rhs: R) -> Self::Output {
        match (self, rhs.into()) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a + b),
            (Self::Integer(a), Self::Real(b)) => Self::Real( a as f64 + b ),
            (Self::Integer(a), Self::Complex(b)) => Self::Complex( Complex::from(a) + b ),

            (Self::Real(a), Self::Integer(b)) => Self::Real( a + b as f64 ),
            (Self::Real(a), Self::Real(b)) => Self::Real( a + b ),
            (Self::Real(a), Self::Complex(b)) => Self::Complex( Complex::from(a) + b ),

            (Self::Complex(a), Self::Integer(b)) => Self::Complex( a + Complex::from(b) ),
            (Self::Complex(a), Self::Real(b) ) => Self::Complex(a + Complex::from(b) ),
            (Self::Complex(a), Self::Complex(b) ) => Self::Complex( a + b )
        }
    }
}
impl<R> Sub<R> for Numeric where R: Into<Numeric> {
    type Output = Self;

    fn sub(self, rhs: R) -> Self::Output {
         match (self, rhs.into()) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a - b),
            (Self::Integer(a), Self::Real(b)) => Self::Real( a as f64 - b ),
            (Self::Integer(a), Self::Complex(b)) => Self::Complex( Complex::from(a) - b ),

            (Self::Real(a), Self::Integer(b)) => Self::Real( a - b as f64 ),
            (Self::Real(a), Self::Real(b)) => Self::Real( a - b ),
            (Self::Real(a), Self::Complex(b)) => Self::Complex( Complex::from(a) - b ),

            (Self::Complex(a), Self::Integer(b)) => Self::Complex( a - Complex::from(b) ),
            (Self::Complex(a), Self::Real(b) ) => Self::Complex(a - Complex::from(b) ),
            (Self::Complex(a), Self::Complex(b) ) => Self::Complex( a - b )
        }
    }
}
impl<R> Mul<R> for Numeric where R: Into<Numeric> {
    type Output = Self;

    fn mul(self, rhs: R) -> Self::Output {
         match (self, rhs.into()) {
            (Self::Integer(a), Self::Integer(b)) => Self::Integer(a * b),
            (Self::Integer(a), Self::Real(b)) => Self::Real( a as f64 * b ),
            (Self::Integer(a), Self::Complex(b)) => Self::Complex( Complex::from(a) * b ),

            (Self::Real(a), Self::Integer(b)) => Self::Real( a * b as f64 ),
            (Self::Real(a), Self::Real(b)) => Self::Real( a * b ),
            (Self::Real(a), Self::Complex(b)) => Self::Complex( Complex::from(a) * b ),

            (Self::Complex(a), Self::Integer(b)) => Self::Complex( a * Complex::from(b) ),
            (Self::Complex(a), Self::Real(b) ) => Self::Complex(a * Complex::from(b) ),
            (Self::Complex(a), Self::Complex(b) ) => Self::Complex( a * b )
        }
    }
}
impl<R> Div<R> for Numeric where R: Into<Numeric> {
    type Output = Self;

    fn div(self, rhs: R) -> Self::Output {
         match (self, rhs.into()) {
            (Self::Integer(a), Self::Integer(b)) => Self::Real(a as f64 / b as f64),
            (Self::Integer(a), Self::Real(b)) => Self::Real( a as f64 / b ),
            (Self::Integer(a), Self::Complex(b)) => Self::Complex( Complex::from(a) / b ),

            (Self::Real(a), Self::Integer(b)) => Self::Real( a / b as f64 ),
            (Self::Real(a), Self::Real(b)) => Self::Real( a / b ),
            (Self::Real(a), Self::Complex(b)) => Self::Complex( Complex::from(a) / b ),

            (Self::Complex(a), Self::Integer(b)) => Self::Complex( a / Complex::from(b) ),
            (Self::Complex(a), Self::Real(b) ) => Self::Complex(a / Complex::from(b) ),
            (Self::Complex(a), Self::Complex(b) ) => Self::Complex( a / b )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::super::Complex;

    fn test_values<const N: usize>(all_values: [(usize, usize, Numeric); N], lhs: &[Numeric], rhs: &[Numeric], op: fn(Numeric, Numeric) -> Numeric, name: &str) {
        for (lhs_index, rhs_index, expected) in all_values {
            let lhs_value = lhs[lhs_index];
            let rhs_value =  rhs[rhs_index];
            let value = op(lhs_value, rhs_value);

            assert!(expected == value, "LHS: {lhs_value:?}, RHS: {rhs_value:?}, RESULT: {value:?}, EXPECTED: {expected:?}, on index ({lhs_index}, {rhs_index}), test: {name}");
        }
    }

    #[test]
    fn numeric_ops() {
        let lhs_raw = (
            3.25,
            30i64,
            Complex::new(1.0, 3.0)
        );
        let rhs_raw = (
            1.65,
            10i64,
            Complex::new(2.1, 4.3)
        );

        let lhs: [Numeric; _] = [lhs_raw.0.into(), lhs_raw.1.into(), lhs_raw.2.into()];
        let rhs: [Numeric; _] = [rhs_raw.0.into(), rhs_raw.1.into(), rhs_raw.2.into()];

        test_values(
            [
                (0, 0, (lhs_raw.0 + rhs_raw.0).into()),
                (0, 1, (lhs_raw.0 + rhs_raw.1 as f64).into()),
                (0, 2, (Complex::from(lhs_raw.0) + rhs_raw.2).into()),

                (1, 0, (lhs_raw.1 as f64 + rhs_raw.0).into()),
                (1, 1, (lhs_raw.1 + rhs_raw.1).into()),
                (1, 2, (Complex::from(lhs_raw.1) + rhs_raw.2).into()),

                (2, 0, (lhs_raw.2 + Complex::from(rhs_raw.0)).into()),
                (2, 1, (lhs_raw.2 + Complex::from(rhs_raw.1)).into()),
                (2, 2, (lhs_raw.2 + rhs_raw.2).into()),
            ],
            &lhs,
            &rhs,
            Numeric::add,
            "add"
        );

        test_values(
            [
                (0, 0, (lhs_raw.0 - rhs_raw.0).into()),
                (0, 1, (lhs_raw.0 - rhs_raw.1 as f64).into()),
                (0, 2, (Complex::from(lhs_raw.0) - rhs_raw.2).into()),

                (1, 0, (lhs_raw.1 as f64 - rhs_raw.0).into()),
                (1, 1, (lhs_raw.1 - rhs_raw.1).into()),
                (1, 2, (Complex::from(lhs_raw.1) - rhs_raw.2).into()),

                (2, 0, (lhs_raw.2 - Complex::from(rhs_raw.0)).into()),
                (2, 1, (lhs_raw.2 - Complex::from(rhs_raw.1)).into()),
                (2, 2, (lhs_raw.2 - rhs_raw.2).into()),
            ],
            &lhs,
            &rhs,
            Numeric::sub,
            "sub"
        );

        test_values(
            [
                (0, 0, (lhs_raw.0 * rhs_raw.0).into()),
                (0, 1, (lhs_raw.0 * rhs_raw.1 as f64).into()),
                (0, 2, (Complex::from(lhs_raw.0) * rhs_raw.2).into()),

                (1, 0, (lhs_raw.1 as f64 * rhs_raw.0).into()),
                (1, 1, (lhs_raw.1 * rhs_raw.1).into()),
                (1, 2, (Complex::from(lhs_raw.1) * rhs_raw.2).into()),

                (2, 0, (lhs_raw.2 * Complex::from(rhs_raw.0)).into()),
                (2, 1, (lhs_raw.2 * Complex::from(rhs_raw.1)).into()),
                (2, 2, (lhs_raw.2 * rhs_raw.2).into()),
            ],
            &lhs,
            &rhs,
            Numeric::mul,
            "mul"
        );

        test_values(
            [
                (0, 0, (lhs_raw.0 / rhs_raw.0).into()),
                (0, 1, (lhs_raw.0 / rhs_raw.1 as f64).into()),
                (0, 2, (Complex::from(lhs_raw.0) / rhs_raw.2).into()),

                (1, 0, (lhs_raw.1 as f64 / rhs_raw.0).into()),
                (1, 1, (lhs_raw.1 as f64 / rhs_raw.1 as f64).into()),
                (1, 2, (Complex::from(lhs_raw.1) / rhs_raw.2).into()),

                (2, 0, (lhs_raw.2 / Complex::from(rhs_raw.0)).into()),
                (2, 1, (lhs_raw.2 / Complex::from(rhs_raw.1)).into()),
                (2, 2, (lhs_raw.2 / rhs_raw.2).into()),
            ],
            &lhs,
            &rhs,
            Numeric::div,
            "div"
        );

        test_values(
            [
                (0, 0, (lhs_raw.0.powf(rhs_raw.0)).into()),
                (0, 1, (lhs_raw.0.powf(rhs_raw.1 as f64)).into()),
                (0, 2, (Complex::from(lhs_raw.0).pow(rhs_raw.2)).into()),

                (1, 0, ((lhs_raw.1 as f64).powf(rhs_raw.0)).into()),
                (1, 1, (lhs_raw.1.pow(rhs_raw.1 as u32)).into()),
                (1, 2, (Complex::from(lhs_raw.1).pow(rhs_raw.2)).into()),

                (2, 0, (lhs_raw.2.pow_sca(rhs_raw.0)).into()),
                (2, 1, (lhs_raw.2.pow_sca(rhs_raw.1 as f64)).into()),
                (2, 2, (lhs_raw.2.pow(rhs_raw.2)).into()),
            ],
            &lhs,
            &rhs,
            Numeric::pow,
            "pow"
        );
    }
}