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
            (Self::Complex(a), Self::Integer(b)) | ( Self::Integer(b), Self::Complex(a)) => Ok( *a == Complex::from(*b) ),
            (Self::Complex(a), Self::Real(b)) | (Self::Real(b), Self::Complex(a)) => Ok( *a == Complex::from(*b) ),
            (Self::Complex(a), Self::Complex(b)) => Ok( a == b )
        }
    }
    fn oper_greater(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok( a > b ),
            (Self::Integer(a), Self::Real(b)) => Ok(*a as f64 > *b),
            (Self::Real(a), Self::Integer(b)) => Ok(*a > *b as f64),
            (Self::Real(a), Self::Real(b)) => Ok( a > b ),
            (a, b) => Err( UndefinedBiOperation::new(">", a.flat_type(), b.flat_type() ) )
        }
    }
    fn oper_less(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
             (Self::Integer(a), Self::Integer(b)) => Ok( a < b ),
            (Self::Integer(a), Self::Real(b)) => Ok((*a as f64) < *b),
            (Self::Real(a), Self::Integer(b)) => Ok(*a < *b as f64),
            (Self::Real(a), Self::Real(b)) => Ok( a < b ),
            (a, b) => Err( UndefinedBiOperation::new("<", a.flat_type(), b.flat_type() ) )
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

    #[inline(always)]
    pub fn flat_type(&self) -> FlatType {
        FlatType::Scalar
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

    fn test_values_ops<const N: usize>(all_values: [(usize, usize, Numeric); N], op: fn(Numeric, Numeric) -> Numeric, name: &str) {
        for (lhs_index, rhs_index, expected) in all_values {
            let lhs_value = LHS[lhs_index];
            let rhs_value =  RHS[rhs_index];
            let value = op(lhs_value, rhs_value);

            assert!(expected == value, "LHS: {lhs_value:?}, RHS: {rhs_value:?}, RESULT: {value:?}, EXPECTED: {expected:?}, on index ({lhs_index}, {rhs_index}), test: {name}");
        }
    }
    fn test_values_cmp<T>(all_values: T, op: fn(&Numeric, &Numeric) -> Result<bool, UndefinedBiOperation>, name: &str) where T: IntoIterator<Item = (usize, usize, Option<bool>)> {
        for (lhs_index, rhs_index, expected) in all_values {
            let lhs_value = &LHS[lhs_index];
            let rhs_value = &RHS[rhs_index];
            let value = op(lhs_value, rhs_value).ok();

            assert_eq!(expected, value, "LHS: {lhs_value:?}, RHS: {rhs_value:?}, RESULT: {value:?}, EXPECTED: {expected:?}, on index ({lhs_index}, {rhs_index}), test: {name}");
        }
    }

    const LHS_RAW: (f64, i64, Complex) = (
        3.25,
        30i64,
        Complex::new(1.0, 3.0)
    );
    const RHS_RAW: (f64, i64, Complex) = (
        1.65,
        10i64,
        Complex::new(2.1, 4.3)
    );
    const LHS: [Numeric; 3] = [
        Numeric::Real(3.25),
        Numeric::Integer(30),
        Numeric::Complex(Complex::new(1.0, 3.0))
    ];
    const RHS: [Numeric; 3] = [
        Numeric::Real(1.65),
        Numeric::Integer(10),
        Numeric::Complex(Complex::new(2.1, 4.3))
    ];

    #[test]
    fn numeric_ops() {
        test_values_ops(
            [
                (0, 0, (LHS_RAW.0 + RHS_RAW.0).into()),
                (0, 1, (LHS_RAW.0 + RHS_RAW.1 as f64).into()),
                (0, 2, (Complex::from(LHS_RAW.0) + RHS_RAW.2).into()),

                (1, 0, (LHS_RAW.1 as f64 + RHS_RAW.0).into()),
                (1, 1, (LHS_RAW.1 + RHS_RAW.1).into()),
                (1, 2, (Complex::from(LHS_RAW.1) + RHS_RAW.2).into()),

                (2, 0, (LHS_RAW.2 + Complex::from(RHS_RAW.0)).into()),
                (2, 1, (LHS_RAW.2 + Complex::from(RHS_RAW.1)).into()),
                (2, 2, (LHS_RAW.2 + RHS_RAW.2).into()),
            ],
            Numeric::add,
            "add"
        );

        test_values_ops(
            [
                (0, 0, (LHS_RAW.0 - RHS_RAW.0).into()),
                (0, 1, (LHS_RAW.0 - RHS_RAW.1 as f64).into()),
                (0, 2, (Complex::from(LHS_RAW.0) - RHS_RAW.2).into()),

                (1, 0, (LHS_RAW.1 as f64 - RHS_RAW.0).into()),
                (1, 1, (LHS_RAW.1 - RHS_RAW.1).into()),
                (1, 2, (Complex::from(LHS_RAW.1) - RHS_RAW.2).into()),

                (2, 0, (LHS_RAW.2 - Complex::from(RHS_RAW.0)).into()),
                (2, 1, (LHS_RAW.2 - Complex::from(RHS_RAW.1)).into()),
                (2, 2, (LHS_RAW.2 - RHS_RAW.2).into()),
            ],
            Numeric::sub,
            "sub"
        );

        test_values_ops(
            [
                (0, 0, (LHS_RAW.0 * RHS_RAW.0).into()),
                (0, 1, (LHS_RAW.0 * RHS_RAW.1 as f64).into()),
                (0, 2, (Complex::from(LHS_RAW.0) * RHS_RAW.2).into()),

                (1, 0, (LHS_RAW.1 as f64 * RHS_RAW.0).into()),
                (1, 1, (LHS_RAW.1 * RHS_RAW.1).into()),
                (1, 2, (Complex::from(LHS_RAW.1) * RHS_RAW.2).into()),

                (2, 0, (LHS_RAW.2 * Complex::from(RHS_RAW.0)).into()),
                (2, 1, (LHS_RAW.2 * Complex::from(RHS_RAW.1)).into()),
                (2, 2, (LHS_RAW.2 * RHS_RAW.2).into()),
            ],
            Numeric::mul,
            "mul"
        );

        test_values_ops(
            [
                (0, 0, (LHS_RAW.0 / RHS_RAW.0).into()),
                (0, 1, (LHS_RAW.0 / RHS_RAW.1 as f64).into()),
                (0, 2, (Complex::from(LHS_RAW.0) / RHS_RAW.2).into()),

                (1, 0, (LHS_RAW.1 as f64 / RHS_RAW.0).into()),
                (1, 1, (LHS_RAW.1 as f64 / RHS_RAW.1 as f64).into()),
                (1, 2, (Complex::from(LHS_RAW.1) / RHS_RAW.2).into()),

                (2, 0, (LHS_RAW.2 / Complex::from(RHS_RAW.0)).into()),
                (2, 1, (LHS_RAW.2 / Complex::from(RHS_RAW.1)).into()),
                (2, 2, (LHS_RAW.2 / RHS_RAW.2).into()),
            ],
            Numeric::div,
            "div"
        );

        test_values_ops(
            [
                (0, 0, (LHS_RAW.0.powf(RHS_RAW.0)).into()),
                (0, 1, (LHS_RAW.0.powf(RHS_RAW.1 as f64)).into()),
                (0, 2, (Complex::from(LHS_RAW.0).pow(RHS_RAW.2)).into()),

                (1, 0, ((LHS_RAW.1 as f64).powf(RHS_RAW.0)).into()),
                (1, 1, (LHS_RAW.1.pow(RHS_RAW.1 as u32)).into()),
                (1, 2, (Complex::from(LHS_RAW.1).pow(RHS_RAW.2)).into()),

                (2, 0, (LHS_RAW.2.pow_sca(RHS_RAW.0)).into()),
                (2, 1, (LHS_RAW.2.pow_sca(RHS_RAW.1 as f64)).into()),
                (2, 2, (LHS_RAW.2.pow(RHS_RAW.2)).into()),
            ],
            Numeric::pow,
            "pow"
        );
    }

    #[test]
    fn numeric_cmp() {
        test_values_cmp(
            [
                (0, 0, Some(false)),
                (0, 1, Some(false)),
                (0, 2, Some(false)),

                (1, 0, Some(false)),
                (1, 1, Some(false)),
                (1, 2, Some(false)),

                (2, 0, Some(false)),
                (2, 1, Some(false)),
                (2, 2, Some(false)),
            ],
            Numeric::oper_eq,
            "=="
        );

        test_values_cmp(
            [
                (0, 0, Some(true)),
                (0, 1, Some(false)),
                (0, 2, None),

                (1, 0, Some(true)),
                (1, 1, Some(true)),
                (1, 2, None),

                (2, 0, None),
                (2, 1, None),
                (2, 2, None),
            ],
            Numeric::oper_greater,
            ">"
        );

        test_values_cmp(
            [
                (0, 0, Some(true)),
                (0, 1, Some(false)),
                (0, 2, None),

                (1, 0, Some(true)),
                (1, 1, Some(true)),
                (1, 2, None),

                (2, 0, None),
                (2, 1, None),
                (2, 2, None),
            ],
            Numeric::oper_greater_eq,
            ">="
        );

        test_values_cmp(
            [
                (0, 0, Some(false)),
                (0, 1, Some(true)),
                (0, 2, None),

                (1, 0, Some(false)),
                (1, 1, Some(false)),
                (1, 2, None),

                (2, 0, None),
                (2, 1, None),
                (2, 2, None),
            ],
            Numeric::oper_less,
            "<"
        );

        test_values_cmp(
            [
                (0, 0, Some(false)),
                (0, 1, Some(true)),
                (0, 2, None),

                (1, 0, Some(false)),
                (1, 1, Some(false)),
                (1, 2, None),

                (2, 0, None),
                (2, 1, None),
                (2, 2, None),
            ],
            Numeric::oper_less_eq,
            "<="
        );
    }
}