use std::fmt::Display;
use std::ops::{Add, Sub, Mul, Div, Neg};

use serde::{Deserialize, Serialize};

use crate::calc::Complex;

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize, Debug)]
pub enum Numeric {
    Integer(i64),
    Real(f64),
    Complex(Complex)
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

            (Self::Complex(a), Self::Integer(b) ) => Self::Complex( a.pow_sca(b) ),
            (Self::Complex(a), Self::Real(b) ) => Self::Complex( a.pow_sca(b) ),
            (Self::Complex(a), Self::Complex(b) ) => Self::Complex( a.pow(b) )
        }
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