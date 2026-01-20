use crate::calc::num::{NullIdentity, UnitIdentity};

use std::ops::{Add, Sub, Mul, Div, Neg};
use std::fmt::{Display, Debug, Formatter};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Debug, Serialize, Deserialize, Default)]
pub struct Complex {
    a: f64,
    b: f64
}
impl Display for Complex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.b < 0.0 {
            write!(f, "{} - {}i", self.a, -self.b)
        } else {
            write!(f, "{} + {}i", self.a, self.b)
        }
    }
}

/*

    Complex Operators:

    A, B -> Complex
    A + B (comm)
    A - B
    A * B (comm)
    A / B

    The approach includes converting scalars to complex numbers, saving the number of operators.
 */

impl From<f64> for Complex where {
    fn from(value: f64) -> Self {
        Self { a: value, b: 0.0 }
    }
}
impl From<f32> for Complex where {
    fn from(value: f32) -> Self {
        Self { a: value as f64, b: 0.0 }
    }
}
impl From<i64> for Complex {
    fn from(value: i64) -> Self {
        Self { a: value as f64, b: 0.0 }
    }
}

// PURE
impl Neg for Complex {
    type Output = Complex;
    fn neg(self) -> Complex {
        Complex { a: -self.a, b: -self.b }
    }
}
impl Add for Complex {
    type Output = Complex;
    fn add(self, rhs: Self) -> Self::Output {
        Complex { a: self.a + rhs.a, b: self.b + rhs.b }
    }
}
impl Sub for Complex {
    type Output = Complex;
    fn sub(self, rhs: Self) -> Self::Output {
        Complex { a: self.a - rhs.a, b: self.b - rhs.b }
    }
}
impl Mul for Complex {
    type Output = Complex;
    fn mul(self, rhs: Self) -> Self::Output {
        /*
            (a+bi) * (c + di)
            (ac + adi + bci + bdi^2)
            (ac - bd) + (ad + bc)i
         */

        Complex::new(
            self.a * rhs.a - self.b * rhs.b,
            self.a * rhs.b + self.b * rhs.a
        )
    }
}
impl Div for Complex {
    type Output = Complex;
    fn div(self, rhs: Self) -> Self::Output {
        /*
          (a + bi) / (c + di)
          (a + bi) * (c - di) / (c + di)(c - di)
          lhs * conj / mul_conj
         */

        let starting = self * rhs.conjugate();
        let fac = rhs.mul_conjugate();

        Complex { a: starting.a / fac, b: starting.b / fac }
    }
}

impl NullIdentity for Complex {
    fn null_id() -> Self {
        Self { a: 0.0, b: 0.0 }
    }
}
impl UnitIdentity for Complex {
    fn unit_id() -> Self {
        Self { a: 1.0, b: 0.0 }
    }
}

impl Complex {
    pub fn new(a: f64, b: f64) -> Self {
        Self {
            a,
            b
        }
    }

    pub fn conjugate(self) -> Complex {
        Complex::new(self.a, -self.b)
    }
    pub fn mul_conjugate(self) -> f64 {
        self.a.powi(2) + self.b.powi(2)
    }
    pub fn argument(&self) -> f64 {
        self.b.atan2(self.a)
    }
    pub fn magnitude(&self) -> f64 {
        (self.a.powi(2) + self.b.powi(2)).sqrt()
    }

    pub fn complex_ln(self) -> Self {
        Self {
            a: self.magnitude().ln(),
            b: self.argument()
        }
    }

    pub fn pow_sca<T>(self, b: T) -> Self where T: Into<f64> {
        let b = b.into();
        let r = self.magnitude().powf(b);
        let theta = self.argument() * b;
        
        let x = (theta).cos() * r;
        let y = (theta).sin() * r;

        Self {
            a: x,
            b: y
        }
    }
    /// Returns the priciple value for the complex number 
    pub fn pow(self, b: Self) -> Self {
        let top = b * self.complex_ln();

        //This will take the product of e^(z ln b)
        let a = top.a;
        let b = top.b;

        let e = std::f64::consts::E.powf(a);

        Self {
            a: e * b.cos(),
            b: e * b.sin()
        }
    }
}

#[test]
fn complex_test() {
    let a = Complex::new(1.0, 0.0);
    let b = Complex::new(3.0, 1.4);
    let c: Complex = 3.0.into();
    //let d = Complex::new_with(2.4, 3.1);

    assert_eq!(a + b, Complex::new(4.0, 1.4));
    assert_eq!(a - b, Complex::new(-2.0, -1.4));
    assert_eq!(a + c, Complex::new(4.0, 0.0));
    assert_eq!(a - c, Complex::new(-2.0, 0.0));
    assert_eq!(a * c, Complex::new(3.0, 0.0));
    assert_eq!(a * b, b);
    assert_eq!(b * a, b);

    assert_eq!(Complex::new(1.0, 2.5) - Complex::from(1.0), Complex::new(0.0, 2.5));

    //These two are technically correct, but due to floating point errors these may fail.
    //assert_eq!(d.clone() / b.clone(), Complex::new_with(577.0/548.0, 297.0/548.0));
    //assert_eq!(b.clone() * d.clone(), Complex::new_with(2.86, 12.66));
}