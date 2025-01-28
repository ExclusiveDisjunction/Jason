use super::variable_type::*;
use super::scalar::{Scalar, ScalarLike};
use std::ops::{Add, Sub, Mul, Div};
use std::fmt::{Display, Debug, Formatter};
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize, Default)]
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
impl Debug for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl VariableData for Complex {
    fn get_type(&self) -> VariableType {
        VariableType::Complex
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

impl Into<Complex> for Scalar {
    fn into(self) -> Complex {
        Complex { a: self.a, b: 0.0 }
    }
}
impl Into<Complex> for f64 {
    fn into(self) -> Complex {
        Complex { a: self, b: 0.0 }
    }
}
impl Into<Complex> for (f64, f64) {
    fn into(self) -> Complex {
        Complex { a: self.0, b: self.1 }
    }
}

// PURE
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
        (&self).mul(&rhs)
    }
}
impl Div for Complex {
    type Output = Complex;
    fn div(self, rhs: Self) -> Self::Output {
        (&self).div(&rhs)
    }
}

// REFERENCE
impl<'a, 'b> Add<&'b Complex> for &'a Complex {
    type Output = Complex;
    fn add(self, rhs: &'b Complex) -> Self::Output {
        Complex { a: self.a + rhs.a, b: self.b + rhs.b }
    }
}
impl<'a, 'b> Sub<&'b Complex> for &'a Complex {
    type Output = Complex;
    fn sub(self, rhs: &'b Complex) -> Self::Output {
        Complex{ a: self.a - rhs.a, b: self.b - rhs.b }
    }
}
impl<'a, 'b> Mul<&'b Complex> for &'a Complex {
    type Output = Complex;
    fn mul(self, rhs: &'b Complex) -> Self::Output {
        Complex::new(
            self.a * rhs.a - self.b * rhs.b,
            self.a * rhs.b + self.b * rhs.a
        )
    }
}
impl<'a, 'b> Div<&'b Complex> for &'a Complex {
    type Output = Complex;
    fn div(self, rhs: &'b Complex) -> Self::Output {
        /*
          (a + bi) / (c + di)
          (a + bi) * (c - di) / (c + di)(c - di)
          lhs * conj / mul_conj
         */

        let starting = self * &rhs.conjugate();
        let fac = rhs.mul_conjugate().as_scalar();

        Complex { a: starting.a / fac, b: starting.b / fac }
    }
}

impl PartialEq for Complex {
    fn eq(&self, other: &Self) -> bool {
        self.a == other.a && self.b == other.b
    }
}
impl Complex {
    pub fn new(a: f64, b: f64) -> Self {
        Self {
            a,
            b
        }
    }
    pub fn conjugate(&self) -> Complex {
        Complex::new(self.a, -self.b)
    }
    pub fn mul_conjugate(&self) -> Scalar {
        Scalar::new(self.a.powi(2) + self.b.powi(2))
    }
}

#[test]
fn complex_test() {
    let a = Complex::new(1.0, 0.0);
    let b = Complex::new(3.0, 1.4);
    let c: Complex = Scalar::new(3.0).into();
    //let d = Complex::new_with(2.4, 3.1);

    assert_eq!(a.clone() + b.clone(), Complex::new(4.0, 1.4));
    assert_eq!(a.clone() - b.clone(), Complex::new(-2.0, -1.4));
    assert_eq!(a.clone() + c.clone(), Complex::new(4.0, 0.0));
    assert_eq!(a.clone() - c.clone(), Complex::new(-2.0, 0.0));
    assert_eq!(a.clone() * c.clone(), Complex::new(3.0, 0.0));

    //These two are technically correct, but due to floating point errors these may fail.
    //assert_eq!(d.clone() / b.clone(), Complex::new_with(577.0/548.0, 297.0/548.0));
    //assert_eq!(b.clone() * d.clone(), Complex::new_with(2.86, 12.66));

    //Assert that operators with references are equal to non-references
    //Pure
    assert_eq!(&a + &b, a.clone() + b.clone());
    assert_eq!(&a - &b, a.clone() - b.clone());
    assert_eq!(&a * &b, a.clone() * b.clone());
    assert_eq!(&a / &b, a.clone() / b.clone());
}