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
    fn get_type() -> VariableType {
        VariableType::Complex
    }
}
impl Add for Complex {
    type Output = Complex;
    fn add(self, rhs: Self) -> Self::Output {
        Complex { a: self.a + rhs.a, b: self.b + rhs.b }
    }
}
impl<T> Add<T> for Complex where T: ScalarLike {
    type Output = Complex;

    fn add(self, rhs: T) -> Self::Output {
        Complex { a: self.a + rhs.as_scalar(), b: self.b }
    }
}
impl Add<Complex> for Scalar {
    type Output = Complex;
    fn add(self, rhs: Complex) -> Self::Output {
        rhs + self
    }
}
impl Sub for Complex {
    type Output = Complex;
    fn sub(self, rhs: Self) -> Self::Output {
        Complex { a: self.a - rhs.a, b: self.b - rhs.b }
    }
}
impl<T> Sub<T> for Complex where T: ScalarLike {
    type Output = Complex;

    fn sub(self, rhs: T) -> Self::Output {
        Complex { a: self.a - rhs.as_scalar(), b: self.b }
    }
}
impl Sub<Complex> for Scalar {
    type Output = Complex;
    fn sub(self, rhs: Complex) -> Self::Output {
        rhs - self
    }
}
impl Mul for Complex {
    type Output = Complex;
    fn mul(self, rhs: Self) -> Self::Output {
        Complex::new_with(
            self.a * rhs.a - self.b * rhs.b,
            self.a * rhs.b + self.b * rhs.a
        )
    }
}
impl<T> Mul<T> for Complex where T: ScalarLike {
    type Output = Complex;

    fn mul(self, rhs: T) -> Self::Output {
        let sca = rhs.as_scalar();
        Complex::new_with(
            self.a * sca,
            self.b * sca
        )
    }
}
impl Mul<Complex> for Scalar {
    type Output = Complex;
    fn mul(self, rhs: Complex) -> Self::Output {
        rhs * self
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

        (self * rhs.conjugate()) / rhs.mul_conjugate()
    }
}
impl<T> Div<T> for Complex where T: ScalarLike {
    type Output = Complex;
    fn div(self, rhs: T) -> Self::Output {
        let fac = rhs.as_scalar();
        Self {
            a: self.a / fac,
            b: self.b / fac
        }

    }
}
impl Div<Complex> for Scalar {
    type Output = Complex;
    fn div(self, rhs: Complex) -> Self::Output {
        // a/ b != b / a, and this results in a different output.
        /*
            s -> Scalar, (a + bi) = Z -> Complex, Z* = (a - bi) -> Complex

            s / (a + bi)
            s * (a - bi) / (a + bi) (a - bi)
            (s * Z*) / (ZZ*)
            (s / ZZ*) * Z*
         */

        (self / rhs.mul_conjugate()) * rhs.conjugate()
    }
}

impl PartialEq for Complex {
    fn eq(&self, other: &Self) -> bool {
        self.a == other.a && self.b == other.b
    }
}
impl Complex {
    pub fn new_with(a: f64, b: f64) -> Self {
        Self {
            a,
            b
        }
    }
    pub fn conjugate(&self) -> Complex {
        Complex::new_with(self.a, -self.b)
    }
    pub fn mul_conjugate(&self) -> Scalar {
        Scalar::new(self.a.powi(2) + self.b.powi(2))
    }
}

#[test]
fn complex_test() {
    let a = Complex::new_with(1.0, 0.0);
    let b = Complex::new_with(3.0, 1.4);
    let c = Scalar::new(3.0);
    //let d = Complex::new_with(2.4, 3.1);

    assert_eq!(a.clone() + b.clone(), Complex::new_with(4.0, 1.4));
    assert_eq!(a.clone() - b.clone(), Complex::new_with(-2.0, -1.4));
    assert_eq!(a.clone() + c, Complex::new_with(4.0, 0.0));
    assert_eq!(a.clone() - c, Complex::new_with(-2.0, 0.0));
    assert_eq!(a.clone() * c, Complex::new_with(3.0, 0.0));

    //These two are technically correct, but due to floating point errors these may fail.
    //assert_eq!(d.clone() / b.clone(), Complex::new_with(577.0/548.0, 297.0/548.0));
    //assert_eq!(b.clone() * d.clone(), Complex::new_with(2.86, 12.66));
}