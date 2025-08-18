use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use super::core::LogicalCmp;
use crate::calc::err::{BiOperationError, PowError, UndefinedBiOperation, UndefinedUniOperation};
use crate::calc::ScalarLike;
use crate::prelude::FlatType;
use super::lit::Literal;
use super::super::scalar::Scalar;
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::FloatVector;
use super::super::matrix::FloatMatrix;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralReference<'a> {
    Sca(Scalar),
    Cmp(Complex),
    Bool(Boolean),
    Vec(&'a FloatVector),
    Mat(&'a FloatMatrix)
}

impl From<Scalar> for LiteralReference<'_> {
    fn from(value: Scalar) -> Self {
        Self::Sca(value)
    }
}
impl From<Complex> for LiteralReference<'_> {
    fn from(value: Complex) -> Self {
        Self::Cmp(value)
    }
}
impl From<Boolean> for LiteralReference<'_> {
    fn from(value: Boolean) -> Self {
        Self::Bool(value)
    }
}
impl From<bool> for LiteralReference<'_> {
    fn from(value: bool) -> Self {
        Self::Bool(value.into())
    }
}
impl<'a> From<&'a FloatVector> for LiteralReference<'a> {
    fn from(value: &'a FloatVector) -> Self {
        Self::Vec(value)
    }
}
impl<'a> From<&'a FloatMatrix> for LiteralReference<'a> {
    fn from(value: &'a FloatMatrix) -> Self {
        Self::Mat(value)
    }
}

impl Display for LiteralReference<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Sca(s) => s,
            Self::Cmp(s) => s,
            Self::Vec(s) => s,
            Self::Mat(s) => s,
            Self::Bool(s) => s
        };

        x.fmt(f)
    }
}

impl Neg for LiteralReference<'_> {
    type Output = Result<Literal, UndefinedUniOperation>;
    fn neg(self) -> Self::Output {
        match self {
            Self::Sca(s) => Ok( Literal::Sca(-s) ),
            Self::Cmp(s) => Ok( Literal::Cmp(-s) ),
            Self::Bool(_) => Err( UndefinedUniOperation::new("-", FlatType::Boolean)),
            Self::Vec(s) => Ok( Literal::Vec(-s) ),
            Self::Mat(s) => Ok( Literal::Mat(-s) )
        }
    }
}
impl Add for LiteralReference<'_> {
    type Output = Result<Literal, BiOperationError>;
    fn add(self, rhs: Self) -> Self::Output {
        /* Add is defined for all types upon themselves, and the following ones:
            Sca + Sca
            (Sca as Cmp) + Cmp
            Cmp + (Sca as Cmp)
            Cmp + Cmp
            Vec + Vec
            Mat + Mat
         */

        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok(Literal::Sca(a + b)),
            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok( (a + b).into() )
            },
            (Self::Cmp(a), Self::Cmp(b)) => Ok(Literal::Cmp(a + b)),
            (Self::Vec(a), Self::Vec(b)) => Ok(Literal::Vec(a + b)),
            (Self::Mat(a), Self::Mat(b)) => (a + b).map(Literal::from).map_err(BiOperationError::from),
            (a, b) => Err(BiOperationError::new_undef("+", a.flat_type(), b.flat_type()))
        }
    }
}
impl Sub for LiteralReference<'_> {
    type Output = Result<Literal, BiOperationError>;
    fn sub(self, rhs: Self) -> Self::Output {
        /* Sub is defined for all types upon themselves, and the following ones:
            Sca - Sca
            (Sca as Cmp) - Cmp
            Cmp - (Sca as Cmp)
            Cmp + Cmp
            Vec + Vec
            Mat + Mat
         */

        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok(Literal::Sca(a - b)),
            (Self::Sca(a), Self::Cmp(b)) => {
                let a: Complex = a.into();
                Ok(Literal::Cmp(a - b))
            },
            (Self::Cmp(a), Self::Sca(b)) => { //These are different, because order matters
                let b = b.into();
                Ok(Literal::Cmp(a - b))
            },
            (Self::Cmp(a), Self::Cmp(b)) => Ok(Literal::Cmp(a - b)),
            (Self::Vec(a), Self::Vec(b)) => Ok(Literal::Vec(a - b)),
            (Self::Mat(a), Self::Mat(b)) => (a - b).map(Literal::from).map_err(BiOperationError::from),
            (a, b) => Err(BiOperationError::new_undef("-", a.flat_type(), b.flat_type()))
        }
    }
}
impl Mul for LiteralReference<'_> {
    type Output = Result<Literal, BiOperationError>;
    fn mul(self, rhs: Self) -> Self::Output {
        /*
            Mul is the biggest one out here. It is defined over

            Sca * Sca
            Sca * Cmp, Cmp * Sca
            Sca * Vec, Vec * Sca
            Sca * Mat, Mat * Sca
            Cmp * Cmp,
            Mat * Mat
         */

        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok(Literal::Sca(a * b)),
            (Self::Cmp(a), Self::Cmp(b)) => Ok(Literal::Cmp(a * b)),
            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok(Literal::Cmp(a * b))
            },
            (Self::Mat(a), Self::Mat(b)) => (a * b).map(Literal::from).map_err(BiOperationError::from),
            (Self::Vec(a), Self::Sca(b)) | (Self::Sca(b), Self::Vec(a)) => Ok(Literal::Vec(a * b.as_scalar())),
            (Self::Mat(a), Self::Sca(b)) | (Self::Sca(b), Self::Mat(a)) => Ok(Literal::Mat(a * b.as_scalar())),

            (a, b) => Err(BiOperationError::new_undef("*", a.flat_type(), b.flat_type()))
        }
    }
}
impl Div for LiteralReference<'_> {
    type Output = Result<Literal, BiOperationError>;
    fn div(self, rhs: Self) -> Self::Output {
        /*
            Div is simple

            Sca / Sca
            Cmp / Cmp
            Cmp / Sca, Sca / Cmp
            Vec / Sca
            Mat / Sca
         */

        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok(Literal::Sca(a / b)),
            (Self::Cmp(a), Self::Cmp(b)) => Ok(Literal::Cmp(a / b)),
            (Self::Sca(a), Self::Cmp(b)) => {
                let a: Complex = a.into();
                Ok(Literal::Cmp(a / b))
            },
            (Self::Cmp(a), Self::Sca(b)) => { //Order matters!
                let b: Complex = b.into();
                Ok(Literal::Cmp(a / b))
            }
            (Self::Vec(a), Self::Sca(b)) => Ok(Literal::Vec(a / b.as_scalar())),
            (Self::Mat(a), Self::Sca(b)) => Ok(Literal::Mat(a / b.as_scalar())),
            (a, b) => Err(BiOperationError::new_undef("/", a.flat_type(), b.flat_type()))
        }
    }
}

impl LogicalCmp for LiteralReference<'_> {
    fn oper_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok( a == b ),
            (Self::Cmp(a), Self::Cmp(b)) => Ok( a == b ),
            (Self::Vec(a), Self::Vec(b)) => Ok( a == b ),
            (Self::Mat(a), Self::Mat(b)) => Ok( a == b ),
            (Self::Bool(a), Self::Bool(b)) => Ok( a == b ),
            (a, b) => Err( UndefinedBiOperation::new("==", a.flat_type(), b.flat_type()) )
        }
    }
    fn oper_less(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        if let Self::Sca(a) = self && let Self::Sca(b) = rhs {
            Ok( a < b )
        }
        else {
            Err( UndefinedBiOperation::new("<", self.flat_type(), rhs.flat_type()) )
        }
    }
    fn oper_greater(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        if let Self::Sca(a) = self && let Self::Sca(b) = rhs {
            Ok( a > b )
        }
        else {
            Err( UndefinedBiOperation::new(">", self.flat_type(), rhs.flat_type()) )
        }
    }
}

impl LiteralReference<'_> {
    pub fn to_union(self) -> Literal {
        use Literal::*;
        match self {
            Self::Sca(s) => Sca(s),
            Self::Cmp(c) => Cmp(c),
            Self::Bool(b) => Bool(b),
            Self::Vec(v) => Vec(v.clone()),
            Self::Mat(m) => Mat(m.clone())
        }
    }

    pub fn pow(self, rhs: Self) -> Result<Literal, PowError> {
        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok( a.pow(b).into() ),
            (Self::Cmp(a), Self::Sca(b)) => Ok( a.pow_sca(b).into() ),
            (Self::Cmp(a), Self::Cmp(b)) => Ok( a.pow(b).into() ),
            (Self::Mat(a), Self::Sca(b)) => a.powf(b.as_scalar()).map(Literal::from ).map_err(PowError::from),
            (a, b) => Err(PowError::new_undef(a.flat_type(), b.flat_type()))
        }
    }

    pub fn static_type() -> FlatType {
        FlatType::Any
    }
    pub fn flat_type(&self) -> FlatType {
        match self {
            Self::Sca(_)  => FlatType::Scalar,
            Self::Cmp(_)  => FlatType::Complex,
            Self::Bool(_) => FlatType::Boolean,
            Self::Vec(_)  => FlatType::Vector,
            Self::Mat(_)  => FlatType::Matrix
        }
    }
}

struct LiteralTester {
    lhs_raw: (Scalar, Complex, FloatVector, FloatMatrix, Boolean),
    lhs: [Literal; 5],
    rhs_raw: (Scalar, Complex, FloatVector, FloatMatrix, Boolean),
    rhs: [Literal; 5],
    results: [[Option<Literal>; 5]; 5]
}
impl Default for LiteralTester {
    fn default() -> Self {
        let lhs_raw = (
            Scalar::new(4.0), 
            Complex::new(1.0, 2.5),
            FloatVector::from([1.0, 4.0, 3.0]),
            FloatMatrix::identity(2),
            Boolean::True
        );
        let rhs_raw = (
            Scalar::new(1.0),
            Complex::new(2.1, 4.0),
            FloatVector::from([2.0, 1.0, -4.0]),
            FloatMatrix::try_from(vec![vec![1.0, 4.0], vec![5.0, 2.0]]).unwrap(),
            Boolean::False
        );

        Self {
            lhs: [lhs_raw.0.into(), lhs_raw.1.into(), lhs_raw.2.clone().into(), lhs_raw.3.clone().into(), lhs_raw.4.into()],
            rhs: [rhs_raw.0.into(), rhs_raw.1.into(), rhs_raw.2.clone().into(), rhs_raw.3.clone().into(), rhs_raw.4.into()],
            lhs_raw,
            rhs_raw,
            results: core::array::from_fn(|_| [const { None }; 5])
        }
    }
}
impl LiteralTester {
    pub fn new() -> Self {
        Self::default()
    }

    
}

#[cfg(test)]
mod tests {
    use super::*;

    fn perform_test<F>(val: &[(usize, usize, Option<Literal>)], oper: F) 
        where F: Fn() -> Option<Literal> {

        }
}

#[test]
fn test_variable_union_ref() {
    //We are primarily testing the operators.

    let raw = (
        Scalar::new(4.0), 
        Complex::new(1.0, 2.5),
        FloatVector::from([1.0, 4.0, 3.0]),
        FloatMatrix::identity(2),
        Boolean::True
    );

    let as_union: [Literal; 5] = [
        raw.0.into(),
        raw.1.into(),
        raw.2.clone().into(),
        raw.3.clone().into(),
        raw.4.into()
    ];
    let lhs: [LiteralReference; 5] = [
        raw.0.into(),
        raw.1.into(),
        (&raw.2).into(),
        (&raw.3).into(),
        raw.4.into()
    ];

    //Printing
    assert_eq!(raw.0.to_string(), lhs[0].to_string());
    assert_eq!(raw.1.to_string(), lhs[1].to_string());
    assert_eq!(raw.2.to_string(), lhs[2].to_string());
    assert_eq!(raw.3.to_string(), lhs[3].to_string());
    assert_eq!(raw.4.to_string(), lhs[4].to_string());

    // Negation
    assert_eq!(-lhs[0], -as_union[0].clone());
    assert_eq!(-lhs[1], -as_union[1].clone());
    assert_eq!(-lhs[2], -as_union[2].clone());
    assert_eq!(-lhs[3], -as_union[3].clone());
    assert!((-lhs[4]).is_err());

    let rhs_raw = (
            Scalar::new(1.0),
            Complex::new(2.1, 4.0),
            FloatVector::from([2.0, 1.0, -4.0]),
            FloatMatrix::try_from(vec![vec![1.0, 4.0], vec![5.0, 2.0]]).unwrap(),
            Boolean::False
        );

    let rhs: [LiteralReference; 5] = [
        rhs_raw.0.into(),
        rhs_raw.1.into(),
        (&rhs_raw.2).into(),
        (&rhs_raw.3).into(),
        rhs_raw.4.into()
    ];

    let add_expect = [
        (0, 0, Some( (raw.0 + rhs_raw.0).into() ) ),
        (0, 1, Some( (Complex::from(raw.0) + rhs_raw.1).into() ) ),
        (0, 2, None),
        (0, 3, None),
        (0, 4, None),

        (1, 0, Some( (raw.1 + Complex::from(rhs_raw.0)).into() ) ),
        (1, 1, Some( (raw.1 + rhs_raw.1).into() ) ),
        (1, 2, None),
        (1, 3, None),
        (1, 4, None),
        
        (2, 0, None),
        (2, 1, None),
        (2, 2, Some( (raw.2.clone() + rhs_raw.2.clone()).into() ) ),
        (2, 3, None),
        (2, 4, None),

        (3, 0, None),
        (3, 1, None),
        (3, 2, None),
        (3, 3, Some( (raw.3.clone() + rhs_raw.3.clone()).unwrap().into() ) ),
        (3, 4, None),

        (4, 0, None),
        (4, 1, None),
        (4, 2, None),
        (4, 3, None),
        (4, 4, None) 
    ];
    for (a, b, result) in add_expect {
        let a_v = lhs[a];
        let b_v = rhs[b];
        assert_eq!( (a_v + b_v).ok(), result, "lhs: {a}, rhs: {b}");
    }
//Subtraction
    let sub_expect = [
        (0, 0, Some( (raw.0 - rhs_raw.0).into() ) ),
        (0, 1, Some( (Complex::from(raw.0) - rhs_raw.1).into() ) ),
        (0, 2, None),
        (0, 3, None),
        (0, 4, None),

        (1, 0, Some( (raw.1 - Complex::from(rhs_raw.0)).into() ) ),
        (1, 1, Some( (raw.1 - rhs_raw.1).into() ) ),
        (1, 2, None),
        (1, 3, None),
        (1, 4, None),
        
        (2, 0, None),
        (2, 1, None),
        (2, 2, Some( (raw.2.clone() - rhs_raw.2.clone()).into() ) ),
        (2, 3, None),
        (2, 4, None),

        (3, 0, None),
        (3, 1, None),
        (3, 2, None),
        (3, 3, Some( (raw.3.clone() - rhs_raw.3.clone()).unwrap().into() ) ),
        (3, 4, None),

        (4, 0, None),
        (4, 1, None),
        (4, 2, None),
        (4, 3, None),
        (4, 4, None) 
    ];
    for (a, b, result) in sub_expect {
        let a_v = lhs[a];
        let b_v = rhs[b];
        assert_eq!( (a_v - b_v).ok(), result, "lhs: {a}, rhs: {b}");
    }

    //Multiplication
    let mul_expect = [
        (lhs[0], rhs[0], Some( (raw.0 * rhs_raw.0).into() ) ), //Sca sca
        (lhs[0], rhs[1], Some( (Complex::from(raw.0) * rhs_raw.1).into() ) ), //Sca cmp
        (lhs[0], rhs[2], Some( (rhs_raw.2.clone() * raw.0.as_scalar()).into() ) ), //Sca vec
        (lhs[0], rhs[3], Some( (rhs_raw.3.clone() * raw.0.as_scalar()).into() ) ), //Sca mat
        (lhs[0], rhs[4], None),

        (lhs[1], rhs[0], Some( (rhs_raw.1 * Complex::from(raw.0)).into() ) ), //Cmp sca
        (lhs[1], rhs[1], Some( (raw.1 * rhs_raw.1).into() ) ), //Cmp cmp
        (lhs[1], rhs[2], None),
        (lhs[1], rhs[3], None),
        (lhs[1], rhs[4], None),
        
        (lhs[2], rhs[0], Some( (raw.2.clone() * rhs_raw.0.as_scalar()).into() ) ),
        (lhs[2], rhs[1], None),
        (lhs[2], rhs[2], None),
        (lhs[2], rhs[3], None),
        (lhs[2], rhs[4], None),

        (lhs[3], rhs[0], None),
        (lhs[3], rhs[1], None),
        (lhs[3], rhs[2], None),
        (lhs[3], rhs[3], (raw.3.clone() * rhs_raw.3.clone()).ok().map(|x| x.into()) ), //Mat mat
        (lhs[3], rhs[4], None),

        (lhs[4], rhs[0], None),
        (lhs[4], rhs[1], None),
        (lhs[4], rhs[2], None),
        (lhs[4], rhs[3], None),
        (lhs[4], rhs[4], None) 
    ];

    for (a, b, result) in mul_expect {
        assert_eq!( (a * b).ok(), result );
    }

    //Division
    let div_expect = [
        (lhs[0], rhs[0], Some( (raw.0 / rhs_raw.0).into() ) ), //Sca sca
        (lhs[0], rhs[1], Some( (Complex::from(raw.0) / rhs_raw.1 ).into() ) ), //Sca cmp
        (lhs[0], rhs[2], Some( (raw.2.clone() / raw.0.as_scalar()).into() ) ), //Sca vec
        (lhs[0], rhs[3], Some( (raw.3.clone() / raw.0.as_scalar()).into() ) ), //Sca mat
        (lhs[0], rhs[4], None),

        (lhs[1], rhs[0], Some( (raw.1 / Complex::from(rhs_raw.0) ).into() ) ), //Cmp sca
        (lhs[1], rhs[1], Some( (raw.1 / rhs_raw.1).into() ) ), //Cmp cmp
        (lhs[1], rhs[2], None),
        (lhs[1], rhs[3], None),
        (lhs[1], rhs[4], None),
        
        (lhs[2], rhs[0], Some( (raw.2.clone() / raw.0.as_scalar()).into() ) ), //Vec sca
        (lhs[2], rhs[1], None),
        (lhs[2], rhs[2], None),
        (lhs[2], rhs[3], None),
        (lhs[2], rhs[4], None),

        (lhs[3], rhs[0], Some( (raw.3.clone() / raw.0.as_scalar()).into() ) ), //Mat sca
        (lhs[3], rhs[1], None),
        (lhs[3], rhs[2], None),
        (lhs[3], rhs[3], None),
        (lhs[3], rhs[4], None),

        (lhs[4], rhs[0], None),
        (lhs[4], rhs[1], None),
        (lhs[4], rhs[2], None),
        (lhs[4], rhs[3], None),
        (lhs[4], rhs[4], None)
    ];
    
    for (a, b, result) in div_expect {
        assert_eq!( (a / b).ok(), result );
    }

    //Pow
    let pow_expect = [
        (lhs[0], rhs[0], Some( (raw.0.pow(rhs_raw.0)).into() ) ), //Sca sca
        (lhs[0], rhs[1], Some( rhs_raw.1.pow_sca(raw.0).into() ) ), //Sca cmp
        (lhs[0], rhs[2], None),
        (lhs[0], rhs[3], None),
        (lhs[0], rhs[4], None),

        (lhs[1], rhs[0], Some( raw.1.pow_sca(rhs_raw.0).into() ) ), //Cmp sca
        (lhs[1], rhs[1], Some( raw.1.pow(rhs_raw.1).into() ) ), //Cmp cmp
        (lhs[1], rhs[2], None),
        (lhs[1], rhs[3], None),
        (lhs[1], rhs[4], None),
        
        (lhs[2], rhs[0], None),
        (lhs[2], rhs[1], None),
        (lhs[2], rhs[2], None),
        (lhs[2], rhs[3], None),
        (lhs[2], rhs[4], None),

        (lhs[3], rhs[0], raw.3.powf(rhs_raw.0.as_scalar()).ok().map(|x| x.into()) ), //Mat sca
        (lhs[3], rhs[1], None),
        (lhs[3], rhs[2], None),
        (lhs[3], rhs[3], None),
        (lhs[3], rhs[4], None),

        (lhs[4], rhs[0], None),
        (lhs[4], rhs[1], None),
        (lhs[4], rhs[2], None),
        (lhs[4], rhs[3], None),
        (lhs[4], rhs[4], None)
    ];

    for (a, b, result) in pow_expect {
        assert_eq!( a.pow(b).ok(), result );
    }
}