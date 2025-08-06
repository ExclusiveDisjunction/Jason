use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use crate::calc::err::{BiOperationError, PowError, UndefinedUniOperation};
use crate::calc::ScalarLike;
use crate::prelude::FlatType;
use super::union::VariableUnion;
use super::super::scalar::Scalar;
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::FloatVector;
use super::super::matrix::FloatMatrix;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VariableUnionRef<'a> {
    Sca(Scalar),
    Cmp(Complex),
    Bool(Boolean),
    Vec(&'a FloatVector),
    Mat(&'a FloatMatrix)
}

impl From<Scalar> for VariableUnionRef<'_> {
    fn from(value: Scalar) -> Self {
        Self::Sca(value)
    }
}
impl<'a> From<Complex> for VariableUnionRef<'a> {
    fn from(value: Complex) -> Self {
        Self::Cmp(value)
    }
}
impl From<Boolean> for VariableUnionRef<'_> {
    fn from(value: Boolean) -> Self {
        Self::Bool(value)
    }
}
impl<'a> From<&'a FloatVector> for VariableUnionRef<'a> {
    fn from(value: &'a FloatVector) -> Self {
        Self::Vec(value)
    }
}
impl<'a> From<&'a FloatMatrix> for VariableUnionRef<'a> {
    fn from(value: &'a FloatMatrix) -> Self {
        Self::Mat(value)
    }
}

impl Display for VariableUnionRef<'_> {
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

impl Neg for VariableUnionRef<'_> {
    type Output = Result<VariableUnion, UndefinedUniOperation>;
    fn neg(self) -> Self::Output {
        match self {
            Self::Sca(s) => Ok( VariableUnion::Sca(-s) ),
            Self::Cmp(s) => Ok( VariableUnion::Cmp(-s) ),
            Self::Bool(b) => Err( UndefinedUniOperation::new("-", FlatType::Boolean)),
            Self::Vec(s) => Ok( VariableUnion::Vec(-s) ),
            Self::Mat(s) => Ok( VariableUnion::Mat(-s) )
        }
    }
}
impl Add for VariableUnionRef<'_> {
    type Output = Result<VariableUnion, BiOperationError>;
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
            (Self::Sca(a), Self::Sca(b)) => Ok(VariableUnion::Sca(a + b)),
            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok( (a + b).into() )
            },
            (Self::Cmp(a), Self::Cmp(b)) => Ok(VariableUnion::Cmp(a + b)),
            (Self::Vec(a), Self::Vec(b)) => Ok(VariableUnion::Vec(a + b)),
            (Self::Mat(a), Self::Mat(b)) => (a + b).map(VariableUnion::from).map_err(BiOperationError::from),
            (a, b) => Err(BiOperationError::new_undef("+", a.flat_type(), b.flat_type()))
        }
    }
}
impl Sub for VariableUnionRef<'_> {
    type Output = Result<VariableUnion, BiOperationError>;
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
            (Self::Sca(a), Self::Sca(b)) => Ok(VariableUnion::Sca(a - b)),
            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok(VariableUnion::Cmp(a - b))
            },
            (Self::Cmp(a), Self::Cmp(b)) => Ok(VariableUnion::Cmp(a - b)),
            (Self::Vec(a), Self::Vec(b)) => Ok(VariableUnion::Vec(a - b)),
            (Self::Mat(a), Self::Mat(b)) => (a - b).map(VariableUnion::from).map_err(BiOperationError::from),
            (a, b) => Err(BiOperationError::new_undef("-", a.flat_type(), b.flat_type()))
        }
    }
}
impl Mul for VariableUnionRef<'_> {
    type Output = Result<VariableUnion, BiOperationError>;
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
            (Self::Sca(a), Self::Sca(b)) => Ok(VariableUnion::Sca(a * b)),
            (Self::Cmp(a), Self::Cmp(b)) => Ok(VariableUnion::Cmp(a * b)),
            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok(VariableUnion::Cmp(a * b))
            },
            (Self::Mat(a), Self::Mat(b)) => (a * b).map(VariableUnion::from).map_err(BiOperationError::from),
            (Self::Vec(a), Self::Sca(b)) | (Self::Sca(b), Self::Vec(a)) => Ok(VariableUnion::Vec(a * b.as_scalar())),
            (Self::Mat(a), Self::Sca(b)) | (Self::Sca(b), Self::Mat(a)) => Ok(VariableUnion::Mat(a * b.as_scalar())),

            (a, b) => Err(BiOperationError::new_undef("*", a.flat_type(), b.flat_type()))
        }
    }
}
impl Div for VariableUnionRef<'_> {
    type Output = Result<VariableUnion, BiOperationError>;
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
            (Self::Sca(a), Self::Sca(b)) => Ok(VariableUnion::Sca(a / b)),
            (Self::Cmp(a), Self::Cmp(b)) => Ok(VariableUnion::Cmp(a / b)),
            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok(VariableUnion::Cmp(a / b))
            },
            (Self::Vec(a), Self::Sca(b)) => Ok(VariableUnion::Vec(a / b.as_scalar())),
            (Self::Mat(a), Self::Sca(b)) => Ok(VariableUnion::Mat(a / b.as_scalar())),
            (a, b) => Err(BiOperationError::new_undef("/", a.flat_type(), b.flat_type()))
        }
    }
}

impl VariableUnionRef<'_> {
    pub fn to_union(self) -> VariableUnion {
        use VariableUnion::*;
        match self {
            Self::Sca(s) => Sca(s),
            Self::Cmp(c) => Cmp(c),
            Self::Bool(b) => Bool(b),
            Self::Vec(v) => Vec(v.clone()),
            Self::Mat(m) => Mat(m.clone())
        }
    }

    pub fn pow(self, rhs: Self) -> Result<VariableUnion, PowError> {
        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok( a.pow(b).into() ),
            (Self::Cmp(a), Self::Sca(b)) => Ok( a.pow_sca(b).into() ),
            (Self::Cmp(a), Self::Cmp(b)) => Ok( a.pow(b).into() ),
            (Self::Mat(a), Self::Sca(b)) => a.powf(b.as_scalar()).map(VariableUnion::from ).map_err(PowError::from),
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


#[test]
fn test_variable_union_ref() {
    //We are primarily testing the operators.
    let ar = Scalar::new(4.0);
    let br = Complex::new(1.0, 2.5);
    let cr = FloatVector::from(vec![1.0, 4.0, 3.0]);
    let dr = FloatMatrix::identity(2);
    let er = Boolean::True;
    
    // Conversion
    let a = VariableUnionRef::from(ar);
    let b = VariableUnionRef::from(br);
    let c = VariableUnionRef::from(&cr);
    let d = VariableUnionRef::from(&dr);
    let e = VariableUnionRef::from(er);

    //Printing
    assert_eq!(format!("{}", &a), format!("{}", &ar));
    assert_eq!(format!("{}", &b), format!("{}", &br));
    assert_eq!(format!("{}", &c), format!("{}", &cr));
    assert_eq!(format!("{}", &d), format!("{}", &dr));
    assert_eq!(format!("{}", &e), format!("{}", &er));

    // Negation
    assert_eq!(-a, Ok( VariableUnion::Sca(-ar) ));
    assert_eq!(-b, Ok( VariableUnion::Cmp(-br) ));
    assert_eq!(-c, Ok( VariableUnion::Vec(-cr.clone()) ));
    assert_eq!(-d, Ok( VariableUnion::Mat(-dr.clone()) ));
    assert!((-e).is_err());


    let er = Scalar::new(1.0);
    let fr = Complex::new(2.1, 4.0);
    let gr = MathVector::from(vec![2, 1, -4]);
    let hr = Matrix::try_from(vec![vec![1, 4], vec![5,2]]).unwrap();

    //Addition
    assert_eq!(a + VariableUnionRef::from(&er), Ok( VariableUnion::from( ar + er) ));
    assert_eq!(b + VariableUnionRef::from(&fr), Ok( VariableUnion::from( &br + &fr ) ));
    assert_eq!(c + VariableUnionRef::from(&gr), Ok( VariableUnion::from( &cr + &gr ) ));
    assert_eq!(d + VariableUnionRef::from(&hr), Ok( VariableUnion::from( (&dr + &hr).unwrap() ) ));

    //Subtraction
    assert_eq!(a - VariableUnionRef::from(&er), Ok( VariableUnion::from( ar - er) ));
    assert_eq!(b - VariableUnionRef::from(&fr), Ok( VariableUnion::from( &br - &fr ) ));
    assert_eq!(c - VariableUnionRef::from(&gr), Ok( VariableUnion::from( &cr - &gr ) ));
    assert_eq!(d - VariableUnionRef::from(&hr), Ok( VariableUnion::from( (&dr - &hr).unwrap() ) ));

    //Multiplication
    assert_eq!(a * VariableUnionRef::from(&er), Ok( VariableUnion::from( ar * er) ));
    assert_eq!(b * VariableUnionRef::from(&fr), Ok( VariableUnion::from( &br * &fr ) ));
    assert_eq!(b * VariableUnionRef::Cmp(&ar.into()), Ok( VariableUnion::from( &br * &ar.into() ) ));
    assert_eq!(c * VariableUnionRef::from(&ar), Ok( VariableUnion::from( &cr * ar ) ));
    assert!( (c * VariableUnionRef::from(&gr)).is_err() );
    assert_eq!(d * VariableUnionRef::from(&hr), Ok( VariableUnion::from( (&dr * &hr).unwrap() ) ));
    assert_eq!(d * VariableUnionRef::from(&ar), Ok( VariableUnion::from( &dr * ar ) ));

    //Division
    assert_eq!(a / VariableUnionRef::from(&er), Ok( VariableUnion::from( ar / er) ));
    assert_eq!(b / VariableUnionRef::from(&fr), Ok( VariableUnion::from( &br / &fr ) ));
    assert_eq!(b / VariableUnionRef::Cmp(&ar.into()), Ok( VariableUnion::from( &br / &ar.into() ) ));
    assert!( (c / VariableUnionRef::from(&gr)).is_err() );
    assert_eq!(c / VariableUnionRef::from(&ar), Ok( VariableUnion::from( &cr / ar ) ));
    assert!( (d / VariableUnionRef::from(&hr)).is_err() );
    assert_eq!(d / VariableUnionRef::from(&ar), Ok( VariableUnion::from( &dr / ar ) ));
}