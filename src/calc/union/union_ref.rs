use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use super::union::VariableUnion;
use super::super::scalar::{Scalar, ScalarLike};
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::MathVector;
use super::super::matrix::Matrix;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VariableUnionRef<'a> {
    Sca(Scalar),
    Cmp(Complex),
    Boo(Boolean),
    Vec(&'a MathVector),
    Mat(&'a Matrix)
}

impl<T> From<T> for VariableUnionRef<'_> where T: ScalarLike {
    fn from(value: T) -> Self {
        Self::Sca(Scalar::new(value))
    }
}
impl<'a> From<&'a Scalar> for VariableUnionRef<'a> {
    fn from(value: &'a Scalar) -> Self {
        Self::Sca(*value)
    }
}
impl<'a> From<&'a Complex> for VariableUnionRef<'a> {
    fn from(value: &'a Complex) -> Self {
        Self::Cmp(value)
    }
}
impl<'a> From<&'a MathVector> for VariableUnionRef<'a> {
    fn from(value: &'a MathVector) -> Self {
        Self::Vec(value)
    }
}
impl<'a> From<&'a Matrix> for VariableUnionRef<'a> {
    fn from(value: &'a Matrix) -> Self {
        Self::Mat(value)
    }
}

impl Display for VariableUnionRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sca(s) => (s as &dyn Display).fmt(f), 
            Self::Cmp(s) => (s as &dyn Display).fmt(f), 
            Self::Vec(s) => (s as &dyn Display).fmt(f), 
            Self::Mat(s) => (s as &dyn Display).fmt(f), 
        }
    }
}

impl Neg for VariableUnionRef<'_> {
    type Output = VariableUnion;
    fn neg(self) -> Self::Output {
        match self {
            Self::Sca(s) => VariableUnion::Sca(-s),
            Self::Cmp(s) => VariableUnion::Cmp(-s),
            Self::Vec(s) => VariableUnion::Vec(-s),
            Self::Mat(s) => VariableUnion::Mat(-s),
        }
    }
}
impl Add for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
    fn add(self, rhs: Self) -> CalcResult<VariableUnion> {
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
                Ok(VariableUnion::Cmp(&a + b))
            },
            (Self::Cmp(a), Self::Cmp(b)) => Ok(VariableUnion::Cmp(a + b)),
            (Self::Vec(a), Self::Vec(b)) => Ok(VariableUnion::Vec(a + b)),
            (Self::Mat(a), Self::Mat(b)) => {
                match a + b {
                    Ok(m) => Ok(VariableUnion::Mat(m)),
                    Err(e) => Err(CalcError::MatDim(e))
                }
            },
            (a, b) => Err(CalcError::Oper(OperationError::new_fmt("+", &a, &b, Some("operator not defined"))))
        }
    }
}
impl Sub for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
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
                Ok(VariableUnion::Cmp(&a - b))
            },
            (Self::Cmp(a), Self::Cmp(b)) => Ok(VariableUnion::Cmp(a - b)),
            (Self::Vec(a), Self::Vec(b)) => Ok(VariableUnion::Vec(a - b)),
            (Self::Mat(a), Self::Mat(b)) => {
                match a - b {
                    Ok(m) => Ok(VariableUnion::Mat(m)),
                    Err(e) => Err(CalcError::MatDim(e))
                }
            },
            (a, b) => Err(CalcError::Oper(OperationError::new_fmt("-", &a, &b, Some("operator not defined"))))
        }
    }
}
impl Mul for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
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
            (Self::Mat(a), Self::Mat(b)) => {
                match a * b {
                    Ok(m) => Ok(VariableUnion::Mat(m)),
                    Err(e) => Err(CalcError::Dim(e))
                }
            },

            (Self::Sca(a), Self::Cmp(b)) | (Self::Cmp(b), Self::Sca(a)) => {
                let a: Complex = a.into();
                Ok(VariableUnion::Cmp(&a * b))
            },
            (Self::Vec(a), Self::Sca(b)) | (Self::Sca(b), Self::Vec(a)) => Ok(VariableUnion::Vec(a * b)),
            (Self::Mat(a), Self::Sca(b)) | (Self::Sca(b), Self::Mat(a)) => Ok(VariableUnion::Mat(a * b)),

            (a, b) => Err(CalcError::Oper(OperationError::new_fmt("*", &a, &b, Some("operator not defined"))))
        }
    }
}
impl Div for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
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
                Ok(VariableUnion::Cmp(&a / b))
            },
            (Self::Vec(a), Self::Sca(b)) => Ok(VariableUnion::Vec(a / b)),
            (Self::Mat(a), Self::Sca(b)) => Ok(VariableUnion::Mat(a / b)),
            (a, b) => Err(CalcError::Oper(OperationError::new_fmt("/", &a, &b, Some("operator not defined"))))
        }
    }
}

impl VariableUnionRef<'_> {
    pub fn pow(self, rhs: Self) -> CalcResult<VariableUnion> {
        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok( a.pow(b).into() ),
            (Self::Cmp(a), Self::Sca(b)) => Ok( a.pow_sca(b).into() ),
            (Self::Cmp(a), Self::Cmp(b)) => Ok( a.pow(b).into() ),
            (Self::Mat(a), Self::Sca(b)) => a.pow(b).map(VariableUnion::from ),
            (a, b) => Err(OperationError::new_fmt('^', &a, &b, None).into())
        }
    }
}


#[test]
fn test_variable_union_ref() {
    //We are primarily testing the operators.
    let ar = Scalar::new(4.0);
    let br = Complex::new(1.0, 2.5);
    let cr = MathVector::from(vec![1, 4, 3]);
    let dr = Matrix::identity(2);
    
    // Conversion
    let a = VariableUnionRef::from(&ar);
    let b = VariableUnionRef::from(&br);
    let c = VariableUnionRef::from(&cr);
    let d = VariableUnionRef::from(&dr);

    //Printing
    assert_eq!(format!("{:?}", &a), format!("{:?}", &ar));
    assert_eq!(format!("{:?}", &b), format!("{:?}", &br));
    assert_eq!(format!("{:?}", &c), format!("{:?}", &cr));
    assert_eq!(format!("{:?}", &d), format!("{:?}", &dr));

    assert_eq!(format!("{}", &a), format!("{}", &ar));
    assert_eq!(format!("{}", &b), format!("{}", &br));
    assert_eq!(format!("{}", &c), format!("{}", &cr));
    assert_eq!(format!("{}", &d), format!("{}", &dr));

    // Negation
    assert_eq!(-a, VariableUnion::Sca(-ar));
    assert_eq!(-b, VariableUnion::Cmp(-br.clone()));
    assert_eq!(-c, VariableUnion::Vec(-cr.clone()));
    assert_eq!(-d, VariableUnion::Mat(-dr.clone()));


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