use serde::{Serialize, Deserialize};

use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use super::super::scalar::{Scalar, ScalarLike};
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::MathVector;
use super::super::matrix::Matrix;
use super::super::err::{UndefinedUniOperation, UndefinedBiOperation};

use crate::expr::raw::types::FlatType;

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum VariableUnion {
    Sca(Scalar),
    Cmp(Complex),
    Bool(Boolean),
    Vec(MathVector),
    Mat(Matrix)
}
impl Default for VariableUnion {
    fn default() -> Self {
        Self::Sca(Scalar::default())
    }
}
impl Neg for VariableUnion {
    type Output = Result<VariableUnion, UndefinedUniOperation>;
    fn neg(self) -> Self::Output {
        match self {
            VariableUnion::Sca(s)     => Ok( VariableUnion::Sca(-s) ),
            VariableUnion::Cmp(c)    => Ok( VariableUnion::Cmp(-c) ),
            VariableUnion::Vec(v) => Ok( VariableUnion::Vec(-v) ),
            VariableUnion::Mat(m)     => Ok( VariableUnion::Mat(-m) ),
            VariableUnion::Bool(_)            => Err( UndefinedUniOperation::new("-", FlatType::Boolean) )
        }
    }
}
impl Add for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn add(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Vec(a), Self::Vec(b)) => Ok(Self::Vec(a + b)),
            (Self::Mat(a), Self::Mat(b)) => {
                match a + b {
                    Ok(m) => Ok(Self::Mat(m)),
                    Err(e) => Err(CalcError::MatDim(e))
                }
            },
            (a, b) => a.get_ref().add(b.get_ref())
        }
    }
}
impl Sub for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn sub(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Vec(a), Self::Vec(b)) => Ok(Self::Vec(a - b)),
            (Self::Mat(a), Self::Mat(b)) => {
                match a - b {
                    Ok(m) => Ok(Self::Mat(m)),
                    Err(e) => Err(CalcError::MatDim(e))
                }
            },
            (a, b) => a.get_ref().sub(b.get_ref())
        }
    }
}
impl Mul for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn mul(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Vec(a), Self::Sca(b)) | (Self::Sca(b), Self::Vec(a)) => Ok(Self::Vec(a * b)),
            (Self::Mat(a), Self::Sca(b)) | (Self::Sca(b), Self::Mat(a)) => Ok(Self::Mat(a * b)),
            (a, b) => a.get_ref().mul(b.get_ref())
        }
    }
}
impl Div for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn div(self, rhs: Self) -> Self::Output {
        self.get_ref().div(rhs.get_ref())
    }
}

impl<T> From<T> for VariableUnion where T: ScalarLike {
    fn from(value: T) -> Self {
        Self::Sca(Scalar::new(value))
    }
}
impl From<Complex> for VariableUnion {
    fn from(value: Complex) -> Self {
        Self::Cmp(value)
    }
}
impl From<MathVector> for VariableUnion {
    fn from(value: MathVector) -> Self {
        Self::Vec(value)
    }
}
impl From<Matrix> for VariableUnion {
    fn from(value: Matrix) -> Self {
        Self::Mat(value)   
    }
}

impl Display for VariableUnion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.get_ref() as &dyn Display).fmt(f)
    }
}

impl VariableUnion {
    pub fn get_ref(&self) -> super::union_ref::VariableUnionRef<'_> {
        use super::union_ref::VariableUnionRef::*;
        match self {
            Self::Sca(s) => Sca(*s),
            Self::Cmp(s) => Cmp(*s),
            Self::Bool(b) => Boo(*b),
            Self::Vec(s) => Vec(s),
            Self::Mat(s) => Mat(s)
        }
    }
    pub fn get_ref_mut(&mut self) -> super::union_mut::VariableUnionMut<'_> {
        use super::union_mut::VariableUnionMut::*;
        match self {
            Self::Sca(s) => Sca(s),
            Self::Cmp(s) => Cmp(s),
            Self::Bool(b) => Bool(b),
            Self::Vec(s) => Vec(s),
            Self::Mat(s) => Mat(s)
        }
    }

    pub fn pow(self, rhs: Self) -> CalcResult<Self> {
        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok( a.pow(b).into() ),
            (Self::Cmp(a), Self::Sca(b)) => Ok( a.pow_sca(b).into() ),
            (Self::Cmp(a), Self::Cmp(b)) => Ok( a.pow(b).into() ),
            (Self::Mat(a), Self::Sca(b)) => a.pow(b).map( VariableUnion::from ),
            (a, b) => Err(OperationError::new_fmt('^', &a, &b, None).into())
        }
    }
}

#[test]
fn test_variable_union() {
    //We are primarily testing the operators.
    let ar = Scalar::new(4.0);
    let br = Complex::new(1.0, 2.5);
    let cr = MathVector::from(vec![1, 4, 3]);
    let dr = Matrix::identity(2);

    // Conversion
    let a = VariableUnion::from(ar);
    let b = VariableUnion::from(br.clone());
    let c = VariableUnion::from(cr.clone());
    let d = VariableUnion::from(dr.clone());

    assert_eq!(a, VariableUnion::Sca(ar));
    assert_eq!(b, VariableUnion::Cmp(br.clone()));
    assert_eq!(c, VariableUnion::Vec(cr.clone()));
    assert_eq!(d, VariableUnion::Mat(dr.clone()));

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
    assert_eq!(-a.clone(), Ok( VariableUnion::Sca(-ar) ));
    assert_eq!(-b.clone(), Ok( VariableUnion::Cmp(-br) ));
    assert_eq!(-c.clone(), Ok( VariableUnion::Vec(-cr.clone()) ));
    assert_eq!(-d.clone(), Ok( VariableUnion::Mat(-dr.clone()) ));

    let er = Scalar::new(1.0);
    let fr = Complex::new(2.1, 4.0);
    let gr = MathVector::from(vec![2, 1, -4]);
    let hr = Matrix::try_from(vec![vec![1, 4], vec![5,2]]).unwrap();

    //Addition
    assert_eq!(a.clone() + VariableUnion::from(er), Ok( VariableUnion::from( ar + er) ));
    assert_eq!(b.clone() + VariableUnion::from(fr.clone()), Ok( VariableUnion::from( br + fr ) ));
    assert_eq!(c.clone() + VariableUnion::from(gr.clone()), Ok( VariableUnion::from( cr + gr ) ));
    assert_eq!(d.clone() + VariableUnion::from(hr.clone()), Ok( VariableUnion::from( (&dr + &hr).unwrap() ) ));

    //Subtraction
    assert_eq!(a.clone() - VariableUnion::from(er), Ok( VariableUnion::from( ar - er) ));
    assert_eq!(b.clone() - VariableUnion::from(fr.clone()), Ok( VariableUnion::from( br - fr ) ));
    assert_eq!(c.clone() - VariableUnion::from(gr.clone()), Ok( VariableUnion::from( cr - gr ) ));
    assert_eq!(d.clone() - VariableUnion::from(hr.clone()), Ok( VariableUnion::from( (&dr - &hr).unwrap() ) ));

    //Multiplication
    assert_eq!(a.clone() * VariableUnion::from(er), Ok( VariableUnion::from( ar * er) ));
    assert_eq!(b.clone() * VariableUnion::from(fr.clone()), Ok( VariableUnion::from( br * fr ) ));
    assert_eq!(b.clone() * VariableUnion::Cmp(ar.into()), Ok( VariableUnion::from( br * ar.into() ) ));
    assert_eq!(c.clone() * VariableUnion::from(ar), Ok( VariableUnion::from( &cr * ar ) ));
    assert!( (c.clone() * VariableUnion::from(gr.clone())).is_err() );
    assert_eq!(d.clone() * VariableUnion::from(hr.clone()), Ok( VariableUnion::from( (&dr * &hr).unwrap() ) ));
    assert_eq!(d.clone() * VariableUnion::from(ar), Ok( VariableUnion::from( &dr * ar ) ));

    //Division
    assert_eq!(a.clone() / VariableUnion::from(er), Ok( VariableUnion::from( ar / er) ));
    assert_eq!(b.clone() / VariableUnion::from(fr.clone()), Ok( VariableUnion::from( br / fr ) ));
    assert_eq!(b.clone() / VariableUnion::Cmp(ar.into()), Ok( VariableUnion::from( br / ar.into() ) ));
    assert!( (c.clone() / VariableUnion::from(gr.clone())).is_err() );
    assert_eq!(c.clone() / VariableUnion::from(ar), Ok( VariableUnion::from( &cr / ar ) ));
    assert!( (d.clone() / VariableUnion::from(hr.clone())).is_err() );
    assert_eq!(d.clone() / VariableUnion::from(ar), Ok( VariableUnion::from( &dr / ar ) ));
}