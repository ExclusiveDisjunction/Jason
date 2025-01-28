pub mod scalar;
pub mod complex;
pub mod vector;
pub mod matrix;
pub mod variable_type;
pub mod calc_error;

use std::ops::{Add, Sub, Mul, Div, Neg};
use std::fmt::{Display, Debug};

use serde::{Serialize, Deserialize};

pub use variable_type::{VariableData, VariableType};
pub use scalar::Scalar;
pub use complex::Complex;
pub use vector::MathVector;
pub use matrix::Matrix;
pub use calc_error::{CalcResult, OperationError, CalcError};

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum VariableUnion {
    Sca(Scalar),
    Cmp(Complex),
    Vec(MathVector),
    Mat(Matrix)
}
impl Default for VariableUnion {
    fn default() -> Self {
        Self::Sca(Scalar::default())
    }
}

impl Neg for VariableUnion {
    type Output = VariableUnion;
    fn neg(self) -> Self::Output {
        match self {
            VariableUnion::Sca(sc) => VariableUnion::Sca(-sc),
            VariableUnion::Cmp(c) => VariableUnion::Cmp(-c),
            VariableUnion::Vec(v) => VariableUnion::Vec(-v),
            VariableUnion::Mat(m) => VariableUnion::Mat(-m)
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

impl From<Scalar> for VariableUnion {
    fn from(value: Scalar) -> Self {
        Self::Sca(value)
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
impl Debug for VariableUnion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.get_ref() as &dyn Debug).fmt(f)
    }
}

impl VariableData for VariableUnion {
    fn get_type(&self) -> VariableType {
        match self {
            Self::Sca(a) => a.get_type(),
            Self::Cmp(a) => a.get_type(),
            Self::Vec(a) => a.get_type(),
            Self::Mat(a) => a.get_type()
        }
    }
}

impl VariableUnion {
    pub fn get_ref(&self) -> VariableUnionRef<'_> {
        match self {
            Self::Sca(s) => VariableUnionRef::Sca(*s),
            Self::Cmp(s) => VariableUnionRef::Cmp(s),
            Self::Vec(s) => VariableUnionRef::Vec(s),
            Self::Mat(s) => VariableUnionRef::Mat(s)
        }
    }
    pub fn get_ref_mut(&mut self) -> VariableUnionRefMut<'_> {
        match self {
            Self::Sca(s) => VariableUnionRefMut::Sca(s),
            Self::Cmp(s) => VariableUnionRefMut::Cmp(s),
            Self::Vec(s) => VariableUnionRefMut::Vec(s),
            Self::Mat(s) => VariableUnionRefMut::Mat(s)
        }
    }
}

pub enum VariableUnionRef<'a> {
    Sca(Scalar),
    Cmp(&'a Complex),
    Vec(&'a MathVector),
    Mat(&'a Matrix)
}

impl Debug for VariableUnionRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sca(s) => (s as &dyn Debug).fmt(f), 
            Self::Cmp(s) => (s as &dyn Debug).fmt(f), 
            Self::Vec(s) => (s as &dyn Debug).fmt(f), 
            Self::Mat(s) => (s as &dyn Debug).fmt(f), 
        }
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

pub enum VariableUnionRefMut<'a> {
    Sca(&'a mut Scalar),
    Cmp(&'a mut Complex),
    Vec(&'a mut MathVector),
    Mat(&'a mut Matrix)
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
    assert_eq!(-a.clone(), VariableUnion::Sca(-ar));
    assert_eq!(-b.clone(), VariableUnion::Cmp(-br.clone()));
    assert_eq!(-c.clone(), VariableUnion::Vec(-cr.clone()));
    assert_eq!(-d.clone(), VariableUnion::Mat(-dr.clone()));


    let er = Scalar::new(1.0);
    let fr = Complex::new(2.1, 4.0);
    let gr = MathVector::from(vec![2, 1, -4]);
    let hr = Matrix::try_from(vec![vec![1, 4], vec![5,2]]).unwrap();

    //Addition
    assert_eq!(a.clone() + VariableUnion::from(er), Ok( VariableUnion::from( ar + er) ));
    assert_eq!(b.clone() + VariableUnion::from(fr.clone()), Ok( VariableUnion::from( &br + &fr ) ));
    assert_eq!(c.clone() + VariableUnion::from(gr.clone()), Ok( VariableUnion::from( &cr + &gr ) ));
    assert_eq!(d.clone() + VariableUnion::from(hr.clone()), Ok( VariableUnion::from( (&dr + &hr).unwrap() ) ));

    //Subtraction
    assert_eq!(a.clone() - VariableUnion::from(er), Ok( VariableUnion::from( ar - er) ));
    assert_eq!(b.clone() - VariableUnion::from(fr.clone()), Ok( VariableUnion::from( &br - &fr ) ));
    assert_eq!(c.clone() - VariableUnion::from(gr.clone()), Ok( VariableUnion::from( &cr - &gr ) ));
    assert_eq!(d.clone() - VariableUnion::from(hr.clone()), Ok( VariableUnion::from( (&dr - &hr).unwrap() ) ));

    //Multiplication
    assert_eq!(a.clone() * VariableUnion::from(er), Ok( VariableUnion::from( ar * er) ));
    assert_eq!(b.clone() * VariableUnion::from(fr.clone()), Ok( VariableUnion::from( &br * &fr ) ));
    assert_eq!(b.clone() * VariableUnion::Cmp(ar.into()), Ok( VariableUnion::from( &br * &ar.into() ) ));
    assert_eq!(c.clone() * VariableUnion::from(ar), Ok( VariableUnion::from( &cr * ar ) ));
    assert!( (c.clone() * VariableUnion::from(gr.clone())).is_err() );
    assert_eq!(d.clone() * VariableUnion::from(hr.clone()), Ok( VariableUnion::from( (&dr * &hr).unwrap() ) ));
    assert_eq!(d.clone() * VariableUnion::from(ar), Ok( VariableUnion::from( &dr * ar ) ));

    //Division
    assert_eq!(a.clone() / VariableUnion::from(er), Ok( VariableUnion::from( ar / er) ));
    assert_eq!(b.clone() / VariableUnion::from(fr.clone()), Ok( VariableUnion::from( &br / &fr ) ));
    assert_eq!(b.clone() / VariableUnion::Cmp(ar.into()), Ok( VariableUnion::from( &br / &ar.into() ) ));
    assert!( (c.clone() / VariableUnion::from(gr.clone())).is_err() );
    assert_eq!(c.clone() / VariableUnion::from(ar), Ok( VariableUnion::from( &cr / ar ) ));
    assert!( (d.clone() / VariableUnion::from(hr.clone())).is_err() );
    assert_eq!(d.clone() / VariableUnion::from(ar), Ok( VariableUnion::from( &dr / ar ) ));
}

#[test]
fn test_variable_union_ref() {

}