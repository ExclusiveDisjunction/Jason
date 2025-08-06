use serde::{Serialize, Deserialize};

use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use super::super::scalar::{Scalar, ScalarLike};
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::FloatVector;
use super::super::matrix::FloatMatrix;
use super::super::err::{UndefinedUniOperation, PowError, BiOperationError};

use crate::prelude::FlatType;

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum VariableUnion {
    Sca(Scalar),
    Cmp(Complex),
    Bool(Boolean),
    Vec(FloatVector),
    Mat(FloatMatrix)
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
    type Output = Result<Self, BiOperationError>;
    fn add(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Vec(a), Self::Vec(b)) => Ok(Self::Vec(a + b)),
            (Self::Mat(a), Self::Mat(b)) => {
                match a + b {
                    Ok(m) => Ok(Self::Mat(m)),
                    Err(e) => Err(e.into())
                }
            },
            (a, b) => a.get_ref().add(b.get_ref())
        }
    }
}
impl Sub for VariableUnion {
    type Output = Result<Self, BiOperationError>;
    fn sub(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Vec(a), Self::Vec(b)) => Ok(Self::Vec(a - b)),
            (Self::Mat(a), Self::Mat(b)) => {
                match a - b {
                    Ok(m) => Ok(Self::Mat(m)),
                    Err(e) => Err(e.into())
                }
            },
            (a, b) => a.get_ref().sub(b.get_ref())
        }
    }
}
impl Mul for VariableUnion {
    type Output = Result<Self, BiOperationError>;
    fn mul(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Vec(a), Self::Sca(b)) | (Self::Sca(b), Self::Vec(a)) => Ok(Self::Vec(a * b.as_scalar())),
            (Self::Mat(a), Self::Sca(b)) | (Self::Sca(b), Self::Mat(a)) => Ok(Self::Mat(a * b.as_scalar())),
            (a, b) => a.get_ref().mul(b.get_ref())
        }
    }
}
impl Div for VariableUnion {
    type Output = Result<Self, BiOperationError>;
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
impl From<Boolean> for VariableUnion {
    fn from(value: Boolean) -> Self {
        Self::Bool(value)
    }
}
impl From<FloatVector> for VariableUnion {
    fn from(value: FloatVector) -> Self {
        Self::Vec(value)
    }
}
impl From<FloatMatrix> for VariableUnion {
    fn from(value: FloatMatrix) -> Self {
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
            Self::Bool(b) => Bool(*b),
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

    pub fn pow(self, rhs: Self) -> Result<Self, PowError> {
        match (self, rhs) {
            (Self::Sca(a), Self::Sca(b)) => Ok( a.pow(b).into() ),
            (Self::Cmp(a), Self::Sca(b)) => Ok( a.pow_sca(b).into() ),
            (Self::Cmp(a), Self::Cmp(b)) => Ok( a.pow(b).into() ),
            (Self::Mat(a), Self::Sca(b)) => a.powf(b.as_scalar()).map(VariableUnion::from).map_err(PowError::from),
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

/*
    (lhs[0].clone(), rhs[0].clone(), None),
    (lhs[0].clone(), rhs[1].clone(), None),
    (lhs[0].clone(), rhs[2].clone(), None),
    (lhs[0].clone(), rhs[3].clone(), None),
    (lhs[0].clone(), rhs[4].clone(), None),

    (lhs[1].clone(), rhs[0].clone(), None),
    (lhs[1].clone(), rhs[1].clone(), None)
    (lhs[1].clone(), rhs[2].clone(), None),
    (lhs[1].clone(), rhs[3].clone(), None),
    (lhs[1].clone(), rhs[4].clone(), None),
    
    (lhs[2].clone(), rhs[0].clone(), None),
    (lhs[2].clone(), rhs[1].clone(), None),
    (lhs[2].clone(), rhs[2].clone(), None),
    (lhs[2].clone(), rhs[3].clone(), None),
    (lhs[2].clone(), rhs[4].clone(), None),

    (lhs[3].clone(), rhs[0].clone(), None),
    (lhs[3].clone(), rhs[1].clone(), None),
    (lhs[3].clone(), rhs[2].clone(), None),
    (lhs[3].clone(), rhs[3].clone(), None),
    (lhs[3].clone(), rhs[4].clone(), None),

    (lhs[4].clone(), rhs[0].clone(), None),
    (lhs[4].clone(), rhs[1].clone(), None),
    (lhs[4].clone(), rhs[2].clone(), None),
    (lhs[4].clone(), rhs[3].clone(), None),
    (lhs[4].clone(), rhs[4].clone(), None) 
*/

#[test]
fn test_variable_union() {
    //We are primarily testing the operators.

    let raw = (
        Scalar::new(4.0), 
        Complex::new(1.0, 2.5),
        FloatVector::from([1.0, 4.0, 3.0]),
        FloatMatrix::identity(2),
        Boolean::True
    );

    let raw_wrapped: [VariableUnion; 5] = [
        VariableUnion::Sca(raw.0),
        VariableUnion::Cmp(raw.1),
        VariableUnion::Vec(raw.2.clone()),
        VariableUnion::Mat(raw.3.clone()),
        VariableUnion::Bool(raw.4)
    ];

    // Conversion
    let lhs: [VariableUnion; 5] = [raw.0.into(), raw.1.into(), raw.2.clone().into(), raw.3.clone().into(), raw.4.into()];
    assert_eq!(&raw_wrapped, &lhs);

    //Printing
    assert_eq!(raw.0.to_string(), lhs[0].to_string());
    assert_eq!(raw.1.to_string(), lhs[1].to_string());
    assert_eq!(raw.2.to_string(), lhs[2].to_string());
    assert_eq!(raw.3.to_string(), lhs[3].to_string());
    assert_eq!(raw.4.to_string(), lhs[4].to_string());

    // Negation
    assert_eq!(-lhs[0].clone(), Ok( (-raw.0).into() ));
    assert_eq!(-lhs[1].clone(), Ok( (-raw.1).into() ));
    assert_eq!(-lhs[2].clone(), Ok( (-raw.2.clone()).into() ));
    assert_eq!(-lhs[3].clone(), Ok( (-raw.3.clone()).into() ));
    assert!( (-lhs[4].clone()).is_err() );

    let rhs_raw = (
        Scalar::new(1.0),
        Complex::new(2.1, 4.0),
        FloatVector::from([2.0, 1.0, -4.0]),
        FloatMatrix::try_from(vec![vec![1.0, 4.0], vec![5.0, 2.0]]).unwrap(),
        Boolean::False
    );

    let rhs: [VariableUnion; 5] = [
        rhs_raw.0.into(),
        rhs_raw.1.into(),
        rhs_raw.2.clone().into(),
        rhs_raw.3.clone().into(),
        rhs_raw.4.into()
    ];

    //Addition
    let add_expect = [
        (lhs[0].clone(), rhs[0].clone(), Some( (raw.0 + rhs_raw.0).into() ) ),
        (lhs[0].clone(), rhs[1].clone(), None),
        (lhs[0].clone(), rhs[2].clone(), None),
        (lhs[0].clone(), rhs[3].clone(), None),
        (lhs[0].clone(), rhs[4].clone(), None),

        (lhs[1].clone(), rhs[0].clone(), None),
        (lhs[1].clone(), rhs[1].clone(), Some( (raw.1 + rhs_raw.1).into() ) ),
        (lhs[1].clone(), rhs[2].clone(), None),
        (lhs[1].clone(), rhs[3].clone(), None),
        (lhs[1].clone(), rhs[4].clone(), None),
        
        (lhs[2].clone(), rhs[0].clone(), None),
        (lhs[2].clone(), rhs[1].clone(), Some( (raw.2.clone() + rhs_raw.2.clone()).into() ) ),
        (lhs[2].clone(), rhs[2].clone(), None),
        (lhs[2].clone(), rhs[3].clone(), None),
        (lhs[2].clone(), rhs[4].clone(), None),

        (lhs[3].clone(), rhs[0].clone(), None),
        (lhs[3].clone(), rhs[1].clone(), None),
        (lhs[3].clone(), rhs[2].clone(), None),
        (lhs[3].clone(), rhs[3].clone(), Some( (raw.3.clone() + rhs_raw.3.clone()).unwrap().into() ) ),
        (lhs[3].clone(), rhs[4].clone(), None),

        (lhs[4].clone(), rhs[0].clone(), None),
        (lhs[4].clone(), rhs[1].clone(), None),
        (lhs[4].clone(), rhs[2].clone(), None),
        (lhs[4].clone(), rhs[3].clone(), None),
        (lhs[4].clone(), rhs[4].clone(), None) 
    ];
    for (a, b, result) in add_expect {
        assert_eq!( (a + b).ok(), result );
    }

    //Subtraction
    let sub_expect = [
        (lhs[0].clone(), rhs[0].clone(), Some( (raw.0 - rhs_raw.0).into() ) ),
        (lhs[0].clone(), rhs[1].clone(), None),
        (lhs[0].clone(), rhs[2].clone(), None),
        (lhs[0].clone(), rhs[3].clone(), None),
        (lhs[0].clone(), rhs[4].clone(), None),

        (lhs[1].clone(), rhs[0].clone(), None),
        (lhs[1].clone(), rhs[1].clone(), Some( (raw.1 - rhs_raw.1).into() ) ),
        (lhs[1].clone(), rhs[2].clone(), None),
        (lhs[1].clone(), rhs[3].clone(), None),
        (lhs[1].clone(), rhs[4].clone(), None),
        
        (lhs[2].clone(), rhs[0].clone(), None),
        (lhs[2].clone(), rhs[1].clone(), Some( (raw.2.clone() - rhs_raw.2.clone()).into() ) ),
        (lhs[2].clone(), rhs[2].clone(), None),
        (lhs[2].clone(), rhs[3].clone(), None),
        (lhs[2].clone(), rhs[4].clone(), None),

        (lhs[3].clone(), rhs[0].clone(), None),
        (lhs[3].clone(), rhs[1].clone(), None),
        (lhs[3].clone(), rhs[2].clone(), None),
        (lhs[3].clone(), rhs[3].clone(), Some( (raw.3.clone() - rhs_raw.3.clone()).unwrap().into() ) ),
        (lhs[3].clone(), rhs[4].clone(), None),

        (lhs[4].clone(), rhs[0].clone(), None),
        (lhs[4].clone(), rhs[1].clone(), None),
        (lhs[4].clone(), rhs[2].clone(), None),
        (lhs[4].clone(), rhs[3].clone(), None),
        (lhs[4].clone(), rhs[4].clone(), None) 
    ];
    for (a, b, result) in sub_expect {
        assert_eq!( (a - b).ok(), result );
    }

    //Multiplication
    let mul_expect = [
        (lhs[0].clone(), rhs[0].clone(), Some( (raw.0 * rhs_raw.0).into() ) ), //Sca sca
        (lhs[0].clone(), rhs[1].clone(), Some( (Complex::from(raw.0) * rhs_raw.1).into() ) ), //Sca cmp
        (lhs[0].clone(), rhs[2].clone(), Some( (rhs_raw.2.clone() * raw.0.as_scalar()).into() ) ), //Sca vec
        (lhs[0].clone(), rhs[3].clone(), Some( (rhs_raw.3.clone() * raw.0.as_scalar()).into() ) ), //Sca mat
        (lhs[0].clone(), rhs[4].clone(), None),

        (lhs[1].clone(), rhs[0].clone(), Some( (rhs_raw.1 * Complex::from(raw.0)).into() ) ), //Cmp sca
        (lhs[1].clone(), rhs[1].clone(), Some( (raw.1 * rhs_raw.1).into() ) ), //Cmp cmp
        (lhs[1].clone(), rhs[2].clone(), None),
        (lhs[1].clone(), rhs[3].clone(), None),
        (lhs[1].clone(), rhs[4].clone(), None),
        
        (lhs[2].clone(), rhs[0].clone(), None),
        (lhs[2].clone(), rhs[1].clone(), None),
        (lhs[2].clone(), rhs[2].clone(), None),
        (lhs[2].clone(), rhs[3].clone(), None),
        (lhs[2].clone(), rhs[4].clone(), None),

        (lhs[3].clone(), rhs[0].clone(), None),
        (lhs[3].clone(), rhs[1].clone(), None),
        (lhs[3].clone(), rhs[2].clone(), None),
        (lhs[3].clone(), rhs[3].clone(), (raw.3.clone() * rhs_raw.3.clone()).ok().map(|x| x.into()) ), //Mat mat
        (lhs[3].clone(), rhs[4].clone(), None),

        (lhs[4].clone(), rhs[0].clone(), None),
        (lhs[4].clone(), rhs[1].clone(), None),
        (lhs[4].clone(), rhs[2].clone(), None),
        (lhs[4].clone(), rhs[3].clone(), None),
        (lhs[4].clone(), rhs[4].clone(), None) 
    ];

    for (a, b, result) in mul_expect {
        assert_eq!( (a * b).ok(), result );
    }

    //Division
    let div_expect = [
        (lhs[0].clone(), rhs[0].clone(), Some( (raw.0 / rhs_raw.0).into() ) ), //Sca sca
        (lhs[0].clone(), rhs[1].clone(), Some( (Complex::from(raw.0) / rhs_raw.1 ).into() ) ), //Sca cmp
        (lhs[0].clone(), rhs[2].clone(), Some( (raw.2.clone() / raw.0.as_scalar()).into() ) ), //Sca vec
        (lhs[0].clone(), rhs[3].clone(), Some( (raw.3.clone() / raw.0.as_scalar()).into() ) ), //Sca mat
        (lhs[0].clone(), rhs[4].clone(), None),

        (lhs[1].clone(), rhs[0].clone(), Some( (raw.1 / Complex::from(rhs_raw.0) ).into() ) ), //Cmp sca
        (lhs[1].clone(), rhs[1].clone(), Some( (raw.1 / rhs_raw.1).into() ) ), //Cmp cmp
        (lhs[1].clone(), rhs[2].clone(), None),
        (lhs[1].clone(), rhs[3].clone(), None),
        (lhs[1].clone(), rhs[4].clone(), None),
        
        (lhs[2].clone(), rhs[0].clone(), Some( (raw.2.clone() / raw.0.as_scalar()).into() ) ), //Vec sca
        (lhs[2].clone(), rhs[1].clone(), None),
        (lhs[2].clone(), rhs[2].clone(), None),
        (lhs[2].clone(), rhs[3].clone(), None),
        (lhs[2].clone(), rhs[4].clone(), None),

        (lhs[3].clone(), rhs[0].clone(), Some( (raw.3.clone() / raw.0.as_scalar()).into() ) ), //Mat sca
        (lhs[3].clone(), rhs[1].clone(), None),
        (lhs[3].clone(), rhs[2].clone(), None),
        (lhs[3].clone(), rhs[3].clone(), None),
        (lhs[3].clone(), rhs[4].clone(), None),

        (lhs[4].clone(), rhs[0].clone(), None),
        (lhs[4].clone(), rhs[1].clone(), None),
        (lhs[4].clone(), rhs[2].clone(), None),
        (lhs[4].clone(), rhs[3].clone(), None),
        (lhs[4].clone(), rhs[4].clone(), None)
    ];
    
    for (a, b, result) in div_expect {
        assert_eq!( (a / b).ok(), result );
    }

    //Pow
    let pow_expect = [
        (lhs[0].clone(), rhs[0].clone(), Some( (raw.0.pow(rhs_raw.0)).into() ) ), //Sca sca
        (lhs[0].clone(), rhs[1].clone(), Some( rhs_raw.1.pow_sca(raw.0).into() ) ), //Sca cmp
        (lhs[0].clone(), rhs[2].clone(), None),
        (lhs[0].clone(), rhs[3].clone(), None),
        (lhs[0].clone(), rhs[4].clone(), None),

        (lhs[1].clone(), rhs[0].clone(), Some( raw.1.pow_sca(rhs_raw.0).into() ) ), //Cmp sca
        (lhs[1].clone(), rhs[1].clone(), Some( raw.1.pow(rhs_raw.1).into() ) ), //Cmp cmp
        (lhs[1].clone(), rhs[2].clone(), None),
        (lhs[1].clone(), rhs[3].clone(), None),
        (lhs[1].clone(), rhs[4].clone(), None),
        
        (lhs[2].clone(), rhs[0].clone(), None),
        (lhs[2].clone(), rhs[1].clone(), None),
        (lhs[2].clone(), rhs[2].clone(), None),
        (lhs[2].clone(), rhs[3].clone(), None),
        (lhs[2].clone(), rhs[4].clone(), None),

        (lhs[3].clone(), rhs[0].clone(), raw.3.powf(rhs_raw.0.as_scalar()).ok().map(|x| x.into()) ), //Mat sca
        (lhs[3].clone(), rhs[1].clone(), None),
        (lhs[3].clone(), rhs[2].clone(), None),
        (lhs[3].clone(), rhs[3].clone(), None),
        (lhs[3].clone(), rhs[4].clone(), None),

        (lhs[4].clone(), rhs[0].clone(), None),
        (lhs[4].clone(), rhs[1].clone(), None),
        (lhs[4].clone(), rhs[2].clone(), None),
        (lhs[4].clone(), rhs[3].clone(), None),
        (lhs[4].clone(), rhs[4].clone(), None)
    ];

    for (a, b, result) in pow_expect {
        assert_eq!( a.pow(b).ok(), result );
    }
}