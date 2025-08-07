use serde::{Serialize, Deserialize};

use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use super::core::LogicalCmp;
use super::super::scalar::{Scalar, ScalarLike};
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::FloatVector;
use super::super::matrix::FloatMatrix;
use super::super::err::{UndefinedUniOperation, PowError, BiOperationError};

use crate::calc::err::UndefinedBiOperation;
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
impl From<bool> for VariableUnion {
    fn from(value: bool) -> Self {
        Self::Bool (
            if value {
                Boolean::True
            }
            else {
                Boolean::False
            }
        )
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

impl LogicalCmp for VariableUnion {
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
            (Self::Sca(a), Self::Cmp(b)) => Ok( Complex::from(a).pow(b).into() ),
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

#[cfg(test)]
mod tests {
    use super::*;

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
            let a_v = lhs[a].clone();
            let b_v = rhs[b].clone();
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
            let a_v = lhs[a].clone();
            let b_v = rhs[b].clone();
            assert_eq!( (a_v - b_v).ok(), result, "lhs: {a}, rhs: {b}");
        }

        //Multiplication
        let mul_expect = [
            (0, 0, Some( (raw.0 * rhs_raw.0).into() ) ), //Sca sca
            (0, 1, Some( (Complex::from(raw.0) * rhs_raw.1).into() ) ), //Sca cmp
            (0, 2, Some( (rhs_raw.2.clone() * raw.0.as_scalar()).into() ) ), //Sca vec
            (0, 3, Some( (rhs_raw.3.clone() * raw.0.as_scalar()).into() ) ), //Sca mat
            (0, 4, None),

            (1, 0, Some( (raw.1 * Complex::from(rhs_raw.0)).into() ) ), //Cmp sca
            (1, 1, Some( (raw.1 * rhs_raw.1).into() ) ), //Cmp cmp
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            
            (2, 0, Some( (raw.2.clone() * rhs_raw.0.as_scalar()).into() ) ),
            (2, 1, None),
            (2, 2, None),
            (2, 3, None),
            (2, 4, None),

            (3, 0, Some( (raw.3.clone() * rhs_raw.0.as_scalar()).into() ) ),
            (3, 1, None),
            (3, 2, None),
            (3, 3, (raw.3.clone() * rhs_raw.3.clone()).ok().map(|x| x.into()) ), //Mat mat
            (3, 4, None),

            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None) 
        ];

        for (a, b, result) in mul_expect {
            let a_v = lhs[a].clone();
            let b_v = rhs[b].clone();
            assert_eq!( (a_v * b_v).ok(), result, "lhs: {a}, rhs: {b}");
        }

        //Division
        let div_expect = [
            (0, 0, Some( (raw.0 / rhs_raw.0).into() ) ), //Sca sca
            (0, 1, Some( (Complex::from(raw.0) / rhs_raw.1 ).into() ) ), //Sca cmp
            (0, 2, None),
            (0, 3, None),
            (0, 4, None),

            (1, 0, Some( (raw.1 / Complex::from(rhs_raw.0) ).into() ) ), //Cmp sca
            (1, 1, Some( (raw.1 / rhs_raw.1).into() ) ), //Cmp cmp
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            
            (2, 0, Some( (raw.2.clone() / rhs_raw.0.as_scalar()).into() ) ), //Vec sca
            (2, 1, None),
            (2, 2, None),
            (2, 3, None),
            (2, 4, None),

            (3, 0, Some( (raw.3.clone() / rhs_raw.0.as_scalar()).into() ) ), //Mat sca
            (3, 1, None),
            (3, 2, None),
            (3, 3, None),
            (3, 4, None),

            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None)
        ];
        
        for (a, b, result) in div_expect {
            let a_v = lhs[a].clone();
            let b_v = rhs[b].clone();
            assert_eq!( (a_v / b_v).ok(), result, "lhs: {a}, rhs: {b}");
        }

        //Pow
        let pow_expect = [
            (0, 0, Some( (raw.0.pow(rhs_raw.0)).into() ) ), //Sca sca
            (0, 1, Some( Complex::from(raw.0).pow(rhs_raw.1).into() ) ), //Sca cmp
            (0, 2, None),
            (0, 3, None),
            (0, 4, None),

            (1, 0, Some( raw.1.pow_sca(rhs_raw.0).into() ) ), //Cmp sca
            (1, 1, Some( raw.1.pow(rhs_raw.1).into() ) ), //Cmp cmp
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            
            (2, 0, None),
            (2, 1, None),
            (2, 2, None),
            (2, 3, None),
            (2, 4, None),

            (3, 0, raw.3.powf(rhs_raw.0.as_scalar()).ok().map(|x| x.into()) ), //Mat sca
            (3, 1, None),
            (3, 2, None),
            (3, 3, None),
            (3, 4, None),

            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None)
        ];

        for (a, b, result) in pow_expect {
            let a_v = lhs[a].clone();
            let b_v = rhs[b].clone();
            assert_eq!( a_v.pow(b_v).ok(), result, "lhs: {a}, rhs: {b}");
        }
    }

    #[test]
    fn var_union_cmp() {
        let raw = (
            Scalar::new(1.0),
            Complex::new(1.2, 3.4),
            Boolean::True,
            FloatVector::from([1.0, 2.2, 3.3]),
            FloatMatrix::identity(2)
        );

        let wrap: [VariableUnion; 5] = [
            raw.0.into(),
            raw.1.into(),
            raw.2.into(),
            raw.3.into(),
            raw.4.into()
        ];

        for (i, lhs) in wrap.iter().enumerate() {
            for (j, rhs) in wrap.iter().enumerate() {
                if i == j {
                    assert_eq!( lhs.oper_eq(rhs), Ok( true ) );
                    assert_eq!( lhs.oper_neq(rhs), Ok( false ) );
                }
                else {
                    assert!( lhs.oper_eq(rhs).is_err() );
                }
            }
        }

        let other_sca: VariableUnion = Scalar::new(1.2).into();
        let slice = &wrap[1..=4];
        assert_eq!(slice.len(), 4, "{:?} is not 4 elements...", &slice);

        for item in slice {
            assert!( item.oper_less(&other_sca).is_err() );
            assert!( item.oper_less_eq(&other_sca).is_err() );
            assert!( item.oper_greater(&other_sca).is_err() );
            assert!( item.oper_greater_eq(&other_sca).is_err() );
        }

        assert_eq!( wrap[0].oper_less(&other_sca), Ok( true ) );
        assert_eq!( wrap[0].oper_less_eq(&other_sca), Ok( true ) );
        assert_eq!( wrap[0].oper_less_eq(&wrap[0]), Ok( true ) );
        assert_eq!( wrap[0].oper_greater(&other_sca), Ok( false ) );
        assert_eq!( wrap[0].oper_greater_eq(&other_sca), Ok( false ) );
        assert_eq!( wrap[0].oper_greater_eq(&wrap[0]), Ok( true ) );
    }
}