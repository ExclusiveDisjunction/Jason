use serde::{Serialize, Deserialize};

use std::fmt::Display;
use std::ops::{Neg, Add, Sub, Mul, Div};

use super::prelude::LogicalCmp;
use super::super::err::{UndefinedUniOperation, PowError, BiOperationError};

use crate::calc::LiteralReference;
use crate::calc::err::UndefinedBiOperation;
use crate::calc::literal::composite::Composite;
use crate::calc::literal::numeric::Numeric;
use crate::prelude::FlatType;

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum Literal {
    Num(Numeric),
    Comp(Composite),
    Bool(bool)
}
impl Default for Literal {
    fn default() -> Self {
        Self::Num(Default::default())
    }
}
impl Neg for Literal {
    type Output = Result<Literal, UndefinedUniOperation>;
    fn neg(self) -> Self::Output {
        match self {
            Self::Num(v) => Ok( Self::Num(-v) ),
            Self::Comp(v) => Ok( Self::Comp(-v) ),
            Self::Bool(_) => Err( UndefinedUniOperation::new("-", FlatType::Boolean) )
        }
    }
}
impl Add for Literal {
    type Output = Result<Self, BiOperationError>;
    fn add(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Self::Num(a + b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Self::Comp( (a + b)? ) ),
            (a, b) => Err( BiOperationError::new_undef("+", a.flat_type(), b.flat_type()) )
        }
    }
}
impl Sub for Literal {
    type Output = Result<Self, BiOperationError>;
    fn sub(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Self::Num(a - b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Self::Comp( (a - b)? ) ),
            (a, b) => Err( BiOperationError::new_undef("-", a.flat_type(), b.flat_type()) )
        }
    }
}
impl Mul for Literal {
    type Output = Result<Self, BiOperationError>;
    fn mul(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Self::Num(a * b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Self::Comp( (a * b)? ) ),
            (Self::Num(a), Self::Comp(b)) | (Self::Comp(b), Self::Num(a)) => Ok( Self::Comp( b * a ) ),
            (a, b) => Err( BiOperationError::new_undef("*", a.flat_type(), b.flat_type()) )
        }
    }
}
impl Div for Literal {
    type Output = Result<Self, BiOperationError>;
    fn div(self, rhs: Self) -> Self::Output {
         match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Literal::Num(a * b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Literal::Comp( (a * b)? ) ),
            (Self::Comp(a), Self::Num(b)) => Ok( Literal::Comp( a / b ) ),
            (a, b) => Err( BiOperationError::new_undef("/", a.flat_type(), b.flat_type()) )
        }
    }
}

impl From<Numeric> for Literal {
    fn from(value: Numeric) -> Self {
        Self::Num(value)
    }
}
impl From<Composite> for Literal {
    fn from(value: Composite) -> Self {
        Self::Comp(value)
    }
}
impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.get_ref() as &dyn Display).fmt(f)
    }
}

impl LogicalCmp for Literal {
    fn oper_eq(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( a == b ),
            (Self::Comp(a), Self::Comp(b)) => Ok( a == b ),
            (Self::Bool(a), Self::Bool(b)) => Ok( a == b ),
            (a, b) => Err( UndefinedBiOperation::new("==", a.flat_type(), b.flat_type()) )
        }
    }
    fn oper_less(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        if let Self::Num(a) = self && let Self::Num(b) = rhs {
            a.oper_less(b)
        }
        else {
            Err( UndefinedBiOperation::new("<", self.flat_type(), rhs.flat_type()) )
        }
    }
    fn oper_greater(&self, rhs: &Self) -> Result<bool, UndefinedBiOperation> {
        if let Self::Num(a) = self && let Self::Num(b) = rhs {
            a.oper_greater(b)
        }
        else {
            Err( UndefinedBiOperation::new("<", self.flat_type(), rhs.flat_type()) )
        }
    }
}

impl Literal {
    pub fn get_ref<'a>(&'a self) -> LiteralReference<'a> {
        match self {
            Self::Num(v) => LiteralReference::Num(*v),
            Self::Bool(v) => LiteralReference::Bool(*v),
            Self::Comp(v) => LiteralReference::Comp(v.get_ref())
        }
    }

    pub fn pow(self, rhs: Self) -> Result<Self, PowError> {
        self.get_ref().pow(rhs.get_ref())
    }

    pub fn static_type() -> FlatType {
        FlatType::Any
    }
    pub fn flat_type(&self) -> FlatType {
        match self {
            Self::Num(v) => v.flat_type(),
            Self::Bool(_) => FlatType::Boolean,
            Self::Comp(v) => v.flat_type()
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

/*
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_ops() {
        //We are primarily testing the operators.

        let raw = (
            Scalar::new(4.0), 
            Complex::new(1.0, 2.5),
            FloatVector::from([1.0, 4.0, 3.0]),
            FloatMatrix::identity(2),
            Boolean::True
        );

        let raw_wrapped: [Literal; 5] = [
            Literal::Sca(raw.0),
            Literal::Cmp(raw.1),
            Literal::Vec(raw.2.clone()),
            Literal::Mat(raw.3.clone()),
            Literal::Bool(raw.4)
        ];

        // Conversion
        let lhs: [Literal; 5] = [raw.0.into(), raw.1.into(), raw.2.clone().into(), raw.3.clone().into(), raw.4.into()];
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

        let rhs: [Literal; 5] = [
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
    fn literal_comparisons() {
        let raw = (
            Scalar::new(1.0),
            Complex::new(1.2, 3.4),
            Boolean::True,
            FloatVector::from([1.0, 2.2, 3.3]),
            FloatMatrix::identity(2)
        );

        let wrap: [Literal; 5] = [
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

        let other_sca: Literal = Scalar::new(1.2).into();
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
     */