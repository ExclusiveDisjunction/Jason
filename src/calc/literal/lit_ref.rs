use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Sub};

use super::prelude::LogicalCmp;
use super::lit::Literal;
use crate::calc::err::{BiOperationError, PowError, UndefinedBiOperation, UndefinedUniOperation};
use crate::calc::literal::composite::CompositeRef;
use crate::calc::literal::numeric::Numeric;
use crate::calc::matrix::MatrixPowError;
use crate::prelude::FlatType;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralReference<'a> {
    Num(Numeric),
    Bool(bool),
    Comp(CompositeRef<'a>)
}

impl From<Numeric> for LiteralReference<'static> {
    fn from(value: Numeric) -> Self {
        Self::Num(value)
    }
}
impl From<bool> for LiteralReference<'static> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
impl<'a> From<CompositeRef<'a>> for LiteralReference<'a> {
    fn from(value: CompositeRef<'a>) -> Self {
        Self::Comp(value)
    }
}
impl<'a> From<&'a Literal> for LiteralReference<'a> {
    fn from(value: &'a Literal) -> Self {
        value.get_ref()
    }
}

impl Display for LiteralReference<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Num(v) => v,
            Self::Bool(v) => v,
            Self::Comp(v) => v
        };

        x.fmt(f)
    }
}

impl Neg for LiteralReference<'_> {
    type Output = Result<Literal, UndefinedUniOperation>;
    fn neg(self) -> Self::Output {
        match self {
            Self::Num(v) => Ok( Literal::Num(-v) ),
            Self::Comp(v) => Ok( Literal::Comp(-v) ),
            Self::Bool(_) => Err( UndefinedUniOperation::new("-", FlatType::Boolean) )
        }
    }
}
impl Add for LiteralReference<'_>  {
    type Output = Result<Literal, BiOperationError>;
    fn add(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Literal::Num(a + b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Literal::Comp( (a + b)? ) ),
            (a, b) => Err( BiOperationError::new_undef("+", a.flat_type(), b.flat_type()) )
        }
    }
}
impl Sub for LiteralReference<'_> {
    type Output = Result<Literal, BiOperationError>;
    fn sub(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Literal::Num(a - b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Literal::Comp( (a - b)? ) ),
            (a, b) => Err( BiOperationError::new_undef("-", a.flat_type(), b.flat_type()) )
        }
    }
}
impl Mul for LiteralReference<'_>  {
    type Output = Result<Literal, BiOperationError>;
    fn mul(self, rhs: Self) -> Self::Output {
        //Some of these have optimizations.
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Literal::Num(a * b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Literal::Comp( (a * b)? ) ),
            (Self::Num(a), Self::Comp(b)) | (Self::Comp(b), Self::Num(a)) => Ok( Literal::Comp( b * a ) ),
            (a, b) => Err( BiOperationError::new_undef("*", a.flat_type(), b.flat_type()) )
        }
    }
}
impl Div for LiteralReference<'_>  {
    type Output = Result<Literal, BiOperationError>;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Literal::Num(a * b) ),
            (Self::Comp(a), Self::Comp(b)) => Ok( Literal::Comp( (a * b)? ) ),
            (Self::Comp(a), Self::Num(b)) => Ok( Literal::Comp( a / b ) ),
            (a, b) => Err( BiOperationError::new_undef("/", a.flat_type(), b.flat_type()) )
        }
    }
}

impl LogicalCmp for LiteralReference<'_> {
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

impl LiteralReference<'_> {
    pub fn to_literal(&self) -> Literal {
        match self {
            Self::Num(v) => Literal::Num(*v),
            Self::Bool(v) => Literal::Bool(*v),
            Self::Comp(v) => Literal::Comp(v.to_composite())
        }
    }

    pub fn pow(self, rhs: Self) -> Result<Literal, PowError> {
        match (self, rhs) {
            (Self::Num(a), Self::Num(b)) => Ok( Literal::Num(a.pow(b)) ),
            (Self::Comp(CompositeRef::Mat(a)), Self::Num(Numeric::Integer(b))) => {
                match a.powi(b) {
                    Ok(v) => Ok( Literal::Comp(v.into()) ),
                    Err(_) => Err( 
                        PowError::Mat(MatrixPowError::NonSquare)
                    )
                }
            },
            (a, b) => Err( PowError::from(
                UndefinedBiOperation::new("^", a.flat_type(), b.flat_type())
            ) )
        }
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
#[cfg(test)]
mod tests {
    use super::*;

    struct LiteralTester {
        lhs_raw: (Scalar, Complex, FloatVector, FloatMatrix, Boolean),
        lhs: [Literal; 5],
        rhs_raw: (Scalar, Complex, FloatVector, FloatMatrix, Boolean),
        rhs: [Literal; 5],
        results: [[Option<Literal>; 5]; 5],
    }
    impl Default for LiteralTester {
        fn default() -> Self {
            let lhs_raw = (
                Scalar::new(4.0),
                Complex::new(1.0, 2.5),
                FloatVector::from([1.0, 4.0, 3.0]),
                FloatMatrix::identity(2),
                Boolean::True,
            );
            let rhs_raw = (
                Scalar::new(1.0),
                Complex::new(2.1, 4.0),
                FloatVector::from([2.0, 1.0, -4.0]),
                FloatMatrix::try_from(vec![vec![1.0, 4.0], vec![5.0, 2.0]]).unwrap(),
                Boolean::False,
            );

            Self {
                lhs: [
                    lhs_raw.0.into(),
                    lhs_raw.1.into(),
                    lhs_raw.2.clone().into(),
                    lhs_raw.3.clone().into(),
                    lhs_raw.4.into(),
                ],
                rhs: [
                    rhs_raw.0.into(),
                    rhs_raw.1.into(),
                    rhs_raw.2.clone().into(),
                    rhs_raw.3.clone().into(),
                    rhs_raw.4.into(),
                ],
                lhs_raw,
                rhs_raw,
                results: core::array::from_fn(|_| [const { None }; 5]),
            }
        }
    }
    impl LiteralTester {
        fn new() -> Self {
            Self::default()
        }
    }

    fn perform_test<F>(val: &[(usize, usize, Option<Literal>)], oper: F)
    where
        F: Fn() -> Option<Literal>,
    {
    }

    #[test]
    fn test_variable_union_ref() {
        //We are primarily testing the operators.

        let raw = (
            Scalar::new(4.0),
            Complex::new(1.0, 2.5),
            FloatVector::from([1.0, 4.0, 3.0]),
            FloatMatrix::identity(2),
            Boolean::True,
        );

        let as_union: [Literal; 5] = [
            raw.0.into(),
            raw.1.into(),
            raw.2.clone().into(),
            raw.3.clone().into(),
            raw.4.into(),
        ];
        let lhs: [LiteralReference; 5] = [
            raw.0.into(),
            raw.1.into(),
            (&raw.2).into(),
            (&raw.3).into(),
            raw.4.into(),
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
            Boolean::False,
        );

        let rhs: [LiteralReference; 5] = [
            rhs_raw.0.into(),
            rhs_raw.1.into(),
            (&rhs_raw.2).into(),
            (&rhs_raw.3).into(),
            rhs_raw.4.into(),
        ];

        let add_expect = [
            (0, 0, Some((raw.0 + rhs_raw.0).into())),
            (0, 1, Some((Complex::from(raw.0) + rhs_raw.1).into())),
            (0, 2, None),
            (0, 3, None),
            (0, 4, None),
            (1, 0, Some((raw.1 + Complex::from(rhs_raw.0)).into())),
            (1, 1, Some((raw.1 + rhs_raw.1).into())),
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            (2, 0, None),
            (2, 1, None),
            (2, 2, Some((raw.2.clone() + rhs_raw.2.clone()).into())),
            (2, 3, None),
            (2, 4, None),
            (3, 0, None),
            (3, 1, None),
            (3, 2, None),
            (
                3,
                3,
                Some((raw.3.clone() + rhs_raw.3.clone()).unwrap().into()),
            ),
            (3, 4, None),
            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None),
        ];
        for (a, b, result) in add_expect {
            let a_v = lhs[a];
            let b_v = rhs[b];
            assert_eq!((a_v + b_v).ok(), result, "lhs: {a}, rhs: {b}");
        }
        //Subtraction
        let sub_expect = [
            (0, 0, Some((raw.0 - rhs_raw.0).into())),
            (0, 1, Some((Complex::from(raw.0) - rhs_raw.1).into())),
            (0, 2, None),
            (0, 3, None),
            (0, 4, None),
            (1, 0, Some((raw.1 - Complex::from(rhs_raw.0)).into())),
            (1, 1, Some((raw.1 - rhs_raw.1).into())),
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            (2, 0, None),
            (2, 1, None),
            (2, 2, Some((raw.2.clone() - rhs_raw.2.clone()).into())),
            (2, 3, None),
            (2, 4, None),
            (3, 0, None),
            (3, 1, None),
            (3, 2, None),
            (
                3,
                3,
                Some((raw.3.clone() - rhs_raw.3.clone()).unwrap().into()),
            ),
            (3, 4, None),
            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None),
        ];
        for (a, b, result) in sub_expect {
            let a_v = lhs[a];
            let b_v = rhs[b];
            assert_eq!((a_v - b_v).ok(), result, "lhs: {a}, rhs: {b}");
        }

        //Multiplication
        let mul_expect = [
            (0, 0, Some((raw.0 * rhs_raw.0).into())), //Sca sca
            (
                0,
                1,
                Some((Complex::from(raw.0) * rhs_raw.1).into()),
            ), //Sca cmp
            (
                0,
                2,
                Some((rhs_raw.2.clone() * raw.0.as_scalar()).into()),
            ), //Sca vec
            (
                0,
                3,
                Some((rhs_raw.3.clone() * raw.0.as_scalar()).into()),
            ), //Sca mat
            (0, 4, None),
            (
                1,
                0,
                Some((raw.1 * Complex::from(rhs_raw.0)).into()),
            ), //Cmp sca
            (1, 1, Some((raw.1 * rhs_raw.1).into())), //Cmp cmp
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            (
                2,
                0,
                Some((raw.2.clone() * rhs_raw.0.as_scalar()).into()),
            ),
            (2, 1, None),
            (2, 2, None),
            (2, 3, None),
            (2, 4, None),
            (
                3,
                0,
                Some( (raw.3.clone() * rhs_raw.0.as_scalar()).into() )
            ),
            (3, 1, None),
            (3, 2, None),
            (
                3,
                3,
                (raw.3.clone() * rhs_raw.3.clone()).ok().map(|x| x.into()),
            ), //Mat mat
            (3, 4, None),
            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None),
        ];

        for (a, b, result) in mul_expect{
            let av = lhs[a];
            let bv = rhs[b];
            assert_eq!(
                (
                    av * bv
                ).ok(), 
                result, 
                "From result ({a}: {av}, {b}: {bv})"
            );
        }

        //Division
        let div_expect = [
            (0, 0, Some((raw.0 / rhs_raw.0).into())), //Sca sca
            (
                0,
                1,
                Some((Complex::from(raw.0) / rhs_raw.1).into()),
            ), //Sca cmp
            (0, 2, None), //Sca vec
            (0, 3, None),
            (0, 4, None),
            (
                1,
                0,
                Some((raw.1 / Complex::from(rhs_raw.0)).into()),
            ), //Cmp sca
            (1, 1, Some((raw.1 / rhs_raw.1).into())), //Cmp cmp
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            (
                2,
                0,
                Some((raw.2.clone() / rhs_raw.0.as_scalar()).into()),
            ), //Vec sca
            (2, 1, None),
            (2, 2, None),
            (2, 3, None),
            (2, 4, None),
            (
                3,
                0,
                Some((raw.3.clone() / rhs_raw.0.as_scalar()).into()),
            ), //Mat sca
            (3, 1, None),
            (3, 2, None),
            (3, 3, None),
            (3, 4, None),
            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None),
        ];

        for (a, b, result) in div_expect {
            assert_eq!(
                (
                    lhs[a] / rhs[b]
                ).ok(), 
                result,
                "From result ({a},{b})"
            );
        }

        //Pow
        let pow_expect = [
            (0, 0, Some((raw.0.pow(rhs_raw.0)).into())), //Sca sca
            (0, 1, Some(rhs_raw.1.pow_sca(raw.0).into())), //Sca cmp
            (0, 2, None),
            (0, 3, None),
            (0, 4, None),
            (1, 0, Some(raw.1.pow_sca(rhs_raw.0).into())), //Cmp sca
            (1, 1, Some(raw.1.pow(rhs_raw.1).into())),     //Cmp cmp
            (1, 2, None),
            (1, 3, None),
            (1, 4, None),
            (2, 0, None),
            (2, 1, None),
            (2, 2, None),
            (2, 3, None),
            (2, 4, None),
            (
                3,
                0,
                raw.3.powf(rhs_raw.0.as_scalar()).ok().map(|x| x.into()),
            ), //Mat sca
            (3, 1, None),
            (3, 2, None),
            (3, 3, None),
            (3, 4, None),
            (4, 0, None),
            (4, 1, None),
            (4, 2, None),
            (4, 3, None),
            (4, 4, None),
        ];

        for (a, b, result) in pow_expect {
            assert_eq!(
                lhs[a].pow(rhs[b]).ok(),
                result,
                "Result ({a}, {b})"
            );
        }
    }
}

 */