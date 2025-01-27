use super::variable_type::*;
use super::scalar::{Scalar, ScalarLike};
use super::calc_error::{DimensionError, OperationError, CalcError, CalcResult};
use std::ops::{Add, Sub, Mul, Div, Index, IndexMut, Neg};
use std::fmt::{Display, Debug, Formatter};
use std::iter::zip;
use serde::{Deserialize, Serialize};

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct MathVector {
    data: Vec<f64>
}

impl Display for MathVector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let joined: Vec<String> = self.data.iter().map(|x| x.to_string()).collect();
        write!(f, "[ {} ]", joined.join(", "))
    }
}
impl Debug for MathVector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}

impl<T> From<&[T]> for MathVector where T: ScalarLike {
    fn from(value: &[T]) -> Self {
        Self {
            data: value.iter().map(|x| x.as_scalar()).collect()
        }
    }
}
impl<T> From<Vec<T>> for MathVector where T: ScalarLike {
    fn from(value: Vec<T>) -> Self {
        Self {
            data: value.into_iter().map(|x| x.as_scalar()).collect()
        }
    }
}
impl MathVector {
    pub fn with_capacity(size: usize) -> Self {
        Self {
            data: vec![0.0; size]
        }
    }

    pub fn dim(&self) -> usize {
        self.data.len()
    }
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn magnitude(&self) -> f64 {
        self.dot_product(self).unwrap().sqrt() //note that the dot product returns error if the dims do not match, and when dot product with self, the dims cannot mismatch
    }
    pub fn angle(&self) -> Option<f64> {
        if self.dim() == 2 {
            Some( self.data[1].atan2(self.data[0]) )
        }
        else {
            None
        }
    }

    pub fn dot_product(&self, rhs: &Self) -> Result<f64, DimensionError<usize>> {
        if self.dim() != rhs.dim() {
            return Err(DimensionError::new(self.dim(), rhs.dim()));
        }

        let mut result: f64 = 0.0;
        for (a, b) in zip(self.data.iter(), rhs.data.iter()) {
            result += a * b;
        }

        Ok(result)
    }
    pub fn cross_product(&self, rhs: &Self) -> CalcResult<Self> {
        if self.dim() != rhs.dim() {
            return Err(CalcError::Dim(DimensionError::new(self.dim(), rhs.dim())))
        }

        let a: (f64, f64, f64);
        let b: (f64, f64, f64);

        match self.dim() {
            2 => {
                a = (self.data[0], self.data[1], 0.0);
                b = (rhs.data[0], rhs.data[1], 0.0);
            }
            3 => {
                a = (self.data[0], self.data[1], self.data[2]);
                b = (rhs.data[0], rhs.data[1], rhs.data[2]);
            }
            _ => {
                return Err(CalcError::Oper(OperationError::new_fmt("X", self, rhs, Some("dimension must be either 2 or 3 for cross product"))));
            }
        }

        /*
             i   j   k 
            a.0 a.1 a.2 
            b.0 b.1 b.2

            i(a.1 * b.2 - a.2 * b.1) - j(a.0 * b.2 - a.2 * b.0) + k(a.0 * b.1 - a.1 * b.0)
         */

        Ok(
            MathVector::from(
                vec![
                    a.1 * b.2 - a.2 * b.1,
                    a.0 * b.2 - a.2 * b.0,
                    a.0 * b.1 - a.1 * b.0
                ]
            )
        )
    }
}

impl Index<usize> for MathVector {
    type Output = f64;
    fn index(&self, index: usize) -> &Self::Output {
        self.data.index(index)
    }
}
impl IndexMut<usize> for MathVector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.data.index_mut(index)
    }
}

impl VariableData for MathVector {
    fn get_type(&self) -> VariableType {
        VariableType::Vector
    }
}

impl Neg for MathVector {
    type Output = Self;
    fn neg(self) -> Self::Output {
        let contents: Vec<f64> = self.data.into_iter().map(|x| -x).collect();
        Self::from(contents)
    }
}

impl Add for MathVector {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        let mut a: MathVector;
        let b: MathVector;

        if self.dim() < rhs.dim() {
            a = rhs;
            b = self;
        }
        else {
            a = self;
            b = rhs;
        }

        for (i, elem) in b.data.into_iter().enumerate() {
            a.data[i] += elem;
        }

        a
    }
}
impl Sub for MathVector {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        //Note that a - b != b - a, despite widening. an equivalent result is a + (-b).
        self + (-rhs)
    }
}
impl<T> Mul<T> for MathVector where T: ScalarLike {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        let b = rhs.as_scalar();
        let result: Vec<f64> = self.data.into_iter().map(|x| x * b).collect();
        MathVector::from(result)
    }
}
impl Mul<MathVector> for Scalar {
    type Output = MathVector;
    fn mul(self, rhs: MathVector) -> Self::Output {
        rhs * self
    }
}
impl<T> Div<T> for MathVector where T: ScalarLike {
    type Output = Self;
    fn div(self, rhs: T) -> Self::Output {
        let b = rhs.as_scalar();
        let result: Vec<f64> = self.data.into_iter().map(|x| x / b).collect();
        MathVector::from(result)
    }
}

#[test]
fn test_vector() {
    let a = MathVector::from(vec![1, 2, 3]);
    let b = MathVector::from(vec![2, 3]);
    let c = Scalar::new(4.0);

    //Negation
    assert_eq!(-a.clone(), MathVector::from(vec![-1, -2, -3]));

    //Index
    assert_eq!(a[0], 1.0);

    //Other operations
    assert!(a.dot_product(&b).is_err());
    assert_eq!(a.dot_product(&a).unwrap(), (1 * 1 + 2 * 2 + 3 * 3) as f64);
    assert_eq!(a.cross_product(&MathVector::from(vec![3, 2, 1])).unwrap(), MathVector::from(vec![-4, -8, -4]));
    assert_eq!(b.cross_product(&MathVector::from(vec![3, 2])).unwrap(), MathVector::from(vec![0, 0, -5]));

    //Properties
    assert_eq!(a.magnitude(), ((1 * 1 + 2 * 2 + 3 * 3) as f64).sqrt());
    assert!(a.angle().is_none());
    //Due to floating point error, this may fail. 
    assert_eq!(b.angle().unwrap(), 3f64.atan2(2.0));

    // a + b == b + a
    assert_eq!(a.clone() + b.clone(), MathVector::from(vec![3, 5, 3]));
    assert_eq!(b.clone() + a.clone(), a.clone() + b.clone());

    // a - b != b - a
    assert_eq!(a.clone() - b.clone(), MathVector::from(vec![-1, -1, 3]));
    assert_eq!(b.clone() - a.clone(), MathVector::from(vec![1, 1, -3]));

    // a * b == b * a
    assert_eq!(a.clone() * c, MathVector::from(vec![4, 8, 12]));
    assert_eq!(c * a.clone(), a.clone() * c);

    // b / a DNE
    assert_eq!(a.clone() / c, MathVector::from(vec![1.0/4.0, 2.0/4.0, 3.0/4.0]));
}