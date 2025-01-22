use super::variable_type::*;
use super::scalar::{Scalar, ScalarLike};
use super::calc_error::{DimensionError, OperationError, CalcError, CalcResult};
use std::ops::{Add, Sub, Mul, Div, Index, IndexMut};
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

impl MathVector {
    pub fn from<T>(data: &[T]) -> Self where T: ScalarLike {
        Self {
            data: data.iter().map(|x| x.as_scalar()).collect()
        }
    }
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
            Some( self.data[0].atan2(self.data[1]) )
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
    pub fn cross_product(&self, rhs: &Self) -> CalcResult<Self, usize> {
        if self.dim() != rhs.dim() {
            return Err(CalcError::dimension(DimensionError::new(self.dim(), rhs.dim())))
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
                return Err(CalcError::operation(OperationError::new_fmt("X", self, rhs, Some("dimension must be either 2 or 3 for cross product"))));
            }
        }

        Ok(
            MathVector::from(
                &[
                    a.1 * b.2 - a.2 * b.1,
                    a.2 * b.0 - a.0 * b.2,
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
    fn get_type() -> VariableType {
        VariableType::Vector
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

        return a;
    }
}
impl Sub for MathVector {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
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
            a.data[i] -= elem;
        }

        return a;
    }
}
impl<T> Mul<T> for MathVector where T: ScalarLike {
    type Output = Self;

    fn mul(self, rhs: T) -> Self::Output {
        let b = rhs.as_scalar();
        let result: Vec<f64> = self.data.into_iter().map(|x| x * b).collect();
        MathVector::from(&result)
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
        MathVector::from(&result)
    }
}

#[test]
fn test_vector() {

}