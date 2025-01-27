pub mod scalar;
pub mod complex;
pub mod vector;
pub mod matrix;
pub mod variable_type;
pub mod calc_error;

use std::ops::{Add, Sub, Mul, Div};
use std::fmt::{Display, Debug};

use serde::{Serialize, Deserialize};

pub use variable_type::{VariableData, VariableType};
pub use scalar::Scalar;
pub use complex::Complex;
pub use vector::MathVector;
pub use matrix::Matrix;
pub use calc_error::CalcResult;

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
impl Add for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn add(self, rhs: Self) -> Self::Output {
        self.get_ref().add(rhs.get_ref())
    }
}
impl Sub for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn sub(self, rhs: Self) -> Self::Output {
        self.get_ref().sub(rhs.get_ref())
    }
}
impl Mul for VariableUnion {
    type Output = CalcResult<VariableUnion>;
    fn mul(self, rhs: Self) -> Self::Output {
        self.get_ref().mul(rhs.get_ref())
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
            Self::Sca(ref a) => a.get_type(),
            Self::Cmp(ref a) => a.get_type(),
            Self::Vec(ref a) => a.get_type(),
            Self::Mat(ref a) => a.get_type()
        }
    }
}

impl VariableUnion {
    pub fn get_ref(&self) -> VariableUnionRef<'_> {
        match self {
            Self::Sca(ref s) => VariableUnionRef::Sca(s),
            Self::Cmp(ref s) => VariableUnionRef::Cmp(s),
            Self::Vec(ref s) => VariableUnionRef::Vec(s),
            Self::Mat(ref s) => VariableUnionRef::Mat(s)
        }
    }
    pub fn get_ref_mut(&mut self) -> VariableUnionRefMut<'_> {
        match self {
            Self::Sca(ref mut s) => VariableUnionRefMut::Sca(s),
            Self::Cmp(ref mut s) => VariableUnionRefMut::Cmp(s),
            Self::Vec(ref mut s) => VariableUnionRefMut::Vec(s),
            Self::Mat(ref mut s) => VariableUnionRefMut::Mat(s)
        }
    }
}


pub enum VariableUnionRef<'a> {
    Sca(&'a Scalar),
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

impl Add for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
    fn add(self, rhs: Self) -> Self::Output {
        todo!()
    }
}
impl Sub for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
    fn sub(self, rhs: Self) -> Self::Output {
        todo!()
    }
}
impl Mul for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
    fn mul(self, rhs: Self) -> Self::Output {
        todo!()   
    }
}
impl Div for VariableUnionRef<'_> {
    type Output = CalcResult<VariableUnion>;
    fn div(self, rhs: Self) -> Self::Output {
        todo!()
    }
}

pub enum VariableUnionRefMut<'a> {
    Sca(&'a mut Scalar),
    Cmp(&'a mut Complex),
    Vec(&'a mut MathVector),
    Mat(&'a mut Matrix)
}