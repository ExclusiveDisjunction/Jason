use super::super::scalar::Scalar;
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::MathVector;
use super::super::matrix::Matrix;

pub enum VariableUnionMut<'a> {
    Sca(&'a mut Scalar),
    Cmp(&'a mut Complex),
    Vec(&'a mut MathVector),
    Mat(&'a mut Matrix),
    Bool(&'a mut Boolean)
}