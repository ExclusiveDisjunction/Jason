use super::super::scalar::Scalar;
use super::super::complex::Complex;
use super::super::bool::Boolean;
use super::super::vector::FloatVector;
use super::super::matrix::FloatMatrix;

pub enum VariableUnionMut<'a> {
    Sca(&'a mut Scalar),
    Cmp(&'a mut Complex),
    Vec(&'a mut FloatVector),
    Mat(&'a mut FloatMatrix),
    Bool(&'a mut Boolean)
}