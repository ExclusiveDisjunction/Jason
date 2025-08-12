pub mod prelude;
pub mod num;
pub mod err;

pub mod scalar;
pub mod complex;
pub mod vector;
pub mod matrix;
//pub mod func;
pub mod bool;
pub mod union;

pub use scalar::{Scalar, ScalarLike};
pub use complex::Complex;
pub use vector::{MathVector, FloatVector};
pub use matrix::{Matrix, FloatMatrix};
pub use bool::Boolean;

pub use union::{VariableUnion, VariableUnionRef, VariableUnionMut};