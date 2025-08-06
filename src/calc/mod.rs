pub mod scalar;
pub mod complex;
pub mod vector;
pub mod matrix;
pub mod base;
pub mod calc_error;
pub mod func;
pub mod bool;
pub mod union;
pub mod err;

pub use base::{VariableData, VariableType};
pub use scalar::{Scalar, ScalarLike};
pub use complex::Complex;
pub use vector::MathVector;
pub use matrix::Matrix;
pub use bool::Boolean;
pub use calc_error::{CalcResult, OperationError, CalcError};

pub use union::{VariableUnion, VariableUnionRef, VariableUnionMut};