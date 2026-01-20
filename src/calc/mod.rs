pub mod prelude;
pub mod num;
pub mod err;

pub mod complex;
pub mod vector;
pub mod matrix;
//pub mod func;
pub mod literal;

pub use complex::Complex;
pub use vector::{MathVector, FloatVector};
pub use matrix::{Matrix, FloatMatrix};

pub use literal::{Literal, LiteralReference};