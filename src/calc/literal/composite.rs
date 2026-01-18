
use std::fmt::Display;

use serde::{Deserialize, Serialize};

use super::numeric::Numeric;
use crate::calc::{MathVector, Matrix};

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum Composite {
    Vec(MathVector<Numeric>),
    Mat(Matrix<Numeric>)
}
impl Display for Composite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Vec(v) => v,
            Self::Mat(v) => v
        };

        x.fmt(f)
    }
}
