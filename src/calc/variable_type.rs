use std::fmt::{Display, Debug};
use serde::{Serialize, Deserialize};

#[derive(Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum VariableType {
    Scalar,
    Complex,
    Vector,
    Matrix
}


pub trait VariableData: Display + Debug + Clone + Serialize + for<'a> Deserialize<'a> + Default {
    fn get_type(&self) -> VariableType;
}