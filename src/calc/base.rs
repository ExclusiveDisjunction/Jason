use std::fmt::{Display, Debug};
use serde::{Serialize, Deserialize};

#[derive(Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub enum VariableType {
    Scalar,
    Complex,
    Vector,
    Matrix
}
impl Display for VariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f, 
            "{}",
            match self {
                Self::Scalar => "Scalar",
                Self::Complex => "Complex",
                Self::Vector => "Vector",
                Self::Matrix => "Matrix"
            }
        )
    }
}


pub trait VariableData: Display + Debug + Clone + Serialize + for<'a> Deserialize<'a> + Default {
    fn get_type(&self) -> VariableType;
}