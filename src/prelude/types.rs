use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum FlatType {
    Any,
    Scalar,
    Complex,
    Vector,
    Matrix,
    Boolean
}
impl Display for FlatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::Any => "Any",
                Self::Scalar => "Sca",
                Self::Complex => "Comp",
                Self::Vector => "Vec",
                Self::Matrix => "Mat",
                Self::Boolean => "Bool"
            }
        )
    }
}