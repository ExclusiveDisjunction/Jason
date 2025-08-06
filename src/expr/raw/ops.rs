use std::fmt::{Display, Debug};
use std::str::FromStr;

use serde::{de::DeserializeOwned, Deserialize, Serialize};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct OperatorParsingError(String);
impl Display for OperatorParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the following expression '{}' could not be represented as a valid operator", &self.0)
    }
}
impl From<&str> for OperatorParsingError {
    fn from(value: &str) -> Self {
        Self ( value.to_string() )
    }
}
impl From<String> for OperatorParsingError {
    fn from(value: String) -> Self {
        Self ( value )
    }
}
impl std::error::Error for OperatorParsingError { }

pub trait OperatorRepresentation : 
    PartialEq + Eq + 
    Clone + Copy + 
    Display + Debug + 
    Serialize + DeserializeOwned + 
    FromStr<Err=OperatorParsingError> {
    fn symbol(&self) -> &'static str;
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum BinaryOperator {
    Add, 
    Sub,
    Mul,
    Div, 
    Pow,
    Eq,
    Neq,
    Le,
    Lt,
    Ge,
    Gt
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}
impl FromStr for BinaryOperator {
    type Err = OperatorParsingError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+"  => Ok( Self::Add ),
            "-"  => Ok( Self::Sub ),
            "*"  => Ok( Self::Mul ),
            "/"  => Ok( Self::Div ),
            "^"  => Ok( Self::Pow ),
            "==" => Ok( Self::Eq  ),
            "!=" => Ok( Self::Neq ),
            "<"  => Ok( Self::Lt  ),
            "<=" => Ok( Self::Le  ),
            ">"  => Ok( Self::Gt  ),
            ">=" => Ok( Self::Ge  ),
            _ => Err( s.into() )
        }
    }
}
impl OperatorRepresentation for BinaryOperator {
    fn symbol(&self) -> &'static str {
        match self {
            Self::Add => "+" ,
            Self::Sub => "-" ,
            Self::Mul => "*" ,
            Self::Div => "/" ,
            Self::Pow => "^" ,
            Self::Eq  => "==",
            Self::Neq => "!=",
            Self::Lt  => "<" ,
            Self::Le  => "<=",
            Self::Gt  => ">" ,
            Self::Ge  => ">="
        }
    }
}
impl BinaryOperator {
    pub fn rank(&self) -> u8 {
        match self {
            Self::Add | Self::Sub => 1,
            Self::Mul | Self::Div | Self::Pow => 2,
            Self::Eq | Self::Neq | Self::Le | Self::Lt | Self::Ge | Self::Gt => 3
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum UnaryOperator {
    Neg, 
    Not
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}
impl FromStr for UnaryOperator {
    type Err = OperatorParsingError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok( Self::Neg ),
            "!" => Ok( Self::Not ),
            _ =>   Err( s.into() )
        }
    }
}
impl OperatorRepresentation for UnaryOperator {
    fn symbol(&self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "!"
        }
    }
}