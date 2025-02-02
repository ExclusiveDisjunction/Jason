use std::fmt::{Display, Debug};

use super::base::{ASTNode, PrintKind};
use crate::calc::{VariableUnion, CalcResult, CalcError, calc_error::{IndexOutOfRangeError, OperationError}};

pub struct ConstExpr {
    value: VariableUnion
}

impl Display for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.value)
    }
}
impl Debug for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.value as &dyn Debug).fmt(f)
    }
}

impl ASTNode for ConstExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Ok(self.value.clone())
    }

    fn print_self(&self, kind: PrintKind) -> String {
        format!("{}", self)
    }
}
impl ConstExpr {
    pub fn new(with: VariableUnion) -> Self {
        Self {
            value: with
        }
    }
}

pub struct VariableExpr {
    symbol: char,
    along: usize
}

impl Display for VariableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol)
    }
}
impl Debug for VariableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (along: {})", self.symbol, self.along)
    }
}

impl ASTNode for VariableExpr {
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion> {
        if self.along >= on.len() {
            Err(CalcError::Index(IndexOutOfRangeError::new(self.along)))
        }
        else {
            Ok(on[self.along].clone())
        }
    }

    fn print_self(&self, kind: PrintKind) -> String  {
        format!("{}", self)
    }
}
impl VariableExpr {
    pub fn new(symbol: char, along: usize) -> Self {
        Self {
            symbol,
            along
        }
    }
}

pub struct RawLeafExpr {
    contents: String
}

impl Display for RawLeafExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.contents)
    }
}
impl Debug for RawLeafExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}

impl ASTNode for RawLeafExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Err(OperationError::new("eval", "raw expression".to_string(), String::new(), Some("cannot evaluate a raw expression")).into())
    }

    fn print_self(&self, kind: PrintKind) -> String {
        format!("{}", self)
    }
}