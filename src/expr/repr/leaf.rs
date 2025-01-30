use std::fmt::{Display, Debug};

use super::base::ASTNode;
use crate::calc::{VariableUnion, CalcResult, CalcError, calc_error::IndexOutOfRangeError};

pub struct ConstExpr {
    value: VariableUnion
}

impl Display for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.value as &dyn Display).fmt(f)
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

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       (self as &dyn Display).fmt(f)
    }
    fn to_string(&self) -> String {
        format!("{}", &self.value)
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

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
    fn to_string(&self) -> String {
        format!("{}", self.symbol)
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

pub enum ASTLeafNodeKind {
    Const(ConstExpr),
    Var(VariableExpr),
    Other(Box<dyn ASTNode>)
}
impl ASTNode for ASTLeafNodeKind {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        let x: &dyn ASTNode = self.as_ref();
        x.evaluate(on)
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn ASTNode = self.as_ref();
        x.display(f)
    }
    fn to_string(&self) -> String {
        let x: &dyn ASTNode = self.as_ref();
        x.to_string()
    }
}
impl Display for ASTLeafNodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &ASTNode = self.as_ref();
        x.display(f)
    }
}
impl<'a> AsRef<dyn ASTNode + 'a> for ASTLeafNodeKind {
    fn as_ref(&self) -> &(dyn ASTNode + 'a) {
        match self {
            Self::Const(c) => c,
            Self::Var(v) => v,
            Self::Other(b) => b.as_ref()
        }
    }
}