use super::base::{ASTNode, TreeOrderTraversal};
use crate::calc::{VariableData, VariableUnion, CalcResult, CalcError, calc_error::IndexOutOfRangeError};

use serde::{Serialize, Deserialize};
use std::fmt::{Display, Debug};

/// Represents a single number via `VariableUnion`. This is a leaf node.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstExpr {
    value: VariableUnion
}
impl ASTNode for ConstExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Ok(self.value.clone())
    }

    fn print_self(&self) -> String {
        self.value.to_string()
    }
}
impl Debug for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "const-expr {}", self.value.get_type())
    }
}
impl Display for ConstExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_traversal(TreeOrderTraversal::Inorder))
    }
}
impl From<VariableUnion> for ConstExpr {
    fn from(value: VariableUnion) -> Self {
        Self {
            value
        }
    }
}
impl ConstExpr {
    pub fn new(with: VariableUnion) -> Self {
        Self {
            value: with
        }
    }
}

/// Represents a variable along a specific dimension. Upon evaluation, it will pull data out of the `on` parameter. This is a leaf node.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableExpr {
    symbol: char,
    along: usize
}
impl Debug for VariableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var-expr '{}' along: {}", self.symbol, self.along)
    }
}
impl Display for VariableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_traversal(TreeOrderTraversal::Inorder))
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

    fn print_self(&self) -> String  {
        format!("{}", &self.symbol)
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