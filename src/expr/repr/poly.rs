use std::fmt::{Display, Debug};

use serde::{Deserialize, Serialize};

use crate::calc::{CalcResult, VariableUnion};
use super::{base::ASTNode, leaf::{ConstExpr, VariableExpr}, combine::{OperatorExpr, CommaExpr, RawExpr}};

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub enum TotalNodes {
    Const(ConstExpr),
    Var(VariableExpr),
    Oper(OperatorExpr),
    Comma(CommaExpr),
    Raw(RawExpr)
}
impl Display for TotalNodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Const(c) => c,
            Self::Var(v) => v,
            Self::Oper(o) => o,
            Self::Comma(c) => c,
            Self::Raw(r) => r
        };

        x.fmt(f)
    }
}
impl Debug for TotalNodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Debug = match self {
            Self::Const(c) => c,
            Self::Var(v) => v,
            Self::Oper(o) => o,
            Self::Comma(c) => c,
            Self::Raw(r) => r
        };

        x.fmt(f)
    }
}
impl ASTNode for TotalNodes {
    fn left(&self) -> Option<&TotalNodes> {
        match self {
            Self::Const(c) => c.left(),
            Self::Var(v) => v.left(),
            Self::Oper(o) => o.left(),
            Self::Comma(c) => c.left(),
            Self::Raw(r) => r.left()
        }
    }
    fn right(&self) -> Option<&TotalNodes> {
        match self {
            Self::Const(c) => c.right(),
            Self::Var(v) => v.right(),
            Self::Oper(o) => o.right(),
            Self::Comma(c) => c.right(),
            Self::Raw(r) => r.right()
        }
    }

    fn set_left(&mut self, n: Box<TotalNodes>) {
        match self {
            Self::Const(c) => c.set_left(n),
            Self::Var(v) => v.set_left(n),
            Self::Oper(o) => o.set_left(n),
            Self::Comma(c) => c.set_left(n),
            Self::Raw(r) => r.set_left(n)
        }
    }
    fn set_right(&mut self, n: Box<TotalNodes>) {
        match self {
            Self::Const(c) => c.set_right(n),
            Self::Var(v) => v.set_right(n),
            Self::Oper(o) => o.set_right(n),
            Self::Comma(c) => c.set_right(n),
            Self::Raw(r) => r.set_right(n)
        }
    }

    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion> {
        match self {
            Self::Const(c) => c.evaluate(on),
            Self::Var(v) => v.evaluate(on),
            Self::Oper(o) => o.evaluate(on),
            Self::Comma(c) => c.evaluate(on),
            Self::Raw(r) => r.evaluate(on)
        }
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        match self {
            Self::Const(c) => c.evaluate_list(on),
            Self::Var(v) => v.evaluate_list(on),
            Self::Oper(o) => o.evaluate_list(on),
            Self::Comma(c) => c.evaluate_list(on),
            Self::Raw(r) => r.evaluate_list(on)
        }
    }

    fn print_self(&self) -> String {
        match self {
            Self::Const(c) => c.print_self(),
            Self::Var(v) => v.print_self(),
            Self::Oper(o) => o.print_self(),
            Self::Comma(c) => c.print_self(),
            Self::Raw(r) => r.print_self()
        }
    }
}

impl From<ConstExpr> for TotalNodes {
    fn from(value: ConstExpr) -> Self {
        Self::Const(value)
    }
}
impl From<VariableExpr> for TotalNodes {
    fn from(value: VariableExpr) -> Self {
        Self::Var(value)
    }
}
impl From<OperatorExpr> for TotalNodes {
    fn from(value: OperatorExpr) -> Self {
        Self::Oper(value)
    }
}
impl From<CommaExpr> for TotalNodes {
    fn from(value: CommaExpr) -> Self {
        Self::Comma(value)
    }
}
impl From<RawExpr> for TotalNodes {
    fn from(value: RawExpr) -> Self {
        Self::Raw(value)
    }
}