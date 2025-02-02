use std::fmt::{Debug, Display};
use crate::calc::{CalcError, CalcResult, VariableUnion};
use super::{base::{ASTNode, ASTJoinNode, PrintKind}, leaf::{ConstExpr, VariableExpr, RawLeafExpr}, combine::{OperatorExpr, CommaExpr, RawJoinExpr}};

pub enum LeafNodes {
    Const(ConstExpr),
    Var(VariableExpr),
    Raw(RawLeafExpr),
    Other(Box<dyn ASTNode>)
}
impl ASTNode for LeafNodes {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        let x: &dyn ASTNode = self.as_ref();
        x.evaluate(on)
    }

    fn print_self(&self, kind: PrintKind) -> String {
        format!("{}", self)
    }
}
impl Display for LeafNodes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn ASTNode = self.as_ref();
        write!(f, "{}", x.print_self(PrintKind::Inorder))
    }
}
impl<'a> AsRef<dyn ASTNode + 'a> for LeafNodes {
    fn as_ref(&self) -> &(dyn ASTNode + 'a) {
        match self {
            Self::Const(c) => c,
            Self::Var(v) => v,
            Self::Raw(r) => r,
            Self::Other(b) => b.as_ref()
        }
    }
}
impl<'a> AsMut<dyn ASTNode + 'a> for LeafNodes {
    fn as_mut(&mut self) -> &mut (dyn ASTNode + 'a) {
        match self {
            Self::Const(c) => c,
            Self::Var(v) => v,
            Self::Raw(r) => r,
            Self::Other(b) => b.as_mut()
        }
    }
}

impl From<ConstExpr> for LeafNodes {
    fn from(c: ConstExpr) -> Self {
        Self::Const(c)
    }
}
impl From<VariableExpr> for LeafNodes {
    fn from(v: VariableExpr) -> Self {
        Self::Var(v)
    }
}
impl From<RawLeafExpr> for LeafNodes {
    fn from(v: RawLeafExpr) -> Self {
        Self::Raw(v)
    }
}
impl<T> From<Box<T>> for LeafNodes where T: ASTNode {
    fn from(b: Box<T>) -> Self {
        Self::Other(b.into())
    }
}
impl From<Box<dyn ASTNode>> for LeafNodes {
    fn from(b: Box<dyn ASTNode>) -> Self {
        Self::Other(b)
    }
}

pub enum JoinedNodes {
    Operator(OperatorExpr),
    Comma(CommaExpr),
    Raw(RawLeafExpr),
    Other(Box<dyn ASTJoinNode>)
}
impl ASTNode for JoinedNodes {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        let x: &dyn ASTNode = self.as_ref();
        match self {
            Self::Operator(o) => o.evaluate(on),
            Self::Comma(c) => c.evaluate(on),
            Self::Raw(r) => r.evaluate(on),
            Self::Other(o) => o.evaluate(on)
        }
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        let x: &dyn ASTJoinNode = self.as_ref();
        x.evaluate_list(on)
    }

    fn print_self(&self, kind: PrintKind) -> String {
        match self {
            Self::Operator(o) => o.print_self(kind),
            Self::Comma(c) => c.print_self(kind),
            Self::Raw(r) => r.print_self(kind),
            Self::Other(o) => o.print_self(kind)
        }
    }
}
impl ASTJoinNode for JoinedNodes {


    fn left(&self) -> Option<&ASTNodeKind> {
        let x: &dyn CombiningASTNode = self.as_ref();
        x.left()
    }
    fn right(&self) -> Option<&ASTNodeKind> {
        let x: &dyn CombiningASTNode = self.as_ref();
        x.right()
    }

    fn set_left(&mut self, new: ASTNodeKind) {
        let x: &mut dyn CombiningASTNode = self.as_mut();
        x.set_left(new)
    }
    fn set_right(&mut self, new: ASTNodeKind) {
        let x: &mut dyn CombiningASTNode = self.as_mut();
        x.set_right(new)
    }
}
impl<'a> AsRef<dyn CombiningASTNode + 'a> for JoinedNodes {
    fn as_ref(&self) -> &(dyn CombiningASTNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Other(b) => b.as_ref()
        }
    }
}
impl<'a> AsMut<dyn CombiningASTNode + 'a> for JoinedNodes {
    fn as_mut(&mut self) -> &mut (dyn CombiningASTNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Other(b) => b.as_mut()
        }
    }
}

pub enum ASTNodeKind {
    Join(JoinedNodes),
    Leaf(ASTLeafNodeKind)
}
impl ASTNode for ASTNodeKind {
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion> {
        match self {
            Self::Join(j) => j.evaluate(on),
            Self::Leaf(l) => l.evaluate(on)
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Join(j) => j.display(f),
            Self::Leaf(l) => l.display(f)
        }
    }
    fn to_string(&self) -> String {
        match self {
            Self::Join(j) => j.to_string(),
            Self::Leaf(l) => (l as &dyn ASTNode).to_string()
        }
    }
}
impl CombiningASTNode for ASTNodeKind {
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        match self {
            Self::Join(j) => j.evaluate_list(on),
            Self::Leaf(l) => Ok( vec![ l.evaluate(on)? ] )
        }
    }

    fn left(&self) -> Option<&ASTNodeKind> {
        match self {
            Self::Join(j) => j.left(),
            Self::Leaf(_) => None
        }
    }
    fn right(&self) -> Option<&ASTNodeKind> {
        match self {
            Self::Join(j) => j.right(),
            Self::Leaf(_) => None
        }
    }

    fn set_left(&mut self, new: ASTNodeKind) {
        match self {
            Self::Join(j) => j.set_left(new),
            Self::Leaf(_) => ()
        }
    }
    fn set_right(&mut self, new: ASTNodeKind) {
        match self {
            Self::Join(j) => j.set_right(new),
            Self::Leaf(_) => ()
        }
    }

    fn print_self(&self, kind: PrintKind) -> String {
        match self {
            Self::Join(j) => j.print_self(kind),
            Self::Leaf(s) => (s as &dyn ASTNode).to_string()
        }
    }
}
impl Display for ASTNodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f)
    }
}
impl Debug for ASTNodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl From<ASTLeafNodeKind> for ASTNodeKind {
    fn from(value: ASTLeafNodeKind) -> Self {
        Self::Leaf(value)
    }
}
impl From<JoinedNodes> for ASTNodeKind {
    fn from(value: JoinedNodes) -> Self {
        Self::Join(value)
    }
}