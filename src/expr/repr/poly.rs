use crate::calc::{CalcError, CalcResult, VariableUnion};
use super::{base::{ASTNode, ASTJoinNode, TreeOrderTraversal}, leaf::{ConstExpr, VariableExpr, RawLeafExpr}, combine::{OperatorExpr, CommaExpr, RawJoinExpr}};

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

    fn print_self(&self, kind: TreeOrderTraversal) -> String {
        let x: &dyn ASTNode = self.as_ref();
        x.print_self(kind)
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn ASTNode = self.as_ref();
        x.debug_print(f)
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
impl From<Box<dyn ASTNode>> for LeafNodes {
    fn from(b: Box<dyn ASTNode>) -> Self {
        Self::Other(b)
    }
}

pub enum JoinedNodes {
    Operator(OperatorExpr),
    Comma(CommaExpr),
    Raw(RawJoinExpr),
    Other(Box<dyn ASTJoinNode>)
}
impl ASTNode for JoinedNodes {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
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

    fn print_self(&self, kind: TreeOrderTraversal) -> String {
        match self {
            Self::Operator(o) => (o as &dyn ASTNode).print_self(kind),
            Self::Comma(c) => (c as &dyn ASTNode).print_self(kind),
            Self::Raw(r) => (r as &dyn ASTNode).print_self(kind),
            Self::Other(o) => o.print_self(kind)
        }
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operator(o) => (o as &dyn ASTNode).debug_print(f),
            Self::Comma(c) => (c as &dyn ASTNode).debug_print(f),
            Self::Raw(r) => (r as &dyn ASTNode).debug_print(f),
            Self::Other(o) => o.debug_print(f)
        }
    }
}
impl ASTJoinNode for JoinedNodes {
    fn left(&self) -> Option<&dyn ASTNode> {
        let x: &dyn ASTJoinNode = self.as_ref();
        x.left()
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        let x: &dyn ASTJoinNode = self.as_ref();
        x.right()
    }

    fn set_left(&mut self, new: Box<dyn ASTNode>) {
        let x: &mut dyn ASTJoinNode = self.as_mut();
        x.set_left(new)
    }
    fn set_right(&mut self, new: Box<dyn ASTNode>) {
        let x: &mut dyn ASTJoinNode = self.as_mut();
        x.set_right(new)
    }
}
impl<'a> AsRef<dyn ASTJoinNode + 'a> for JoinedNodes {
    fn as_ref(&self) -> &(dyn ASTJoinNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Raw(r) => r,
            Self::Other(b) => b.as_ref()
        }
    }
}
impl<'a> AsMut<dyn ASTJoinNode + 'a> for JoinedNodes {
    fn as_mut(&mut self) -> &mut (dyn ASTJoinNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Raw(r) => r,
            Self::Other(b) => b.as_mut()
        }
    }
}

impl From<OperatorExpr> for JoinedNodes {
    fn from(value: OperatorExpr) -> Self {
        Self::Operator(value)
    }
}
impl From<CommaExpr> for JoinedNodes {
    fn from(value: CommaExpr) -> Self {
        Self::Comma(value)
    }
}
impl From<RawJoinExpr> for JoinedNodes {
    fn from(value: RawJoinExpr) -> Self {
        Self::Raw(value)
    }
}
impl From<Box<dyn ASTJoinNode>> for JoinedNodes {
    fn from(value: Box<dyn ASTJoinNode>) -> Self {
        Self::Other(value)
    }
}

pub enum TotalNodes {
    Join(JoinedNodes),
    Leaf(LeafNodes)
}
impl ASTNode for TotalNodes {
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion> {
        match self {
            Self::Join(j) => j.evaluate(on),
            Self::Leaf(l) => l.evaluate(on)
        }
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        let x: &dyn ASTNode = self.as_ref();
        x.evaluate_list(on)
    }

    fn print_self(&self, kind: TreeOrderTraversal) -> String {
        let x: &dyn ASTNode = self.as_ref();
        x.print_self(kind)
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn ASTNode = self.as_ref();
        x.debug_print(f)
    }
}
impl ASTJoinNode for TotalNodes {
    fn left(&self) -> Option<&dyn ASTNode> {
        match self {
            Self::Join(j) => j.left(),
            Self::Leaf(_) => None
        }
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        match self {
            Self::Join(j) => j.right(),
            Self::Leaf(_) => None
        }
    }

    fn set_left(&mut self, new: Box<dyn ASTNode>) {
        match self {
            Self::Join(j) => j.set_left(new),
            Self::Leaf(_) => ()
        }
    }
    fn set_right(&mut self, new: Box<dyn ASTNode>) {
        match self {
            Self::Join(j) => j.set_right(new),
            Self::Leaf(_) => ()
        }
    }
}
impl From<LeafNodes> for TotalNodes {
    fn from(value: LeafNodes) -> Self {
        Self::Leaf(value)
    }
}
impl From<JoinedNodes> for TotalNodes {
    fn from(value: JoinedNodes) -> Self {
        Self::Join(value)
    }
}
impl From<Box<dyn ASTNode>> for TotalNodes {
    fn from(value: Box<dyn ASTNode>) -> Self {
        Self::Leaf(value.into())
    }
}
impl From<Box<dyn ASTJoinNode>> for TotalNodes {
    fn from(value: Box<dyn ASTJoinNode>) -> Self {
        Self::Join(value.into())
    }
}

impl<'a> AsRef<dyn ASTNode + 'a> for TotalNodes {
    fn as_ref(&self) -> &(dyn ASTNode + 'a) {
        match self {
            Self::Join(j) => j,
            Self::Leaf(l) => l
        }
    }
}
impl<'a> AsMut<dyn ASTNode + 'a> for TotalNodes {
    fn as_mut(&mut self) -> &mut (dyn ASTNode + 'a) {
        match self {
            Self::Join(j) => j,
            Self::Leaf(l) => l
        }
    }
}