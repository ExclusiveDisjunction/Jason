use super::{base::ASTNode, leaf::ASTLeafNodeKind, raw_oper::RawOperator};
use crate::calc::{VariableUnion, CalcResult, CalcError, calc_error::OperationError};

use std::fmt::{Display, Debug};

pub enum ASTNodeKind {
    Join(ASTJoinNodeKind),
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
impl From<ASTJoinNodeKind> for ASTNodeKind {
    fn from(value: ASTJoinNodeKind) -> Self {
        Self::Join(value)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum PrintKind {
    Preorder,
    Inorder,
    Postorder
}
impl PrintKind {
    fn join_strings(&self, left: Option<String>, root: String, right: Option<String>) -> String{
        match (left, right) {
            (Some(a), Some(b)) => {
                match self {
                    Self::Preorder => format!("{root} {a} {b}"),
                    Self::Inorder => format!("{a} {root} {b}"),
                    Self::Postorder => format!("{a} {b} {root}"),
                }
            },
            (Some(a), None) => {
                match self {
                    Self::Preorder => format!("{root} {a}"),
                    Self::Inorder => format!("{a} {root}"),
                    Self::Postorder => format!("{a} {root}"),
                }
            },
            (None, Some(b)) => {
                match self {
                    Self::Preorder => format!("{root} {b}"),
                    Self::Inorder => format!("{root} {b}"),
                    Self::Postorder => format!("{b} {root}"),
                }
            },
            (None, None) => root
        }
    }
}

pub trait CombiningASTNode: ASTNode {
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>>;

    /// Returns the left node for our combination.
    fn left(&self) -> Option<&ASTNodeKind>;
    /// Returns the right node for our combination
    fn right(&self) -> Option<&ASTNodeKind>;

    /// Sets the left node. The implementer may ignore this, and calling this does not imply that self.left() will return the value stored here.
    fn set_left(&mut self, new: ASTNodeKind);
    /// Sets the right node. The implementer may ignore this, and calling this does not imply that self.right() will return the value stored here.
    fn set_right(&mut self, new: ASTNodeKind);

    fn print_self(&self, kind: PrintKind) -> String {
        let left = self.left().map(|x| x.print_self(kind) );
        let right = self.right().map(|x| x.print_self(kind) );

        kind.join_strings(left, self.to_string(), right)
    }
}

pub enum ASTJoinNodeKind {
    Operator(OperatorExpr),
    Comma(CommaExpr),
    Other(Box<dyn CombiningASTNode>)
}
impl ASTNode for ASTJoinNodeKind {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        match self {
            Self::Operator(o) => o.evaluate(on),
            Self::Comma(c) => c.evaluate(on),
            Self::Other(o) => o.evaluate(on)
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Operator(o) => o.display(f),
            Self::Comma(c) => c.display(f),
            Self::Other(o) => o.display(f)
        }
    }
    fn to_string(&self) -> String {
        match self {
            Self::Operator(o) => o.to_string(),
            Self::Comma(c) => (c as &dyn ASTNode).to_string(),
            Self::Other(o) => o.to_string()
        }
    }
}
impl CombiningASTNode for ASTJoinNodeKind {
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
       let x: &dyn CombiningASTNode = self.as_ref();
       x.evaluate_list(on)
    }

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
impl<'a> AsRef<dyn CombiningASTNode + 'a> for ASTJoinNodeKind {
    fn as_ref(&self) -> &(dyn CombiningASTNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Other(b) => b.as_ref()
        }
    }
}
impl<'a> AsMut<dyn CombiningASTNode + 'a> for ASTJoinNodeKind {
    fn as_mut(&mut self) -> &mut (dyn CombiningASTNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Other(b) => b.as_mut()
        }
    }
}

pub struct OperatorExpr {
    data: RawOperator,
    left: Box<ASTNodeKind>,
    right: Box<ASTNodeKind>
}
impl ASTNode for OperatorExpr {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError>{
        let left_eval = self.left.evaluate(on)?;
        let right_eval = self.right.evaluate(on)?;

        self.data.apply_owned(left_eval, right_eval)
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.data)
    }
    fn to_string(&self) -> String {
        format!("{}", &self.data)
    }
}
impl CombiningASTNode for OperatorExpr {
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        Ok( vec![ self.evaluate(on)? ] )
    }

    fn left(&self) -> Option<&ASTNodeKind> {
        Some(& self.left )
    }
    fn right(&self) -> Option<&ASTNodeKind> {
        Some( &self.right)
    }

    fn set_left(&mut self, new: ASTNodeKind) {
        self.left = Box::new(new);
    }
    fn set_right(&mut self, new: ASTNodeKind) {
        self.right = Box::new(new);
    }
}
impl OperatorExpr {
    pub fn new(oper: RawOperator, left: ASTNodeKind, right: ASTNodeKind) -> Self {
        Self {
            data: oper,
            left: Box::new(left),
            right: Box::new(right)
        }
    }
}

pub struct CommaExpr {
    left: Box<ASTNodeKind>,
    right: Box<ASTNodeKind>
}
impl ASTNode for CommaExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Err( CalcError::from( OperationError::new_fmt(",", &self.left, &self.right, Some("comma can not be combined on one object")) ) )
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
    fn to_string(&self) -> String {
        format!("{}", self)
    }
}
impl Display for CommaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}", &self.left, &self.right)
    }
}
impl CombiningASTNode for CommaExpr {
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        Ok(
            vec![
                self.left.evaluate(on)?,
                self.right.evaluate(on)?
            ]
        )
    }

    fn left(&self) -> Option<&ASTNodeKind> {
        Some(&self.left)
    }
    fn right(&self) -> Option<&ASTNodeKind> {
        Some(&self.right)
    }

    fn set_left(&mut self, new: ASTNodeKind) {
        self.left = Box::new(new);
    }
    fn set_right(&mut self, new: ASTNodeKind) {
        self.right = Box::new(new);
    }
}