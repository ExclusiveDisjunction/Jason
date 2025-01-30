use super::{base::ASTNode, leaf::ASTLeafNodeKind, raw_oper::RawOperator};
use crate::calc::{VariableUnion, VariableUnionRef, CalcResult, CalcError};

use std::fmt::{Display, Debug};

pub enum ASTNodeKind {
    Join(ASTJoinNodeKind),
    Leaf(ASTLeafNodeKind)
}

#[derive(Clone, Copy, PartialEq)]
enum PrintKind {
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

pub trait CombiningASTNode: ASTNode{
    fn left(&self) -> Option<&ASTNodeKind>;
    fn right(&self) -> Option<&ASTNodeKind>;

    fn set_left(&mut self, new: ASTNodeKind);
    fn set_right(&mut self, new: ASTNodeKind);

    fn left_print(&self, kind: PrintKind) -> Option<String> {
        let x = self.left()?;
        match x {
            ASTNodeKind::Join(j) => Some(j.print_self(kind)),
            ASTNodeKind::Leaf(l) => Some(l.to_string())
        }
    }
    fn right_print(&self, kind: PrintKind) -> Option<String> {
        let x = self.right()?;
        match x {
            ASTNodeKind::Join(j) => Some(j.print_self(kind)),
            ASTNodeKind::Leaf(l) => Some(l.to_string())
        }
    }
    fn print_self(&self, kind: PrintKind) -> String {
        kind.join_strings(self.left_print(kind), self.to_string(), self.right_print(kind))
    }
}

pub enum ASTJoinNodeKind {
    Operator(OperatorExpr),
    Comma(CommaExpr),
    Other(Box<dyn CombiningASTNode>)
}
impl ASTNode for ASTJoinNodeKind {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        todo!()
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
    fn to_string(&self) -> String {
        todo!()
    }
}
impl CombiningASTNode for ASTJoinNodeKind {
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
            Self::Other(b) => b
        }
    }
}
impl<'a> AsRef<dyn ASTNode + 'a> for ASTJoinNodeKind {
    fn as_ref(&self) -> &(dyn ASTNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Other(b) => (b.as_ref() as &(dyn ASTNode + 'a))
        }
    }
}
impl<'a> AsMut<dyn CombiningASTNode + 'a> for ASTJoinNodeKind {
    fn as_mut(&mut self) -> &mut (dyn CombiningASTNode + 'a) {
        match self {
            Self::Operator(x) => x,
            Self::Comma(c) => c,
            Self::Other(b) => b
        }
    }
}

pub struct OperatorExpr {
    data: RawOperator,
    left: Box<dyn ASTNode>,
    right: Box<dyn ASTNode>
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
    fn left(&self) -> Option<&dyn ASTNode> {
        Some(self.left.as_ref())
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        Some(self.right.as_ref())
    }

    fn set_left(&mut self, new: ASTNodeKind) {
        
    }
    fn set_right(&mut self, new: ASTNodeKind) {
        
    }
}
impl OperatorExpr {
    pub fn new(oper: RawOperator, left: Box<dyn ASTNode>, right: Box<dyn ASTNode>) -> Self {
        Self {
            data: oper,
            left,
            right
        }
    }
}

pub struct CommaExpr {

}