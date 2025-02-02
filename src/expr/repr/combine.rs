use super::{base::{ASTNode, ASTJoinNode, PrintKind}, raw_oper::RawOperator};
use crate::calc::{VariableUnion, CalcResult, CalcError, calc_error::OperationError};

use std::fmt::{Display, Debug};

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

    fn print_self(&self, kind: PrintKind) -> String {
        kind.join_strings(Some(self.left.print_self(kind)), self.data.to_string(), Some(self.right.print_self(kind)))
    }
}
impl ASTJoinNode for OperatorExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        Some(& self.left )
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        Some( &self.right)
    }

    fn set_left<T>(&mut self, new: T) where T: ASTNode {
        self.left = Box::new(new);
    }
    fn set_right<T>(&mut self, new: T) where T: ASTNode {
        self.right = Box::new(new);
    }
}
impl OperatorExpr {
    pub fn new<T, U>(oper: RawOperator, left: T, right: U) -> Self where T: ASTNode, U: ASTNode {
        Self {
            data: oper,
            left: Box::new(left),
            right: Box::new(right)
        }
    }
}

pub struct CommaExpr {
    left: Box<dyn ASTNode>,
    right: Box<dyn ASTNode>
}
impl ASTNode for CommaExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Err( CalcError::from( OperationError::new_fmt(",", &self.left, &self.right, Some("comma can not be combined on one object")) ) )
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        let mut left = self.left.evaluate_list(on)?;
        let mut right = self.right.evaluate_list(on)?;

        left.append(&mut right);

        Ok(left)
    }

    fn print_self(&self, kind: PrintKind) -> String {
        kind.join_strings(Some(self.left.print_self(kind)), ",".to_string(), Some(self.right.print_self(kind)))
    }
}
impl Display for CommaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}", self.left.print_self(PrintKind::Inorder), self.right.print_self(PrintKind::Inorder))
    }
}
impl ASTJoinNode for CommaExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        Some(&self.left)
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        Some(&self.right)
    }

    fn set_left<T>(&mut self, new: T) where T: ASTNode {
        self.left = Box::new(new);
    }
    fn set_right<T>(&mut self, new: T) where T: ASTNode {
        self.right = Box::new(new);
    }
}
impl CommaExpr {
    pub fn new<T, U>(left: T, right: U) -> Self where T: ASTNode, U: ASTNode {
        Self {
            left: Box::new(left),
            right: Box::new(right)
        }
    }
}

pub struct RawJoinExpr {
    data: String,
    left: Option<Box<dyn ASTNode>>,
    right: Option<Box<dyn ASTNode>>
}
impl ASTNode for RawJoinExpr {
    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError> {

    }

    fn print_self(&self, kind: PrintKind) -> String {
        kind.join_strings(self.left.as_ref().map(|x| x.print_self(kind)), self.data.clone(), self.right.as_ref().map(|x| x.print_self(kind)))
    }
}
impl ASTJoinNode for RawJoinExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        self.left.as_ref()
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        self.right.as_ref()
    }

    fn set_left<T>(&mut self, new: T) where T: ASTNode {
        self.left = Some(Box::new(new));
    }
    fn set_right<T>(&mut self, new: T) where T: ASTNode {
        self.right = Some(Box::new(new));
    }
}

impl RawJoinExpr {
    fn new<S, T, U>(contents: S, left: Option<T>, right: Option<U>) -> Self where S: ToString, T: ASTNode, U: ASTNode {
        Self {
            data: contents.to_string(),
            left: left.map(|x| Box::new(x)),
            right: right.map(|x| Box::new(x))
        }
    }
}