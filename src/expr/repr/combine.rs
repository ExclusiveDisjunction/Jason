use std::fmt::{Display, Debug};

use serde::{Deserialize, Serialize};

use super::{base::{ASTNode, TreeOrderTraversal}, raw_oper::RawOperator};
use crate::calc::{calc_error::{OperationError, UndefinedError}, CalcError, CalcResult, VariableUnion};
use super::poly::TotalNodes;

/// Represents an operation done on two sub children nodes. This is a joined node.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct OperatorExpr {
    data: RawOperator,
    left: Box<TotalNodes>,
    right: Box<TotalNodes>
}
impl Display for OperatorExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_traversal(TreeOrderTraversal::Inorder))
    }
}
impl Debug for OperatorExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "oper-expr {} left: '{:?}' right: '{:?}'", &self.data, &self.left, &self.right)
    }
}
impl ASTNode for OperatorExpr {
    fn left(&self) -> Option<&TotalNodes> {
        Some( self.left.as_ref() )
    }
    fn right(&self) -> Option<&TotalNodes> {
        Some( self.right.as_ref() )
    }

    fn set_left(&mut self, new: Box<TotalNodes>) {
        self.left = new;
    }
    fn set_right(&mut self, new: Box<TotalNodes>) {
        self.right = new;
    }

    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError>{
        let left_eval = self.left.evaluate(on)?;
        let right_eval = self.right.evaluate(on)?;

        self.data.apply_owned(left_eval, right_eval)
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        let left = self.left.evaluate_list(on)?;
        let right = self.right.evaluate_list(on)?;

        if left.len() == 1 && right.len() == 1 { //We can only combine elements together like this
            let left = left.into_iter().next().unwrap();
            let right = right.into_iter().next().unwrap();

            self.data.apply_owned(left, right).map(|x| vec![x] )
        }
        else {
            Err( OperationError::new_fmt(self.data.symbol(), &left, &right, Some("operators resulting in lists can only evaluate on left-right lists of length 1")).into() )
        }
    }

    fn print_self(&self) -> String {
        self.data.to_string()
    }
}
impl OperatorExpr {
    pub fn new(oper: RawOperator, left: TotalNodes, right: TotalNodes) -> Self {
        Self {
            data: oper,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
    pub fn new_box(oper: RawOperator, left: Box<TotalNodes>, right: Box<TotalNodes>) -> Self {
        Self {
            data: oper,
            left,
            right
        }
    }
}

/// Represents a join between two nodes. Note that calling `evaluate` will always fail, as it does not make sense. 
#[derive(Serialize, Deserialize, PartialEq, Clone)]
pub struct CommaExpr {
    left: Box<TotalNodes>,
    right: Box<TotalNodes>
}
impl Display for CommaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_traversal(TreeOrderTraversal::Inorder))
    }
}
impl Debug for CommaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "comma-expr left: '{:?}' right: '{:?}'", &self.left, &self.right)
    }
}
impl ASTNode for CommaExpr {
    fn left(&self) -> Option<&TotalNodes> {
        Some(self.left.as_ref())
    }
    fn right(&self) -> Option<&TotalNodes> {
        Some(self.right.as_ref())
    }

    fn set_left(&mut self, new: Box<TotalNodes>) {
        self.left = new;
    }
    fn set_right(&mut self, new: Box<TotalNodes>) {
        self.right = new;
    }

    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Err( CalcError::from( OperationError::new_fmt(",", &self.left, &self.right, Some("comma can not be combined on objects, unless a list is the expected result")) ) )
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        let mut left = self.left.evaluate_list(on)?;
        let mut right = self.right.evaluate_list(on)?;

        left.append(&mut right);

        Ok(left)
    }

    fn print_self(&self) -> String {
        ",".to_string()
    }
}
impl CommaExpr {
    pub fn new(left: TotalNodes, right: TotalNodes) -> Self {
        Self {
            left: Box::new(left),
            right: Box::new(right),
        }
    }
    pub fn new_box(left: Box<TotalNodes>, right: Box<TotalNodes>) -> Self {
        Self {
            left,
            right,
        }
    }
}

/// Represents a joined, but not yet processed node. Calling `evaluate` or `evaluate_list` will always result in an error
#[derive(Default, PartialEq, Serialize, Deserialize, Clone)]
pub struct RawExpr {
    data: String,
    left: Option<Box<TotalNodes>>,
    right: Option<Box<TotalNodes>>
}
impl Display for RawExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_traversal(TreeOrderTraversal::Inorder))
    }
}
impl Debug for RawExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "raw-expr '{}' left: '{:?}' right: '{:?}'", &self.data, &self.left, &self.right)
    }
}
impl ASTNode for RawExpr {
    fn left(&self) -> Option<&TotalNodes> {
        self.left.as_deref()
    }
    fn right(&self) -> Option<&TotalNodes> {
        self.right.as_deref()
    }

    fn set_left(&mut self, new: Box<TotalNodes>) {
        self.left = Some(new);
    }
    fn set_right(&mut self, new: Box<TotalNodes>) {
        self.right = Some(new);
    }

    fn evaluate(&self, _: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        Err(UndefinedError::new("raw expressions have no evaluation").into())
    }

    fn print_self(&self) -> String {
        "'".to_string() + self.data.as_str() + "'"
    }
}