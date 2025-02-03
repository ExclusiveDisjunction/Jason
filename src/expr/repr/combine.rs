use super::{base::{ASTJoinNode, ASTNode, ASTRawNode, TreeOrderTraversal}, raw_oper::RawOperator, RawLeafExpr};
use crate::calc::{calc_error::{OperationError, UndefinedError}, CalcError, CalcResult, VariableUnion};


/// Represents an operation done on two sub children nodes. This is a joined node.
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

    fn print_self(&self, kind: TreeOrderTraversal) -> String {
        kind.join_strings(Some(self.left.print_self(kind)), self.data.to_string(), Some(self.right.print_self(kind)))
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "operator-expr {} left: {:?} right: {:?}", &self.data, &self.left, &self.right)
    }
}
impl ASTJoinNode for OperatorExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        Some( self.left.as_ref() )
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        Some( self.right.as_ref() )
    }

    fn set_left(&mut self, new: Box<dyn ASTNode>) {
        self.left = new;
    }
    fn set_right(&mut self, new: Box<dyn ASTNode>) {
        self.right = new;
    }
}
impl OperatorExpr {
    pub fn new<T, U>(oper: RawOperator, left: Box<dyn ASTNode>, right: Box<dyn ASTNode>) -> Self {
        Self {
            data: oper,
            left,
            right,
        }
    }
}

/// Represents a join between two nodes. Note that calling `evaluate` will always fail, as it does not make sense. 
pub struct CommaExpr {
    left: Box<dyn ASTNode>,
    right: Box<dyn ASTNode>
}
impl ASTNode for CommaExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Err( CalcError::from( OperationError::new_fmt(",", &self.left, &self.right, Some("comma can not be combined on objects, unless a list is the expected result")) ) )
    }
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        let mut left = self.left.evaluate_list(on)?;
        let mut right = self.right.evaluate_list(on)?;

        left.append(&mut right);

        Ok(left)
    }

    fn print_self(&self, kind: TreeOrderTraversal) -> String {
        kind.join_strings(Some(self.left.print_self(kind)), ",".to_string(), Some(self.right.print_self(kind)))
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "comma-expr left: {:?} right: {:?}", &self.left, &self.right)
    }
}
impl ASTJoinNode for CommaExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        Some(self.left.as_ref())
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        Some(self.right.as_ref())
    }

    fn set_left(&mut self, new: Box<dyn ASTNode>) {
        self.left = new;
    }
    fn set_right(&mut self, new: Box<dyn ASTNode>) {
        self.right = new;
    }
}
impl CommaExpr {
    pub fn new(left: Box<dyn ASTNode>, right: Box<dyn ASTNode>) -> Self {
        Self {
            left,
            right,
        }
    }
}

/// Represents a joined, but not yet processed node. Calling `evaluate` or `evaluate_list` will always result in an error
#[derive(Default)]
pub struct RawJoinExpr {
    data: String,
    left: Option<Box<dyn ASTNode>>,
    right: Option<Box<dyn ASTNode>>
}
impl ASTNode for RawJoinExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
        Err(UndefinedError::new("raw expressions have no evaluation").into())
    }

    fn print_self(&self, kind: TreeOrderTraversal) -> String {
        kind.join_strings(self.left.as_ref().map(|x| x.print_self(kind)), self.data.clone(), self.right.as_ref().map(|x| x.print_self(kind)))
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "raw-joined-expr '{}' left: {:?} right: {:?}", &self.data, &self.left, &self.right)
    }
}
impl ASTJoinNode for RawJoinExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        self.left.as_deref()
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        self.right.as_deref()
    }

    fn set_left(&mut self, new: Box<dyn ASTNode>) {
        self.left = Some(new);
    }
    fn set_right(&mut self, new: Box<dyn ASTNode>) {
        self.right = Some(new);
    }
}
impl ASTRawNode for RawJoinExpr {
    fn get_contents(&self) -> &str {
        &self.data
    }
    fn set_contents(&mut self, new: String) {
        self.data = new
    }
}
impl From<RawLeafExpr> for RawJoinExpr {
    fn from(value: RawLeafExpr) -> Self {
        Self {
            data: value.into(),
            left: None,
            right: None
        }
    }
}
impl From<RawJoinExpr> for (String, Option<Box<dyn ASTNode>>, Option<Box<dyn ASTNode>>) {
    fn from(value: RawJoinExpr) -> Self {
        (value.data, value.left, value.right)
    }
}
impl From<RawJoinExpr> for (RawLeafExpr, Option<Box<dyn ASTNode>>, Option<Box<dyn ASTNode>>) {
    fn from(value: RawJoinExpr) -> Self {
        (RawLeafExpr::from(value.data), value.left, value.right)
    }
}
impl RawJoinExpr {
    pub fn new<S>(contents: S, left: Option<Box<dyn ASTNode>>, right: Option<Box<dyn ASTNode>>) -> Self where S: ToString {
        Self{
            data: contents.to_string(),
            left,
            right
        }
    }
}