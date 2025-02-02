use std::fmt::Display;
use crate::calc::{VariableUnion, CalcResult};

/*
    What is AST?

    AST, or Abstract Syntax Tree, is a collection of symbols, constants, and evaluations used to represent and evaluate expressions. 
    The base of this is the ASTBase trait, which is used to store the information in the AST Nodes.

 */

#[derive(Clone, Copy, PartialEq)]
pub enum PrintKind {
    Preorder,
    Inorder,
    Postorder
}
impl PrintKind {
    pub fn join_strings(&self, left: Option<String>, root: String, right: Option<String>) -> String{
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

pub trait ASTNode {
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion>;
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        Ok( vec![ self.evaluate(on)? ] )
    }

    fn print_self(&self, kind: PrintKind) -> String;
}

pub trait ASTJoinNode: ASTNode {

    /// Returns the left node for our combination.
    fn left(&self) -> Option<&dyn ASTNode>;
    /// Returns the right node for our combination
    fn right(&self) -> Option<&dyn ASTNode>;

    /// Sets the left node. The implementer may ignore this, and calling this does not imply that self.left() will return the value stored here.
    fn set_left<T>(&mut self, new: T)
    where
        T: ASTNode;
    /// Sets the right node. The implementer may ignore this, and calling this does not imply that self.right() will return the value stored here.
    fn set_right<T>(&mut self, new: T)
    where
        T: ASTNode;

    fn print_self(&self, kind: PrintKind) -> String {
        let left = self.left().map(|x| x.print_self(kind));
        let right = self.right().map(|x| x.print_self(kind));

        kind.join_strings(left, self.to_string(), right)
    }
}