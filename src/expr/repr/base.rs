use std::fmt::{Display, Debug};
use crate::calc::{VariableUnion, CalcResult};

 /// A untility for the requesting and evlauation of pre-order, inorder, and post-order traversal of something. 
#[derive(Clone, Copy, PartialEq)]
pub enum TreeOrderTraversal {
    Preorder,
    Inorder,
    Postorder
}
impl Debug for TreeOrderTraversal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f, 
            "{}",
            match self {
                Self::Preorder => "preorder",
                Self::Inorder => "inorder",
                Self::Postorder => "postorder"
            }
        )
    }
}
impl TreeOrderTraversal {
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

/// The base representation for a node that can be evaluated (in singular or list form), and supports printing through methods.
pub trait ASTNode {
    /// Evaluates the node based on an input and returns a singular element.
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion>;
    /// Evaluates the nodes based on an input and returns a list of elements, depending on the implementing node.
    fn evaluate_list(&self, on: &[VariableUnion]) -> CalcResult<Vec<VariableUnion>> {
        Ok( vec![ self.evaluate(on)? ] )
    }

    /// Prints the node using `PrintKind`.
    fn print_self(&self, kind: TreeOrderTraversal) -> String;
    /// Prints who the node is, and some debug information. 
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl Display for dyn ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_self(TreeOrderTraversal::Inorder))
    }
}
impl Debug for dyn ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.debug_print(f)
    }
}

/// Represents an ASTNode that also can has children. Supports reading and writing these children.
pub trait ASTJoinNode: ASTNode {
    /// Returns the left node for our combination.
    fn left(&self) -> Option<&dyn ASTNode>;
    /// Returns the right node for our combination
    fn right(&self) -> Option<&dyn ASTNode>;

    /// Sets the left node. The implementer may ignore this, and calling this does not imply that self.left() will return the value stored here.
    fn set_left(&mut self, new: Box<dyn ASTNode>);
    /// Sets the right node. The implementer may ignore this, and calling this does not imply that self.right() will return the value stored here.
    fn set_right(&mut self, new: Box<dyn ASTNode>);
}

pub trait ASTRawNode : ASTNode {
    fn get_contents(&self) -> &str;
    fn set_contents(&mut self, new: String);
}