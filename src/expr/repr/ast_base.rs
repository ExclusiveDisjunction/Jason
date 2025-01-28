use std::fmt::{Display, Debug};
use crate::calc::VariableUnion;

/*
    What is AST?

    AST, or Abstract Syntax Tree, is a collection of symbols, constants, and evaluations used to represent and evaluate expressions. 
    The base of this is the ASTBase trait, which is used to store the information in the AST Nodes.

 */

pub trait ASTBase {
    fn evaluate(&self, env: Vec<VariableUnion>) -> VariableUnion;
}

pub enum ASTEntry {
    Operator(char),
    Constant(VariableUnion),
    Variable(char),
    
}

pub struct ASTNode {
    data: Box<dyn ASTBase>,
    left: Option<Box<ASTNode>>,
    right: Option<Box<ASTNode>>
}