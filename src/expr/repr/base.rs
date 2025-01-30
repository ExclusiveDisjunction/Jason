use std::fmt::Display;
use crate::calc::{VariableUnion, CalcResult};

/*
    What is AST?

    AST, or Abstract Syntax Tree, is a collection of symbols, constants, and evaluations used to represent and evaluate expressions. 
    The base of this is the ASTBase trait, which is used to store the information in the AST Nodes.

 */

pub trait ASTNode {
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion>;

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn to_string(&self) -> String;
 }
 impl Display for dyn ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f)
    }
}