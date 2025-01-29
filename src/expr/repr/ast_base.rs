use crate::calc::{calc_error::IndexOutOfRangeError, CalcError, MathVector, Scalar, VariableUnion};
use super::raw_oper::RawOperator;

use std::fmt::Display;

/*
    What is AST?

    AST, or Abstract Syntax Tree, is a collection of symbols, constants, and evaluations used to represent and evaluate expressions. 
    The base of this is the ASTBase trait, which is used to store the information in the AST Nodes.

 */

 pub trait ASTNode {
    fn left(&self) -> Option<&dyn ASTNode>;
    fn right(&self) -> Option<&dyn ASTNode>;

    fn set_left(&mut self, with: Box<dyn ASTNode>) -> bool;
    fn set_right(&mut self, with: Box<dyn ASTNode>) -> bool;

    fn can_change_children(&self) -> bool;

    fn evaluate(&self, on: &Vec<VariableUnion>) -> Result<VariableUnion, CalcError>;

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    fn to_string(&self) -> String;
    fn print_inorder(&self) -> String {
        let helper = |x: Option<&dyn ASTNode>| -> Option<String> {
            x.map(|t| t.print_inorder())
        };

        let myself = self.to_string();
        match (helper(self.left()), helper(self.right())) {
            (Some(a), Some(b)) => format!("{} {} {}", a, myself, b),
            (None, Some(b)) => format!("{} {}", myself, b),
            (Some(a), None) => format!("{} {}", a, myself),
            (None, None) => myself
        }
    }
    fn print_postorder(&self) -> String {
        let helper = |x: Option<&dyn ASTNode>| -> Option<String> {
            x.map(|t| t.print_postorder())
        };

        let myself = self.to_string();
        match (helper(self.left()), helper(self.right())) {
            (Some(a), Some(b)) => format!("{} {} {}", a, b, myself),
            (None, Some(b)) => format!("{} {}", b, myself),
            (Some(a), None) => format!("{} {}", a, myself),
            (None, None) => myself
        }
    }
 }

 impl Display for dyn ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f)
    }
 }

 pub struct OperatorExpr {
    data: RawOperator,
    left: Box<dyn ASTNode>,
    right: Box<dyn ASTNode>
 }

impl ASTNode for OperatorExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        Some(self.left.as_ref())
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        Some(self.right.as_ref())
    }

    fn set_left(&mut self, _: Box<dyn ASTNode>) -> bool {
        false
    }
    fn set_right(&mut self, _: Box<dyn ASTNode>) -> bool {
        false
    }

    fn can_change_children(&self) -> bool {
        false
    }

    fn evaluate(&self, on: &Vec<VariableUnion>) -> Result<VariableUnion, CalcError>{
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
impl OperatorExpr {
    pub fn new(oper: RawOperator, left: Box<dyn ASTNode>, right: Box<dyn ASTNode>) -> Self {
        Self {
            data: oper,
            left,
            right
        }
    }
}

pub struct ConstExpr {
    value: VariableUnion
}
impl ASTNode for ConstExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        None
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        None
    }

    fn set_left(&mut self, _: Box<dyn ASTNode>) -> bool {
        false
    }
    fn set_right(&mut self, _: Box<dyn ASTNode>) -> bool {
        false
    }

    fn can_change_children(&self) -> bool {
        false
    }

    fn evaluate(&self, _: &Vec<VariableUnion>) -> Result<VariableUnion, CalcError> {
        Ok(self.value.clone())
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.value)
    }
    fn to_string(&self) -> String {
        format!("{}", &self.value)
    }
}
impl ConstExpr {
    pub fn new(with: VariableUnion) -> Self {
        Self {
            value: with
        }
    }
}

pub struct VariableExpr {
    symbol: char,
    along: usize
}
impl ASTNode for VariableExpr {
    fn left(&self) -> Option<&dyn ASTNode> {
        None
    }
    fn right(&self) -> Option<&dyn ASTNode> {
        None
    }

    fn set_left(&mut self, _: Box<dyn ASTNode>) -> bool {
        false
    }
    fn set_right(&mut self, _: Box<dyn ASTNode>) -> bool {
        false
    }

    fn can_change_children(&self) -> bool {
        false
    }

    fn evaluate(&self, on: &Vec<VariableUnion>) -> Result<VariableUnion, CalcError> {
        if self.along >= on.len() {
            Err(CalcError::Index(IndexOutOfRangeError::new(self.along)))
        }
        else {
            Ok(on[self.along].clone())
        }
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol)
    }
    fn to_string(&self) -> String {
        format!("{}", self.symbol)
    }
}
impl VariableExpr {
    pub fn new(symbol: char, along: usize) -> Self {
        Self {
            symbol,
            along
        }
    }
}

pub fn basic_tree_functionality() {
    let tree_a = Box::new( ConstExpr::new(VariableUnion::from(Scalar::new(4.0))) );
    let tree_b = Box::new( OperatorExpr::new(
        RawOperator::new('*').unwrap(),
        Box::new(
            ConstExpr::new(VariableUnion::from(Scalar::new(1.5)))
        ),
        Box::new(
            ConstExpr::new(VariableUnion::from(MathVector::from(vec![1, 2, 3])))
        )
    ));
    let tree_c = Box::new( OperatorExpr::new( 
        RawOperator::new('+').unwrap(),
        Box::new( 
            OperatorExpr::new(
                RawOperator::new('*').unwrap(),
                Box::new(
                    VariableExpr::new('x', 0)
                ),
                Box::new(
                    ConstExpr::new(VariableUnion::from(Scalar::new(4.0)))
                )
            )
        ),
        Box::new(
            ConstExpr::new(VariableUnion::from(Scalar::new(1.66)))
        )
    ));

    let our_things: Vec<Box<dyn ASTNode>> = vec![tree_a, tree_b, tree_c];
    
    println!("Inorder printing:");
    for (i, expr) in our_things.iter().enumerate() {
        println!("For {i}: {}", expr.print_inorder())
    }
    println!();

    println!("Postorder printing:");
    for (i, expr) in our_things.iter().enumerate() {
        println!("For {i}: {}", expr.print_postorder())
    }
    println!();

    let on: Vec<VariableUnion> = vec![VariableUnion::from(Scalar::new(4.101))];
    let evals: Vec<Result<VariableUnion, CalcError>> = our_things.into_iter().map(|x| x.evaluate(&on) ).collect();
    for (i, eval) in evals.iter().enumerate() {
        match eval {
            Ok(v) => println!("Got {} from evaluation {i}", v),
            Err(e) => panic!("Got error '{}'", e)
        }
    }
}

#[test]
fn test_tree_structure() {
    basic_tree_functionality();
}