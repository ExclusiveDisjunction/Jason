use crate::calc::{VariableUnion, VariableUnionRef, Scalar, MathVector, CalcError, CalcResult, OperationError, calc_error::IndexOutOfRangeError};

use std::fmt::{Debug, Display};

#[derive(Clone)]
pub struct RawOperator {
    symbol: char,
    order: u8
}

impl PartialEq for RawOperator {
    fn eq(&self, other: &Self) -> bool {
        self.order == other.order
    }
}
impl PartialEq<char> for RawOperator {
    fn eq(&self, other: &char) -> bool {
        self.symbol == *other
    }
}
impl Eq for RawOperator { }
impl PartialOrd for RawOperator {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for RawOperator {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.order.cmp(&other.order)
    }
}

impl Display for RawOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol)
    }
}
impl Debug for RawOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (p: {})", self.symbol, self.order)
    }
}

impl RawOperator {
    pub fn new(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self { symbol, order: 1 }),
            '-' => Some(Self { symbol, order: 1 }),
            '*' => Some(Self { symbol, order: 2 }),
            '/' => Some(Self { symbol, order: 2 }),
            '^' => Some(Self { symbol, order: 2 }),
            '(' => Some(Self { symbol, order: 3 }),
            ')' => Some(Self { symbol, order: 3 }),
            _ => None
        }
    }

    pub fn apply(&self, a: VariableUnionRef<'_>, b: VariableUnionRef<'_>) -> CalcResult<VariableUnion> {
        match self.symbol {
            '+' => a + b,
            '-' => a - b,
            '*' => a * b,
            '/' => a / b,
            _ => Err(CalcError::from(OperationError::new_fmt(&self.symbol.to_string(), &a, &b, None)))
        }
    }
    pub fn apply_owned(&self, a: VariableUnion, b: VariableUnion) -> CalcResult<VariableUnion> {
        match self.symbol {
            '+' => a + b,
            '-' => a - b,
            '*' => a * b,
            '/' => a / b,
            _ => Err(CalcError::from(OperationError::new_fmt(&self.symbol.to_string(), &a, &b, None)))
        }
    }
}

/*
    What is AST?

    AST, or Abstract Syntax Tree, is a collection of symbols, constants, and evaluations used to represent and evaluate expressions. 
    The base of this is the ASTBase trait, which is used to store the information in the AST Nodes.

 */

 pub trait ASTNode {
    fn left(&self) -> Option<&dyn ASTNode>;
    fn right(&self) -> Option<&dyn ASTNode>;

    fn evaluate(&self, on: &[VariableUnion]) -> Result<VariableUnion, CalcError>;

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

    fn evaluate(&self, _: &[VariableUnion]) -> Result<VariableUnion, CalcError> {
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

    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion> {
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