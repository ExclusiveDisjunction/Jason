pub mod raw_oper;
pub mod base;
pub mod leaf;
pub mod combine;
pub mod poly;

pub use raw_oper::RawOperator;
pub use base::{ASTNode, ASTJoinNode, TreeOrderTraversal};
pub use leaf::{RawLeafExpr, ConstExpr, VariableExpr};
pub use combine::{RawJoinExpr, OperatorExpr, CommaExpr};
pub use poly::{LeafNodes, JoinedNodes, TotalNodes};

use crate::calc::{VariableUnion, Scalar, MathVector, CalcError};

pub fn basic_tree_functionality() {
    let tree_a = Box::new( ConstExpr::new(4.into()) );
    let tree_b = Box::new( OperatorExpr::new(
        RawOperator::Mul,
        Box::new(
            ConstExpr::new(3.into() )
        ),
        Box::new(
            ConstExpr::new(MathVector::from(vec![1, 2, 3]).into() )
        )
    ));
    let tree_c = Box::new( OperatorExpr::new( 
        RawOperator::Plus,
        Box::new( 
            OperatorExpr::new(
                RawOperator::Mul,
                Box::new(
                    VariableExpr::new('x', 0)
                ),
                Box::new(
                    ConstExpr::new(4.into() )
                )
            )
        ),
        Box::new(
            ConstExpr::new(1.5.into() )
        )
    ));

    let our_things: Vec<Box<dyn ASTNode>> = vec![tree_a, tree_b, tree_c];
    
    println!("Inorder printing:");
    for (i, expr) in our_things.iter().enumerate() {
        println!("For {i}: {}", expr.print_self(TreeOrderTraversal::Inorder))
    }
    println!();

    println!("Postorder printing:");
    for (i, expr) in our_things.iter().enumerate() {
        println!("For {i}: {}", expr.print_self(TreeOrderTraversal::Postorder))
    }
    println!();

    let on: Vec<VariableUnion> = vec![4.101.into()];
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