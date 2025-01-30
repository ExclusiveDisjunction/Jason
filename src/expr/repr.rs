use crate::calc::{VariableUnion, VariableUnionRef, Scalar, MathVector, CalcError, CalcResult, OperationError, calc_error::IndexOutOfRangeError};

use std::fmt::{Debug, Display};

pub mod raw_oper;
pub mod base;
pub mod leaf;
pub mod combine;




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