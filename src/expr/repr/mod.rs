pub mod raw_oper;
pub mod base;
pub mod leaf;
pub mod combine;
pub mod poly;

pub use raw_oper::RawOperator;
pub use base::{ASTNode, TreeOrderTraversal};
pub use leaf::{ConstExpr, VariableExpr};
pub use combine::{OperatorExpr, CommaExpr, RawExpr};
pub use poly::TotalNodes;

#[test]
fn test_tree_eval() {
    use crate::calc::{VariableUnion, MathVector, CalcResult, Matrix};

    let tree_a: TotalNodes = ConstExpr::new(4.into()).into();
    let tree_b: TotalNodes = OperatorExpr::new_box(
        RawOperator::Mul,
        Box::new(
            ConstExpr::new(3.into() ).into()
        ),
        Box::new(
            ConstExpr::new(MathVector::from(vec![1, 2, 3]).into() ).into()
        )
    ).into();
    let mut tree_c: TotalNodes = OperatorExpr::new_box( 
        RawOperator::Plus,
        Box::new( 
            OperatorExpr::new_box(
                RawOperator::Mul,
                Box::new(
                    VariableExpr::new('x', 0).into()
                ),
                Box::new(
                    ConstExpr::new(4.into() ).into()
                )
            ).into()
        ),
        Box::new(
            ConstExpr::new(1.5.into() ).into()
        )
    ).into();

    let on: Vec<VariableUnion> = vec![4.into()];

    {
        let ours: Vec<&TotalNodes> = vec![&tree_a, &tree_b, &tree_c];
        let eval: Vec<CalcResult<VariableUnion>> = ours.into_iter().map(|x| x.evaluate(&on) ).collect();

        assert_eq!(eval[0], Ok( 4.0.into() ) );
        assert_eq!(eval[1], Ok( MathVector::from(vec![3, 6, 9]).into() ) );
        assert_eq!(eval[2], Ok( 17.5.into() ) );
    }

    tree_c.set_right(
        Box::new(
            OperatorExpr::new_box (
                RawOperator::Pow,
                Box::new(
                    VariableExpr::new('x', 0).into()
                ),
                Box::new(
                    ConstExpr::new( 2.into() ).into()
                )
            ).into()
        )
    );

    assert_eq!(tree_c.evaluate(&on), Ok( 32.into() ) );

    tree_c.set_left(
        Box::new(
            RawExpr::new("Hello there, I will not evaluate".to_string(), None, None).into()
        )
    );
    assert!(tree_c.evaluate(&on).is_err());

    tree_c.set_left(
        Box::new(
            CommaExpr::new_box(
                Box::new(
                    ConstExpr::new(4.into()).into()
                ),
                Box::new(
                    ConstExpr::new(5.into()).into()
                )
            ).into()
        )
    );
    assert!(tree_c.evaluate(&on).is_err());
    assert!(tree_c.evaluate_list(&on).is_err()); //Error because comma is stored under operator.
    
    let tree_d: TotalNodes = CommaExpr::new(
        CommaExpr::new(
            VariableExpr::new('x', 0).into(),
            ConstExpr::new( Matrix::identity(2).into() ).into(),
        ).into(),
        ConstExpr::new(MathVector::from(vec![1, 2, 3]).into() ).into()
    ).into();

    assert_eq!(tree_d.evaluate_list(&on), Ok( vec![VariableUnion::from(4), Matrix::identity(2).into(), MathVector::from(vec![1, 2, 3]).into() ] ));

    let tree_e = RawExpr::new("hello".to_string(), None, None);

    assert!(tree_e.evaluate(&on).is_err() && tree_e.evaluate_list(&on).is_err());

}