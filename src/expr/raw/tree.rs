use super::id::Identifier;
use super::name::NameRef;
use super::ops::{BinaryOperator, UnaryOperator};

pub enum RawExpression<'a> {
    Scalar(f64),                     // 3.4
    Vector(Vec<f64>),                // [3.1, 2.4]
    Matrix(Vec<Vec<f64>>),           // [[1, 0], [0, 1]]
    Complex(f64),                    // 1.3i or 1.3j or j1.2 or i1.2
    BinOperation {                   // 2 + 4
        x: BinaryOperator, 
        lhs: Box<RawExpression<'a>>,
        rhs: Box<RawExpression<'a>>
    },
    UniOperation {                   // -1, -(1+4), !true, !(1 == 4)
        x: UnaryOperator,
        expr: Box<RawExpression<'a>>
    },
    Wrapped(Box<RawExpression<'a>>), // (3+4) <- The wrap is the parenthisis 
    FnCall {
        name: NameRef<'a>,
        args: Vec<RawExpression<'a>>
    },
    Identifier(Identifier<'a>)       //_::$x, $func()
}