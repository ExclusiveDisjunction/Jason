use std::collections::vec_deque::{VecDeque, Iter, IterMut, IntoIter};
use std::fmt::{Write as _, Display};

use super::prelude::*;
use super::ops::BinaryOperator;

use crate::calc;
use crate::calc::{FloatMatrix, matrix::MatrixConversionError, Literal};

#[derive(PartialEq, Clone, Debug)]
pub enum RawExpression {
    Scalar(f64),                     // 3.4
    Vector(Vec<f64>),                // [3.1, 2.4]
    Matrix(Vec<Vec<f64>>),           // [[1, 0], [0, 1]]
    Complex(f64),                    // 1.3i or 1.3j or j1.2 or i1.2
    Bool(bool),
    BinOper(BinaryOperation<Self>),  // 2 + 4
    Wrapped(Box<Self>),              // (3+4) <- The wrap is the parenthisis 
}
impl Display for RawExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(v) => write!(f, "{}", v),
            Self::Complex(v) => write!(f, "{}", v),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Vector(v) => write!(f, "{}", v),
            Self::Matrix(v) => write!(f, "{}", v),
            Self::BinOper(v) => write!(f, "{}", v),
            Self::Wrapped(b) => write!(f, "({})", b)
        }
    }
}
impl Default for RawExpression {
    fn default() -> Self {
        Self::Scalar(0.0)
    }
}

/// A structure used for testing and internal systems.
/// It is used to construct [`RawExpression`] instances.
#[derive(Default, Debug)]
pub struct RawExpressionBuilder {
    internal: RawExpression
}
impl RawExpressionBuilder {
    pub fn new() -> Self {
        Self {
            internal: RawExpression::default()
        }
    }

    pub fn scalar(mut self, val: f64) -> Self {
        self.internal = RawExpression::Scalar(val);
        self
    }
    pub fn complex(mut self, val: f64) -> Self {
        self.internal = RawExpression::Complex(val);
        self
    }
    pub fn boolean(mut self, val: bool) -> Self {
        self.internal = RawExpression::Bool(val);
        self
    }
    pub fn vec(mut self, val: Vec<f64>) -> Self {
        self.internal = RawExpression::Vector(val);
        self
    }
    pub fn matrix(mut self, val: Vec<Vec<f64>>) -> Self {
        self.internal = RawExpression::Matrix(val);
        self
    }
    pub fn wrapped<F>(mut self, clos: F) -> Self where F: FnOnce(Self) -> Self {
        let helper = Self::new();
        let inner = clos(helper).build();
        self.internal = RawExpression::Wrapped(Box::new(inner));

        self
    }
    pub fn bin_oper<L, R>(mut self, op: BinaryOperator, lhs: L, rhs: R) -> Self
        where L: FnOnce(Self) -> Self,
        R: FnOnce(Self) -> Self {
            let helper_a = Self::new();
            let helper_b = Self::new();

            let lhs = lhs(helper_a).build();
            let rhs = rhs(helper_b).build();

            self.internal = RawExpression::BinOper(
                BinaryOperation::new(op, lhs, rhs)
            );
            self
    }

    pub fn build(self) -> RawExpression {
        self.internal
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionElement {
    Literal(Literal),
    Operator(BinaryOperator),
    LBrace,
    RBrace
}
impl Display for ExpressionElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{}", l),
            Self::Operator(o) => write!(f, "{}", o),
            Self::LBrace => f.write_char('('),
            Self::RBrace => f.write_char(')')
        }
    }
}
impl<T> From<T> for ExpressionElement where T: Into<Literal> {
    fn from(value: T) -> Self {
        Self::Literal(value.into())
    }
}
impl From<BinaryOperator> for ExpressionElement {
    fn from(value: BinaryOperator) -> Self {
        Self::Operator(value)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Queue<T>(VecDeque<T>);
impl<T> Display for Queue<T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let collected: Vec<String> = self.0.iter().map(|x| x.to_string()).collect();
        let as_string = collected.join(", ");

        f.write_str(&as_string)
    }
}
impl<A> FromIterator<A> for Queue<A> {
    fn from_iter<T>(iter: T) -> Self where T: IntoIterator<Item = A> {
        Self(VecDeque::from_iter(iter))
    }
}
impl<T> Queue<T> {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }
    
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn front(&self) -> Option<&T> {
        self.0.front()
    }
    pub fn front_mut(&mut self) -> Option<&mut T> {
        self.0.front_mut()
    }
    pub fn back(&self) -> Option<&T> {
        self.0.front()
    }
    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.0.back_mut()
    }

    pub fn push(&mut self, value: T) {
        self.0.push_back(value)
    }
    pub fn pop(&mut self) -> Option<T> {
        self.0.pop_front()
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.0.iter()
    }
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        self.0.iter_mut()
    }
}
impl<T> IntoIterator for Queue<T> {
    type IntoIter = IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}


impl TryFrom<RawExpression> for Queue<ExpressionElement> {
    type Error = MatrixConversionError;
    fn try_from(value: RawExpression) -> Result<Self, Self::Error> {
        let mut result: Queue<ExpressionElement> = Queue::new();
        result.flatten(value)?;

        Ok( result )
    }
}
impl Queue<ExpressionElement> {
    fn flatten(&mut self, value: RawExpression) -> Result<(), MatrixConversionError> {
        use RawExpression::*;
        match value {
            Scalar(v) => self.push(v.into()),
            Complex(v) => self.push(calc::Complex::new(0.0, v).into()),
            Vector(v) => self.push(calc::FloatVector::from(v).into()),
            Matrix(v) => {
                let matrix = FloatMatrix::try_from(v)?;

                self.push(matrix.into());
            },
            Bool(v) => self.push(calc::Boolean::from(v).into()),
            BinOper(o) => {
                let oper: ExpressionElement = o.oper().into();
                let (lhs, rhs) = o.take();
                self.flatten(lhs)?;
                self.push(oper);
                self.flatten(rhs)?;
            },
            Wrapped(v) => {
                let v = *v;

                self.push(ExpressionElement::LBrace);
                self.flatten(v)?;
                self.push(ExpressionElement::RBrace);
            }
        }

        Ok( () )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flatten_test() {
        let tests = [
            // Binary operators and all literals
            (
                RawExpressionBuilder::new()
                    .bin_oper(
                        BinaryOperator::Add,
                        |b| b.scalar(4.0),
                        |b| b.scalar(3.0)
                    )
                    .build(),
                Some( [calc::Scalar::new(4.0).into(), BinaryOperator::Add.into(), calc::Scalar::new(3.0).into()].into_iter().collect() )
            ),

            (
                RawExpressionBuilder::new()
                    .bin_oper(
                        BinaryOperator::Sub,
                        |b| b.complex(4.0),
                        |b| b.complex(3.0)
                    )
                    .build(),
                Some( [calc::Complex::new(0.0, 4.0).into(), BinaryOperator::Sub.into(), calc::Complex::new(0.0, 3.0).into()].into_iter().collect() )
            ),
                
            (
                RawExpressionBuilder::new()
                    .bin_oper( 
                        BinaryOperator::And, 
                        |b| b.boolean(true),
                        |b| b.boolean(true)
                    )
                    .build(),
                Some( [calc::Boolean::True.into(), BinaryOperator::And.into(), calc::Boolean::True.into() ].into_iter().collect() )
            ),

            (
                RawExpressionBuilder::new()
                    .bin_oper(
                        BinaryOperator::Add,
                        |b| b.vec(vec![1.0, 2.0, 3.0]),
                        |b| b.vec(vec![2.0, 4.0, 6.0])
                    )
                    .build(),

                Some( [calc::FloatVector::from([1.0, 2.0, 3.0]).into(), BinaryOperator::Add.into(), calc::FloatVector::from([2.0, 4.0, 6.0]).into()].into_iter().collect() )
            ),

            (
                RawExpressionBuilder::new()
                    .bin_oper(
                        BinaryOperator::Add,
                        |b| b.matrix(vec![vec![1.0, 2.0], vec![3.0, 4.0]]),
                        |b| b.matrix(vec![vec![1.0, 2.0], vec![3.0, 4.0]])
                    )
                    .build(),

                Some( [calc::FloatMatrix::try_from(vec![vec![1.0, 2.0], vec![3.0, 4.0]]).unwrap().into(), BinaryOperator::Add.into(), calc::FloatMatrix::try_from(vec![vec![1.0, 2.0], vec![3.0, 4.0]]).unwrap().into()].into_iter().collect() )
            ),

            //Wrapped expression
            (
                RawExpressionBuilder::new()
                    .wrapped(|b| b.scalar(3.0))
                    .build(),
                
                Some( [ExpressionElement::LBrace, calc::Scalar::new(3.0).into(), ExpressionElement::RBrace].into_iter().collect() )
            ),

            //Invalid matrix expression

            (
                RawExpressionBuilder::new()
                    .matrix(vec![vec![1.0, 0.0], vec![1.0]])
                    .build(),

                None
            )
        ];

        for (raw, result) in tests {
            let built = Queue::try_from(raw).ok();

            assert_eq!(built, result);
        }
    }
}