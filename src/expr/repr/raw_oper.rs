use std::fmt::{Debug, Display};
use crate::calc::{VariableUnion, VariableUnionRef, CalcError, CalcResult, OperationError};

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
 