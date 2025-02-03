use std::fmt::{Debug, Display};
use crate::calc::{VariableUnion, VariableUnionRef, CalcResult, OperationError};

#[derive(PartialEq, Clone, Copy)]
pub enum RawOperator {
    Plus,
    Minus,
    Mul,
    Div,
    Pow,
    LBrace, // Normal braces ()
    RBrace,
    LCBrace, // Curly braces {}
    RCBrace, 
    LBBrace, // Block braces []
    RBBrace,
    Comma
}
impl Display for RawOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol())   
    }
}
impl Debug for RawOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} rank: {}", self.symbol(), self.rank())
    }
}
impl RawOperator {
    pub fn lookup(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self::Plus),
            '-' => Some(Self::Minus),
            '*' => Some(Self::Mul),
            '/' => Some(Self::Div),
            '^' => Some(Self::Pow),
            '(' => Some(Self::LBrace),
            ')' => Some(Self::RBrace),
            '{' => Some(Self::LCBrace),
            '}' => Some(Self::RCBrace),
            '[' => Some(Self::LBBrace),
            ']' => Some(Self::RBBrace),
            ',' => Some(Self::Comma),
            _ => None
        }
    }

    pub fn rank(&self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 1,
            Self::Mul | Self::Div | Self::Pow => 2,
            Self::LBrace | Self::RBrace | Self::LCBrace | Self::RCBrace | Self::LBBrace | Self::RBBrace => 3,
            Self::Comma => 4
        }
    }
    pub fn symbol(&self) -> char {
        match self {
            Self::Plus => '+',
            Self::Minus => '-',
            Self::Mul => '*',
            Self::Div => '/',
            Self::Pow => '^',
            Self::LBrace => '(',
            Self::RBrace => ')',
            Self::LCBrace => '{',
            Self::RCBrace => '}',
            Self::LBBrace => '[',
            Self::RBBrace => ']',
            Self::Comma => ','
        }
    }
    pub fn properties(&self) -> (char, u8) {
        (self.symbol(), self.rank())
    }

    pub fn apply(&self, a: VariableUnionRef<'_>, b: VariableUnionRef<'_>) -> CalcResult<VariableUnion> {
        match self {
            Self::Plus => a + b,
            Self::Minus => a - b,
            Self::Mul => a * b,
            Self::Div => a / b,
            Self::Pow => a.pow(b),
            _ => Err(OperationError::new_fmt(self.symbol(), &a, &b, None).into())
        }
    }
    pub fn apply_owned(&self, a: VariableUnion, b: VariableUnion) -> CalcResult<VariableUnion> {
        match self {
            Self::Plus => a + b,
            Self::Minus => a - b,
            Self::Mul => a * b,
            Self::Div => a / b,
            Self::Pow => a.pow(b),
            _ => Err(OperationError::new_fmt(self.symbol(), &a, &b, None).into())
        }
    }
}
 