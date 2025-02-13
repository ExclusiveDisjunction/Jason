use std::fmt::{Debug, Display};
use crate::calc::{VariableUnion, VariableUnionRef, CalcResult, OperationError};

use serde::{Serialize, Deserialize};

#[derive(Clone, Copy, Serialize, Deserialize)]
/// Represents different operators as a symbol and a rank.
/// Note that this classes comparisons (PartialEq, Eq, PartialOrd, Ord) all rely on the ***rank***, not the ***symbol***. Therefore `RawOperator::Plus == RawOperator::Minus` is true because they have the same rank. To equal based on characters, call the `eq_symbol` function.
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

impl PartialEq for RawOperator {
    fn eq(&self, other: &Self) -> bool {
        self.rank() == other.rank()
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
        self.rank().cmp(&other.rank())
    }
}

impl RawOperator {
    /// Attempts to resolve the symbol provided, returning an instance of this enum. if the symbol could not be resolved, this returns `None`. 
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

    /// Determines the operator rank (lower is less).
    pub fn rank(&self) -> u8 {
        match self {
            Self::Plus | Self::Minus => 1,
            Self::Mul | Self::Div | Self::Pow => 2,
            Self::LBrace | Self::RBrace | Self::LCBrace | Self::RCBrace | Self::LBBrace | Self::RBBrace => 3,
            Self::Comma => 4
        }
    }
    /// Gets the symbol associated with the current operator.
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
    /// Returns the symbol and rank.
    pub fn properties(&self) -> (char, u8) {
        (self.symbol(), self.rank())
    }

    /// Applies the operator to two `VariableUnionRef` instances.
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
    /// Applies the operator to two `VariableUnion` instances.
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

    pub fn eq_symbol(&self, other: &Self) -> bool {
        self.symbol() == other.symbol()
    }
    /// Determines if the `other` RawOperator is the closing pair to the current. This only applies to braces.
    /// For example, if `self` is `RawOperator::LBrace`, and other is `RawOperator::RBrace`, this returns true.
    pub fn closing_pair_symbol(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::LBrace, Self::RBrace) => true,
            (Self::LCBrace, Self::RCBrace) => true,
            (Self::LBBrace, Self::LBBrace) => true,
            (_, _) => false
        }
    }
    /// Determines if the `other` RawOperator is the opening pair to the current. This only applies to braces.
    /// For example, if `self` is `RawOperator::RBrace`, and other is `RawOperator::LBrace`, this returns true.
    pub fn opening_pair_symbol(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::RBrace, Self::LBrace) => true,
            (Self::RCBrace, Self::LCBrace) => true,
            (Self::RBBrace, Self::LBBrace) => true,
            (_, _) => false
        }
    }
}
 