use std::{fmt::Display, ops::Not};
use std::str::FromStr;

use exdisj::error::FormattingError;
use serde::{Deserialize, Serialize};

/*
pub trait LogicAnd {

}
pub trait LogicOr {

}
pub trait LogicCmp {
    type Error;
    fn eq(&self, rhs: &Self) -> Result<bool, Self::Error>;
    fn less(&self, rhs: &Self) -> Result<bool, Self::Error>;

    fn ne(&self, rhs: &Self) -> Result<bool, Self::Error> {
        
    }
} */

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum Boolean {
    True,
    False
}
impl FromStr for Boolean {
    type Err = FormattingError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim().to_lowercase().as_str() {
            "true" => Ok( Self::True ),
            "false" => Ok( Self::False ),
            _ => Err( FormattingError::new(&s, "the expression must be either 'true' or 'false' to be a boolean"))
        }
    }
}
impl From<bool> for Boolean {
    fn from(value: bool) -> Self {
        if value {
            Self::True
        }
        else {
            Self::False
        }
    }
}
impl From<Boolean> for bool {
    fn from(value: Boolean) -> Self {
        matches!(value, Boolean::True)
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::True => "true",
                Self::False => "false"
            }
        )
    }
}
impl Not for Boolean {
    type Output = Self;
    fn not(self) -> Self::Output {
         match self {
            Self::True => Self::False,
            Self::False => Self::True
        }
    }
}
impl Boolean {
    pub fn and(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::True, Self::True) => Self::True,
            _ => Self::False
        }
    }
    pub fn or(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::False, Self::False) => Self::False,
            _ => Self::True
        }
    }
}

#[test]
fn test_boolean() {
    assert_eq!( Boolean::True.and(Boolean::True),   Boolean::True  );
    assert_eq!( Boolean::True.and(Boolean::False),  Boolean::False );
    assert_eq!( Boolean::False.and(Boolean::True),  Boolean::False );
    assert_eq!( Boolean::False.and(Boolean::False), Boolean::False );

    assert_eq!( Boolean::True.or(Boolean::True),   Boolean::True  );
    assert_eq!( Boolean::True.or(Boolean::False),  Boolean::True  );
    assert_eq!( Boolean::False.or(Boolean::True),  Boolean::True  );
    assert_eq!( Boolean::False.or(Boolean::False), Boolean::False );

    assert_eq!( Boolean::True.not(),  Boolean::False );
    assert_eq!( Boolean::False.not(), Boolean::True  );

    assert_eq!( Boolean::True.to_string().parse(),  Ok( Boolean::True )  );
    assert_eq!( Boolean::False.to_string().parse(), Ok( Boolean::False ) );
}