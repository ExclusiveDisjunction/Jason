use std::fmt::Display;

use super::ops::{UnaryOperator, BinaryOperator};
use crate::prelude::name::{Name, NameRef};

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOperation<T> where T: ?Sized {
    x: BinaryOperator,
    lhs: Box<T>,
    rhs: Box<T>
}
impl<T> Display for BinaryOperation<T> where T: ?Sized + Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", &self.lhs, self.x, &self.rhs)
    }
}
impl<T> BinaryOperation<T> where T: ?Sized {
    pub fn new_boxed(x: BinaryOperator, lhs: Box<T>, rhs: Box<T>) -> Self {
        Self {
            x,
            lhs,
            rhs
        }
    }

    pub fn oper(&self) -> BinaryOperator {
        self.x
    }
    pub fn lhs(&self) -> &T {
        &self.lhs
    }
    pub fn rhs(&self) -> &T {
        &self.rhs
    }
}
impl<T> BinaryOperation<T> where T: Sized {
    pub fn new(x: BinaryOperator, lhs: T, rhs: T) -> Self {
        Self::new_boxed(x, Box::new(lhs), Box::new(rhs))
    }

    pub fn take(self) -> (T, T) where T: Sized {
        let lhs = *self.lhs;
        let rhs = *self.rhs;

        (lhs, rhs)
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct UnaryOperation<T> where T: ?Sized {
    x: UnaryOperator,
    rhs: Box<T>
}
impl<T> Display for UnaryOperation<T> where T: ?Sized + Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.x, &self.rhs)
    }
}
impl<T> UnaryOperation<T> where T: ?Sized {
    pub fn new_boxed(x: UnaryOperator, rhs: Box<T>) -> Self {
        Self {
            x,
            rhs
        }
    }

    pub fn oper(&self) -> UnaryOperator {
        self.x
    }
    pub fn rhs(&self) -> &T {
        &self.rhs
    }
}
impl<T> UnaryOperation<T> where T: Sized {
    pub fn new(x: UnaryOperator, rhs: T) -> Self {
        Self::new_boxed(x, Box::new(rhs))
    }
    
    pub fn take(self) -> T {
        *self.rhs
    }    
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnCallRef<'a, T> {
    name: NameRef<'a>,
    args: Vec<T>
}
impl<'a, T> FnCallRef<'a, T> {
    pub fn new<I>(name: NameRef<'a>, args: I) -> Self where I: IntoIterator<Item = T> {
        Self {
            name,
            args: args.into_iter().collect()
        }
    }
    
    pub fn name(&self) -> NameRef<'a> {
        self.name.clone()
    }
    pub fn args(&self) -> &[T] {
        &self.args
    }

    pub fn take(self) -> (NameRef<'a>, Vec<T>) {
        (self.name, self.args)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct FnCall<T> {
    name: Name,
    args: Vec<T>
}
impl<T> FnCall<T> {
    pub fn new<I>(name: Name, args: I) -> Self where I: IntoIterator<Item = T> {
        Self {
            name,
            args: args.into_iter().collect()
        }
    }
    
    pub fn name(&self) -> NameRef<'_> {
        self.name.as_name_ref()
    }
    pub fn args(&self) -> &[T] {
        &self.args
    }

    pub fn take(self) -> (Name, Vec<T>) {
        (self.name, self.args)
    }
} 