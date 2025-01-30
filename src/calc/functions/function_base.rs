use crate::calc::{VariableUnion, VariableUnionRef, CalcError, CalcResult, VariableType};
use crate::expr::repr::{ASTNode, VariableExpr, OperatorExpr, RawOperator, ConstExpr};

use std::fmt::{Display, Debug};
use std::collections::HashMap;

pub trait FunctionBase: PartialEq + Display + Debug {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion>;

    fn name(&self) -> &str;
}

#[derive(Clone)]
pub struct ArgSignature {
    input_kind: VariableType,
    name: String
}
impl Display for ArgSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", &self.name, &self.input_kind)
    }
}
impl ArgSignature {
    pub fn new<T>(kind: VariableType, name: T) -> Self where T: ToString{
        Self {
            input_kind: kind,
            name: name.to_string()
        }
    }
}

#[derive(Clone, Default)]
pub struct FunctionArgSignature {
    sig: Vec<ArgSignature>
}
impl Display for FunctionArgSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc = self.sig.iter().map(|x| format!("{}", x) ).collect::<Vec<String>>().join(", ");
        write!(f, "{}", desc)
    }
}
impl From<Vec<ArgSignature>> for FunctionArgSignature {
    fn from(value: Vec<ArgSignature>) -> Self {
        Self {
            sig: value
        }
    }
}

pub struct ASTBasedFunction {
    name: String,
    inner: Box<dyn ASTNode>,
    signature: FunctionArgSignature
}
impl Display for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}) = {}", &self.name, &self.signature, self.inner.print_inorder())
    }
}
impl Debug for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ast-func '{}' ({})", &self.name, &self.signature)
    }
}

pub struct ImplBasedFunction {
    name: String,
    action: fn (&[VariableUnion]) -> CalcResult<VariableUnion>,
    signature: FunctionArgSignature
}

pub fn test_func_display() {
    let sig = FunctionArgSignature::from(vec![ArgSignature::new(VariableType::Scalar, "x"), ArgSignature::new(VariableType::Vector, "y")]);
    println!("Signature is: {}", &sig);
}
