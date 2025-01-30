use crate::calc::{VariableUnion, CalcResult, VariableType};
use crate::expr::repr::{ASTNode, ASTNodeKind};

use std::fmt::{Display, Debug};

pub trait FunctionBase {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion>;

    fn name(&self) -> &str;
    fn signature(&self) -> &FunctionArgSignature;
}
impl PartialEq for dyn FunctionBase {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.signature() == other.signature()
    }
}

#[derive(Clone, PartialEq)]
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

#[derive(Clone, Default, PartialEq)]
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
    inner: Box<ASTNodeKind>,
    signature: FunctionArgSignature
}
impl Display for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}) = {}", &self.name, &self.signature, &self.inner)
    }
}
impl Debug for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ast-func '{}' ({})", &self.name, &self.signature)
    }
}
impl FunctionBase for ASTBasedFunction {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion> {
        self.inner.evaluate(args)
    }

    fn name(&self) -> &str {
        &self.name
    }
    fn signature(&self) -> &FunctionArgSignature {
        &self.signature
    }
}

pub struct ImplBasedFunction {
    name: String,
    action: fn (&[VariableUnion]) -> CalcResult<VariableUnion>,
    signature: FunctionArgSignature
}
impl Display for ImplBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Debug for ImplBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl FunctionBase for ImplBasedFunction {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion> {
        (self.action)(args)    
    }

    fn name(&self) -> &str {
        &self.name
    }
    fn signature(&self) -> &FunctionArgSignature {
        &self.signature
    }
}

pub fn test_func_display() {
    let sig = FunctionArgSignature::from(
        vec![
            ArgSignature::new(VariableType::Scalar, "x"), 
            ArgSignature::new(VariableType::Vector, "y")
            ]
        );

    println!("Signature is: {}", &sig);
}
