use std::fmt::{Debug, Display};

use serde::{Serialize, Deserialize};

use super::base::{FunctionArgSignature, FunctionBase};
use super::astb::ASTBasedFunction;
use super::implb::ImplBasedFunction;
use crate::calc::{VariableUnion, CalcResult};

#[derive(Serialize, Deserialize)]
pub enum Function { 
    AST(ASTBasedFunction),
    Impl(ImplBasedFunction)
}

impl From<ASTBasedFunction> for Function {
    fn from(value: ASTBasedFunction) -> Self {
        Self::AST(value)
    }
}
impl From<ImplBasedFunction> for Function {
    fn from(value: ImplBasedFunction) -> Self {
        Self::Impl(value)
    }
}
impl<'a> AsRef<dyn FunctionBase + 'a> for Function {
    fn as_ref(&self) -> &(dyn FunctionBase + 'a) {
        match self {
            Self::AST(a) => a,
            Self::Impl(i) => i
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::AST(a) => a as &dyn Display,
            Self::Impl(i) => i as &dyn Display
        };
        x.fmt(f)
    }
}
impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Debug = match self {
            Self::AST(a) => a as &dyn Debug,
            Self::Impl(i) => i as &dyn Debug
        };
        x.fmt(f)
    }
}

impl FunctionBase for Function {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion> {
        self.as_ref().evaluate(args)
    }

    fn name(&self) -> &str {
        self.as_ref().name()
    }
    fn signature(&self) -> &FunctionArgSignature {
        self.as_ref().signature()
    }
}