use std::fmt::{Display, Debug};

use super::base::*;
use crate::expr::repr::{ASTNode, TotalNodes};
use crate::calc::{VariableUnion, CalcResult};

use serde::{Serialize, Deserialize};


#[derive(Serialize, Deserialize, PartialEq, Clone)]
pub struct ASTBasedFunction {
    inner: TotalNodes,
    signature: FunctionArgSignature
}
impl Display for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}) = {}", &self.signature, &self.inner)
    }
}
impl Debug for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ast-func sig: ({}) has: '{:?}'", &self.signature, &self.inner)
    }
}
impl FunctionBase for ASTBasedFunction {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion> {
        self.signature.validate(args)?;

        self.inner.evaluate(args)
    }

    fn signature(&self) -> &FunctionArgSignature {
        &self.signature
    }
}
impl ASTBasedFunction {
    pub fn new( inner: TotalNodes, signature: FunctionArgSignature) -> Self {
        Self {
            inner,
            signature
        }
    }
}

#[test]
fn test_ast_based_function() {
    use crate::expr::repr::{OperatorExpr, VariableExpr, RawOperator, ConstExpr};

    let ast: TotalNodes = OperatorExpr::new(
        RawOperator::Mul,
        ConstExpr::new( 4.into() ).into(),
        VariableExpr::new( 'x', 0 ).into()
    ).into();

    let func = ASTBasedFunction::new(ast, FunctionArgSignature::just_x());

    let on = vec![3.into()];
    assert_eq!(func.evaluate(&on), Ok( (3 * 4).into() ));
}
