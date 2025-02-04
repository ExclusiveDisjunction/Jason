use std::fmt::{Display, Debug};

use super::base::*;
use crate::expr::repr::ASTNode;
use crate::calc::{VariableUnion, CalcResult};

pub struct ASTBasedFunction {
    name: String,
    inner: Box<dyn ASTNode>,
    signature: FunctionArgSignature
}
impl Display for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({}) = {}", &self.name, &self.signature, &self.inner)
    }
}
impl Debug for ASTBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ast-func '{}' ({}) has: {:?}", &self.name, &self.signature, &self.inner)
    }
}
impl FunctionBase for ASTBasedFunction {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion> {
        self.signature.validate(args)?;

        self.inner.evaluate(args)
    }

    fn name(&self) -> &str {
        &self.name
    }
    fn signature(&self) -> &FunctionArgSignature {
        &self.signature
    }
}
impl ASTBasedFunction {
    pub fn new<T>(name: T, inner: Box<dyn ASTNode>, signature: FunctionArgSignature) -> Self where T: ToString {
        Self {
            name: name.to_string(),
            inner,
            signature
        }
    }
}

#[test]
fn test_ast_based_function() {
    use crate::expr::repr::{OperatorExpr, VariableExpr, RawOperator, ConstExpr};
    use crate::calc::VariableType;

    let ast: Box<dyn ASTNode> = Box::new(
        OperatorExpr::new(
            RawOperator::Mul,
            Box::new( 
                ConstExpr::new( 4.into() )
            ),
            Box::new(
                VariableExpr::new( 'x', 0 )
            )
        )
    );

    let func = ASTBasedFunction::new("f", ast, FunctionArgSignature::from(ArgSignature::new(VariableType::Scalar, "x")));

    let on = vec![VariableUnion::from(3.0)];
    assert_eq!(func.evaluate(&on), Ok(VariableUnion::from(3.0 * 4.0)));
}
