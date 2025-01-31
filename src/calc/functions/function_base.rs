use crate::{calc::{
    calc_error:: {
        ArgCountError, ArgTypeError, CalcError, OperationError
    }, scalar::ScalarLike, CalcResult, VariableData, VariableType, VariableUnion
}, expr::repr::{ASTLeafNodeKind, ConstExpr, RawOperator, VariableExpr, OperatorExpr, ASTJoinNodeKind}};
use crate::expr::repr::{ASTNode, ASTNodeKind, CombiningASTNode, combine::PrintKind};

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
impl From<ArgSignature> for FunctionArgSignature {
    fn from(value: ArgSignature) -> Self {
        Self {
            sig: vec![ value ]
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
        write!(f, "{}({}) = {}", &self.name, &self.signature, self.inner.print_self(PrintKind::Inorder))
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
impl ASTBasedFunction {
    pub fn new<T>(name: T, inner: ASTNodeKind, signature: FunctionArgSignature) -> Self where T: ToString {
        Self::new_box(name, Box::new(inner), signature)
    }
    pub fn new_box<T>(name: T, inner: Box<ASTNodeKind>, signature: FunctionArgSignature) -> Self where T: ToString {
        Self {
            name: name.to_string(),
            inner,
            signature
        }
    }
}

pub struct ImplBasedFunction {
    name: String,
    action: fn (&[VariableUnion]) -> CalcResult<VariableUnion>,
    signature: FunctionArgSignature
}
impl Display for ImplBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", &self.name, &self.signature)
    }
}
impl Debug for ImplBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "impl-func {}({})", &self.name, &self.signature)
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
impl ImplBasedFunction {
    pub fn new<T>(name: T, action: fn (&[VariableUnion]) -> CalcResult<VariableUnion>, signature: FunctionArgSignature) -> Self where T: ToString {
        Self {
            name: name.to_string(),
            action,
            signature
        }
    }
}

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

pub fn make_standard_functions() -> Vec<ImplBasedFunction> {
    let cos_func = | on: &[VariableUnion] | -> CalcResult<VariableUnion> {
        if on.len() != 1 {
            return Err(CalcError::from(ArgCountError::new(1, on.len())));
        }

        match &on[0] {
            VariableUnion::Sca(a) => Ok( VariableUnion::from(a.as_scalar().cos()) ),
            b => Err( CalcError::from(ArgTypeError::new(VariableType::Scalar, b.get_type()) ))
        }
    };
    let sin_func = | on: &[VariableUnion] | -> CalcResult<VariableUnion> {
        if on.len() != 1 {
            return Err(CalcError::from(ArgCountError::new(1, on.len())));
        }

        match &on[0] {
            VariableUnion::Sca(a) => Ok( VariableUnion::from(a.as_scalar().sin()) ),
            b => Err( CalcError::from(ArgTypeError::new(VariableType::Scalar, b.get_type()) ))
        }
    };
    let ln_func = | on: &[VariableUnion] | -> CalcResult<VariableUnion> {
        if on.len() != 1 {
            return Err(CalcError::from(ArgCountError::new(1, on.len())));
        }

        match &on[0] {
            VariableUnion::Sca(a) => Ok( VariableUnion::from(a.as_scalar().ln()) ),
            b => Err( CalcError::from(ArgTypeError::new(VariableType::Scalar, b.get_type()) ))
        }
    };
    let log_func = | on: &[VariableUnion] | -> CalcResult<VariableUnion> {
        if on.len() != 2 {
            return Err(CalcError::from(ArgCountError::new(2, on.len())));
        }

        match (&on[0], &on[1]) {
            (VariableUnion::Sca(a), VariableUnion::Sca(b)) => Ok( VariableUnion::from(a.as_scalar().log(b.as_scalar())) ),
            (a, b) => Err( CalcError::from( OperationError::new_fmt("log", a, b, None) ) )
        }
    };
    let dot_func = | on: &[VariableUnion] | -> CalcResult<VariableUnion> {
        if on.len() != 2 {
            return Err(CalcError::from(ArgCountError::new(2, on.len())));
        }

        match (&on[0], &on[1]) {
            (VariableUnion::Vec(a), VariableUnion::Vec(b)) => Ok( VariableUnion::from( a.dot_product(b)? ) ),
            (a, b) => Err( CalcError::from( OperationError::new_fmt("dot", a, b, None) ) )
        }
    };

    vec![
        ImplBasedFunction::new(
            "cos",
            cos_func,
            FunctionArgSignature::from(
                ArgSignature::new(VariableType::Scalar, "x")
            )
        ),
        ImplBasedFunction::new(
            "sin",
            sin_func,
            FunctionArgSignature::from(
                ArgSignature::new(VariableType::Scalar, "x")
            )
        ),
        ImplBasedFunction::new(
            "ln",
            ln_func,
            FunctionArgSignature::from(
                ArgSignature::new(VariableType::Scalar, "x")
            )
        ),
        ImplBasedFunction::new(
            "log",
            log_func,
            FunctionArgSignature::from(
                vec![
                    ArgSignature::new(VariableType::Scalar, "x"),
                    ArgSignature::new(VariableType::Scalar, "b")
                ]
            )
        ),
        ImplBasedFunction::new(
            "dot",
            dot_func,
            FunctionArgSignature::from(
                vec![
                    ArgSignature::new(VariableType::Vector, "a"),
                    ArgSignature::new(VariableType::Vector, "b")
                ]
            )
        )
    ]
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

#[test]
fn test_ast_based_function() {
    let ast = ASTNodeKind::Join(
        ASTJoinNodeKind::Operator(
            OperatorExpr::new(
                RawOperator::new('*').unwrap(),
                ASTNodeKind::from(
                    ASTLeafNodeKind::Const( ConstExpr::new(VariableUnion::from(4.0)) )
                ),
                ASTNodeKind::from(
                    ASTLeafNodeKind::Var( VariableExpr::new( 'x', 0) )
                )
            )
        )
    );

    let func = ASTBasedFunction::new("f", ast, FunctionArgSignature::from(ArgSignature::new(VariableType::Scalar, "x")));

    let on = vec![VariableUnion::from(3.0)];
    assert_eq!(func.evaluate(&on), Ok(VariableUnion::from(3.0 * 4.0)));
}
