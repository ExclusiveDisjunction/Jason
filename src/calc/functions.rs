pub use crate::expr::repr::{ASTNode, ASTJoinNode, TreeOrderTraversal};
use crate::calc::calc_error::{ArgCountError, ArgTypeError, CalcError, CalcResult};
use crate::calc::{ScalarLike, VariableData, VariableType, VariableUnion};

use std::fmt::{Display, Debug};
use std::iter::zip;

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

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn kind(&self) -> VariableType {
        self.input_kind
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
impl FunctionArgSignature {
    /// Determines if the signatures stored in `on` are valid against the signature. If this function returns Ok, there is a total guarentee that the data stored in `on` matches what the function "expects", so long as the signature is valid for the function.
    pub fn validate(&self, on: &[VariableUnion]) -> Result<(), CalcError> {
        /*
            There are a few checks.
            1. The size must match the size of sig.
            2. For each argument, the type of each argument, in order, should match. 
         */

        if on.len() != self.sig.len() {
            Err( ArgCountError::new(self.sig.len(), on.len()).into() )
        }
        else {
            for (a, b) in zip(on.iter(), self.sig.iter()) {
                if a.get_type() != b.kind() {
                    return Err( ArgTypeError::new(a.get_type(), b.kind(), b.name()).into() )
                }
            }

            Ok(())
        }
    }

    pub fn just_x() -> Self {
        Self {
            sig: vec![ArgSignature::new(VariableType::Scalar, 'x')]
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

pub struct ImplBasedFunction {
    name: String,
    action: fn (&[VariableUnion], &FunctionArgSignature) -> CalcResult<VariableUnion>,
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
        (self.action)(args, &self.signature)    
    }

    fn name(&self) -> &str {
        &self.name
    }
    fn signature(&self) -> &FunctionArgSignature {
        &self.signature
    }
}
impl ImplBasedFunction {
    pub fn new<T>(name: T, action: fn (&[VariableUnion], &FunctionArgSignature) -> CalcResult<VariableUnion>, signature: FunctionArgSignature) -> Self where T: ToString {
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
    let cos_func = | on: &[VariableUnion], sig: &FunctionArgSignature | -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().cos().into() ),
            _ => panic!() //This cannot happen
        }
    };
    let sin_func = | on: &[VariableUnion], sig: &FunctionArgSignature | -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().sin().into() ),
            _ => panic!() //This cannot happen
        }
    };
    let ln_func = | on: &[VariableUnion], sig: &FunctionArgSignature | -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().ln().into() ),
            _ => panic!() //This cannot happen
        }
    };
    let log_func = | on: &[VariableUnion], sig: &FunctionArgSignature | -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Sca(a), VariableUnion::Sca(b)) => Ok( a.as_scalar().log(b.as_scalar()).into() ),
            _ => panic!() //This cannot happen
        }
    };
    let dot_func = | on: &[VariableUnion], sig: &FunctionArgSignature | -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Vec(a), VariableUnion::Vec(b)) => Ok( VariableUnion::from( a.dot_product(b)? ) ),
            _ => panic!() // Cannot happen
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

pub struct StandardFunctions { }
impl StandardFunctions {
    //Implementations
    pub fn cos_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().cos().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn acos_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().acos().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn sin_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().sin().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn asin_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().asin().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn tan_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().tan().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn atan_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().atan().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn atan2_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Sca(a), VariableUnion::Sca(b)) => Ok( a.as_scalar().atan2(b.as_scalar()).into() ),
            _ => panic!() //This cannot happen
        }
    }

    pub fn ln_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().ln().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn nlog_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match &on[0] {
            VariableUnion::Sca(a) => Ok( a.as_scalar().log10().into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn log_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Sca(a), VariableUnion::Sca(b)) => Ok( a.as_scalar().log(b.as_scalar()).into() ),
            _ => panic!() //This cannot happen
        }
    }

    pub fn dot_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Vec(a), VariableUnion::Vec(b)) => Ok( a.dot_product(b)?.into() ),
            _ => panic!() //This cannot happen
        }
    }
    pub fn cross_proc(on: &[VariableUnion], sig: &FunctionArgSignature) -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Vec(a), VariableUnion::Vec(b)) => Ok( a.cross_product(b)?.into() ),
            _ => panic!() //This cannot happen
        }
    }

    //Functions
    pub fn cos_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "cos",
            StandardFunctions::cos_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn acos_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "acos",
            StandardFunctions::acos_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn sin_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "sin",
            StandardFunctions::sin_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn asin_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "asin",
            StandardFunctions::asin_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn tan_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "tan",
            StandardFunctions::tan_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn atan_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "atan",
            StandardFunctions::atan_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn atan2_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            "atan2",
            StandardFunctions::atan2_proc,
            vec![ArgSignature::new(VariableType::Scalar, 'y'), ArgSignature::new(VariableType::Scalar, 'x')].into()
        )
    }

    //All
    pub fn get_all() -> Vec<ImplBasedFunction> {

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

#[test]
fn test_ast_based_function() {
    use crate::expr::repr::{OperatorExpr, VariableExpr, RawOperator, ConstExpr};

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

#[test]
fn test_impl_based_function() {
    let log_func = | on: &[VariableUnion], sig: &FunctionArgSignature | -> CalcResult<VariableUnion> {
        sig.validate(on)?;

        match (&on[0], &on[1]) {
            (VariableUnion::Sca(a), VariableUnion::Sca(b)) => Ok( a.as_scalar().log(b.as_scalar()).into() ),
            _ => panic!() //This cannot happen
        }
    };

    let func = ImplBasedFunction::new(
        "log",
        log_func,
        FunctionArgSignature::from(
            vec![
                ArgSignature::new(VariableType::Scalar, "x"),
                ArgSignature::new(VariableType::Scalar, "b")
            ]
        )
    );

    let a = 64.0f64;
    let b = 2.0f64;
    let on: Vec<VariableUnion> = vec![a.into(), b.into()];

    assert_eq!(func.evaluate(&on), Ok(a.log(b).into()));
}
