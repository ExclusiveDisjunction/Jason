use std::fmt::{Display, Debug};

use super::base::*;
use crate::calc::{VariableUnion, CalcResult, ScalarLike, VariableType};

pub struct ImplBasedFunction {
    action: fn (&[VariableUnion], &FunctionArgSignature) -> CalcResult<VariableUnion>,
    signature: FunctionArgSignature
}
impl Display for ImplBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", &self.signature)
    }
}
impl Debug for ImplBasedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "impl-func sig: ({})", &self.signature)
    }
}
impl PartialEq for ImplBasedFunction {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
impl FunctionBase for ImplBasedFunction {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion> {
        (self.action)(args, &self.signature)    
    }

    fn signature(&self) -> &FunctionArgSignature {
        &self.signature
    }
}
impl ImplBasedFunction {
    pub fn new(action: fn (&[VariableUnion], &FunctionArgSignature) -> CalcResult<VariableUnion>, signature: FunctionArgSignature) -> Self {
        Self {
            action,
            signature
        }
    }
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
            StandardFunctions::cos_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn acos_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::acos_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn sin_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::sin_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn asin_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::asin_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn tan_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::tan_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn atan_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::atan_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn atan2_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::atan2_proc,
            vec![ArgSignature::new(VariableType::Scalar, 'y'), ArgSignature::new(VariableType::Scalar, 'x')].into()
        )
    }

    pub fn ln_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::ln_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn nlog_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::nlog_proc,
            FunctionArgSignature::just_x()
        )
    }
    pub fn log_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::log_proc,
            vec![ArgSignature::new(VariableType::Scalar, 'x'), ArgSignature::new(VariableType::Scalar, 'b')].into()
        )
    }

    pub fn dot_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::dot_proc,
            vec![ArgSignature::new(VariableType::Vector, 'a'), ArgSignature::new(VariableType::Vector, 'b')].into()
        )
    }
    pub fn cross_func() -> ImplBasedFunction {
        ImplBasedFunction::new(
            StandardFunctions::cross_proc,
            vec![ArgSignature::new(VariableType::Vector, 'a'), ArgSignature::new(VariableType::Vector, 'b')].into()
        )
    }

    //All
    pub fn get_all() -> Vec<(ImplBasedFunction, String)> {
        vec![
            (Self::cos_func(), "cos".to_string()),
            (Self::acos_func(), "acos".to_string()),
            (Self::sin_func(), "sin".to_string()),
            (Self::asin_func(), "asin".to_string()),
            (Self::tan_func(), "tan".to_string()),
            (Self::atan_func(), "atan".to_string()),
            (Self::atan2_func(), "atan2".to_string()),
            (Self::ln_func(), "ln".to_string()),
            (Self::nlog_func(), "nlog".to_string()),
            (Self::log_func(), "log".to_string()),
            (Self::dot_func(), "dot".to_string()),
            (Self::cross_func(), "cross".to_string())
        ]
    }
}

#[test]
fn test_impl_based_function() {
    use crate::calc::MathVector;

    let a = 64.0f64;
    let b = 2.0f64;
    let atrig = 2.0f64.sqrt() / 2.0f64;
    let c = MathVector::from(vec![1, 2, 3]);
    let d = MathVector::from(vec![3, 2, 1]);
    let single_on: Vec<VariableUnion> = vec![a.into()];
    let asingle_on: Vec<VariableUnion> = vec![atrig.into()];
    let double_on: Vec<VariableUnion> = vec![a.into(), b.into()];
    let vdouble_on: Vec<VariableUnion> = vec![c.clone().into(), d.clone().into()];

    assert_eq!(StandardFunctions::cos_func().evaluate(&single_on), Ok(a.cos().into()));
    assert_eq!(StandardFunctions::acos_func().evaluate(&asingle_on), Ok(atrig.acos().into()));
    assert_eq!(StandardFunctions::sin_func().evaluate(&single_on), Ok(a.sin().into()));
    assert_eq!(StandardFunctions::asin_func().evaluate(&asingle_on), Ok(atrig.asin().into()));
    assert_eq!(StandardFunctions::tan_func().evaluate(&single_on), Ok(a.tan().into()));
    assert_eq!(StandardFunctions::atan_func().evaluate(&asingle_on), Ok(atrig.atan().into()));
   // assert_eq!(StandardFunctions::atan2_func().evaluate(&asingle_on), Ok(a.atan2(b).into()));

    assert_eq!(StandardFunctions::ln_func().evaluate(&single_on), Ok(a.ln().into()));
    assert_eq!(StandardFunctions::nlog_func().evaluate(&single_on), Ok(a.log10().into()));
    assert_eq!(StandardFunctions::log_func().evaluate(&double_on), Ok(a.log(b).into()));

    let dot = c.dot_product(&d).unwrap();
    let cross = c.cross_product(&d).unwrap();

    assert_eq!(StandardFunctions::dot_func().evaluate(&vdouble_on), Ok( dot.into() ));
    assert_eq!(StandardFunctions::cross_func().evaluate(&vdouble_on), Ok( cross.into() ));
}