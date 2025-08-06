use std::fmt::{Display, Debug};
use std::iter::zip;

use serde::{Serialize, Deserialize};

use crate::calc::{VariableUnion, CalcResult, VariableType, CalcError, VariableData, calc_error::{ArgCountError, ArgTypeError}};

pub trait FunctionBase: Display + Debug + PartialEq {
    fn evaluate(&self, args: &[VariableUnion]) -> CalcResult<VariableUnion>;

    fn signature(&self) -> &FunctionArgSignature;
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct FunctionArgSignature {
    sig: Vec<ArgSignature>
}
impl Display for FunctionArgSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc = self.sig.iter().map(|x| x.to_string() ).collect::<Vec<String>>().join(", ");
        f.write_str(&desc)
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

#[test]
fn test_func_signatures() {
    use crate::calc::{Complex, MathVector};

    let target_a = FunctionArgSignature::just_x();
    let target_b = FunctionArgSignature::from(vec![ArgSignature::new(VariableType::Complex, 'x'), ArgSignature::new(VariableType::Scalar, 'y')]);
    let target_c = FunctionArgSignature::from(ArgSignature::new(VariableType::Vector, 'x'));

    let test_a: Vec<VariableUnion> = vec![1.4.into()];
    let test_b: Vec<VariableUnion> = vec![Complex::new(1.2, 3.6).into(), 1.into()];
    let test_c: Vec<VariableUnion> = vec![MathVector::from(vec![1, 2, 3]).into()];

    assert_eq!(format!("{}", &target_a), "x: Scalar".to_string());
    assert_eq!(format!("{}", &target_b), "x: Complex, y: Scalar");
    assert_eq!(format!("{}", &target_c), "x: Vector");

    for (i, target) in vec![target_a, target_b, target_c].into_iter().enumerate() {
        for (j, test) in vec![&test_a, &test_b, &test_c].iter().enumerate() {
            //Along i, the target should pass the current test, but fail everything else. 
            if i == j {
                assert!(target.validate(test).is_ok());
            }
            else {
                assert!(target.validate(test).is_err());
            }
        }
    }
}
