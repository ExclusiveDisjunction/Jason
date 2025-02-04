use std::fmt::{Display, Debug};
use std::iter::zip;

use crate::calc::{VariableUnion, CalcResult, VariableType, CalcError, VariableData, calc_error::{ArgCountError, ArgTypeError}};

pub trait FunctionBase: Display + Debug {
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
