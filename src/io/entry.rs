pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::calc::func::{ASTBasedFunction, FunctionBase};
use crate::core::errors::{FormattingError, NamingError};
use super::id::{is_name_valid, Locator, NumericalPackID, NumericalResourceID, PackageID, ResourceID, ResourceKind};

use std::fmt::{Display, Debug};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum EntryType {
    Variable,
    Environment
}
impl Display for EntryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Variable => "Variable",
                Self::Environment => "Environment"
            }
        )
    }
}
impl TryFrom<String> for EntryType {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "Variable" => Ok(Self::Variable),
            "Environment" => Ok(Self::Environment),
            a => Err(FormattingError::new(&a, "cannot parse value"))
        }
    }
}
impl From<EntryType> for String {
    fn from(val: EntryType) -> Self {
        val.to_string()
    }
}
impl EntryType {
    pub fn symbol(&self) -> String {
        match self {
            Self::Environment => String::new(),
            Self::Variable => "$".to_string()
        }
    }
}

pub trait IOEntry: Serialize + for<'a> Deserialize<'a> + Clone + PartialEq + Display + Debug {
    fn name(&self) -> &str;
    fn set_name(&mut self, new: String) -> Result<(), NamingError>;

    fn accepts_id(&self, id: NumericalResourceID) -> bool {
        self.id() == id
    }
    fn accepts_locator(&self, id: &Locator) -> bool {
        if id.kind() != self.resource_kind() { return false; }

        if id.resource().is_weak() {
            id.resource().name() == Some(self.name())
        }
        else {
            id == &self.id()
        }
    }

    fn resource_kind(&self) -> ResourceKind;
    fn id(&self) -> NumericalResourceID;
    fn package_id(&self) -> NumericalPackID {
        self.id().package()
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID;
    fn strong_locator(&self) -> Locator {
        Locator::new(
            PackageID::Num(self.package_id()),
            ResourceID::Strong(self.name().to_string(), self.id().resx()),
            self.resource_kind()
        )
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct VariableEntry {
    #[serde(skip)]
    key: NumericalResourceID,
    name: String,
    kind: EntryType,
    data: VariableUnion
}
impl Display for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{}", 
            match self.kind {
                EntryType::Environment => "",
                EntryType::Variable => "$"
            },
            &self.name,
            self.data.get_type()
        )
    }
}
impl IOEntry for VariableEntry {
    fn name(&self) -> &str {
        &self.name
    }
    fn set_name(&mut self, new: String) -> Result<(), NamingError> {
        if let Err(e) = is_name_valid(&new) {
            Err(e)
        }
        else {
            self.name = new.trim().to_lowercase();
            Ok(())
        }
    }

    fn resource_kind(&self) -> ResourceKind {
        ResourceKind::Entry(self.kind)
    }
    fn id(&self) -> NumericalResourceID {
        self.key
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID {
        &mut self.key
    }
}
impl VariableEntry {
    pub fn new(key: NumericalResourceID, name: String, is_var: bool, data: Option<VariableUnion>) -> Result<Self, NamingError> {
        let mut result = Self {
            name: String::new(), //Temporary string, we will use the set_name function
            key,
            kind: if is_var {
                EntryType::Variable
            }
            else {
                EntryType::Environment
            },
            data: data.unwrap_or_else(|| 0.0f64.into())
        };

        result.set_name(name)?;

        Ok(result)
    }

    pub fn get_data(&self) -> VariableUnionRef<'_> {
        self.data.get_ref()
    }
    pub fn get_data_mut(&mut self) -> VariableUnionRefMut<'_> {
        self.data.get_ref_mut()
    }
    pub fn set_data(&mut self, new: VariableUnion) {
        self.data = new;
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FunctionEntry {
    inner: ASTBasedFunction,
    #[serde(skip)]
    key: NumericalResourceID
}
impl Display for FunctionEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.inner)
    }
}
impl IOEntry for FunctionEntry {
    fn name(&self) -> &str {
        self.inner.name()
    }
    fn set_name(&mut self, new: String) -> Result<(), NamingError> {
        if let Err(e) = is_name_valid(&new) {
            Err(e)
        }
        else {
            self.inner.set_name( new.trim().to_lowercase() );
            Ok(())
        }
    }

    fn id(&self) -> NumericalResourceID {
        self.key
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID {
        &mut self.key
    }
    fn resource_kind(&self) -> ResourceKind {
        ResourceKind::Function   
    }
}
impl FunctionEntry {
    pub fn new(key: NumericalResourceID, name: String, data: ASTBasedFunction) -> Result<Self, NamingError> {
        let mut result = Self {
            inner: data,
            key
        };

        result.set_name(name)?;
        Ok(result)        
    }

    pub fn get_func(&self) -> &ASTBasedFunction {
        &self.inner
    }
    pub fn get_func_mut(&mut self) -> &mut ASTBasedFunction {
        &mut self.inner
    }
    pub fn set_func(&mut self, new: ASTBasedFunction) {
        self.inner = new
    }
}