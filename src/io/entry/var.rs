pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::log_error;
use crate::core::errors::{FormattingError, NamingError};
use super::super::id::{validate_name, NumericalResourceID, ResourceKind};
use super::base::*;

use std::fmt::{Display, Debug};
use std::sync::{Arc, RwLock};
use serde::ser::SerializeStruct;
use serde::{ser, Deserialize, Serialize, Serializer, Deserializer};

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum VarEntryType {
    Variable,
    Environment
}
impl Display for VarEntryType {
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
impl TryFrom<String> for VarEntryType {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "Variable" => Ok(Self::Variable),
            "Environment" => Ok(Self::Environment),
            a => Err(FormattingError::new(&a, "cannot parse value"))
        }
    }
}
impl VarEntryType {
    pub fn symbol(&self) -> String {
        match self {
            Self::Environment => String::new(),
            Self::Variable => "$".to_string()
        }
    }
}

pub struct VariableEntry {
    key: NumericalResourceID,
    name: String,
    kind: VarEntryType,
    data: Arc<RwLock<VariableUnion>>
}
impl Serialize for VariableEntry {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let guard = self.get_data();
        if let Some(inner) = guard.access() {
            let mut s: <S as Serializer>::SerializeStruct = serializer.serialize_struct("VariableEntry", 3)?;
            s.serialize_field("data", inner)?;
            s.serialize_field("name", &self.name)?;
            s.serialize_field("kind", &self.kind)?;

            s.end()
        }
        else {
            let e = guard.get_err().unwrap();

            log_error!("Access failed for serilization: {}", &e);
            Err(ser::Error::custom(e))
        }
    }
}
impl<'de> Deserialize<'de> for VariableEntry {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        todo!()
    }
}
impl PartialEq for VariableEntry {
    fn eq(&self, other: &Self) -> bool {
        self.get_data() == other.get_data()
    }   
}
impl Debug for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{:?}", 
            match self.kind {
                VarEntryType::Environment => "",
                VarEntryType::Variable => "$"
            },
            &self.name,
            self.get_data()
        )
    }
}
impl Display for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{}", 
            match self.kind {
                VarEntryType::Environment => "",
                VarEntryType::Variable => "$"
            },
            &self.name,
            self.get_data()
        )
    }
}
impl IOEntry for VariableEntry {
    fn name(&self) -> &str {
        &self.name
    }
    fn set_name(&mut self, new: String) -> Result<(), NamingError> {
        self.name = validate_name(new)?;
        Ok(())
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
impl IOStorage for VariableEntry {
    type Holding = VariableUnion;
    fn get_arc(&self) -> &Arc<RwLock<Self::Holding>> {
        &self.data
    }
}
impl VariableEntry {
    pub fn new(key: NumericalResourceID, name: String, is_var: bool, data: Option<VariableUnion>) -> Result<Self, NamingError> {
        let mut result = Self {
            name: String::new(), //Temporary string, we will use the set_name function
            key,
            kind: if is_var {
                VarEntryType::Variable
            }
            else {
                VarEntryType::Environment
            },
            data: Arc::new(
                RwLock::new(
                    data.unwrap_or_else(|| 0.0f64.into())
                )
            )
        };

        result.set_name(name)?;

        Ok(result)
    }
}
