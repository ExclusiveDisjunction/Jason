pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::core::{is_string_whitespace, errors::{FormattingError, ArgumentValueError, Error, OperationError as CoreOperErr}};
use super::id::{ResourceID, StrongResourceID};

use std::fmt::{Display, Debug};
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum EntryType {
    Variable,
    Temporary,
    Environment
}
impl Default for EntryType {
    fn default() -> Self {
        Self::Temporary
    }
}
impl Display for EntryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Variable => "Variable",
                Self::Temporary => "Temporary",
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
            "Temporary" => Ok(Self::Temporary),
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
            Self::Temporary => "tmp.".to_string(),
            Self::Variable => "$".to_string()
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct PackageEntry {
    key: StrongResourceID,
    name: String,
    kind: EntryType,
    data: VariableUnion
}

impl Display for PackageEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{}", 
            match self.kind {
                EntryType::Environment => "",
                EntryType::Variable => "$",
                EntryType::Temporary => "tmp"
            },
            &self.name,
            self.data.get_type()
        )
    }
}

impl PackageEntry {
    pub fn new(key: StrongResourceID, name: String, is_var: bool, data: Option<VariableUnion>) -> Result<Self, ArgumentValueError> {
        if name.is_empty() || is_string_whitespace(&name) {
            return Err(ArgumentValueError::new("name", &name));
        }

        let mut result = Self {
            name: String::new(), //Temporary string, we will use the set name function
            key,
            kind: if is_var {
                EntryType::Variable
            }
            else {
                EntryType::Environment
            },
            data: match data {
                Some(d) => d,
                None => 0.0f64.into()
            }
        };

        match result.set_name(name) {
            Ok(_) => (),
            Err(e) => {
                match e {
                    Error::ArgVal(v) => return Err(v),
                    e => panic!("cannot get error from value '{e}'")
                }
            }
        }

        Ok(result)
    }
    pub fn new_temp(key: StrongResourceID, data: Option<VariableUnion>) -> Self {
        Self {
            name: String::new(), //Temporaries do not have names
            key,
            kind: EntryType::Temporary,
            data: match data {
                Some(d) => d,
                None => 0.0f64.into()
            }
        }
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

    pub fn accepts_id<T>(&self, id: &T) -> bool where T: ResourceID {
        if let Some((_, ent)) = id.by_name() {
            Some(ent.as_str()) == self.name()
        }
        else if let Some((pack, ent)) = id.by_id() {
            self.key.pack_id() == pack && self.key.entry_id() == ent
        }
        else {
            false
        }
    }

    pub fn key(&self) -> &StrongResourceID {
        &self.key
    }
    pub fn key_mut(&mut self) -> &mut StrongResourceID {
        &mut self.key
    }
    pub fn kind(&self) -> EntryType {
        self.kind
    }
    pub fn name(&self) -> Option<&str> {
        if self.kind == EntryType::Temporary {
            None
        }
        else {
            Some(&self.name)
        }
    }
    pub fn set_name(&mut self, new: String) -> Result<(), Error> {
        if self.kind == EntryType::Temporary {
            return Err(CoreOperErr::new("set name", "cannot set a name on a temporary variable").into());
        }

        if new.is_empty() || is_string_whitespace(&new) {
            return Err(FormattingError::new(&new, "cannot store an empty or only white space name").into())
        }

        self.name = new.trim().to_string();
        Ok(())
    }
}