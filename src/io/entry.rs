pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::core::{is_string_whitespace, errors::{FormattingError, ArgumentValueError, Error, OperationError as CoreOperErr}};

use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;
use serde::{Serialize, Deserialize};

#[derive(Clone, Eq, Default, Debug, Serialize, Deserialize)]
pub struct EntryKey {
    pack_id: u32,
    entry_id: u32
}

impl Display for EntryKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.pack_id, self.entry_id)
    }
}

impl Hash for EntryKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pack_id.hash(state);
        self.entry_id.hash(state);
    }
}

impl PartialEq for EntryKey {
    fn eq(&self, other: &Self) -> bool {
        self.pack_id == other.pack_id && self.entry_id == other.entry_id
    }
}
impl PartialOrd for EntryKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for EntryKey {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.pack_id.cmp(&other.pack_id) {
            Ordering::Equal => self.entry_id.cmp(&other.entry_id),
            a => a
        }
    }
}

impl From<EntryKey> for String {
    fn from(val: EntryKey) -> Self {
        val.to_string()
    }
}
impl TryFrom<String> for EntryKey {
    type Error = FormattingError;
    fn try_from(from: String) -> Result<Self, Self::Error> {
        let splits: Vec<&str> = from.split('.').collect();
        if splits.len() != 2 {
            return Err(FormattingError::new(&from, "too many or not enough arguments, should be two"));
        }

        let a = splits[0].parse::<u32>();
        let b = splits[1].parse::<u32>();

        match (a, b) {
            (Ok(pack_id), Ok(entry_id)) => Ok(Self::new(pack_id, entry_id)),
            _ => Err(FormattingError::new(&from, "expected two numerical values")),
        }
    }
}

impl EntryKey {
    pub fn new(pack_id: u32, entry_id: u32) -> Self {
        Self {
            pack_id,
            entry_id
        }
    }

    pub fn pack_id(&self) -> u32 {
        self.pack_id
    }
    pub fn set_pack_id(&mut self, pack_id: u32) {
        self.pack_id = pack_id;
    }
    pub fn entry_id(&self) -> u32 {
        self.entry_id
    }
    pub fn set_entry_id(&mut self, entry_id: u32) {
        self.entry_id = entry_id;
    }
}

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

#[derive(Debug, PartialEq, Clone)]
pub struct PackageEntry {
    key: EntryKey,
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
    pub fn new(key: EntryKey, name: String, is_var: bool, data: Option<VariableUnion>) -> Result<Self, ArgumentValueError> {
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
    pub fn new_temp(key: EntryKey, data: Option<VariableUnion>) -> Self {
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

    pub fn key(&self) -> &EntryKey {
        &self.key
    }
    pub fn key_mut(&mut self) -> &mut EntryKey {
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