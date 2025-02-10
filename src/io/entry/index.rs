use std::fmt::{Display, Debug};
use serde::{Serialize, Deserialize};

use super::key::EntryKey;
use super::super::found::LineParsing;
use crate::core::errors::{FormattingError, ArgumentValueError};

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
impl LineParsing for EntryType {
    fn combine_line(&self) -> String {
        format!("{}", &self)
    }
    fn parse_from_line(line: &str) -> Result<Self, FormattingError> where Self: Sized {
        match line {
            "Variable" => Ok(Self::Variable),
            "Temporary" => Ok(Self::Temporary),
            "Environment" => Ok(Self::Environment),
            a => Err(FormattingError::new(&a, "cannot parse value"))
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct EntryIndex {
    key: EntryKey,
    name: String,
    kind: EntryType
}
impl LineParsing for EntryIndex {
    fn combine_line(&self) -> String {
        format!("{} {} {}", self.key.entry_id(), &self.name, self.kind)
    }
    fn parse_from_line(line: &str) -> Result<Self, FormattingError> where Self: Sized {
        let splits: Vec<&str> = line.split(' ').collect();
        if splits.len() != 3 {
            return Err(FormattingError::new(&line, "expected three values separated by spaces"));
        }

        let key = EntryKey::parse_from_line(splits[0])?;
        let name = splits[1].to_string();
        let kind = EntryType::parse_from_line(splits[2])?;

        if kind != EntryType::Temporary && name.is_empty() {
            return Err(FormattingError::new(&name, format!("the name cannot be empty if the type is not `{}`", EntryType::Temporary)))
        }

        Ok(
            Self {
                key,
                name,
                kind
            }
        )
    }
}
impl EntryIndex {
    pub fn new<T: ToString>(key: EntryKey, name: &T, kind: EntryType) -> Result<Self, ArgumentValueError> {
        let name = name.to_string();
        if name.is_empty() && kind != EntryType::Temporary {
            return Err(ArgumentValueError::new("name", &name))
        }

        Ok(
            Self {
                key,
                name: name.to_string(),
                kind
            }
        )
    }

    pub fn is_temporary(&self) -> bool {
        matches!(self.kind, EntryType::Temporary)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn set_name(&mut self, new: String) -> Result<(), ArgumentValueError> {
        if new.is_empty() && self.kind != EntryType::Temporary {
            Err(ArgumentValueError::new(&new, &"name cannot be empty if the type is not temporary"))
        }
        else {
            self.name = new;
            Ok(())
        }
    }
    pub fn kind(&self) -> EntryType {
        self.kind
    }
    pub fn set_kind(&mut self, new_kind: EntryType) {
        self.kind = new_kind;
    }
    pub fn key(&self) -> &EntryKey {
        &self.key
    }
    pub fn key_mut(&mut self) -> &mut EntryKey {
        &mut self.key
    }
}