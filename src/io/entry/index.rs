use std::fmt::{Display, Debug};
use serde::{Serialize, Deserialize};

use super::key::EntryKey;
use crate::core::errors::FormattingError;

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
impl EntryType {
    pub fn try_parse(from: &str) -> Result<EntryType, FormattingError> {

    }
}

#[derive(Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct EntryIndex {
    key: EntryKey,
    name: String,
    kind: EntryType
}
impl EntryIndex {
    pub fn new<T: ToString>(key: EntryKey, name: &T, kind: EntryType) -> Self {
        Self {
            key,
            name: name.to_string(),
            kind
        }
    }
    pub fn try_parse(contents: &str) -> Result<Self, FormattingError> {
        let splits: Vec<&str> = contents.split(' ').collect();
        if splits.len() !=3 {

        }
    }

    pub fn into_pack_line(&self) -> String {
        format!("{} {} {}", self.key.entry_id(), &self.name, self.kind)
    }

    pub fn is_temporary(&self) -> bool {
        matches!(self.kind, EntryType::Temporary)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn set_name(&mut self, new: String) {
        self.name = new;
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