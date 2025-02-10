use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::fmt::Display;
use serde::{Serialize, Deserialize};

use crate::core::errors::FormattingError;
use super::super::found::LineParsing;

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

impl LineParsing for EntryKey {
    fn combine_line(&self) -> String {
        format!("{}", self)
    }
    fn parse_from_line(from: &str) -> Result<Self, FormattingError> where Self: Sized {
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