pub mod key;
pub mod index;

pub use super::entry::key::*;
pub use super::entry::index::*;

pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut};
use crate::core::errors::ArgumentValueError;

//use std::sync::{Arc, RwLock};
use std::fmt::{Display, Debug};

#[derive(Debug, PartialEq, Clone)]
pub struct PackageEntry {
    index: EntryIndex,
    data: VariableUnion
}

impl Display for PackageEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl PackageEntry {
    pub fn new(index: EntryIndex, data: Option<VariableUnion>) -> Self {
        Self {
            index,
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
    /// Sets the new data, and returns the old one
    pub fn set_data(&mut self, new: VariableUnion) {
        self.data = new;
    }

    pub fn get_index(&self) -> &EntryIndex {
        &self.index
    }
    pub fn set_name(&mut self, new: String) -> Result<(), ArgumentValueError> {
        self.index.set_name(new)
    }
    pub fn key(&self) -> &EntryKey {
        self.index.key()
    }
}