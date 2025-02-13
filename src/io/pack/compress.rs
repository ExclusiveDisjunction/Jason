use serde::{Serialize, Deserialize};

use super::header::PackageHeader;
use super::super::core::BlockParsable;

#[derive(Serialize, Deserialize)]
pub struct PackageSnapshot {
    name: String,
    header: PackageHeader,
    entries_data: String,
    functions_data: String
}
impl PackageSnapshot {
    pub fn new(name: String, header: PackageHeader, entries_data: String, functions_data: String) -> Self {
        Self {
            name,
            header,
            entries_data,
            functions_data
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn header(&self) -> &PackageHeader {
        &self.header
    }
    pub fn entries_raw(&self) -> &str {
        &self.entries_data
    }
    pub fn functions_raw(&self) -> &str {
        &self.functions_data
    }
}
impl BlockParsable for PackageSnapshot {}
