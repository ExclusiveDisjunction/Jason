use serde::{Serialize, Deserialize};

pub use crate::core::{Version, is_string_whitespace};

#[derive(Serialize, Deserialize, PartialEq)]
pub struct PackageHeader {
    version: Version,
    author: Option<String>
}

impl PackageHeader {
    pub fn new(version: Version, author: Option<String>) -> Self {
        Self {
            version,
            author
        }
    }

    pub fn author(&self) -> Option<&str> {
        self.author.as_deref()
    }
    pub fn set_author(&mut self, new: Option<String>) {
        self.author = match new {
            Some(x) if !x.is_empty() && !is_string_whitespace(&x) => Some(x),
            _ => None
        }
    }

    pub fn version(&self) -> &Version {
        &self.version
    }
    pub fn set_version(&mut self, new: Version) {
        self.version = new
    }

}