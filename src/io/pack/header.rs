use serde::{Serialize, Deserialize};
use serde_json::{json, Value};

use std::io::{Write, Read};
use std::fs::File;

pub use crate::core::{Version, is_string_whitespace};
use super::super::core::Error as MyError;

#[derive(Serialize, Deserialize, PartialEq, Clone, Debug)]
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

    pub fn write(&self, file: &mut File) -> std::io::Result<()> {
        let mut contents = String::new();
        self.write_str(&mut contents);

        file.write_all(contents.as_bytes())
    }
    pub fn write_str(&self, loc: &mut String) {
        *loc = json!(self).to_string();
    }
    pub fn read_str(contents: &str) -> Result<Self, MyError> {
        let val: Value = Value::from(contents);
        let result: Result<Self, _> = serde_json::from_value(val);

        match result {
            Ok(r) => Ok(r),
            Err(e) => Err(e.into())
        }
    }
    pub fn read(file: &mut File) -> Result<Self, MyError> {
        let mut contents: String = String::new();
        if let Err(e) = file.read_to_string(&mut contents) {
            return Err(e.into())
        }

        Self::read_str(&contents)
    }
}