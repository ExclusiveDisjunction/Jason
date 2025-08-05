use std::fmt::Debug;
use std::io::{Read, Write};
use std::fs::File;

use exdisj::error::Error as CoreError;

use serde::{Deserialize, Serialize};
use serde_json::{json, Value, from_value};

pub trait BlockParsable: Serialize + for<'a> Deserialize<'a> {
    fn write(&self, file: &mut File) -> std::io::Result<()> {
        let contents: String = json!(self).to_string();

        file.write_all(contents.as_bytes())
    }
    fn write_str(&self, dest: &mut String) {
        *dest = json!(self).to_string();
    }
    fn read(file: &mut File) -> Result<Self, Error> {
        let mut contents: String = String::new();

        match file.read_to_string(&mut contents) {
            Ok(_) => Self::read_str(&contents),
            Err(e) => Err(e.into())
        }
    }
    fn read_str(contents: &str) -> Result<Self, Error> {
        let value = Value::from(contents);
        let parsed: Result<Self, _> = from_value(value);
        
        match parsed {
            Ok(r) => Ok(r),
            Err(e) => Err(e.into())
        }
    }
}

pub enum Error {
    IO(std::io::Error),
    Serde(serde_json::Error),
    Core(CoreError)
}
impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Debug = match self {
            Self::IO(i) => i,
            Self::Serde(i) => i,
            Self::Core(c) => c
        };
        x.fmt(f)
    }
}
impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::IO(value)
    }
}
impl From<serde_json::Error> for Error {
    fn from(value: serde_json::Error) -> Self {
        Self::Serde(value)
    }
}
impl From<CoreError> for Error {
    fn from(value: CoreError) -> Self {
        Self::Core(value)
    }
}