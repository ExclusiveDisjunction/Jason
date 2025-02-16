use serde::{Serialize, Deserialize};

use super::super::core::{BlockParsable, Error};

use std::fs::File;
use std::path::Path;
use std::io::Read;

use serde_json::from_str;

#[derive(Serialize, Deserialize)]
pub struct PackageSnapshot {
    name: String,
    header: String,
    entries_data: String,
    functions_data: String
}
impl PackageSnapshot {
    pub fn new(name: String, header: String, entries_data: String, functions_data: String) -> Self {
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
    pub fn header(&self) -> &str {
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

/// Represents a singular file for a package. 
pub struct CompressedPackage {
    cont: PackageSnapshot
}
impl From<PackageSnapshot> for CompressedPackage {
    fn from(value: PackageSnapshot) -> Self {
        Self {
            cont: value
        }
    }
}
impl CompressedPackage {
    pub fn read(path: &Path) -> Result<Self, Error> {
        //Attempt to open a file for reading at that path
        let mut file = File::open(path)?;
        
        Self::read_file(&mut file)
    }
    pub fn read_file(file: &mut File) -> Result<Self, Error> {
        //Extract the contents and parse a PackageSnapshot from it
        let mut contents = String::new();
        if let Err(e) = file.read_to_string(&mut contents) {
            return Err(e.into())
        }

        let contents: Result<PackageSnapshot, _> = from_str(&contents);
        match contents {
            Ok(c) => Ok(Self { cont: c }),
            Err(e) => Err(e.into())
        }
    }

    pub fn write(&self, file: &mut File) -> std::io::Result<()> {
        self.cont.write(file)
    }

    pub fn contents(&self) -> &PackageSnapshot {
        &self.cont
    }
}
