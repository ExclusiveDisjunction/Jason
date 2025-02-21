use serde::{Serialize, Deserialize};

use super::super::core::{BlockParsable, Error};

use std::fs::File;
use std::path::Path;
use std::io::Read;

//use serde_json::from_str;

/// Represents a working image of what the package, at the time of creation, looks like. This is used to write to a directory, in the form of three files, or can be read from a directory. Additionally, this structure can be serialized and deserialized directly, and is used to create `CompressedPackage`. 
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct PackageSnapshot {
    header: String,
    entries_data: String,
    functions_data: String
}
impl PackageSnapshot {
    pub fn new(header: String, entries_data: String, functions_data: String) -> Self {
        Self {
            header,
            entries_data,
            functions_data
        }
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
    cont: PackageSnapshot,
    name: String
}
impl CompressedPackage {
    pub fn new(inner: PackageSnapshot, name: String) -> Self {
        Self{
            cont: inner,
            name
        }
    }

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

        todo!()

        /*
        let contents: Result<Self, _> = from_str(&contents);
        match contents {
            Ok(c) => Ok(Self { cont: c }),
            Err(e) => Err(e.into())
        }
        */
    }

    pub fn write(&self, file: &mut File) -> std::io::Result<()> {
        self.cont.write(file)
    }

    pub fn contents(&self) -> &PackageSnapshot {
        &self.cont
    }
    pub fn name(&self) -> &str {
        &self.name
    }
}
