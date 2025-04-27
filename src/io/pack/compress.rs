use serde::{Serialize, Deserialize};
use serde_json::from_str;

use crate::io::{
    core::{BlockParsable, Error},
    id::VerifiedPath
};
use crate::core::errors::Error as CoreError;

use std::{fs::File, io::ErrorKind};
use std::path::Path;
use std::io::{Read, Write, Error as IOError};

//use serde_json::from_str;

/// Represents a working image of what the package, at the time of creation, looks like. This is used to write to a directory, in the form of three files, or can be read from a directory. Additionally, this structure can be serialized and deserialized directly, and is used to create `CompressedPackage`. 
#[derive(Serialize, Deserialize, PartialEq, Debug, Clone)]
pub struct PackageSnapshot {
    header: String,
    entries_data: String,
    functions_data: String,
    #[serde(skip)]
    loc: Option<VerifiedPath>, //This value is only None before opening. After opening, it is guarenteed to always have a value.
    #[serde(skip)]
    is_dir: bool
}
impl PackageSnapshot {
    pub fn new(header: String, entries_data: String, functions_data: String, location: VerifiedPath) -> Self {
        let is_dir = location.path().is_dir();
        Self {
            header,
            entries_data,
            functions_data,
            loc: Some(location),
            is_dir
        }
    }
    
    pub fn open_file(path: &Path) -> Result<Self, Error> {
        // This assumes that the current path is a file, and we deserialize the struct in it's entirety to get it.

        if !path.exists() || !path.is_file() {
            return Err(IOError::new(std::io::ErrorKind::NotFound, "the path provided does not exist or is not a file").into())
        }

        let name = VerifiedPath::verify(path).map_err(|x| Error::from(CoreError::from(x)))?;
        let mut file = File::open(name.path()).map_err(Error::from)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents).map_err(Error::from)?;

        let mut result: Self = from_str(&contents).map_err(Error::from)?;
        result.loc = Some(name);
        result.is_dir = false;
        
        Ok(result)
    }
    pub fn open_dir(path: &Path) -> Result<Self, Error> {
        if !path.exists() || !path.is_dir() {
            return Err(IOError::new(std::io::ErrorKind::NotADirectory, "the path provided does not exist or is not a directory").into())
        }

        let name = VerifiedPath::verify(path).map_err(|x| Error::from(CoreError::from(x)))?;
        
        let header_p = path.join("header");
        let entry_p = path.join("entry");
        let func_p = path.join("func");

        let mut header_file = File::open(&header_p).map_err(Error::from)?;
        let mut entry_file = File::open(&entry_p).map_err(Error::from)?;
        let mut func_file = File::open(&func_p).map_err(Error::from)?;

        let mut header = String::new();
        let mut entry = String::new();
        let mut func = String::new();

        header_file.read_to_string(&mut header).map_err(Error::from)?;
        entry_file.read_to_string(&mut entry).map_err(Error::from)?;
        func_file.read_to_string(&mut func).map_err(Error::from)?;

        Ok(
            Self {
                header,
                entries_data: entry,
                functions_data: func,
                loc: Some(name),
                is_dir: true
            }
        )
    }
    pub fn open(path: &Path) -> Result<Self, Error> {
        if path.is_dir() {
            Self::open_dir(path)
        }
        else if path.is_file() {
            Self::open_file(path)
        }
        else {
            Err(
                Error::from(
                    IOError::new(
                        std::io::ErrorKind::Unsupported,
                        "the path provided either does not exist, or is not a file nor directory"
                    )
                )
            )
        }
    }

    pub fn save(&self) -> Result<(), IOError> {
        if self.is_dir {
            self.save_to_dir()
        }
        else {
            self.save_to_file()
        }
    }
    pub fn save_to_dir(&self) -> Result<(), IOError> {
        let path = self.path().path();
        let header_p = path.join("header");
        let entry_p = path.join("entry");
        let func_p = path.join("func");

        let mut header_file = File::open(&header_p)?;
        let mut entry_file = File::open(&entry_p)?;
        let mut func_file = File::open(&func_p)?;

        header_file.write_all(self.header.as_bytes())?;
        entry_file.write_all(self.entries_data.as_bytes())?;
        func_file.write_all(self.functions_data.as_bytes())?;

        Ok(())
    }
    pub fn save_to_file(&self) -> Result<(), IOError> {
        let serialized = match serde_json::to_string(self) {
            Ok(v) => v,
            Err(e) => return Err(
                IOError::new(ErrorKind::Interrupted, e)
            )
        };

        let mut file = File::create(self.loc.as_ref().unwrap().path())?;
        file.write_all(serialized.as_bytes())?;

        Ok(())
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
    pub fn path(&self) -> &VerifiedPath {
        self.loc.as_ref().unwrap()
    }
}
impl BlockParsable for PackageSnapshot {}