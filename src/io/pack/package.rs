use crate::calc::func::ASTBasedFunction;
use crate::core::errors::{UnexpectedError, ArgumentValueError};
use crate::core::is_string_whitespace;
use super::compress::PackageSnapshot;
use super::header::PackageHeader;
use super::super::entry::*;
use super::super::id::*;
use crate::core::errors::Error as CoreError;

use serde_json::json;

use std::path::{Path, PathBuf};
use std::fs::{File, copy, remove_file};
use std::io::{Write, Read};

pub struct Package {
    pack_id: StrongPackID,
    current_id: u32,
    header: PackageHeader,
    entries: Vec<PackageEntry>,
    func: Vec<ASTBasedFunction>,

    location: PathBuf
}
impl Package {
    pub fn open_from_directory(path: &Path, id: u32) -> Result<Self, CoreError> {
        todo!()
    }
    
    fn get_next_id(&mut self) -> Option<u32> {
        if self.current_id == u32::MAX {
            None
        }
        else {
            let result = self.current_id;
            self.current_id += 1;
            Some(result)
        }
    }

    pub fn location(&self) -> &Path {
        &self.location
    }
    pub fn id(&self) -> &StrongPackID {
        &self.pack_id
    }
    pub fn header(&self) -> &PackageHeader {
        &self.header
    }
    pub fn header_mut(&mut self) -> &mut PackageHeader {
        &mut self.header
    }

    pub fn snapshot(&self) -> PackageSnapshot {
        let header = self.header.clone();
        let name = self.pack_id.name().to_string();
        let entries = json!(&self.entries).to_string();
        let functions = json!(&self.func).to_string();
        
        PackageSnapshot::new(name, header, entries, functions)
    }
    pub fn save(&self) -> std::io::Result<()> {
        let snapshot = self.snapshot();

        /*
            We have several files:
            header file -> ./header
            entries file ./entry
            functions file ./func
         */

        {
            let mut header_file = File::create(self.location.join("header_new"))?;
            let mut entries_file = File::create(self.location.join("entry_new"))?;
            let mut functions_file = File::create(self.location.join("func_new"))?;

            snapshot.header().write(&mut header_file)?;
            entries_file.write_all(snapshot.entries_raw().as_bytes())?;
            functions_file.write_all(snapshot.functions_raw().as_bytes())?;
        }

        //Now that we wrote to temporary files, we can move these two the new ones.
        {
            copy(self.location.join("header_new"), self.location.join("header"))?;
            copy(self.location.join("entry_new"), self.location.join("entry"))?;
            copy(self.location.join("func_new"), self.location.join("func"))?;

            remove_file(self.location.join("header_new"))?;
            remove_file(self.location.join("entry_new"))?;
            remove_file(self.location.join("entry_new"))?;
        }

        Ok(())
    }

    pub fn resolve<T>(&self, id: T) -> Option<&PackageEntry> where T: ResourceID {
        id.contained_in_pack_sc(&self.pack_id)?;

        for entry in &self.entries {
            if entry.accepts_id(&id) {
                return Some(entry);
            }
        }

        None
    }
    pub fn resolve_mut<T>(&mut self, id: T) -> Option<&mut PackageEntry> where T: ResourceID {
        id.contained_in_pack_sc(&self.pack_id)?;

        for entry in &mut self.entries {
            if entry.accepts_id(&id) {
                return Some(entry);
            }
        }

        None
    }
 
    pub fn remove<T: ResourceID> (&mut self, id: T) -> bool {
        self.release(id).is_some()
    }
    pub fn release<T: ResourceID>(&mut self, id: T) -> Option<PackageEntry> {
        id.contained_in_pack_sc(&self.pack_id)?;

        let index = match self.entries.iter().position(|x| x.accepts_id(&id)) {
            Some(i) => i,
            None => return None
        };

        Some(self.entries.remove(index))
    }
    pub fn remove_all(&mut self) {
        self.entries.clear();
        self.func.clear();
    }
    pub fn add_entry(&mut self, name: String, is_var: bool, data: VariableUnion) -> Result<(), CoreError> {
        if name.is_empty() || is_string_whitespace(&name) {
            return Err(ArgumentValueError::new("name", &String::new()).into() )
        }

        let next_id = match self.get_next_id() {
            Some(i) => i,
            None => return Err(UnexpectedError::new("the max ID has been taken already for this package").into())
        };
        let new_id = StrongResourceID::new(self.pack_id.id(), next_id);

        let result = match PackageEntry::new(
            new_id,
            name,
            is_var, 
            Some(data)
        ) {
            Ok(v) => v,
            Err(e) => return Err(e.into())
        };

        self.entries.push(result);
        Ok(())
    }
}