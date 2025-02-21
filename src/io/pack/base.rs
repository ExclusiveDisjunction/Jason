use crate::calc::func::ASTBasedFunction;
use crate::io::entry::{IOEntry, VariableEntry, FunctionEntry, func::FunctionEntryBase};
use crate::io::id::{Locator, NumericalPackID, NumericalResourceID, PackageID, ResourceKind, validate_name};
use crate::io::core::Error;
use super::compress::{CompressedPackage, PackageSnapshot};
use super::header::PackageHeader;
use crate::calc::VariableUnion;
use crate::core::errors::{Error as CoreError, UnexpectedError};

use std::path::Path;
use std::fs::{create_dir_all, File};
use std::io::Write;

use serde_json::json;

pub trait ReadPackage<FuncT> where FuncT: FunctionEntryBase {
    fn id(&self) -> NumericalPackID;
    fn name(&self) -> &str;
    fn make_pack_id(&self) -> PackageID {
        PackageID::Strong(self.name().to_string(), self.id())
    }

    fn entries(&self) -> &Vec<VariableEntry>;
    fn functions(&self) -> &Vec<FuncT>;

    fn resolve(&self, id: &Locator) -> Option<NumericalResourceID> {
        id.contained_in_sc(&self.make_pack_id())?;

        match id.kind() {
            ResourceKind::Entry(_) => {
                self.entries().iter().find(|x| x.accepts_locator(id)).map(|x| x.id())
            }
            ResourceKind::Function => {
                self.functions().iter().find(|x| x.accepts_locator(id)).map(|x| x.id())
            }
        }
    }

    fn get(&self, id: NumericalResourceID) -> Option<&VariableEntry> {
        id.contained_in_sc(&&self.id())?;

        self.entries().iter().find(|x| x.accepts_id(id))
    }
    fn get_func(&self, id: NumericalResourceID) -> Option<&FuncT> {
        id.contained_in_sc(&self.id())?;

        self.functions().iter().find(|x| x.accepts_id(id))
    }
}

pub trait WritePackage: ReadPackage<FunctionEntry> {
    fn get_provider<'a>(&'a mut self) -> WriteProvider<'a>;

    fn get_mut<'a>(&'a mut self, id: NumericalResourceID) -> Option<&'a mut VariableEntry> where FunctionEntry: 'a {
        let provider = self.get_provider();

        provider.get_mut(id)
    }
    fn get_mut_func(&mut self, id: NumericalResourceID) -> Option<&mut FunctionEntry> {
        self.get_provider().get_func_mut(id)
    }
    
    fn remove(&mut self, id: NumericalResourceID) -> bool {
        self.release_entry(id).is_some() || self.release_func(id).is_some()
    }
    fn release_entry(&mut self, id: NumericalResourceID) -> Option<VariableEntry> { 
        self.get_provider().release_entry(id)
    }
    fn release_func(&mut self, id: NumericalResourceID) -> Option<FunctionEntry> {
        self.get_provider().release_func(id)
    }

    fn remove_all(&mut self) {
        self.get_provider().remove_all()
    }

    fn add_entry(&mut self, name: String, is_var: bool, data: VariableUnion) -> Result<NumericalResourceID, CoreError> {
        self.get_provider().add_entry(name, is_var, data)
    }
    fn add_func(&mut self, name: String, data: ASTBasedFunction) -> Result<NumericalResourceID, CoreError> {
        self.get_provider().add_func(name, data)
    }

    fn index_entries(&mut self) -> Result<(), UnexpectedError> {
        self.get_provider().index_entries()
    }
}

pub struct WriteProvider<'a> {
    entry: &'a mut Vec<VariableEntry>,
    func: &'a mut Vec<FunctionEntry>,
    id: &'a mut NumericalResourceID,
    pack_id: NumericalPackID
}
impl<'a> WriteProvider<'a> {
    pub fn new(entries: &'a mut Vec<VariableEntry>, func: &'a mut Vec<FunctionEntry>, id: &'a mut NumericalResourceID, pack_id: NumericalPackID) -> Self {
        Self {
            entry: entries,
            func,
            id,
            pack_id
        }
    }

    fn get_mut(self, id: NumericalResourceID) -> Option<&'a mut VariableEntry> {
        id.contained_in_sc(&self.pack_id)?;

        self.entry.iter_mut().find(|x| x.accepts_id(id))
    }
    fn get_func_mut(self, id: NumericalResourceID) -> Option<&'a mut FunctionEntry> {
        id.contained_in_sc(&self.pack_id)?;

        self.func.iter_mut().find(|x| x.accepts_id(id))
    }

    fn release_entry(&mut self, id: NumericalResourceID) -> Option<VariableEntry> { 
        id.contained_in_sc(&self.pack_id)?;

        Some(
            self.entry.remove(
                self.entry.iter().position(|x| x.accepts_id(id))?
            )
        )
    }
    fn release_func(&mut self, id: NumericalResourceID) -> Option<FunctionEntry> {
        id.contained_in_sc(&self.pack_id)?;

        Some(
            self.func.remove(
                self.func.iter().position(|x| x.accepts_id(id))?
            )
        )
    }

    fn remove_all(&mut self) {
        self.entry.clear();
        self.func.clear();

        *self.id = NumericalResourceID::new(self.pack_id, 0);
    }

    fn add_entry(&mut self, name: String, is_var: bool, data: VariableUnion) -> Result<NumericalResourceID, CoreError> {
        let name = validate_name(name).map_err(CoreError::from)?;
        let key = self.id.try_increment().map_err(CoreError::from)?;

        self.entry.push(
            VariableEntry::new(
                key,
                name,
                is_var, 
                Some(data)
            ).map_err(CoreError::from)?
        );

        Ok(key)
    }
    fn add_func(&mut self, name: String, data: ASTBasedFunction) -> Result<NumericalResourceID, CoreError> {
        let name = validate_name(name).map_err(CoreError::from)?;
        let key = self.id.try_increment().map_err(CoreError::from)?;

        self.func.push(
            FunctionEntry::new(name, key, data).map_err(CoreError::from)?
        );

        Ok(key)
    }

    fn index_entries(&mut self) -> Result<(), UnexpectedError> {
        *self.id = NumericalResourceID::new(self.pack_id, 0);
        
        for entry in self.entry.iter_mut() {
            *entry.id_mut() = self.id.try_increment()?;
        }

        for func in self.func.iter_mut() {
            *func.id_mut() = self.id.try_increment()?;
        }

        Ok(())
    }
}

pub trait SaveablePackage: ReadPackage<FunctionEntry> {
    fn header(&self) -> &PackageHeader;
    fn header_mut(&mut self) -> &mut PackageHeader;

    fn location(&self) -> &Path;

    fn open(snap: &PackageSnapshot, id: NumericalPackID, from: &Path) -> Result<Self, Error> where Self: Sized;
    fn open_compressed(compressed: &CompressedPackage, id: NumericalPackID, from: &Path) -> Result<Self, Error> where Self: Sized{
        Self::open(compressed.contents(), compressed.name().to_string(), id)
    }

    fn snapshot(&self) -> PackageSnapshot {
        let header = json!(self.header()).to_string();
        let entries = json!(&self.entries()).to_string();
        let functions = json!(&self.functions()).to_string();

        PackageSnapshot::new(header, entries, functions)
    }
    fn make_compressed(&self) -> CompressedPackage {
        let snap = self.snapshot();
        let name = self.name().to_string();

        CompressedPackage::new(snap, name)
    }
    fn save(&self) -> std::io::Result<()> {
        let snapshot = self.snapshot();

        let location = self.location();

        if !location.exists() {
            create_dir_all(self.location())?;
        }

        let mut header_file = File::create(location.join("header"))?;
        let mut entries_file = File::create(location.join("entry"))?;
        let mut functions_file = File::create(location.join("func"))?;

        header_file.write_all(snapshot.header().as_bytes())?;
        entries_file.write_all(snapshot.entries_raw().as_bytes())?;
        functions_file.write_all(snapshot.functions_raw().as_bytes())?;

        Ok(())
    }
}