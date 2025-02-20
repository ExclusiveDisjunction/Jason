use crate::core::errors::{NamingError, Error as CoreError};
use crate::core::errors::UnexpectedError;
use super::compress::CompressedPackage;
use super::compress::PackageSnapshot;
use super::header::PackageHeader;
use super::super::entry::*;
use super::super::id::*;
use super::super::core::Error;

use serde_json::{from_str, json};

use std::path::{Path, PathBuf};
use std::fs::{File, copy, remove_file};
use std::io::{Write, Read, Error as IOError};

pub struct Package {
    pack_id: NumericalPackID,
    current_id: u32,
    name: String,
    header: PackageHeader,
    entries: Vec<VariableEntry>,
    func: Vec<FunctionEntry>,

    location: PathBuf
}
impl Package {
    #[allow(dead_code)]
    fn blank() -> Self {
        use crate::core::Version;
        Self {
            pack_id: NumericalPackID::usr_id(),
            current_id: 0,
            name: "temporary".to_string(),
            header: PackageHeader::new(Version::new(1, 0, 0), None),
            entries: vec![],
            func: vec![],
            location: PathBuf::default()
        }
    }

    pub fn open_from_directory(path: &Path, id: NumericalPackID) -> Result<Self, Error> {
        /*
            In this path, we expect three files:
            1. header
            2. entry
            3. func

            The contents of these files should be read, and then the arguments will be passed into a PackageSnapshot. This will then be passed into Self::open_from_snapshot.
         */

        if !path.is_dir() {
            return Err(IOError::new(std::io::ErrorKind::NotADirectory, "the path provided must be a directory").into());
        }

        let name = match path.file_name().and_then(|x| x.to_str()) {
            Some(n) => n.to_string(),
            None => return Err(CoreError::from(NamingError::InvalidCharacters).into())
        };
        if let Err(e) = is_name_valid(&name) {
            return Err(CoreError::from(e).into())
        }

        let mut header: File = match File::create(path.join("header")) {
            Ok(f) => f,
            Err(e) => return Err(e.into())
        };
        let mut entry: File = match File::create(path.join("entry")) {
            Ok(f) => f,
            Err(e) => return Err(e.into())
        };
        let mut func: File = match File::create(path.join("func")) {
            Ok(f) => f,
            Err(e) => return Err(e.into())
        };

        let mut header_cont: String = String::new();
        let mut entry_cont: String = String::new();
        let mut func_cont: String = String::new();

        if let Err(e) = header.read_to_string(&mut header_cont) {
            return Err(e.into());
        }
        if let Err(e) = entry.read_to_string(&mut entry_cont) {
            return Err(e.into());
        }
        if let Err(e) = func.read_to_string(&mut func_cont) {
            return Err(e.into());
        }

        let snap = PackageSnapshot::new(name, header_cont, entry_cont, func_cont);

        Self::open_from_snapshot(path.to_path_buf(), &snap, id)
    }
    pub fn open_from_compressed(path: PathBuf, file: &CompressedPackage, id: NumericalPackID) -> Result<Self, Error> {
        Self::open_from_snapshot(path, file.contents(), id)
    }
    pub fn open_from_snapshot(path: PathBuf, snap: &PackageSnapshot, id: NumericalPackID) -> Result<Self, Error> { 
        let name: String;
        if let Err(e) = is_name_valid(snap.name()) {
            return Err(CoreError::from(e).into())
        }
        else {
            name = snap.name().trim().to_lowercase();
        }

        let header: Result<PackageHeader, _> = from_str(snap.header());
        let entries: Result<Vec<VariableEntry>, _> = from_str(snap.entries_raw());
        let func: Result<Vec<FunctionEntry>, _> = from_str(snap.functions_raw());

        let header: PackageHeader = match header {
            Ok(v) => v,
            Err(e) => return Err(e.into())
        };
        let mut entries = match entries {
            Ok(v) => v,
            Err(e) => return Err(e.into())
        };
        let mut func = match func {
            Ok(v) => v,
            Err(e) => return Err(e.into())
        };

        // Since serialization results in default IDs, we must assign our own.
        let mut key = NumericalResourceID::new(id, 0);
        for entry in &mut entries {
            *entry.id_mut() = key;
            key += 1;
        }
        for function in &mut func {
            *function.id_mut() = key;
            key += 1;
        }

        Ok(
            Self {
                name,
                pack_id: id,
                current_id: key.resx(), //Sets it to the current 
                header,
                entries,
                func,
                location: path
            }
        )
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

    pub fn id(&self) -> NumericalPackID {
        self.pack_id
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn make_pack_id(&self) -> PackageID {
        PackageID::Strong(self.name.clone(), self.pack_id)
    }

    pub fn header(&self) -> &PackageHeader {
        &self.header
    }
    pub fn header_mut(&mut self) -> &mut PackageHeader {
        &mut self.header
    }

    pub fn snapshot(&self) -> PackageSnapshot {
        let header = json!(self.header).to_string();
        let name = self.name.clone();
        let entries = json!(&self.entries).to_string();
        let functions = json!(&self.func).to_string();
        
        PackageSnapshot::new(name, header, entries, functions)
    }
    pub fn make_compressed(&self) -> CompressedPackage {
        self.snapshot().into()
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

            header_file.write_all(snapshot.header().as_bytes())?;
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

    pub fn resolve(&self, loc: &Locator) -> Option<NumericalResourceID> {
        loc.contained_in_sc(&self.make_pack_id())?;

        match loc.kind() {
            ResourceKind::Entry(_) => {
                let found = self.entries.iter().find(|x| x.accepts_locator(loc));

                found.map(|x| x.id() )
            }
            ResourceKind::Function => {
                None
            }
        }
    }

    pub fn get(&self, id: NumericalResourceID) -> Option<&VariableEntry> {
        id.contained_in_sc(&self.pack_id)?;

        self.entries.iter().find(|x| x.accepts_id(id))
    }
    pub fn get_mut(&mut self, id: NumericalResourceID) -> Option<&mut VariableEntry>{
        id.contained_in_sc(&self.pack_id)?;

        self.entries.iter_mut().find(|x| x.accepts_id(id))
    }
 
    pub fn remove(&mut self, id: NumericalResourceID) -> bool {
        self.release(id).is_some()
    }
    pub fn release(&mut self, id: NumericalResourceID) -> Option<VariableEntry> {
        id.contained_in_sc(&self.pack_id)?;

        let index = self.entries.iter().position(|x| x.accepts_id(id))?;

        Some(self.entries.remove(index))
    }
    pub fn remove_all(&mut self) {
        self.entries.clear();
        self.func.clear();
    }
    pub fn add_entry(&mut self, name: String, is_var: bool, data: VariableUnion) -> Result<(), CoreError> {
        if let Err(e) = is_name_valid(&name) {
            return Err( e.into() )
        }

        let next_id = match self.get_next_id() {
            Some(i) => i,
            None => return Err(UnexpectedError::new("the max ID has been taken already for this package").into())
        };
        let new_id = NumericalResourceID::new(self.pack_id, next_id);

        let result = match VariableEntry::new(
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

#[test]
fn test_package() {
    use crate::calc::MathVector;

    let mut pack = Package::blank();

    assert_eq!(pack.make_pack_id(), pack.id());
    let author = "exdisj".to_string();
    pack.header_mut().set_author(Some(author.clone()));
    assert_eq!(pack.header().author(), Some(author.as_str()));

    assert!(pack.add_entry("var1".to_string(), true, 5.5.into()).is_ok());
    let loc = Locator::new(PackageID::Any, ResourceID::Weak("var1".to_string()), ResourceKind::Entry(VarEntryType::Variable));
    let found = match pack.resolve(&loc) {
        Some(f) => f,
        None => panic!("locator was not able to find the resource")
    };

    {
        if let Some(grabbed) = pack.get(found) {
            println!("value grabbed: {}", grabbed.get_data());
        }
        else {
            panic!("Could not get resource at {}", found);
        }
    }

    {
        if let Some(grabbed) = pack.get_mut(found) {
            assert!(grabbed.set_data(MathVector::from(vec![1, 2, 3]).into()).is_ok());
        }
        else {
            panic!("Could not get resource at {}", found);
        }   
    }

    let loc = Locator::new(PackageID::Usr, ResourceID::Numeric(0), ResourceKind::Entry(VarEntryType::Variable));
    let found = match pack.resolve(&loc) {
        Some(f) => f,
        None => panic!("Using the numerical entry ID, the resource could not be found")
    };
    
    if let Some(grabbed) = pack.get(found) {

        assert!(matches!(grabbed.get_data().access(), Some(&VariableUnion::Vec(_))));
    }
    else {
        panic!("Could not get resource at {}", found);
    }
}