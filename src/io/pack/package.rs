use crate::calc::func::ASTBasedFunction;
use crate::core::errors::{NamingError, UnexpectedError, Error as CoreError};
use crate::core::version::JASON_CURRENT_VERSION;
use super::compress::{PackageSnapshot, CompressedPackage};
use super::header::PackageHeader;
use super::super::entry::*;
use super::super::id::*;
use super::super::core::Error;

use serde_json::{from_str, json};

use std::path::{Path, PathBuf};
use std::fs::{File, create_dir_all};
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

    pub fn create_into_directory(path: &Path, name: String, id: NumericalPackID) -> Result<Self, Error> {
        let loc = path.join(&name);
        if let Err(e) = create_dir_all(&loc) {
            return Err(e.into());
        }

        let header_path = loc.join("header");
        let entry_path = loc.join("entry");
        let func_path = loc.join("func");

        // Create our files so that we can use them later for saving
        File::create(header_path).map_err(Error::from)?;
        File::create(entry_path).map_err(Error::from)?;
        File::create(func_path).map_err(Error::from)?;

        Ok(
            Self {
                pack_id: id,
                current_id: 0,
                name,
                header: PackageHeader::new(JASON_CURRENT_VERSION, None),
                entries: vec![],
                func: vec![],
                location: loc
            }
        )
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

        let name = validate_name(name).map_err(|x| Error::from(CoreError::from(x)))?;

        let header_path = path.join("header");
        let entry_path = path.join("entry");
        let func_path = path.join("func");

        let mut header = File::open(header_path).map_err(Error::from)?;
        let mut entry = File::open(entry_path).map_err(Error::from)?;
        let mut func = File::open(func_path).map_err(Error::from)?;

        let mut header_cont: String = String::new();
        let mut entry_cont: String = String::new();
        let mut func_cont: String = String::new();

        header.read_to_string(&mut header_cont).map_err(Error::from)?;
        entry.read_to_string(&mut entry_cont).map_err(Error::from)?;
        func.read_to_string(&mut func_cont).map_err(Error::from)?;

        let snap = PackageSnapshot::new(name, header_cont, entry_cont, func_cont);

        Self::open_from_snapshot(path.to_path_buf(), &snap, id)
    }
    pub fn open_from_compressed(path: PathBuf, file: &CompressedPackage, id: NumericalPackID) -> Result<Self, Error> {
        Self::open_from_snapshot(path, file.contents(), id)
    }
    pub fn open_from_snapshot(path: PathBuf, snap: &PackageSnapshot, id: NumericalPackID) -> Result<Self, Error> { 
        let name: String = validate_name(snap.name().to_string())
            .map_err(|x| Error::from(CoreError::from(x)))?;

        let header: PackageHeader = from_str(snap.header())
            .map_err(Error::from)?;
        let mut entries: Vec<VariableEntry> = from_str(snap.entries_raw())
            .map_err(Error::from)?;
        let mut func: Vec<FunctionEntry> = from_str(snap.functions_raw())
            .map_err(Error::from)?;

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
    
    fn get_next_id(&mut self) -> Result<NumericalResourceID, UnexpectedError> {
        if self.current_id == u32::MAX {
            Err(UnexpectedError::new("the max id for this package has already been taken"))
        }
        else {
            let result = self.current_id;
            self.current_id += 1;
            Ok(
                NumericalResourceID::new(
                    self.pack_id, 
                    result
                )
            )
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

        let mut header_file = File::create(self.location.join("header"))?;
        let mut entries_file = File::create(self.location.join("entry"))?;
        let mut functions_file = File::create(self.location.join("func"))?;

        header_file.write_all(snapshot.header().as_bytes())?;
        entries_file.write_all(snapshot.entries_raw().as_bytes())?;
        functions_file.write_all(snapshot.functions_raw().as_bytes())?;

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
    pub fn get_func(&self, id: NumericalResourceID) -> Option<&FunctionEntry> {
        id.contained_in_sc(&self.pack_id)?;

        self.func.iter().find(|x| x.accepts_id(id))
    }
    pub fn get_func_mut(&mut self, id: NumericalResourceID) -> Option<&mut FunctionEntry>{
        id.contained_in_sc(&self.pack_id)?;

        self.func.iter_mut().find(|x| x.accepts_id(id))
    }
 
    pub fn remove(&mut self, id: NumericalResourceID) -> bool {
        self.release_entry(id).is_some() || self.release_func(id).is_some()
    }
    pub fn release_entry(&mut self, id: NumericalResourceID) -> Option<VariableEntry> {
        id.contained_in_sc(&self.pack_id)?;

        let index = self.entries.iter().position(|x| x.accepts_id(id))?;

        Some(self.entries.remove(index))
    }
    pub fn release_func(&mut self, id: NumericalResourceID) -> Option<FunctionEntry> {
        id.contained_in_sc(&self.pack_id)?;

        let index = self.func.iter().position(|x| x.accepts_id(id))?;

        Some(self.func.remove(index))
    }
    pub fn remove_all(&mut self) {
        self.entries.clear();
        self.func.clear();
    }
    pub fn add_entry(&mut self, name: String, is_var: bool, data: VariableUnion) -> Result<NumericalResourceID, CoreError> {
        if let Err(e) = is_name_valid(&name) {
            return Err( e.into() )
        }

        let key = self.get_next_id().map_err(CoreError::from)?;

        let result = VariableEntry::new(
            key,
            name,
            is_var, 
            Some(data)
        ).map_err(CoreError::from)?;

        self.entries.push(result);
        Ok(key)
    }
    pub fn add_func(&mut self, name: String, data: ASTBasedFunction) -> Result<NumericalResourceID, CoreError> {
        if let Err(e) = is_name_valid(&name) {
            return Err( e.into() )
        }

        let key = self.get_next_id().map_err(CoreError::from)?;

        let result = FunctionEntry::new(
            key,
            name,
            data
        ).map_err(CoreError::from)?;

        self.func.push(result);
        Ok(key)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_package_locators() {
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

    #[test]
    fn package_loading() {

    }

    #[test]
    fn package_saving() {

    }

    #[test]
    fn package_functionality() {
        use crate::expr::repr::ConstExpr;
        use crate::calc::func::FunctionArgSignature;

        //This tests the getting, removing, releasing, and adding.

        let mut pack = Package::blank();
        
        {
            let id = pack.add_entry("thing1".to_string(), true, 6.1.into()).expect("unable to add");
            {
                let got = pack.get(id).expect("unable to grab id");
                assert_eq!(id, got.id());
            }

            {
                let got = pack.get_mut(id).expect("unable to grab by id");
                got.set_name("thing2".to_string()).unwrap();

            }

            assert!(pack.release_func(id).is_none());
            
            let released = pack.release_entry(id).expect("unable to release entry");
            assert_eq!(released.name(), "thing2");
            assert_eq!(released.id(), id);
        }

        {
            let id = pack.add_func(
                "thing1".to_string(), 
                ASTBasedFunction::new(
                    ConstExpr::new(1.0.into()).into(),
                    FunctionArgSignature::default()
                )
            ).expect("unable to add");

            {
                let got = pack.get_func(id).expect("unable to grab id");
                assert_eq!(id, got.id());
            }

            {
                let got = pack.get_func_mut(id).expect("unable to grab by id");
                got.set_name("thing2".to_string()).unwrap();

            }

            assert!(pack.release_entry(id).is_none());
            
            let released = pack.release_func(id).expect("unable to release entry");
            assert_eq!(released.name(), "thing2");
            assert_eq!(released.id(), id);
        }
    }
}