use crate::calc::func::ASTBasedFunction;
use crate::core::errors::{NamingError, UnexpectedError, Error as CoreError};
use crate::core::version::JASON_CURRENT_VERSION;
use super::compress::{PackageSnapshot, CompressedPackage};
use super::header::PackageHeader;
use super::base::{ReadPackage, WritePackage, WriteProvider, SaveablePackage};
use super::super::entry::*;
use super::super::id::*;
use super::super::core::Error;

use serde_json::{from_str, json};

use std::path::{Path, PathBuf};
use std::fs::{File, create_dir_all};
use std::io::{Read, Error as IOError};

pub struct Package {
    pack_id: NumericalPackID,
    current_id: NumericalResourceID,
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
            pack_id: NumericalPackID::new(2),
            current_id: 0,
            name: "temporary".to_string(),
            header: PackageHeader::new(Version::new(1, 0, 0), None),
            entries: vec![],
            func: vec![],
            location: PathBuf::default()
        }
    }

    /*
    pub fn create_into_directory(path: &Path, name: String, id: NumericalPackID) -> Result<Self, Error> {
        if !id.is_specific() {
            return Err(CoreError::from(UnexpectedError::new(format!("unable create using id: {id}, it is reserved"))).into());
        }

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

         if !id.is_specific() {
            return Err(CoreError::from(UnexpectedError::new(format!("unable create using id: {id}, it is reserved"))).into());
        }

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
     */
}
impl ReadPackage<FunctionEntry> for Package {
    fn id(&self) -> NumericalPackID {
        self.pack_id
    }
    fn name(&self) -> &str {
        &self.name
    }

    fn entries(&self) -> &Vec<VariableEntry> {
        &self.entries
    }
    fn functions(&self) -> &Vec<FunctionEntry> {
        &self.func
    }
}
impl WritePackage for Package {
    fn get_provider<'a>(&'a mut self) -> WriteProvider<'a> {
        WriteProvider::new(&mut self.entries, &mut self.func, &mut self.current_id, self.pack_id)
    }
}
impl SaveablePackage for Package {
    fn header(&self) -> &PackageHeader {
        &self.header
    }
    fn header_mut(&mut self) -> &mut PackageHeader {
        &mut self.header
    }
    fn location(&self) -> &Path {
        &self.location
    }

    fn open(snap: &PackageSnapshot, id: NumericalPackID, from: &Path) -> Result<Self, Error> {
        if !id.is_specific() {
            return Err(CoreError::from(UnexpectedError::new(format!("unable create using id: {id}, it is reserved"))).into());
        }

        if !from.is_dir() {
            return Err(IOError::new(std::io::ErrorKind::NotADirectory, "the package must be opened in a directory").into());
        }

        

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
                current_id: NumericalResourceID::new(id, 0), //Sets it to the current 
                header,
                entries,
                func,
                location: from.to_path_buf()
            }
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::remove_dir;

    #[test]
    fn package_locators() {
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
        let path = PathBuf::from("test");
        let _ = remove_dir(path.join("test_pack"));
        let _ = remove_dir(path.join("test2"));

        let target_snapshot: PackageSnapshot;

        {
            
            let mut pack = match Package::create_into_directory(&path, "test_pack".to_string(), NumericalPackID::new(2)) {
                Ok(p) => p,
                Err(e) => panic!("Unable to create a new package at '{:?}' because of '{:?}'", &path, e)
            };


            let mut snapshot = pack.snapshot();

            let mut pack2 = match Package::open_from_snapshot(path.join("test2"), &snapshot, NumericalPackID::new(3)) {
                Ok(p) => p,
                Err(e) => panic!("unable to openf rom snapshot due to '{:?}'", e)
            };

            assert_eq!(pack.header(), pack2.header());

            pack.add_entry("hello".to_string(), true, 0.into()).expect("unable to add entry");

            if let Err(e) = pack.save() {
                panic!("unable to save '{e}'")
            }
            else {
                println!("saving finished");
            }

            snapshot = pack.snapshot();
            target_snapshot = snapshot.clone();
            pack2 = match Package::open_from_snapshot(path.join("test2"), &snapshot, NumericalPackID::new(3)) {
                Ok(p) => p,
                Err(e) => panic!("unable to open package '{:?}'", e)
            };

            if let Err(e) = pack2.save() {
                panic!("unable to save second pack '{:?}", e)
            }

            assert_eq!(pack.entries, pack2.entries);
        }

        {
            let pack = Package::open_from_directory(&path.join("test_pack"), NumericalPackID::new(2)).expect("unable to re-open package");

            assert!(pack.entries.len() == 1);

            let our_snapshot = pack.snapshot();
            assert_eq!(our_snapshot, target_snapshot);
        }
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