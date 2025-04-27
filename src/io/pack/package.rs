use crate::core::errors::{UnexpectedError, Error as CoreError};
use crate::core::version::JASON_CURRENT_VERSION;
use super::compress::PackageSnapshot;
use super::header::PackageHeader;
use super::base::{OpenablePackage, ReadPackage, SaveablePackage, WritePackage, WriteProvider};
use super::super::entry::*;
use super::super::id::*;
use super::super::core::Error;

use serde_json::from_str;

use std::path::{Path, PathBuf};
use std::fs::create_dir_all;
use std::io::Error as IOError;

pub struct Package {
    pack_id: NumericalPackID,
    current_id: NumericalResourceID,
    header: PackageHeader,
    entries: Vec<VariableEntry>,
    func: Vec<FunctionEntry>,

    location: VerifiedPath
}
impl Package {
    #[allow(dead_code)]
    fn blank() -> Self {
        use crate::core::Version;
        Self {
            pack_id: NumericalPackID::new(3),
            current_id: NumericalResourceID::new(NumericalPackID::new(3), 0),
            header: PackageHeader::new(Version::new(1, 0, 0), None),
            entries: vec![],
            func: vec![],
            location: VerifiedPath::new(
                Name::validate("temporary".to_string()).unwrap(),
                PathBuf::default()
            )
        }
    }
}
impl ReadPackage<FunctionEntry> for Package {
    fn id(&self) -> NumericalPackID {
        self.pack_id
    }
    fn name(&self) -> &Name {
        self.location.name()
    }

    fn entries(&self) -> &Vec<VariableEntry> {
        &self.entries
    }
    fn functions(&self) -> &Vec<FunctionEntry> {
        &self.func
    }
}
impl WritePackage for Package {
    fn get_provider(&mut self) -> WriteProvider<'_> {
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
    fn location(&self) -> &VerifiedPath {
        &self.location
    }
}

impl OpenablePackage for Package {
    fn open(snap: &PackageSnapshot, id: NumericalPackID) -> Result<Self, Error> {
        if !id.is_specific() {
            return Err(CoreError::from(UnexpectedError::new(format!("unable create using id: {id}, it is reserved"))).into());
        }

        let (name, from) = snap.path().pop_ref();
        let name = name.clone();

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
    fn create_into_directory(name: Name, id: NumericalPackID, path: &Path) -> Result<Self, Error> where Self: Sized {
        if !id.is_specific() {
            return Err(CoreError::from(UnexpectedError::new(format!("unable create using id: {id}, it is reserved"))).into());
        }


        if !path.exists() {
            create_dir_all(path).map_err(|x| Error::from(CoreError::from(x)))?;
        }

        Ok(
            Self {
                name,
                pack_id: id,
                current_id: NumericalResourceID::new(id, 0),
                header: PackageHeader::new(JASON_CURRENT_VERSION, None),
                entries: vec![],
                func: vec![],
                location: path.to_path_buf()
            }
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs::remove_dir;
    use crate::calc::{VariableUnion, func::ASTBasedFunction};

    #[test]
    fn package_locators() {
        use crate::calc::MathVector;

        let mut pack = Package::blank();

        assert_eq!(pack.make_pack_id(), pack.id());
        let author = "exdisj".to_string();
        pack.header_mut().set_author(Some(author.clone()));
        assert_eq!(pack.header().author(), Some(author.as_str()));

        let name = Name::validate("var1".to_string()).unwrap();
        assert!(pack.add_entry(name.clone(), true, 5.5.into()).is_ok());
        let loc = Locator::new(PackageID::Any, ResourceID::Weak(name), ResourceKind::Entry(VarEntryType::Variable));
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
            let mut pack = match Package::create_into_directory(Name::validate("test_pack".to_string()).unwrap(), NumericalPackID::new(3), &path)  {
                Ok(p) => p,
                Err(e) => panic!("Unable to create a new package at '{:?}' because of '{:?}'", &path, e)
            };


            let mut snapshot = pack.snapshot();

            let mut pack2 = match Package::open(
                &snapshot, 
                NumericalPackID::new(4), 
                VerifiedPath::verify(&path.join("test2")).expect("unable to verify path")) {
                Ok(p) => p,
                Err(e) => panic!("unable to openf rom snapshot due to '{:?}'", e)
            };

            assert_eq!(pack.header(), pack2.header());

            pack.add_entry(Name::validate("hello".to_string()).unwrap(), true, 0.into()).expect("unable to add entry");

            if let Err(e) = pack.save() {
                panic!("unable to save '{e}'")
            }
            else {
                println!("saving finished");
            }

            snapshot = pack.snapshot();
            target_snapshot = snapshot.clone();
            pack2 = match Package::open(
                &snapshot, 
                NumericalPackID::new(4), 
                VerifiedPath::verify(&path.join("test2")).expect("unable to verify path")) {
                Ok(p) => p,
                Err(e) => panic!("unable to open package '{:?}'", e)
            };

            if let Err(e) = pack2.save() {
                panic!("unable to save second pack '{:?}", e)
            }

            assert_eq!(pack.entries, pack2.entries);
        }

        {
            let snap = PackageSnapshot::open(&path.join("test_path")).expect("unable to open snapshot");

            let pack = Package::open(&snap, NumericalPackID::new(3)).expect("unable to re-open package");

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
        let thing1 = Name::validate("thing1".to_string()).unwrap();
        let thing2 = Name::validate("thing2".to_string()).unwrap();
        
        {
            let id = pack.add_entry(thing1.clone(), true, 6.1.into()).expect("unable to add");
            {
                let got = pack.get(id).expect("unable to grab id");
                assert_eq!(id, got.id());
            }

            {
                let got = pack.get_mut(id).expect("unable to grab by id");
                got.set_name(thing2.clone());

            }

            assert!(pack.release_func(id).is_none());
            
            let released = pack.release_entry(id).expect("unable to release entry");
            assert_eq!(released.name(), "thing2");
            assert_eq!(released.id(), id);
        }

        {
            let id = pack.add_func(
                thing1.clone(), 
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
                let got = pack.get_mut_func(id).expect("unable to grab by id");
                got.set_name(thing2.clone());

            }

            assert!(pack.release_entry(id).is_none());
            
            let released = pack.release_func(id).expect("unable to release entry");
            assert_eq!(released.name(), "thing2");
            assert_eq!(released.id(), id);
        }
    }
}