use crate::io::id::{NumericalPackID, NumericalResourceID, PackageID, STD_ID, USR_ID, Name};
use crate::io::core::Error;

use super::base::{ReadPackage, SaveablePackage, WritePackage, WriteProvider};
use super::header::PackageHeader;
use super::super::entry::{func::*, var::*};

use std::path::{Path, PathBuf};

/// Represents the `std` package. This is a read only (no adding, removing or modifying) package. It stores all of the `ImplBasedFunction` instances, and all constants used by the program. 
pub struct StdPack {
    func: Vec<ImplFunctionEntry>,
    constants: Vec<VariableEntry>,
    name: Name
}
impl Default for  StdPack {
    fn default() -> Self {
        todo!()
    }
}
impl ReadPackage<ImplFunctionEntry> for StdPack {
    fn id(&self) -> NumericalPackID {
        STD_ID
    }
    fn name(&self) -> &Name {
        &self.name
    }
    fn make_pack_id(&self) -> PackageID {
        PackageID::Std
    }

    fn entries(&self) -> &Vec<VariableEntry> {
        &self.constants
    }
    fn functions(&self) -> &Vec<ImplFunctionEntry> {
        &self.func
    }
}

pub struct UsrPack {
    entry: Vec<VariableEntry>,
    func: Vec<FunctionEntry>,
    id: NumericalResourceID,
    loc: PathBuf,
    name: Name,
    header: PackageHeader
}
impl ReadPackage<FunctionEntry> for UsrPack {
    fn id(&self) -> NumericalPackID {
        USR_ID
    }
    fn name(&self) -> &Name {
        &self.name
    }
    fn make_pack_id(&self) -> PackageID {
        PackageID::Usr
    }

    fn entries(&self) -> &Vec<VariableEntry> {
        &self.entry
    }
    fn functions(&self) -> &Vec<FunctionEntry> {
        &self.func
    }


}
impl WritePackage for UsrPack {
    fn get_provider(&mut self) -> WriteProvider<'_> {
        WriteProvider::new(&mut self.entry, &mut self.func, &mut self.id, USR_ID)
    }
}
impl SaveablePackage for UsrPack {
    fn header(&self) -> &PackageHeader {
        &self.header
    }
    fn header_mut(&mut self) -> &mut PackageHeader {
        &mut self.header
    }

    fn location(&self) -> &Path {
        &self.loc
    }
}
impl UsrPack {
    pub fn new(path: &Path) -> Result<Self, Error> {
        todo!()
    }
    pub fn create(path: &Path) -> Result<Self, std::io::Error> {
        todo!()
    }
}