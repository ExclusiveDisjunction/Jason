use crate::calc::func::ASTBasedFunction;
use crate::io::id::{Locator, NumericalPackID, NumericalResourceID, PackageID, ResourceKind, STD_ID, USR_ID};
use crate::core::errors::Error as CoreError;

use super::base::{ReadPackage, WritePackage, WriteProvider};
use super::super::entry::{func::*, var::*};

use std::path::PathBuf;

/// Represents the `std` package. This is a read only (no adding, removing or modifying) package. It stores all of the `ImplBasedFunction` instances, and all constants used by the program. 
pub struct StdPack {
    func: Vec<ImplFunctionEntry>,
    constants: Vec<VariableEntry>
}
impl StdPack {
    pub fn new() -> Self {
        todo!()
    }
}
impl ReadPackage<ImplFunctionEntry> for StdPack {
    fn id(&self) -> NumericalPackID {
        STD_ID
    }
    fn name(&self) -> &str {
        "std"
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
    loc: PathBuf
}
impl ReadPackage<FunctionEntry> for UsrPack {
    fn id(&self) -> NumericalPackID {
        USR_ID
    }
    fn name(&self) -> &str {
        "usr"
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
    fn get_provider<'a>(&'a mut self) -> WriteProvider<'a> {
        WriteProvider::new(&mut self.entry, &mut self.func, &mut self.id, USR_ID)
    }
}