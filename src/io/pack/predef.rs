use crate::io::id::{Locator, NumericalPackID, NumericalResourceID, PackageID, ResourceKind, STD_ID, USR_ID};

use super::base::{ReadPackage, WritePackage};
use super::super::entry::{func::*, var::*, base::IOEntry};

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

}