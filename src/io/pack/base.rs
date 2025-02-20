use crate::io::entry::VariableEntry;
use crate::io::entry::{IOEntry, IOStorage};
use crate::io::entry::func::FunctionEntryBase;
use crate::io::id::{Locator, NumericalPackID, NumericalResourceID, PackageID, ResourceKind};
use crate::calc::func::FunctionBase;
use crate::calc::VariableUnion;
use crate::core::errors::Error as CoreError;

pub trait ReadPackage<FuncT> where FuncT: FunctionEntryBase {
    fn id(&self) -> NumericalPackID;
    fn name(&self) -> &str;
    fn make_pack_id(&self) -> PackageID;

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

pub trait WritePackage<FuncT>: ReadPackage<FuncT> where FuncT: FunctionEntryBase {
    fn get_mut(&mut self, id: NumericalResourceID) -> Option<&mut VariableEntry>;
    fn get_mut_func(&mut self, id: NumericalResourceID) -> Option<&mut FuncT>;
    
    fn remove(&mut self, id: NumericalResourceID) -> bool {
        self.release_entry(id).is_some() || self.release_func(id).is_some()
    }
    fn release_entry(&mut self, id: NumericalResourceID) -> Option<VariableEntry>;
    fn release_func(&mut self, id: NumericalResourceID) -> Option<FuncT>;

    fn remove_all(&mut self);

    fn add_entry(&mut self, name: String, is_var: bool, data: VariableUnion) -> Result<NumericalPackID, CoreError>;
    fn add_func(&mut self, name: String, data: FuncT::Holding) -> Result<NumericalResourceID, CoreError>;
}