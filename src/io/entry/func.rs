pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::calc::func::ASTBasedFunction;
use crate::log_error;
use crate::core::errors::NamingError;
use super::super::id::{validate_name, NumericalResourceID, ResourceKind};
use super::base::*;

use std::fmt::{Display, Debug};
use std::sync::{Arc, RwLock};
use serde::ser::SerializeStruct;
use serde::{ser, Deserialize, Serialize, Serializer, Deserializer};

pub struct FunctionEntry {
    name: String,
    inner: Arc<RwLock<ASTBasedFunction>>,
    key: NumericalResourceID
}
impl Serialize for FunctionEntry {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let guard = self.get_data();
        if let Some(r) = guard.access() {
            let mut s: <S as Serializer>::SerializeStruct = serializer.serialize_struct("FunctionEntry", 2)?;
            s.serialize_field("inner", r)?;
            s.serialize_field("name", &self.name)?;

            s.end()
        }
        else {
            let e = guard.get_err().unwrap();
            log_error!("Access failed for serialization: {}", &e);

            Err(ser::Error::custom(e))
        }
    }
}
impl<'de> Deserialize<'de> for FunctionEntry {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        todo!()
    }
}
impl PartialEq for FunctionEntry {
    fn eq(&self, other: &Self) -> bool {
        self.get_data() == other.get_data()
    }
}
impl Debug for FunctionEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.get_data())
    }
}
impl Display for FunctionEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_data())
    }
}
impl IOEntry for FunctionEntry {
    fn name(&self) -> &str  {
        &self.name
    }
    fn set_name(&mut self, new: String) -> Result<(), NamingError> {
        self.name = validate_name(new)?;
        Ok(())
    }

    fn id(&self) -> NumericalResourceID {
        self.key
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID {
        &mut self.key
    }
    fn resource_kind(&self) -> ResourceKind {
        ResourceKind::Function   
    }
}
impl IOStorage for FunctionEntry {
    type Holding = ASTBasedFunction;
    fn get_arc(&self) -> &Arc<RwLock<Self::Holding>> {
        &self.inner
    }
}
impl FunctionEntry {
    pub fn new(key: NumericalResourceID, name: String, data: ASTBasedFunction) -> Result<Self, NamingError> {
        let mut result = Self {
            inner: Arc::new(RwLock::new(data)),
            name: String::new(),
            key
        };

        result.set_name(name)?;
        Ok(result)        
    }
}