pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::calc::func::ASTBasedFunction;
use crate::log_error;
use crate::core::errors::NamingError;
use super::super::id::{validate_name, NumericalResourceID, ResourceKind};
use super::base::*;

use std::fmt::{Display, Debug};
use std::sync::{Arc, RwLock};
use serde::ser::SerializeStruct;
use serde::{ser, Deserialize, Serialize, Serializer, Deserializer, de::{self, Visitor, SeqAccess, MapAccess}};

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum FunctionEntryFields {
    Data,
    Name
}

struct FunctionEntryVisitor;
impl<'de> Visitor<'de> for FunctionEntryVisitor {
    type Value = FunctionEntry;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("struct FunctionEntry")
    }

    fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<FunctionEntry, V::Error> {
        //data, name

        let data = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        let name = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;

        FunctionEntry::new(
            NumericalResourceID::default(),
            name,
            data
        ).map_err(de::Error::custom)
    }
    fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<FunctionEntry, V::Error> {
        let mut data = None;
        let mut name = None;

        while let Some(key) = map.next_key()? {
            match key {
                FunctionEntryFields::Data => {
                    if data.is_some() {
                        return Err(de::Error::duplicate_field("data"));
                    }

                    data = Some(map.next_value()?);
                }
                FunctionEntryFields::Name => {
                    if name.is_some() {
                        return Err(de::Error::duplicate_field("name"));
                    }

                    name = Some(map.next_value()?);
                }
            }
        }

        let data = data.ok_or_else(|| de::Error::missing_field("data"))?;
        let name = name.ok_or_else(|| de::Error::missing_field("name"))?;

        FunctionEntry::new(
            NumericalResourceID::default(),
            name,
            data
        ).map_err(de::Error::custom)
    }
}

pub struct FunctionEntry {
    name: String,
    data: Arc<RwLock<ASTBasedFunction>>,
    key: NumericalResourceID
}
impl Serialize for FunctionEntry {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let guard = self.get_data();
        if let Some(r) = guard.access() {
            let mut s: <S as Serializer>::SerializeStruct = serializer.serialize_struct("FunctionEntry", 2)?;
            s.serialize_field("data", r)?;
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
        const FIELDS: &[&str] = &["data", "name"];
        deserializer.deserialize_struct("FunctionEntry", FIELDS, FunctionEntryVisitor)
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
        &self.data
    }
}
impl FunctionEntry {
    pub fn new(key: NumericalResourceID, name: String, data: ASTBasedFunction) -> Result<Self, NamingError> {
        let mut result = Self {
            data: Arc::new(RwLock::new(data)),
            name: String::new(),
            key
        };

        result.set_name(name)?;
        Ok(result)        
    }
}

#[test]
fn test_function_serde() {
    use crate::expr::repr::*;
    use crate::calc::func::*;
    use serde_json::{from_str, json};

    let ast: TotalNodes = OperatorExpr::new(
        RawOperator::Plus,
        ConstExpr::new(4.0.into()).into(),
        VariableExpr::new('x', 0).into()
    ).into();

    let func = FunctionEntry::new(NumericalResourceID::default(), "hello".to_string(), ASTBasedFunction::new(ast, FunctionArgSignature::just_x())).unwrap();

    let ser = json!(&func).to_string();
    let de_ser: Result<FunctionEntry, _> = from_str(&ser);
    match de_ser {
        Ok(v) => assert_eq!(v, func),
        Err(e) => panic!("Unable to deserialize due to '{e}'")
    }
}