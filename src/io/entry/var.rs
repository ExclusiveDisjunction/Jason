pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::expr::raw::{Name, NameRef, ResourceKind};
use super::base::*;

use exdisj::log_error;

use std::fmt::{Display, Debug};
use std::sync::{Arc, RwLock};
use serde::ser::SerializeStruct;
use serde::{ser, Deserialize, Serialize, Serializer, Deserializer, de::{self, Visitor, SeqAccess, MapAccess}};

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum VariableEntryFields {
    Name,
    Data,
    Kind
}

struct VariableEntryVisitor;
impl<'de> Visitor<'de> for VariableEntryVisitor {
    type Value = VariableEntry;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("struct VariableEntry")
    }

    fn visit_seq<V: SeqAccess<'de>>(self, mut seq: V) -> Result<VariableEntry, V::Error> {
        //Data, name, kind

        let name = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        let kind = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;
        let data = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;

        Ok(
            VariableEntry::new(
                name,
                kind,
                data
            )
        )
    }
    fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<VariableEntry, V::Error> {
        let mut name = None;
        let mut kind = None;
        let mut data = None;

        while let Some(key) = map.next_key()? {
            match key {
                VariableEntryFields::Data => {
                    if data.is_some() {
                        return Err(de::Error::duplicate_field("data"));
                    }

                    data = Some(map.next_value()?);
                }
                VariableEntryFields::Name => {
                    if name.is_some() {
                        return Err(de::Error::duplicate_field("name"));
                    }

                    name = Some(map.next_value()?);
                }
                VariableEntryFields::Kind => {
                    if kind.is_some() {
                        return Err(de::Error::duplicate_field("kind"));
                    }

                    kind = Some(map.next_value()?);
                }
            }
        }

        let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
        let kind = kind.ok_or_else(|| de::Error::missing_field("kind"))?;
        let data = data.ok_or_else(|| de::Error::missing_field("data"))?;

        Ok(
            VariableEntry::new(
                name,
                kind,
                data
            )
        )

    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Serialize, Deserialize)]
pub enum VariableEntryType {
    Variable,
    EnvVariable,
}
impl From<VariableEntryType> for ResourceKind {
    fn from(value: VariableEntryType) -> Self {
        match value {
            VariableEntryType::Variable => Self::Variable,
            VariableEntryType::EnvVariable => Self::EnvVariable
        }
    }
}
impl VariableEntryType {
    pub fn prefix(&self) -> &'static str {
        match self {
            Self::Variable => "$",
            Self::EnvVariable => "!"
        }
    }
}

#[derive(Debug)]
pub struct VariableEntry {
    name: Name,
    kind: VariableEntryType,
    data: Arc<RwLock<VariableUnion>>
}
impl Serialize for VariableEntry {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let access = self.access();
        let guard = access.read();

        if let Some(inner) = guard.access() {
            let mut s= serializer.serialize_struct("VariableEntry", 3)?;
            s.serialize_field("name", &self.name)?;
            s.serialize_field("kind", &self.kind)?;
            s.serialize_field("data", inner)?;

            s.end()
        }
        else {
            let e = guard.take_err().unwrap();

            log_error!("Access failed for serilization: {}", &e);
            Err(ser::Error::custom(e))
        }
    }
}
impl<'de> Deserialize<'de> for VariableEntry {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        const FIELDS: &[&str] = &["data", "name", "kind"];
        deserializer.deserialize_struct("VariableEntry", FIELDS, VariableEntryVisitor)
    }
}
impl PartialEq for VariableEntry {
    fn eq(&self, other: &Self) -> bool {
        self.access().read() == other.access().read()
    }   
}
impl Display for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}", 
            self.kind.prefix(),
            &self.name
        )
    }
}
impl IOEntry for VariableEntry {
    fn name(&self) -> NameRef {
        self.name.as_name_ref()
    }
    fn set_name(&mut self, new: Name) {
        self.name = new;
    }
    fn kind(&self) -> ResourceKind {
        self.kind.into()
    }
}
impl IOStorage for VariableEntry {
    type Holding = VariableUnion;

    fn access(&self) -> EntryHandle<Self::Holding> {
        EntryHandle::new(Arc::clone(&self.data))
    }
}
impl VariableEntry {
    pub fn new(name: Name, kind: VariableEntryType, data: VariableUnion) -> Self {
        Self {
            name,
            kind,
            data: Arc::new( RwLock::new( data ) )
        }
    }
}

#[test]
fn test_variable_serde() {
    use crate::calc::*;
    use serde_json::{from_str, to_string_pretty};

    let entry = VariableEntry::new(
        Name::try_from("hello").unwrap(),
        VariableEntryType::Variable,
        Matrix::identity(4).into()
    );

    let ser = match to_string_pretty(&entry) {
        Ok(v) => v,
        Err(e) => panic!("{}", e)
    };

    let de_ser: Result<VariableEntry, _> = from_str(&ser);
    match de_ser {
        Ok(v) => assert_eq!(v, entry),
        Err(e) => panic!("unable to deserialize due to '{e}'")
    }
}