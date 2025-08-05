pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use exdisj::log_error;
use exdisj::error::FormattingError;
use super::super::id::{Name, NumericalResourceID, ResourceKind};
use super::base::*;

use std::fmt::{Display, Debug};
use std::sync::{Arc, RwLock};
use serde::ser::SerializeStruct;
use serde::{ser, Deserialize, Serialize, Serializer, Deserializer, de::{self, Visitor, SeqAccess, MapAccess}};

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub enum VarEntryType {
    Variable,
    Environment
}
impl Display for VarEntryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Variable => "Variable",
                Self::Environment => "Environment"
            }
        )
    }
}
impl TryFrom<String> for VarEntryType {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "Variable" => Ok(Self::Variable),
            "Environment" => Ok(Self::Environment),
            a => Err(FormattingError::new(&a, "cannot parse value"))
        }
    }
}
impl VarEntryType {
    pub fn symbol(&self) -> String {
        match self {
            Self::Environment => String::new(),
            Self::Variable => "$".to_string()
        }
    }
}

#[derive(Deserialize)]
#[serde(field_identifier, rename_all = "lowercase")]
enum VariableEntryFields {
    Data,
    Name, 
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

        let data = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(0, &self))?;
        let name = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        let kind = seq.next_element()?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;

        Ok(
            VariableEntry::new_direct(
                NumericalResourceID::default(), //A temporary key is given, the package must create keys themselves for this process
                name,
                kind,
                data
            )
        )
    }
    fn visit_map<V: MapAccess<'de>>(self, mut map: V) -> Result<VariableEntry, V::Error> {
        let mut data = None;
        let mut name = None;
        let mut kind = None;

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

        let data = data.ok_or_else(|| de::Error::missing_field("data"))?;
        let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
        let kind = kind.ok_or_else(|| de::Error::missing_field("kind"))?;

        Ok(
            VariableEntry::new_direct(
                NumericalResourceID::default(),
                name,
                kind,
                data
            )
        )

    }
}

pub struct VariableEntry {
    key: NumericalResourceID,
    name: Name,
    kind: VarEntryType,
    data: Arc<RwLock<VariableUnion>>
}
impl Serialize for VariableEntry {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let guard = self.get_data();
        if let Some(inner) = guard.access() {
            let mut s= serializer.serialize_struct("VariableEntry", 3)?;
            s.serialize_field("data", inner)?;
            s.serialize_field("name", &self.name)?;
            s.serialize_field("kind", &self.kind)?;

            s.end()
        }
        else {
            let e = guard.get_err().unwrap();

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
        self.get_data() == other.get_data()
    }   
}
impl Debug for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{:?}", 
            match self.kind {
                VarEntryType::Environment => "",
                VarEntryType::Variable => "$"
            },
            &self.name,
            self.get_data()
        )
    }
}
impl Display for VariableEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{}", 
            match self.kind {
                VarEntryType::Environment => "",
                VarEntryType::Variable => "$"
            },
            &self.name,
            self.get_data()
        )
    }
}
impl IOEntry for VariableEntry {
    fn name(&self) -> &Name {
        &self.name
    }
    fn set_name(&mut self, new: Name) {
        self.name = new;
    }

    fn resource_kind(&self) -> ResourceKind {
        ResourceKind::Entry(self.kind)
    }
    fn id(&self) -> NumericalResourceID {
        self.key
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID {
        &mut self.key
    }
}
impl IOStorage for VariableEntry {
    type Holding = VariableUnion;
    fn get_arc(&self) -> &Arc<RwLock<Self::Holding>> {
        &self.data
    }
}
impl VariableEntry {
    pub fn new(key: NumericalResourceID, name: Name, is_var: bool, data: Option<VariableUnion>) -> Self {
        Self::new_direct(
            key,
            name,
            if is_var {
                VarEntryType::Variable
            }
            else {
                VarEntryType::Environment
            },
            data.unwrap_or(0.into())
        )
    }
    pub fn new_direct(key: NumericalResourceID, name: Name, kind: VarEntryType, data: VariableUnion) -> Self {
        Self {
            name,
            key,
            kind,
            data: Arc::new(RwLock::new(data))
        }
    }
}

#[test]
fn test_variable_serde() {
    use crate::calc::*;
    use serde_json::{from_str, json};

    let entry = VariableEntry::new_direct(
        NumericalResourceID::default(),
        Name::validate("hello".to_string()).unwrap(),
        VarEntryType::Variable,
        Matrix::identity(4).into()
    );

    let ser = json!(&entry).to_string();
    let de_ser: Result<VariableEntry, _> = from_str(&ser);
    match de_ser {
        Ok(v) => assert_eq!(v, entry),
        Err(e) => panic!("unable to deserialize due to '{e}'")
    }
}