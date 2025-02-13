use crate::core::errors::FormattingError;
use crate::core::is_string_whitespace;
use super::entry::EntryType;

use core::fmt;
use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

use serde::{Serialize, Deserialize};

#[derive(PartialEq, Eq, Clone)]
pub enum WeakPackID {
    Spec(String),
    Any,
    Usr,
    Std
}
impl Display for WeakPackID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", 
            match self {
                Self::Spec(s) => s.as_str(),
                Self::Any => "*",
                Self::Usr => "usr",
                Self::Std => "std"
            }
        )
    }
}
impl WeakPackID {
    pub fn looking_at(&self, name: &str) -> bool {
        if name == "usr" && matches!(self, Self::Usr) { return true; }
        else if name == "std" && matches!(self, Self::Std) { return true; }

        match self {
            WeakPackID::Any => true,
            WeakPackID::Spec(s) => s.as_str() == name,
            _ => false
        }
    }
    pub fn is_usr(&self) -> bool {
        matches!(self, Self::Usr)
    }
    pub fn is_std(&self) -> bool {
        matches!(self, Self::Std)
    }
    pub fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct StrongPackID {
    id: u32,
    name: String
}
impl StrongPackID {
    pub const fn new(id: u32, name: String) -> Self {
        if id == 0 {
            panic!("undefined behavior, the id provided was zero");
        }
        Self {
            id,
            name
        }
    }
    pub fn std_id() -> Self {
        Self {
            id: 1,
            name: "std".to_string()
        }
    }
    pub fn usr_id() -> Self {
        Self {
            id: 2,
            name: "usr".to_string()
        }
    }

    pub fn id(&self) -> u32 {
        self.id
    }
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn is_usr(&self) -> bool {
        self.id == 2 && self.name == "usr"
    }
    pub fn is_std(&self) -> bool {
        self.id == 1 && self.name == "std"
    }
    pub fn is_specific(&self) -> bool {
        self.id > 2
    }
}

impl Into<WeakPackID> for StrongPackID {
    fn into(self) -> WeakPackID {
        if self.is_usr() {
            WeakPackID::Usr
        }
        else if self.is_std() {
            WeakPackID::Std
        }
        else {
            WeakPackID::Spec(self.name)
        }
    }
}

pub trait ResourceID {
    fn looking_at(&self, target: &StrongPackID) -> bool;
    /// Allows for early returning via Option.
    fn looking_at_sc(&self, target: &StrongPackID) -> Option<()> {
        if self.looking_at(target) {
            Some(())
        }
        else {
            None
        }
    }

    fn by_name(&self) -> Option<(String, String)>;
    fn by_id(&self) -> Option<(u32, u32)>;
}

#[derive(Clone, Eq, Default, Debug, Serialize, Deserialize)]
pub struct StrongResourceID {
    pack_id: u32,
    entry_id: u32
}

impl Display for StrongResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{}", self.pack_id, self.entry_id)
    }
}

impl Hash for StrongResourceID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pack_id.hash(state);
        self.entry_id.hash(state);
    }
}

impl PartialEq for StrongResourceID {
    fn eq(&self, other: &Self) -> bool {
        self.pack_id == other.pack_id && self.entry_id == other.entry_id
    }
}
impl PartialOrd for StrongResourceID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for StrongResourceID {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.pack_id.cmp(&other.pack_id) {
            Ordering::Equal => self.entry_id.cmp(&other.entry_id),
            a => a
        }
    }
}

impl From<StrongResourceID> for String {
    fn from(val: StrongResourceID) -> Self {
        val.to_string()
    }
}
impl TryFrom<String> for StrongResourceID {
    type Error = FormattingError;
    fn try_from(from: String) -> Result<Self, Self::Error> {
        let splits: Vec<&str> = from.split('.').collect();
        if splits.len() != 2 {
            return Err(FormattingError::new(&from, "too many or not enough arguments, should be two"));
        }

        let a = splits[0].parse::<u32>();
        let b = splits[1].parse::<u32>();

        match (a, b) {
            (Ok(pack_id), Ok(entry_id)) => Ok(Self::new(pack_id, entry_id)),
            _ => Err(FormattingError::new(&from, "expected two numerical values")),
        }
    }
}

impl ResourceID for StrongResourceID {
    fn looking_at(&self, target: &StrongPackID) -> bool {
        self.pack_id == target.id()
    }
    fn by_id(&self) -> Option<(u32, u32)> {
        Some((self.pack_id, self.entry_id))
    }
    fn by_name(&self) -> Option<(String, String)> {
        None
    }
}
impl StrongResourceID {
    pub fn new(pack_id: u32, entry_id: u32) -> Self {
        Self {
            pack_id,
            entry_id
        }
    }

    pub fn pack_id(&self) -> u32 {
        self.pack_id
    }
    pub fn set_pack_id(&mut self, pack_id: u32) {
        self.pack_id = pack_id;
    }
    pub fn entry_id(&self) -> u32 {
        self.entry_id
    }
    pub fn set_entry_id(&mut self, entry_id: u32) {
        self.entry_id = entry_id;
    }

    pub fn looking_at(&self, pack_id: u32) -> bool {
        self.pack_id == pack_id
    }
}

#[derive(Eq)]
pub struct WeakResourceID {
    pack: WeakPackID,
    entry_kind: Option<EntryType>, //If this is none, then arguments is not none, and vice versa.
    entry_name: String,
    arguments: Option<Vec<String>>
}
impl PartialEq for WeakResourceID {
    fn eq(&self, other: &Self) -> bool {
        self.pack == other.pack && self.entry_kind == other.entry_kind && self.entry_name == other.entry_name && self.arguments == other.arguments
    }
}
impl TryFrom<String> for WeakResourceID {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        let as_ref: &str = &value;
        Self::try_from(as_ref)
    }
}
impl TryFrom<&str> for WeakResourceID {
    type Error = FormattingError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        /*
            We have the following formats:

            [name] => Environment Variable in (usr),
            $[name] => Variable in (usr),
            %[name](args...) => Function in (usr),
            [std | usr | *]::[name] => Environment variable in target
            [std | usr | *]::$[name] => Variable in target,
            [std | usr | *]::%[name](args...) => Function in target,
            [pack]::[name] => Environment variable in (pack),
            [pack]::$[name] => Variable in (pack),
            [pack]::%[name](args...) => Function in (pack)

            Furthermore, there are some constrained properties:
            1: Name, or pack name cannot be blank (with or without formatting specifiers)
            2: If a function is called, the parenthesis are required, no matter what
         */

        //First we check for the existence of the package. Then we will worry about the other part.
        let splits: Vec<&str> = value.split("::").collect();

        let parent: WeakPackID;
        let raw_child: &str;

        match splits.len() {
            1 => {
                parent = WeakPackID::Usr;
                raw_child = splits[0];
            },
            2 => {
                match splits[0] {
                    "usr" => parent = WeakPackID::Usr,
                    "std" => parent = WeakPackID::Std,
                    "*" => parent = WeakPackID::Any,
                    a if !a.is_empty() && !is_string_whitespace(a) => parent = WeakPackID::Spec(a.trim().to_string()),
                    b => return Err(FormattingError::new(&b, "the parent package must not be just whitespace or empty"))
                }
                raw_child = splits[1];
            },
            _ => return Err(FormattingError::new(&value, "there can only be one or two values before and after the '::'"))
        }

        let raw_child = raw_child.trim();
        if raw_child.is_empty() || is_string_whitespace(raw_child) {
            return Err(FormattingError::new(&raw_child, "the identifier cannot be empty or just whitespace"));
        }

        if let Some(c) = raw_child.strip_prefix("$") {
            if c.is_empty() || is_string_whitespace(c) {
                return Err(FormattingError::new(&value, "the name following the $ must not be white space nor empty"));
            }

            Ok(
                Self {
                    pack: parent,
                    entry_kind: Some(EntryType::Variable),
                    entry_name: c.trim().to_string(),
                    arguments: None
                }
            )
        }
        else if let Some(mut c) = raw_child.strip_prefix("%") {
            // Pattern: $[name](Args...)
            c = c.trim();

            if let Some(beg) = c.strip_suffix(')') {
                let splits: Vec<&str> = beg.split('(').collect();
                if splits.len() != 2 {
                    return Err(FormattingError::new(&value, "functions must have a name followed by a '(', and then the arguments (if specified)"));
                }

                let name = splits[0].trim().to_string();
                let args = splits[1].trim();

                if name.is_empty() || is_string_whitespace(&name) {
                    return Err(FormattingError::new(&name, "functions cannot have an empty name"));
                }

                let split_args: Vec<String>;
                if !args.is_empty() && !is_string_whitespace(args) {
                    split_args = args.split(',').map(|x| x.trim().to_string()).collect();
                }
                else {
                    split_args = vec![];
                }

                Ok(
                    Self {
                        pack: parent,
                        entry_kind: None,
                        entry_name: name,
                        arguments: Some(split_args)
                    }
                )
            }
            else {
                return Err(FormattingError::new(&value, "the function signature does not end with a ')'"));
            }
        }
        else {
            Ok(
                Self {
                    pack: parent,
                    entry_kind: Some(EntryType::Environment),
                    entry_name: raw_child.to_string(),
                    arguments: None
                }
            )
        }
    }
}
impl Display for WeakResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(a) = self.arguments.as_deref() {
            write!(f, "{}::%{}({})", &self.pack, &self.entry_name, a.join(", "))
        }
        else {
            write!(f, "{}::{}{}", &self.pack, &self.entry_name, self.entry_kind.unwrap().symbol())
        }
    }
}
impl Debug for WeakResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl ResourceID for WeakResourceID {
    fn looking_at(&self, target: &StrongPackID) -> bool {
        self.pack.looking_at(target.name())
    }

    fn by_id(&self) -> Option<(u32, u32)> {
        None
    }
    fn by_name(&self) -> Option<(String, String)> {
        Some((self.pack.to_string(), self.entry_name.to_string()))
    }
}
impl WeakResourceID {
    pub fn new_entry(package: WeakPackID, kind: EntryType, name: String) -> Self {
        Self {
            pack: package,
            entry_kind: Some(kind),
            entry_name: name,
            arguments: None
        }
    }
    pub fn new_func(package: WeakPackID, name: String, arguments: Vec<String>) -> Self {
        Self {
            pack: package,
            entry_kind: None,
            entry_name: name,
            arguments: Some(arguments)
        }
    }

    pub fn package(&self) -> &WeakPackID {
        &self.pack
    }
    pub fn name(&self) -> &str {
        &self.entry_name
    }
    pub fn function_arguments(&self) -> Option<&Vec<String>> {
        self.arguments.as_ref()
    }
    pub fn entry_kind(&self) -> Option<&EntryType> {
        self.entry_kind.as_ref()
    }
}

#[test]
fn test_io_id() {
    /*
        We have the following formats:

        Part 1:
        [name] => Environment Variable in (usr),
        $[name] => Variable in (usr),
        %[name](args...) => Function in (usr),

        Part 2:
        [std | usr | *]::[name] => Environment variable in target
        [std | usr | *]::$[name] => Variable in target,
        [std | usr | *]::%[name](args...) => Function in target,

        Part 3:
        [pack]::[name] => Environment variable in (pack),
        [pack]::$[name] => Variable in (pack),
        [pack]::%[name](args...) => Function in (pack)

        Furthermore, there are some constrained properties:
        1: Name, or pack name cannot be blank (with or without formatting specifiers)
        2: If a function is called, the parenthesis are required, no matter what
    */

    assert_eq!( WeakResourceID::try_from("aa"), Ok(WeakResourceID::new_entry(WeakPackID::Usr, EntryType::Environment, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("").is_err() );
    assert_eq!( WeakResourceID::try_from("$aa"), Ok(WeakResourceID::new_entry(WeakPackID::Usr, EntryType::Variable, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("$").is_err() );
    assert_eq!( WeakResourceID::try_from("%aa()"), Ok(WeakResourceID::new_func(WeakPackID::Usr, "aa".to_string(), vec![])) ) ;
    assert!( WeakResourceID::try_from("%aa").is_err() );
    assert!( WeakResourceID::try_from("%").is_err() );

    assert_eq!( WeakResourceID::try_from("usr::aa"), Ok(WeakResourceID::new_entry(WeakPackID::Usr, EntryType::Environment, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("usr::").is_err() );
    assert_eq!( WeakResourceID::try_from("usr::$aa"), Ok(WeakResourceID::new_entry(WeakPackID::Usr, EntryType::Variable, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("usr::$").is_err() );
    assert_eq!( WeakResourceID::try_from("usr::%aa()"), Ok(WeakResourceID::new_func(WeakPackID::Usr, "aa".to_string(), vec![])) ) ;
    assert!( WeakResourceID::try_from("usr::%aa").is_err() );
    assert!( WeakResourceID::try_from("usr::%").is_err() );

    assert_eq!( WeakResourceID::try_from("std::aa"), Ok(WeakResourceID::new_entry(WeakPackID::Std, EntryType::Environment, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("std::").is_err() );
    assert_eq!( WeakResourceID::try_from("std::$aa"), Ok(WeakResourceID::new_entry(WeakPackID::Std, EntryType::Variable, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("std::$").is_err() );
    assert_eq!( WeakResourceID::try_from("std::%aa()"), Ok(WeakResourceID::new_func(WeakPackID::Std, "aa".to_string(), vec![])) ) ;
    assert!( WeakResourceID::try_from("std::%aa").is_err() );
    assert!( WeakResourceID::try_from("std::%").is_err() );

    assert_eq!( WeakResourceID::try_from("*::aa"), Ok(WeakResourceID::new_entry(WeakPackID::Any, EntryType::Environment, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("*::").is_err() );
    assert_eq!( WeakResourceID::try_from("*::$aa"), Ok(WeakResourceID::new_entry(WeakPackID::Any, EntryType::Variable, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("*::$").is_err() );
    assert_eq!( WeakResourceID::try_from("*::%aa()"), Ok(WeakResourceID::new_func(WeakPackID::Any, "aa".to_string(), vec![])) ) ;
    assert!( WeakResourceID::try_from("*::%aa").is_err() );
    assert!( WeakResourceID::try_from("*::%").is_err() );

    assert_eq!( WeakResourceID::try_from("foo::aa"), Ok(WeakResourceID::new_entry(WeakPackID::Spec("foo".to_string()), EntryType::Environment, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("foo::").is_err() );
    assert_eq!( WeakResourceID::try_from("foo::$aa"), Ok(WeakResourceID::new_entry(WeakPackID::Spec("foo".to_string()), EntryType::Variable, "aa".to_string()))) ;
    assert!( WeakResourceID::try_from("foo::$").is_err() );
    assert_eq!( WeakResourceID::try_from("foo::%aa()"), Ok(WeakResourceID::new_func(WeakPackID::Spec("foo".to_string()), "aa".to_string(), vec![])) ) ;
    assert!( WeakResourceID::try_from("foo::%aa").is_err() );
    assert!( WeakResourceID::try_from("foo::%").is_err() );

    assert!( WeakResourceID::try_from("::").is_err() );
}