use crate::core::errors::FormattingError;
use crate::core::is_string_whitespace;
use super::entry::EntryType;

use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

use serde::{Serialize, Deserialize};

/*
    Various Identification tools for Packages and Resources. There are two major kinds: Weak and Strong.
    1. Weak - Text based, may or may not exist. 
    2. Strong - u32 based, usually exists unless it gets removed. 

    Futhermore, there is the the trait ResourceID. This trait provides functionality for determining if a resource is owned by a package, or if the ID points to the current object. 
*/

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct NumericalPackID {
    id: u32
}
impl NumericalPackID {
    pub fn new(id: u32) -> Self {
        Self {
            id
        }
    }
    pub fn any_id() -> Self {
        Self {
            id: 0
        }
    }
    pub fn std_id() -> Self {
        Self {
            id: 1
        }
    }
    pub fn usr_id() -> Self {
        Self {
            id: 2
        }
    }
    
    pub fn is_any(&self) -> bool { self.id == 0 }
    pub fn is_std(&self) -> bool { self.id == 1 }
    pub fn is_usr(&self) -> bool { self.id == 2 }
    pub fn is_specific(&self) -> bool { self.id > 2 }
}

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
pub enum StrongPackID {
    Spec(String, NumericalPackID),
    SpecNum(NumericalPackID),
    Any,
    Usr,
    Std
}
impl PartialEq<WeakPackID> for StrongPackID {
    fn eq(&self, other: &WeakPackID) -> bool {
        match (self, other) {
            (Self::Any, WeakPackID::Any) => true,
            (Self::Std, WeakPackID::Std) => true,
            (Self::Usr, WeakPackID::Usr) => true,
            (Self::Spec(s, _), WeakPackID::Spec(t)) => s == t, 
            (Self::SpecNum(_), WeakPackID::Any) => true,
            _ => false
        }
    }
}
impl Into<WeakPackID> for StrongPackID {
    fn into(self) -> WeakPackID {
        match self {
            Self::Any => WeakPackID::Any,
            Self::Std => WeakPackID::Std,
            Self::Usr => WeakPackID::Usr,
            Self::Spec(s, _) => WeakPackID::Spec(s),
            Self::SpecNum(_) => WeakPackID::Any
        }
    }
}
impl StrongPackID {
    pub fn id(&self) -> NumericalPackID {
        match self {
            Self::Spec(_, id) => id.clone(),
            Self::SpecNum(id) => id.clone(),
            Self::Any => NumericalPackID::any_id(),
            Self::Usr => NumericalPackID::usr_id(),
            Self::Std => NumericalPackID::std_id()
        }
    }
    pub fn name(&self) -> &str {
        match self {
            Self::Spec(s, _) => s.as_str(),
            Self::SpecNum(_) => "(id)",
            Self::Any => "*",
            Self::Usr => "usr",
            Self::Std => "std"
        }
    }

    pub fn is_any(&self) -> bool {
        matches!(self, Self::Any)
    }
    pub fn is_usr(&self) -> bool {
        matches!(self, Self::Usr)
    }
    pub fn is_std(&self) -> bool {
        matches!(self, Self::Std)
    }
    pub fn is_specific(&self) -> bool {
        matches!(self, Self::Spec(_, _)) || matches!(self, Self::SpecNum(_))
    }
}

#[derive(Eq, Clone)]
pub enum PackageID {
    Weak(WeakPackID),
    Strong(StrongPackID)
}
impl PartialEq for PackageID {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Strong(a), Self::Strong(b)) => a == b,
            (Self::Weak(a), Self::Weak(b)) => a == b,
            (Self::Strong(a), Self::Weak(b)) | (Self::Weak(b), Self::Strong(a)) => a == b
        }
    }
}
impl From<WeakPackID> for PackageID {
    fn from(value: WeakPackID) -> Self {
        Self::Weak(value)
    }
}
impl From<StrongPackID> for PackageID {
    fn from(value: StrongPackID) -> Self {
        Self::Strong(value)
    }
}
impl Into<WeakPackID> for PackageID {
    fn into(self) -> WeakPackID {
        match self {
            Self::Weak(w) => w,
            Self::Strong(s) => s.into()
        }
    }
}

#[derive(Clone, Eq)]
pub enum ResxID {
    Numeric(u32),
    Strong(String, u32),
    Weak(String)
}
impl Display for ResxID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Strong(a, _) | Self::Weak(a) => write!(f, "{a}"),
            Self::Numeric(a) => write!(f, "(id:{a})")
        }
    }
}
impl Debug for ResxID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Numeric(a) => write!(f, "resx-id: {a}"),
            Self::Strong(a, b) => write!(f, "resx: '{a}' id: {b}"),
            Self::Weak(a) => write!(f, "weak-id: '{a}'")
        }
    }
}
impl PartialEq for ResxID {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Numeric(a), Self::Numeric(b)) => a == b,
            (Self::Strong(a, b), Self::Strong(c, d)) => a == c && b == d,
            (Self::Strong(_, a), Self::Numeric(b)) | (Self::Numeric(b), Self::Strong(_, a)) => a == b,
            (Self::Weak(a), Self::Weak(b)) => a == b,
            (Self::Weak(a), Self::Strong(b, _)) | (Self::Strong(b, _), Self::Weak(a)) => a == b,
            _ => false
        }
    }
}
impl ResxID {
    pub fn id(&self) -> Option<u32> {
        match self {
            Self::Numeric(a) | Self::Strong(_, a) => Some(a),
            _ => None
        }
    }
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Strong(a, _) | Self::Weak(a) => Some(a.as_str()),
            _ => None
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum ResourceKind {
    Function(Vec<String>),
    Entry(EntryType)
}
impl From<EntryType> for ResourceKind {
    fn from(value: EntryType) -> Self {
        Self::Entry(value)
    }
}
impl Display for ResourceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(a) => write!(f, "%({})", a.join(", ")),
            Self::Entry(e) => (e as &dyn Display).fmt(f)
        }
    }
}
impl Debug for ResourceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(a) => write!(f, "%({})", a.join(", ")),
            Self::Entry(e) => (e as &dyn Debug).fmt(f)
        }
    }
}

pub struct dick {
    parent: NumericalPackID,
    entry: u32
}

pub struct ResourceLocator {
    parent: PackageID,
    resource: ResxID,
    kind: ResourceKind
}

impl TryFrom<String> for ResourceLocator {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}
impl TryFrom<&str> for ResourceLocator {
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
                    parent: parent.into(),
                    kind: EntryType::Variable.into(),
                    resource: ResxID::Weak(c.trim().to_string())
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
                        parent: parent.into(),
                        kind: ResourceKind::Function(split_args),
                        resource: ResxID::Weak(name)
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
                    parent: parent.into(),
                    kind: EntryType::Environment.into(),
                    resource: ResxID::Weak(raw_child.to_string())
                }
            )
        }
    }
}
impl ResourceLocator {
    pub fn strong_entry(parent: NumericalPackID, resource: u32, kind: EntryType) -> Self {

    }
    pub fn new(parent: PackageID, resource: ResxID, kind: ResourceKind) -> Self {
        Self {
            parent,
            resource,
            kind
        }
    }
    pub fn new_entry(parent: PackageID, resource: ResxID, kind: EntryType) -> Self {
        Self {
            parent,
            resource,
            kind: kind.into()
        }
    }
    pub fn new_func(parent: PackageID, resource: ResxID, args: Vec<String>) -> Self {
        Self {
            parent,
            resource,
            kind: ResourceKind::Function(args)
        }
    }

    pub fn parent(&self) -> &PackageID {
        &self.parent
    }
    pub fn resource(&self) -> &ResxID {
        &self.resource
    }

    pub fn contained_in(&self, parent: &PackageID) -> bool {
        &self.parent == parent
    }
    pub fn contained_in_sc(&self, parent: &PackageID) -> Option<()> {
        if self.contained_in(parent) {
            Some(())
        }
        else {
            None
        }
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

    assert_eq!( ResourceLocator::try_from("aa"), Ok(ResourceLocator::new_entry(WeakPackID::Usr, EntryType::Environment, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("").is_err() );
    assert_eq!( ResourceLocator::try_from("$aa"), Ok(ResourceLocator::new_entry(WeakPackID::Usr, EntryType::Variable, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("$").is_err() );
    assert_eq!( ResourceLocator::try_from("%aa()"), Ok(ResourceLocator::new_func(WeakPackID::Usr, "aa".to_string(), vec![])) ) ;
    assert!( ResourceLocator::try_from("%aa").is_err() );
    assert!( ResourceLocator::try_from("%").is_err() );

    assert_eq!( ResourceLocator::try_from("usr::aa"), Ok(ResourceLocator::new_entry(WeakPackID::Usr, EntryType::Environment, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("usr::").is_err() );
    assert_eq!( ResourceLocator::try_from("usr::$aa"), Ok(ResourceLocator::new_entry(WeakPackID::Usr, EntryType::Variable, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("usr::$").is_err() );
    assert_eq!( ResourceLocator::try_from("usr::%aa()"), Ok(ResourceLocator::new_func(WeakPackID::Usr, "aa".to_string(), vec![])) ) ;
    assert!( ResourceLocator::try_from("usr::%aa").is_err() );
    assert!( ResourceLocator::try_from("usr::%").is_err() );

    assert_eq!( ResourceLocator::try_from("std::aa"), Ok(ResourceLocator::new_entry(WeakPackID::Std, EntryType::Environment, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("std::").is_err() );
    assert_eq!( ResourceLocator::try_from("std::$aa"), Ok(ResourceLocator::new_entry(WeakPackID::Std, EntryType::Variable, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("std::$").is_err() );
    assert_eq!( ResourceLocator::try_from("std::%aa()"), Ok(ResourceLocator::new_func(WeakPackID::Std, "aa".to_string(), vec![])) ) ;
    assert!( ResourceLocator::try_from("std::%aa").is_err() );
    assert!( ResourceLocator::try_from("std::%").is_err() );

    assert_eq!( ResourceLocator::try_from("*::aa"), Ok(ResourceLocator::new_entry(WeakPackID::Any, EntryType::Environment, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("*::").is_err() );
    assert_eq!( ResourceLocator::try_from("*::$aa"), Ok(ResourceLocator::new_entry(WeakPackID::Any, EntryType::Variable, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("*::$").is_err() );
    assert_eq!( ResourceLocator::try_from("*::%aa()"), Ok(ResourceLocator::new_func(WeakPackID::Any, "aa".to_string(), vec![])) ) ;
    assert!( ResourceLocator::try_from("*::%aa").is_err() );
    assert!( ResourceLocator::try_from("*::%").is_err() );

    assert_eq!( ResourceLocator::try_from("foo::aa"), Ok(ResourceLocator::new_entry(WeakPackID::Spec("foo".to_string()), EntryType::Environment, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("foo::").is_err() );
    assert_eq!( ResourceLocator::try_from("foo::$aa"), Ok(ResourceLocator::new_entry(WeakPackID::Spec("foo".to_string()), EntryType::Variable, "aa".to_string()))) ;
    assert!( ResourceLocator::try_from("foo::$").is_err() );
    assert_eq!( ResourceLocator::try_from("foo::%aa()"), Ok(ResourceLocator::new_func(WeakPackID::Spec("foo".to_string()), "aa".to_string(), vec![])) ) ;
    assert!( ResourceLocator::try_from("foo::%aa").is_err() );
    assert!( ResourceLocator::try_from("foo::%").is_err() );

    assert!( ResourceLocator::try_from("::").is_err() );
}