use crate::core::errors::FormattingError;
use crate::core::is_string_whitespace;
use super::entry::EntryType;

use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

use serde::{Serialize, Deserialize};

/*

*/

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct NumericalPackID {
    id: u32
}
impl PartialOrd for NumericalPackID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for NumericalPackID {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct NumericalResourceID {
    package: NumericalPackID,
    resx: u32
}
impl PartialOrd for NumericalResourceID {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for NumericalResourceID {
    fn cmp(&self, other: &Self) -> Ordering {
        let parent = self.package.cmp(&other.package);
        if parent == Ordering::Equal {
            self.resx.cmp(&other.resx)
        }
        else {
            parent
        }
    }
}
impl NumericalResourceID {
    pub fn new(package: NumericalPackID, resx: u32) -> Self {
        Self {
            package,
            resx
        }
    }

    pub fn contained_in(&self, parent: &NumericalPackID) -> bool {
        &self.package == parent
    }
    pub fn contained_in_sc(&self, parent: &NumericalPackID) -> Option<()> {
        if self.contained_in(parent) {
            Some(())
        }
        else {
            None
        }
    }
}

#[derive(Eq, Clone)]
pub enum PackageID {
    Any,
    Usr,
    Std,
    Weak(String),
    Strong(String, NumericalPackID),
    Num(NumericalPackID)
}
impl Display for PackageID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "*"),
            Self::Usr => write!(f, "usr"),
            Self::Std => write!(f, "std"),
            Self::Strong(s, _) | Self::Weak(s) => write!(f, "{}", s),
            Self::Num(a) => write!(f, "(id: {})", a)
        }
    }
}
impl Debug for PackageID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Display).fmt(f)
    }
}
impl PartialEq for PackageID {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Any, Self::Any) => true,
            (Self::Usr, Self::Usr) => true,
            (Self::Std, Self::Std) => true,
            (Self::Weak(a), Self::Weak(b)) => a == b,
            (Self::Strong(a, _), Self::Strong(b, _)) => a == b,
            (Self::Num(a), Self::Num(b)) => a == b,
            (Self::Weak(a), Self::Strong(b, _)) | (Self::Strong(b, _), Self::Weak(a)) => a == b,
            (Self::Num(a), Self::Strong(_, b)) | (Self::Strong(b, _), Self::Num(a)) => a == b,
            _ => false
        }
    }
}
impl PartialEq<NumericalPackID> for PackageID {
    fn eq(&self, other: &NumericalPackID) -> bool {
        match self {
            Self::Any => other.id == 0,
            Self::Std => other.id == 1,
            Self::Usr => other.id == 2,
            Self::Strong(_, a) | Self::Num(a) => a == other,
            Self::Weak(_) => false
        }
    }
}
impl PackageID {

    pub fn is_any(&self) -> bool { matches!(self, Self::Any) }
    pub fn is_std(&self) -> bool { matches!(self, Self::Std) }
    pub fn is_usr(&self) -> bool { matches!(self, Self::Usr) }

    pub fn name(&self) -> &str {
        match self {
            Self::Any => "any",
            Self::Std => "std",
            Self::Usr => "usr",
            Self::Weak(w) | Self::Strong(w, _) => w.as_str(),
            Self::Num(id) => "(id)"
        }
    }
    pub fn id(&self) -> NumericalPackID {
        match self {
            Self::Any => NumericalPackID::any_id(),
            Self::Std => NumericalPackID::std_id(),
            Self::Usr => NumericalPackID::usr_id(),
            Self::Weak(_) => NumericalPackID::any_id(),
            Self::Strong(_, id) | Self::Num(id) => *id
        }
    }
}

#[derive(Clone, Eq)]
pub enum ResourceID {
    Numeric(u32),
    Strong(String, u32),
    Weak(String)
}
impl Display for ResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Strong(a, _) | Self::Weak(a) => write!(f, "{a}"),
            Self::Numeric(a) => write!(f, "(id:{a})")
        }
    }
}
impl Debug for ResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Numeric(a) => write!(f, "resx-id: {a}"),
            Self::Strong(a, b) => write!(f, "resx: '{a}' id: {b}"),
            Self::Weak(a) => write!(f, "weak-id: '{a}'")
        }
    }
}
impl PartialEq for ResourceID {
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
impl PartialEq<u32> for ResourceID {
    fn eq(&self, other: &u32) -> bool {
        match self {
            Self::Numeric(a) | Self::Strong(_, a) => a == other,
            Self::Weak(_) => false
        }
    }
}
impl ResourceID {
    pub fn id(&self) -> Option<u32> {
        match self {
            Self::Numeric(a) | Self::Strong(_, a) => Some(*a),
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
    Function,
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
            Self::Function => write!(f, "%"),
            Self::Entry(e) => (e as &dyn Display).fmt(f)
        }
    }
}
impl Debug for ResourceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => write!(f, "%"),
            Self::Entry(e) => (e as &dyn Debug).fmt(f)
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct ResourceLocator {
    parent: PackageID,
    resource: ResourceID,
    kind: ResourceKind
}
impl Debug for ResourceLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}::{:?} kind: {:?}", &self.parent, &self.resource, &self.kind)
    }
}
impl Display for ResourceLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", &self.parent, &self.resource)
    }
}
impl PartialEq<NumericalResourceID> for ResourceLocator {
    fn eq(&self, other: &NumericalResourceID) -> bool {
        self.parent == other.package && self.resource == other.resx
    }
}
impl ResourceLocator {
    pub fn new(parent: PackageID, resource: ResourceID, kind: ResourceKind) -> Self {
        Self {
            parent,
            resource,
            kind
        }
    }
    pub fn new_entry(parent: PackageID, resource: ResourceID, kind: EntryType) -> Self {
        Self {
            parent,
            resource,
            kind: kind.into()
        }
    }
    pub fn new_func(parent: PackageID, resource: ResourceID) -> Self {
        Self {
            parent,
            resource,
            kind: ResourceKind::Function
        }
    }

    pub fn parent(&self) -> &PackageID {
        &self.parent
    }
    pub fn resource(&self) -> &ResourceID {
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

pub struct ParsedResourceLocator {
    loc: ResourceLocator,
    residual: Option<String>
}
impl Debug for ParsedResourceLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} residual: {}", &self.loc, &self.residual)
    }
}
impl Display for ParsedResourceLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.loc as &dyn Display).fmt(f)
    }
}
impl TryFrom<String> for ParsedResourceLocator {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}
impl TryFrom<&str> for ParsedResourceLocator {
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

        let parent: PackageID;
        let raw_child: &str;

        match splits.len() {
            1 => {
                parent = PackageID::Usr;
                raw_child = splits[0];
            },
            2 => {
                match splits[0] {
                    "usr" => parent = PackageID::Usr,
                    "std" => parent = PackageID::Std,
                    "*" => parent = PackageID::Any,
                    a if !a.is_empty() && !is_string_whitespace(a) => parent = PackageID::Weak(a.trim().to_string()),
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
                    loc: ResourceLocator::new(
                        parent.into(),
                        ResourceID::Weak(c.trim().to_string()),
                        EntryType::Variable.into()
                    ),
                    residual: None
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

                Ok(
                    Self {
                        loc: ResourceLocator::new(
                            parent.into(),
                            ResourceID::Weak(name),
                            ResourceKind::Function
                        ),
                        residual: if args.is_empty() || is_string_whitespace(args) { None } else { Some(args.to_string()) },
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
                    loc: ResourceLocator::new(
                        parent.into(),
                        ResourceID::Weak(raw_child.to_string()),
                        EntryType::Environment.into()
                    ),
                    residual: None
                }
            )
        }
    }
}
impl ParsedResourceLocator {
    pub fn get_residual(&self) -> Option<&str> {
        self.residual.as_deref()
    }
    pub fn get_loc(&self) -> &ResourceLocator {
        &self.loc
    }

    pub fn is_entry(&self) -> bool {
        matches!(self.loc.kind, ResourceKind::Entry(_))
    }
    pub fn is_function(&self) -> bool {
        matches!(self.loc.kind, ResourceKind::Function)
    }
}
impl From<ParsedResourceLocator> for (ResourceLocator, Option<String>) {
    fn from(value: ParsedResourceLocator) -> Self {
        (value.loc, value.residual)
    }
}
impl From<ParsedResourceLocator> for ResourceLocator {
    fn from(value: ParsedResourceLocator) -> Self {
        value.loc
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

    assert_eq!( ParsedResourceLocator::try_from("aa").into(), Ok(ResourceLocator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Environment.into())) );
    assert!( ParsedResourceLocator::try_from("").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("$aa").into(), Ok(ResourceLocator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Variable.into() )) );
    assert!( ParsedResourceLocator::try_from("$").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("%aa()").into(), Ok(ResourceLocator::new_func(PackageID::Usr, ResourceID::Weak("aa".to_string()) )) ) ;
    assert!( ParsedResourceLocator::try_from("%aa").is_err() );
    assert!( ParsedResourceLocator::try_from("%").is_err() );

    assert_eq!( ParsedResourceLocator::try_from("usr::aa").into(), Ok(ResourceLocator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Environment.into())) );
    assert!( ParsedResourceLocator::try_from("usr::").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("usr::$aa").into(), Ok(ResourceLocator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Variable.into() )) );
    assert!( ParsedResourceLocator::try_from("usr::$").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("usr::%aa()").into(), Ok(ResourceLocator::new_func(PackageID::Usr, ResourceID::Weak("aa".to_string()) )) ) ;
    assert!( ParsedResourceLocator::try_from("usr::%aa").is_err() );
    assert!( ParsedResourceLocator::try_from("usr::%").is_err() );

    assert_eq!( ParsedResourceLocator::try_from("std::aa").into(), Ok(ResourceLocator::new_entry(PackageID::Std, ResourceID::Weak("aa".into()), EntryType::Environment.into())) );
    assert!( ParsedResourceLocator::try_from("std::").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("std::$aa").into(), Ok(ResourceLocator::new_entry(PackageID::Std, ResourceID::Weak("aa".into()), EntryType::Variable.into() )) );
    assert!( ParsedResourceLocator::try_from("std::$").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("std::%aa()").into(), Ok(ResourceLocator::new_func(PackageID::Std, ResourceID::Weak("aa".to_string()) )) ) ;
    assert!( ParsedResourceLocator::try_from("std::%aa").is_err() );
    assert!( ParsedResourceLocator::try_from("std::%").is_err() );

    assert_eq!( ParsedResourceLocator::try_from("*::aa").into(), Ok(ResourceLocator::new_entry(PackageID::Any, ResourceID::Weak("aa".into()), EntryType::Environment.into())) );
    assert!( ParsedResourceLocator::try_from("*::").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("*::$aa").into(), Ok(ResourceLocator::new_entry(PackageID::Any, ResourceID::Weak("aa".into()), EntryType::Variable.into() )) );
    assert!( ParsedResourceLocator::try_from("**::$").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("*::%aa()").into(), Ok(ResourceLocator::new_func(PackageID::Any, ResourceID::Weak("aa".to_string()) )) ) ;
    assert!( ParsedResourceLocator::try_from("*::%aa").is_err() );
    assert!( ParsedResourceLocator::try_from("*::%").is_err() );

    assert_eq!( ParsedResourceLocator::try_from("foo::aa").into(), Ok(ResourceLocator::new_entry(PackageID::Weak("foo".into()), ResourceID::Weak("aa".into()), EntryType::Environment.into())) );
    assert!( ParsedResourceLocator::try_from("foo::").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("foo::$aa").into(), Ok(ResourceLocator::new_entry(PackageID::Weak("foo".into()), ResourceID::Weak("aa".into()), EntryType::Variable.into() )) );
    assert!( ParsedResourceLocator::try_from("foo::$").is_err() );
    assert_eq!( ParsedResourceLocator::try_from("foo::%aa()").into(), Ok(ResourceLocator::new_func(PackageID::Weak("foo".into()), ResourceID::Weak("aa".to_string()) )) ) ;
    assert!( ParsedResourceLocator::try_from("foo::%aa").is_err() );
    assert!( ParsedResourceLocator::try_from("foo::%").is_err() );

    assert!( ParsedResourceLocator::try_from("::").is_err() );
}