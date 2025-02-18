use crate::core::errors::{FormattingError, NamingError, Error as CoreError};
use crate::core::is_string_whitespace;
use super::entry::EntryType;

use std::fmt::{Display, Debug};
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

const MAX_IO_NAME: usize = 25;

/// Checks to see if the name provided meets the following criteria:
/// 1. No starting with numerical values
/// 2. No 0x or *b patterns (addresses)
/// 3. No symbols other than '_' or '-' (no format specifiers)
/// 4. Only latin letters.
/// 5. No whitespace inbetween 
///
/// If the string is valid, it will convert it into a String, such that it is trimmed and does not contain invalid characters.
pub fn is_name_valid(name: &str) -> Result<(), NamingError> {
    let name = name.trim();

    if name.len() >= MAX_IO_NAME {
        return Err(NamingError::TooLong);
    }
    
    if name.is_empty() || is_string_whitespace(name) {
        return Err(NamingError::Empty);
    }

    if let Some(c) = name.chars().next() {
        if c.is_numeric() {
            return Err(NamingError::InvalidCharacters);
        }
    }

    if name.contains("0x") {
        return Err(NamingError::Address)
    }

    //From this point, no addresses, empty strings, or starting with numeric strings have been found. Now we have to just ensure that everything is alphabetic, and only the '_' and '-' sumbols are allowed. No whitespace.
    for c in name.chars() {
        match c {
            '_' | '-' => continue,
            x if x.is_alphabetic() || x.is_numeric() => continue,
            x if x.is_whitespace() => return Err(NamingError::Whitespace),
            _ => return Err(NamingError::InvalidCharacters)
        }
    }

    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct NumericalPackID {
    id: u32
}
impl Debug for NumericalPackID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}
impl Display for NumericalPackID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
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
impl Hash for NumericalPackID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
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

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct NumericalResourceID {
    package: NumericalPackID,
    resx: u32
}
impl Display for NumericalResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", &self.package, &self.resx)
    }
}
impl Debug for NumericalResourceID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}.{}", &self.package, &self.resx)
    }
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
impl Hash for NumericalResourceID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.package.hash(state);
        self.resx.hash(state);
    }
}
impl NumericalResourceID {
    pub fn new(package: NumericalPackID, resx: u32) -> Self {
        Self {
            package,
            resx
        }
    }

    pub fn package(&self) -> NumericalPackID { self.package }
    pub fn resx(&self) -> u32 { self.resx }

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

#[derive(Clone, Eq)]
pub enum PackageID {
    Any,
    Usr,
    Std,
    Weak(String),
    Strong(String, NumericalPackID),
    Num(NumericalPackID)
}
impl Default for PackageID {
    fn default() -> Self {
        Self::Any
    }
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
            (x, y) if (x.is_usr() && y.is_usr()) || (x.is_std() && y.is_std()) || x.is_any() || y.is_any() => true, //The usr and std cases are handled here, but if the IDs are provided numerically, this will allow that. 
            (Self::Weak(a), Self::Weak(b)) => a == b,
            (Self::Strong(a, _), Self::Strong(b, _)) => a == b,
            (Self::Num(a), Self::Num(b)) => a == b,
            (Self::Weak(a), Self::Strong(b, _)) | (Self::Strong(b, _), Self::Weak(a)) => a == b,
            (Self::Num(a), Self::Strong(_, b)) | (Self::Strong(_, b), Self::Num(a)) => a == b,
            _ => false
        }
    }
}
impl PartialEq<NumericalPackID> for PackageID {
    fn eq(&self, other: &NumericalPackID) -> bool {
        match self {
            Self::Any => true, //Any matches any package
            Self::Std => other.id == 1,
            Self::Usr => other.id == 2,
            Self::Strong(_, a) | Self::Num(a) => a == other,
            Self::Weak(_) => false
        }
    }
}
impl PackageID {

    pub fn is_any(&self) -> bool { 
        match self {
            Self::Any => true,
            Self::Strong(a, x) => a == "*" || x.is_any(),
            Self::Weak(a) => a == "*",
            Self::Num(x) => x.is_any(),
            _ => false
        }
     }
    pub fn is_std(&self) -> bool { 
        match self {
            Self::Std => true,
            Self::Strong(a, x) => a == "std" || x.is_std(),
            Self::Weak(a) => a == "std",
            Self::Num(x) => x.is_std(),
            _ => false
        }
    }
    pub fn is_usr(&self) -> bool {
        match self {
            Self::Usr => true,
            Self::Strong(a, x) => a == "usr" || x.is_usr(),
            Self::Weak(a) => a == "usr",
            Self::Num(x) => x.is_usr(),
            _ => false
        }
    }
    pub fn is_weak(&self) -> bool { matches!(self, Self::Weak(_) ) }

    /// Determines if the name stored internally is valid. If it returns None, then the variant has no name property.
    /// ```
    ///     assert_eq!(PackageID::Std.is_name_valid(), Some(true));
    ///     assert_eq!(PackageID::Usr.is_name_valid(), Some(true));
    ///     assert_eq!(PackageID::Num(NumericalPackID::new_std()), None); //No name is provided
    ///     assert_eq!(PackageID::Weak("hello".to_string()).is_name_valid(), Some(true)); //The name is valid
    ///     assert_eq!(PackageID::Weak("0x444".to_string()).is_name_valid(), Some(false)); //The name is flagged as an address, or the first character is interpreted as a number and rejected.
    /// ```
    pub fn is_name_valid(&self) -> Option<bool> {
        match self {
            Self::Any | Self::Usr | Self::Std => Some(true),
            Self::Num(_) => None,
            Self::Strong(s, _) | Self::Weak(s) => Some(is_name_valid(s).is_ok())
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Any => Some("any"),
            Self::Std => Some("std"),
            Self::Usr => Some("usr"),
            Self::Weak(w) | Self::Strong(w, _) => Some(w.as_str()),
            Self::Num(_) => None
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

    pub fn is_weak(&self) -> bool { matches!(self, Self::Weak(_)) }

    pub fn is_name_valid(&self) -> Option<bool> {
        match self {
            Self::Numeric(_) => None,
            Self::Strong(s, _) | Self::Weak(s) => Some(is_name_valid(s).is_ok())
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
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

#[derive(PartialEq, Eq, Clone)]
pub struct Locator {
    parent: PackageID,
    resource: ResourceID,
    kind: ResourceKind
}
impl Debug for Locator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}::{:?} kind: {:?}", &self.parent, &self.resource, &self.kind)
    }
}
impl Display for Locator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", &self.parent, &self.resource)
    }
}
impl PartialEq<NumericalResourceID> for Locator {
    fn eq(&self, other: &NumericalResourceID) -> bool {
        self.parent == other.package && self.resource == other.resx
    }
}
impl Locator {
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
    pub fn new_weak(parent: String, resource: String, kind: ResourceKind) -> Self {
        Self{
            parent: PackageID::Weak(parent),
            resource: ResourceID::Weak(resource),
            kind
        }
    }

    pub fn parent(&self) -> &PackageID {
        &self.parent
    }
    pub fn resource(&self) -> &ResourceID {
        &self.resource
    }
    pub fn kind(&self) -> ResourceKind {
        self.kind
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

#[derive(PartialEq, Eq, Clone)]
pub enum LocatorParsingError {
    Name(NamingError),
    Format(FormattingError)
}
impl Debug for LocatorParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Debug =  match self {
            Self::Format(f) => f,
            Self::Name(n) => n
        };
        x.fmt(f)
    }
}
impl From<LocatorParsingError> for CoreError {
    fn from(value: LocatorParsingError) -> Self {
        match value {
            LocatorParsingError::Name(n) => Self::Name(n),
            LocatorParsingError::Format(f) => Self::Format(f)
        }
    }
}
impl From<NamingError> for LocatorParsingError {
    fn from(value: NamingError) -> Self {
        Self::Name(value)
    }
}
impl From<FormattingError> for LocatorParsingError {
    fn from(value: FormattingError) -> Self {
        Self::Format(value)
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct ParsedLocator {
    loc: Locator,
    residual: Option<String>
}
impl Debug for ParsedLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = self.residual.as_deref() {
            write!(f, "{:?} residual: {}", &self.loc, s)
        }
        else {
            write!(f, "{:?}", &self.loc)
        }

    }
}
impl Display for ParsedLocator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.loc as &dyn Display).fmt(f)
    }
}
impl TryFrom<String> for ParsedLocator {
    type Error = LocatorParsingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}
impl TryFrom<&str> for ParsedLocator {
    type Error = LocatorParsingError;
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
                    a if a.is_empty() || is_string_whitespace(a) => return Err(NamingError::Empty.into()),
                    a => {
                        if let Err(e) = is_name_valid(a) {
                            return Err(e.into())
                        }

                        parent = PackageID::Weak(a.trim().to_lowercase())
                    }
                }
                raw_child = splits[1];
            },
            _ => return Err(FormattingError::new(&value, "there can only be one or two values before and after the '::'").into())
        }

        let raw_child = raw_child.trim();
        if raw_child.is_empty() || is_string_whitespace(raw_child) {
            return Err(NamingError::Empty.into())
        }

        if let Some(c) = raw_child.strip_prefix("$") {
            if let Err(e) = is_name_valid(c) {
                return Err(e.into())
            }

            Ok( 
                Self {
                    loc: Locator::new(
                        parent,
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
                    return Err(FormattingError::new(&value, "functions must have a name followed by a '(', and then the arguments (if specified)").into());
                }

                let name = splits[0].trim().to_string();
                let args = splits[1].trim();

                if let Err(e) = is_name_valid(&name) {
                    return Err(e.into())
                }

                Ok(
                    Self {
                        loc: Locator::new(
                            parent,
                            ResourceID::Weak(name),
                            ResourceKind::Function
                        ),
                        residual: if args.is_empty() || is_string_whitespace(args) { None } else { Some(args.to_string()) },
                    }
                )
            }
            else {
                return Err(FormattingError::new(&value, "the function signature does not end with a ')'").into());
            }
        }
        else {
            if let Err(e) = is_name_valid(raw_child) {
                return Err(e.into())
            }

            Ok(
                Self {
                    loc: Locator::new(
                        parent,
                        ResourceID::Weak(raw_child.to_string()),
                        EntryType::Environment.into()
                    ),
                    residual: None
                }
            )
        }
    }
}
impl ParsedLocator {
    pub fn get_residual(&self) -> Option<&str> {
        self.residual.as_deref()
    }
    pub fn get_loc(&self) -> &Locator {
        &self.loc
    }

    pub fn is_entry(&self) -> bool {
        matches!(self.loc.kind, ResourceKind::Entry(_))
    }
    pub fn is_function(&self) -> bool {
        matches!(self.loc.kind, ResourceKind::Function)
    }
}
impl From<ParsedLocator> for (Locator, Option<String>) {
    fn from(value: ParsedLocator) -> Self {
        (value.loc, value.residual)
    }
}
impl From<ParsedLocator> for Locator {
    fn from(value: ParsedLocator) -> Self {
        value.loc
    }
}
impl From<Locator> for ParsedLocator {
    fn from(value: Locator) -> Self {
        Self {
            loc: value,
            residual: None
        }
    }
}
impl ParsedLocator {
    pub fn new(loc: Locator, residual: Option<String>) -> Self {
        Self {
            loc,
            residual
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn name_validation() {
        let tests = vec![
            //Name, should it pass
            ("hello".to_string(), true),
            ("    hello".to_string(), true),
            ("\t     \t".to_string(), false), //Just whitespace
            ("h ello".to_string(), false), //Whitespace inside
            ("!_hello".to_string(), false), //Non-allowed character
            (String::new(), false), //Empty 
            ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".to_string(), false), //Too long
            ("he_llo".to_string(), true), 
            ("he0xello".to_string(), false), //Hexadecimal pattern
            ("1hello".to_string(), false), //Starts with number
            ("Cześć".to_string(), true)
        ];

        for (i, (test, expected)) in tests.into_iter().enumerate() {
            let result = is_name_valid(&test);
            assert_eq!(result.is_ok(), expected, "failure at test {i}: '{test}'");
        }
    }

    #[test]
    fn locator_parsing() {
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

    assert_eq!( ParsedLocator::try_from("aa"), Ok(ParsedLocator::from(Locator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()).into(), EntryType::Environment))) );
    assert!( ParsedLocator::try_from("").is_err() );
    assert_eq!( ParsedLocator::try_from("$aa"), Ok(Locator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("$").is_err() );
    assert_eq!( ParsedLocator::try_from("%aa()"), Ok(Locator::new_func(PackageID::Usr, ResourceID::Weak("aa".to_string()) ).into()) ) ;
    assert_eq!( 
        ParsedLocator::try_from("%aa(x, y)"), 
        Ok(
            ParsedLocator::new(
                Locator::new_func(PackageID::Usr, ResourceID::Weak("aa".to_string())),
                Some("x, y".to_string())
            )
        )
    );
    assert!( ParsedLocator::try_from("%aa").is_err() );
    assert!( ParsedLocator::try_from("%").is_err() );

    assert_eq!( ParsedLocator::try_from("usr::aa"), Ok(Locator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Environment.into()).into() ) );
    assert!( ParsedLocator::try_from("usr::").is_err() );
    assert_eq!( ParsedLocator::try_from("usr::$aa"), Ok(Locator::new_entry(PackageID::Usr, ResourceID::Weak("aa".into()), EntryType::Variable.into() ).into() ) );
    assert!( ParsedLocator::try_from("usr::$").is_err() );
    assert_eq!( ParsedLocator::try_from("usr::%aa()"), Ok(Locator::new_func(PackageID::Usr, ResourceID::Weak("aa".to_string()) ).into()) ) ;
    assert!( ParsedLocator::try_from("usr::%aa").is_err() );
    assert!( ParsedLocator::try_from("usr::%").is_err() );

    assert_eq!( ParsedLocator::try_from("std::aa"), Ok(Locator::new_entry(PackageID::Std, ResourceID::Weak("aa".into()), EntryType::Environment.into()).into()) );
    assert!( ParsedLocator::try_from("std::").is_err() );
    assert_eq!( ParsedLocator::try_from("std::$aa"), Ok(Locator::new_entry(PackageID::Std, ResourceID::Weak("aa".into()), EntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("std::$").is_err() );
    assert_eq!( ParsedLocator::try_from("std::%aa()"), Ok(Locator::new_func(PackageID::Std, ResourceID::Weak("aa".to_string()) ).into()) ) ;
    assert!( ParsedLocator::try_from("std::%aa").is_err() );
    assert!( ParsedLocator::try_from("std::%").is_err() );

    assert_eq!( ParsedLocator::try_from("*::aa"), Ok(Locator::new_entry(PackageID::Any, ResourceID::Weak("aa".into()), EntryType::Environment.into()).into()) );
    assert!( ParsedLocator::try_from("*::").is_err() );
    assert_eq!( ParsedLocator::try_from("*::$aa"), Ok(Locator::new_entry(PackageID::Any, ResourceID::Weak("aa".into()), EntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("**::$").is_err() );
    assert_eq!( ParsedLocator::try_from("*::%aa()"), Ok(Locator::new_func(PackageID::Any, ResourceID::Weak("aa".to_string()) ).into()) ) ;
    assert!( ParsedLocator::try_from("*::%aa").is_err() );
    assert!( ParsedLocator::try_from("*::%").is_err() );

    assert_eq!( ParsedLocator::try_from("foo::aa"), Ok(Locator::new_entry(PackageID::Weak("foo".into()), ResourceID::Weak("aa".into()), EntryType::Environment.into()).into()) );
    assert!( ParsedLocator::try_from("foo::").is_err() );
    assert_eq!( ParsedLocator::try_from("foo::$aa"), Ok(Locator::new_entry(PackageID::Weak("foo".into()), ResourceID::Weak("aa".into()), EntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("foo::$").is_err() );
    assert_eq!( ParsedLocator::try_from("foo::%aa()"), Ok(Locator::new_func(PackageID::Weak("foo".into()), ResourceID::Weak("aa".to_string()) ).into()) ) ;
    assert!( ParsedLocator::try_from("foo::%aa").is_err() );
    assert!( ParsedLocator::try_from("foo::%").is_err() );

    assert!( ParsedLocator::try_from("::").is_err() );
    }

    #[test]
    fn numerical_id() {
        let a = NumericalPackID::std_id();
        let b = NumericalPackID::usr_id();

        let c = NumericalResourceID::new(a, 1);
        let d = c.clone();
        let e = NumericalResourceID::new(b, 1);

        assert!( a < b );
        assert_ne!( a, b );
        assert_eq!(c, d);
        assert!( c < e );
        assert!( !(e < d) );

        let f = NumericalPackID::new(1);
        assert!(f.is_std());
        assert_eq!(a, f)
    }

    #[test]
    fn package_id() {
        {
            let a = PackageID::Usr;
            let b = PackageID::Num(NumericalPackID::new(2));
            let c = PackageID::Strong("usr".to_string(), NumericalPackID::new(2));
            let d = PackageID::Weak("usr".to_string());

            assert_eq!(a, b);
            assert_eq!(b, c);
            assert_eq!(c, d);
            assert_eq!(a, d); //Since the weak is the 'std' string, it will detect that and mark it as the STD package.

            assert_eq!(a, PackageID::Any);
        }

        {
            let a = PackageID::Std;
            let b = PackageID::Num(NumericalPackID::new(1));
            let c = PackageID::Strong("std".to_string(), NumericalPackID::new(1));
            let d = PackageID::Weak("std".to_string());

            assert_eq!(a, b);
            assert_eq!(b, c);
            assert_eq!(c, d);
            assert_eq!(a, d); //Since the weak is the 'std' string, it will detect that and mark it as the STD package.

            assert_eq!(a, PackageID::Any);
        }

        {
            let a = PackageID::Weak("hello".to_string());
            let b = PackageID::Strong("hello".to_string(), NumericalPackID::new(4));
            let c = PackageID::Num(NumericalPackID::new(4));

            assert_eq!(a, b); //Weak to Strong
            assert_eq!(b, a); //Any order
            assert_eq!(b, c); //Strong to Numerical
            assert_eq!(c, b); //Any order
            assert_ne!(a, c); //Weak to Numerical, no connection
        }

        {
            let a = PackageID::Weak("hello".to_string());
            let b = PackageID::Weak("hello".to_string());
            assert_eq!(a, b); //Weak to weak
        }


        {
            let a = PackageID::Strong("hello".to_string(), NumericalPackID::any_id());
            assert_eq!(a, a.clone());
        }

        {
            let a = PackageID::Num(NumericalPackID::usr_id());
            assert_eq!(a, a.clone());
        }

    }

    #[test]
    fn resource_id() {
        let a = ResourceID::Numeric(4);
        let b = ResourceID::Strong("hello".to_string(), 4);
        let c = ResourceID::Weak("hello".to_string());
        let d = ResourceID::Numeric(5);
        let e = ResourceID::Strong("hello".to_string(), 5);
        
        assert!(a.name().is_none() && a.id().is_some());
        assert!(b.name().is_some() && b.id().is_some());
        assert!(c.name().is_some() && c.id().is_none());

        assert!(c.is_weak());
        assert!(!e.is_weak());
        assert!(!a.is_weak());

        assert_eq!(a, b); //By number
        assert_eq!(b, c); //By name
        assert_ne!(a, c); //No relation
 
        assert_eq!(d, e); //by number
        assert_eq!(c, e); //by name
        assert_ne!(a, e); //by number, not equal
        assert_ne!(b, e); //by name and number, not equal
    }
}
