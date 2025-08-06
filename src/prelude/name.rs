use serde::{Deserialize, Serialize};

use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use std::fmt::Display;

use exdisj::error::NamingError;

#[derive(PartialEq, Eq, Clone, Debug, Serialize, Deserialize)]
pub struct Name(String);
impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.0 as &dyn Display).fmt(f)
    }
}
impl TryFrom<&str> for Name {
    type Error = NamingError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        NameRef::try_from(value).map(|x| x.to_name())
    }
}
impl TryFrom<String> for Name {
    type Error = NamingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}
impl Name {
    pub fn this_pack_name() -> Self {
        NameRef::this_pack_name().to_name()
    }
    pub fn std_name() -> Self {
        NameRef::std_name().to_name()
    }
    pub fn usr_name() -> Self {
        NameRef::usr_name().to_name()
    }

    pub fn as_name_ref<'a>(&'a self) -> NameRef<'a> {
        NameRef(&self.0)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct NameRef<'a>(
    #[serde(borrow)]
    &'a str
);
impl Display for NameRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (&self.0 as &dyn Display).fmt(f)
    }
}
impl<'a> TryFrom<&'a str> for NameRef<'a> {
    type Error = NamingError;
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Self::is_name_valid(value)?;

        Ok( Self( value.trim() ) )
    }
}
impl NameRef<'static> {
    const SCOPE_PACK_NAME: &'static str = "_";
    const STD_PACK_NAME: &'static str = "std";
    const USR_PACK_NAME: &'static str = "usr";

    pub const fn this_pack_name() -> Self {
        Self ( Self::SCOPE_PACK_NAME )
    }
    pub const fn std_name() -> Self {
        Self ( Self::STD_PACK_NAME )
    }
    pub const fn usr_name() -> Self {
        Self ( Self::USR_PACK_NAME )
    }
}
impl NameRef<'_> {
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

        if name == "_" { //this is the this name
            return Ok(());
        }
        
        if name.trim().is_empty() {
            return Err(NamingError::Empty);
        }

        if let Some(c) = name.chars().next() { //The first character cannot be a number
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

    pub fn to_name(self) -> Name {
        Name(self.0.to_string())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum PathValidationError {
    NoFeasibleName,
    Name(NamingError)
}
impl std::error::Error for PathValidationError { }
impl Display for PathValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoFeasibleName => write!(f, "the name could not be extracted from the path"),
            Self::Name(n) => (n as &dyn Display).fmt(f)
        }
    }
}
impl From<NamingError> for PathValidationError {
    fn from(value: NamingError) -> Self {
        Self::Name(value)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct VerifiedPathRef<'a> {
    name: NameRef<'a>,
    path: &'a Path
}
impl<'a> TryFrom<&'a Path> for VerifiedPathRef<'a> {
    type Error = PathValidationError;
    fn try_from(value: &'a Path) -> Result<Self, Self::Error> {
        let target_name: Option<&OsStr> = if value.is_file() {
            value.file_stem()
        }
        else {
            value.file_name()
        };

        let file_name_osstr = match target_name {
            Some(v) => v,
            None => return Err(PathValidationError::NoFeasibleName)
        };

        let file_name = match file_name_osstr.to_str() {
            Some(v) => v,
            None => return Err(PathValidationError::NoFeasibleName)
        };

        let name: NameRef<'a> = file_name.try_into().map_err(PathValidationError::from)?;

        Ok(
            Self {
                name, path: value
            }
        )
    }
}
impl<'a> VerifiedPathRef<'a> {
    pub fn new(name: NameRef<'a>, path: &'a Path) -> Self {
        Self {
            name,
            path
        }
    }

    pub fn name(&self) -> NameRef<'a> {
        self.name
    }
    pub fn path(&self) -> &'a Path {
        self.path
    }

    pub fn to_verified_path(self) -> VerifiedPath {
        VerifiedPath {
            name: self.name.to_name(),
            path: self.path.to_path_buf()
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct VerifiedPath {
    name: Name,
    path: PathBuf
}
impl TryFrom<&Path> for VerifiedPath {
    type Error = PathValidationError;
    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        VerifiedPathRef::try_from(value).map(|x| x.to_verified_path())
    }
}
impl TryFrom<PathBuf> for VerifiedPath {
    type Error = PathValidationError;
    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        Self::try_from(value.as_path())
    }
}
impl VerifiedPath {
    pub fn new(name: Name, path: PathBuf) -> Self {
        Self {
            name,
            path
        }
    }

    pub fn name(&self) -> NameRef<'_> {
        self.name.as_name_ref()
    }
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn pop(self) -> (Name, PathBuf) {
        (self.name, self.path)
    }
    pub fn pop_ref(&self) -> (&Name, &Path) {
        (&self.name, &self.path)
    }
}


#[test]
fn name_validation() {
    use NamingError::*;
    let tests = vec![
        //Name, should it pass
        ("hello",                          None                   ),
        ("    hello",                      None                   ),
        ("\t     \t",                      Some(Empty)            ), //Just whitespace
        ("h ello",                         Some(Whitespace)       ), //Whitespace inside
        ("!_hello",                        Some(InvalidCharacters)), //Non-allowed character
        ("",                               Some(Empty)            ), //Empty 
        ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", None                   ),
        ("he_llo",                         None                   ), 
        ("he0xello",                       Some(Address)          ), //Hexadecimal pattern
        ("1hello",                         Some(InvalidCharacters)), //Starts with number
        ("Cześć",                          None                   )
    ];

    for (i, (test, expected)) in tests.into_iter().enumerate() {
        let result = NameRef::is_name_valid(&test);
        assert_eq!(result.err(), expected, "failure at test {i}: {test}");
    }
}
