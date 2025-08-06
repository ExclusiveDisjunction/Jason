use std::fmt::{Debug, Display};

use super::name::{Name, NameRef};

use exdisj::error::{FormattingError, NamingError};

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum PackageID {
    Scope,
    Usr,
    Std,
    Named(Name)
}
impl Display for PackageID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
impl PackageID {
    pub fn parse(input: &str) -> Result<Self, NamingError> {
        PackageIDRef::try_from(input).map(|x| x.to_package_id())
    }

    pub fn is_scope(&self) -> bool { 
        matches!(self, Self::Scope)
     }
    pub fn is_std(&self) -> bool { 
        matches!(self, Self::Std)
    }
    pub fn is_usr(&self) -> bool {
        matches!(self, Self::Usr)
    }

    pub fn name<'a>(&'a self) -> NameRef<'a> {
        match self {
            Self::Scope => NameRef::this_pack_name(),
            Self::Std => NameRef::std_name(),
            Self::Usr => NameRef::usr_name(),
            Self::Named(n) => n.as_name_ref()
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum PackageIDRef<'a> {
    Scope,
    Usr,
    Std,
    Named(NameRef<'a>)
}
impl Display for PackageIDRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
impl<'a> TryFrom<&'a str> for PackageIDRef<'a> {
    type Error = NamingError;
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let name = NameRef::try_from(s)?;

        Ok (
            if name == NameRef::this_pack_name() {
                Self::Scope
            }
            else if name == NameRef::std_name() {
                Self::Std
            }
            else if name == NameRef::usr_name() {
                Self::Usr
            }
            else {
                Self::Named(name)
            }
        )
    }
}
impl PackageIDRef<'_> {
    pub const fn is_scope(&self) -> bool { 
        matches!(self, Self::Scope)
     }
    pub const fn is_std(&self) -> bool { 
        matches!(self, Self::Std)
    }
    pub const fn is_usr(&self) -> bool {
        matches!(self, Self::Usr)
    }

    pub fn name(&self) -> NameRef<'_> {
        match self {
            Self::Scope => NameRef::this_pack_name(),
            Self::Std => NameRef::std_name(),
            Self::Usr => NameRef::usr_name(),
            Self::Named(n) => n.to_owned()
        }
    }

    pub fn to_package_id(self) -> PackageID {
        match self {
            Self::Scope => PackageID::Scope,
            Self::Std => PackageID::Std,
            Self::Usr => PackageID::Usr,
            Self::Named(n) => PackageID::Named(n.to_name())
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ResourceKind {
    Function,
    Variable,
    EnvVariable,
    FuncVariable,
}
impl ResourceKind {
    pub fn format(&self, name: &NameRef<'_>) -> String {
        match self {
            Self::Function => format!("${name}()"),
            Self::EnvVariable => format!("!{name}"),
            Self::Variable => format!("${name}"),
            Self::FuncVariable => format!("{name}")
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Identifier<'a> {
    parent: PackageIDRef<'a>,
    resource: NameRef<'a>,
    kind: ResourceKind
}
impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", &self.parent, self.kind.format(&self.resource))
    }
}
impl<'a> TryFrom<&'a str> for Identifier<'a> {
    type Error = LocatorParsingError;
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        /*
            The string must match the following grammar:

            @{ ([name] ~ "::")? ~ ("!" | "$") ~ name ~ "()"? }

            ![name]
            $[name]
            $[name]()
            [package]::![name] 
            [package]::$[name]
            [package]::$[name]()
         */

        //First we check for the existence of the package. Then we will worry about the other part.
        let splits: Vec<&str> = s.trim().split("::").collect();

        let parent: PackageIDRef<'_>;
        let raw_child: &str;

        match splits.len() {
            1 => {
                parent = PackageIDRef::Scope;
                raw_child = splits[0];
                
                if raw_child.len() == 1 { //Since function variables cannot be owned by a package (they live in scope), the parent must be scope. 
                    if let Some(f) = raw_child.chars().next() && f.is_alphabetic() {
                        return Ok (
                            Self {
                                parent: PackageIDRef::Scope,
                                resource: NameRef::try_from(raw_child).unwrap(), 
                                kind: ResourceKind::FuncVariable
                            }
                        )
                    }
                }
            },
            2 => {
                parent = splits[0].try_into().map_err(LocatorParsingError::from)?;
                raw_child = splits[1];
            },
            _ => return Err(FormattingError::new(&s, "there can only be one or two values before and after the '::'").into())
        }

        let raw_child = raw_child.trim();
        if raw_child.is_empty() {
            return Err(NamingError::Empty.into())
        }

        let name: NameRef<'_>;
        let kind: ResourceKind;
        if let Some(c) = raw_child.strip_prefix("$") { //Either function or variable
            if let Some(f_name) = c.strip_suffix("()") {
                name = f_name.try_into().map_err(LocatorParsingError::from)?;
                kind = ResourceKind::Function;
            }
            else {
                name = c.try_into().map_err(LocatorParsingError::from)?;
                kind = ResourceKind::Variable;
            }
        }
        else if let Some(c) = raw_child.strip_prefix("!") { //Env Var 
            name = c.try_into().map_err(LocatorParsingError::from)?;
            kind = ResourceKind::EnvVariable;
        }
        else {
            return Err(NamingError::InvalidCharacters.into())
        }

        Ok (
            Self {
                parent,
                resource: name,
                kind
            }
        )
    }
}
impl<'a> Identifier<'a> {
    pub fn new(parent: PackageIDRef<'a>, resource: NameRef<'a>, kind: ResourceKind) -> Self {
        Self {
            parent,
            resource,
            kind
        }
    }

    pub fn parent(&self) -> PackageIDRef<'a> {
        self.parent
    }
    pub fn resource(&self) -> NameRef<'a> {
        self.resource
    }
    pub fn kind(&self) -> ResourceKind {
        self.kind
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn locator_parsing() {
        let name: NameRef<'_> = "aa".try_into().unwrap();
        let f_name: NameRef<'_> = "a".try_into().unwrap();
        let pack: NameRef<'_> = "pack".try_into().unwrap();

        assert_eq!( Identifier::try_from(""),      Err(LocatorParsingError::Name(NamingError::Empty))                            );
        assert_eq!( Identifier::try_from("!"),     Err(LocatorParsingError::Name(NamingError::Empty))                            );
        assert_eq!( Identifier::try_from("$"),     Err(LocatorParsingError::Name(NamingError::Empty))                            );
        assert_eq!( Identifier::try_from("$()"),   Err(LocatorParsingError::Name(NamingError::Empty))                            );
        assert_eq!( Identifier::try_from("a"),     Ok(Identifier::new(PackageIDRef::Scope, name,   ResourceKind::EnvVariable  )) );
        assert_eq!( Identifier::try_from("!aa"),   Ok(Identifier::new(PackageIDRef::Scope, f_name, ResourceKind::FuncVariable )) ); //Special case!
        assert_eq!( Identifier::try_from("$aa"),   Ok(Identifier::new(PackageIDRef::Scope, name,   ResourceKind::Variable     )) );  
        assert_eq!( Identifier::try_from("$aa()"), Ok(Identifier::new(PackageIDRef::Scope, name,   ResourceKind::Function     )) );

        assert_eq!( Identifier::try_from("_::"),      Err(LocatorParsingError::Name(NamingError::Empty))                        );
        assert_eq!( Identifier::try_from("_::!"),     Err(LocatorParsingError::Name(NamingError::Empty))                        );
        assert_eq!( Identifier::try_from("_::$"),     Err(LocatorParsingError::Name(NamingError::Empty))                        );
        assert_eq!( Identifier::try_from("_::$()"),   Err(LocatorParsingError::Name(NamingError::Empty))                        );
        assert_eq!( Identifier::try_from("_::!aa"),   Ok(Identifier::new(PackageIDRef::Scope, name, ResourceKind::EnvVariable)) );
        assert_eq!( Identifier::try_from("_::$aa"),   Ok(Identifier::new(PackageIDRef::Scope, name, ResourceKind::Variable   )) );
        assert_eq!( Identifier::try_from("_::$aa()"), Ok(Identifier::new(PackageIDRef::Scope, name, ResourceKind::Function   )) );

        assert_eq!( Identifier::try_from("usr::"),      Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("usr::!"),     Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("usr::$"),     Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("usr::$()"),   Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("usr::!aa"),   Ok(Identifier::new(PackageIDRef::Usr, name, ResourceKind::EnvVariable)) );
        assert_eq!( Identifier::try_from("usr::$aa"),   Ok(Identifier::new(PackageIDRef::Usr, name, ResourceKind::Variable   )) );
        assert_eq!( Identifier::try_from("usr::$aa()"), Ok(Identifier::new(PackageIDRef::Usr, name, ResourceKind::Function   )) );

        assert_eq!( Identifier::try_from("std::"),      Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("std::!"),     Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("std::$"),     Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("std::$()"),   Err(LocatorParsingError::Name(NamingError::Empty))                      );
        assert_eq!( Identifier::try_from("std::!aa"),   Ok(Identifier::new(PackageIDRef::Std, name, ResourceKind::EnvVariable)) );
        assert_eq!( Identifier::try_from("std::$aa"),   Ok(Identifier::new(PackageIDRef::Std, name, ResourceKind::Variable   )) );
        assert_eq!( Identifier::try_from("std::$aa()"), Ok(Identifier::new(PackageIDRef::Std, name, ResourceKind::Function   )) );

        assert_eq!( Identifier::try_from("pack::"),      Err(LocatorParsingError::Name(NamingError::Empty))                              );
        assert_eq!( Identifier::try_from("pack::!"),     Err(LocatorParsingError::Name(NamingError::Empty))                              );
        assert_eq!( Identifier::try_from("pack::$"),     Err(LocatorParsingError::Name(NamingError::Empty))                              );
        assert_eq!( Identifier::try_from("pack::$()"),   Err(LocatorParsingError::Name(NamingError::Empty))                              );
        assert_eq!( Identifier::try_from("pack::!aa"),   Ok(Identifier::new(PackageIDRef::Named(pack), name, ResourceKind::EnvVariable)) );
        assert_eq!( Identifier::try_from("pack::$aa"),   Ok(Identifier::new(PackageIDRef::Named(pack), name, ResourceKind::Variable   )) );
        assert_eq!( Identifier::try_from("pack::$aa()"), Ok(Identifier::new(PackageIDRef::Named(pack), name, ResourceKind::Function   )) );

        assert!( Identifier::try_from("usr::a").is_err() ); //Since function variables can only be accessed from scope, this is invalid.

        assert_eq!( Identifier::try_from("::"),        Err(LocatorParsingError::Name(NamingError::Empty)) );
        assert_eq!( Identifier::try_from("pack::"),    Err(LocatorParsingError::Name(NamingError::Empty)) );
        assert!( matches!( Identifier::try_from("&"),  Err(_) )                                           );
        assert!( matches!( Identifier::try_from("()"), Err(_) )                                           );
    }
}
