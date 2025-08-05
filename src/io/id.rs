use std::fmt::{Display, Debug};

use super::name::{Name, NameRef};

use exdisj::error::NamingError;
use super::entry::VarEntryType;

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
        PackageIDRef::parse(input).map(|x| x.to_package_id())
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
impl<'a> PackageIDRef<'a> {
    pub fn parse(input: &'a str) -> Result<Self, NamingError> {
        let name = NameRef::validate(input)?;

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

    pub const fn is_scope(&self) -> bool { 
        matches!(self, Self::Scope)
     }
    pub const fn is_std(&self) -> bool { 
        matches!(self, Self::Std)
    }
    pub const fn is_usr(&self) -> bool {
        matches!(self, Self::Usr)
    }

    pub fn name(&self) -> NameRef<'a> {
        match self {
            Self::Scope => NameRef::this_pack_name(),
            Self::Std => NameRef::std_name(),
            Self::Usr => NameRef::usr_name(),
            Self::Named(n) => n.clone()
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ResourceKind {
    Function,
    Variable,
    EnvVariable
}
impl ResourceKind {
    pub fn format(&self, name: &NameRef<'_>) -> String {
        match self {
            Self::Function => format!("${name}()"),
            Self::EnvVariable => format!("!{name}"),
            Self::Variable => format!("${name}")
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Identifier<'a> {
    parent: Option<PackageIDRef<'a>>,
    resource: NameRef<'a>,
    kind: ResourceKind
}
impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(parent) = self.parent.as_ref() {
            write!(f, "{}::{}", parent, self.kind.format(&self.resource))
        }
        else {
            write!(f, "_::{}", self.kind.format(&self.resource))
        }
    }
}
impl<'a> Identifier<'a> {
    pub fn new(parent: Option<PackageIDRef<'a>>, resource: NameRef<'a>, kind: ResourceKind) -> Self {
        Self {
            parent,
            resource,
            kind
        }
    }

    pub fn parent(&self) -> Option<PackageIDRef<'a>> {
        self.parent
    }
    pub fn resource(&self) -> NameRef<'a> {
        self.resource
    }
    pub fn kind(&self) -> ResourceKind {
        self.kind
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
                        parent = PackageID::Weak(Name::validate(a.to_string()).map_err(LocatorParsingError::from)?);
                        
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
            Ok( 
                Self {
                    loc: Locator::new(
                        parent,
                        ResourceID::Weak(
                            Name::validate(c.to_string()).map_err(LocatorParsingError::from)?
                        ),
                        VarEntryType::Variable.into()
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

                let name = Name::validate(splits[0].to_string()).map_err(LocatorParsingError::from)?;
                let args = splits[1].trim();
                Ok(
                    Self {
                        loc: Locator::new(
                            parent,
                            ResourceID::Weak(name),
                            ResourceKind::Function
                        ),
                        residual: if args.is_empty() || is_string_whitespace(args) { 
                            None 
                        } 
                        else { 
                            Some(args.to_string()) 
                        },
                    }
                )
            }
            else {
                return Err(FormattingError::new(&value, "the function signature does not end with a ')'").into());
            }
        }
        else {
            let name = Name::validate(raw_child.to_string()).map_err(LocatorParsingError::from)?;

            Ok(
                Self {
                    loc: Locator::new(
                        parent,
                        ResourceID::Weak(name),
                        VarEntryType::Environment.into()
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
            let result = Name::is_name_valid(&test);
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

    let name = Name::validate("aa".to_string()).unwrap();

    assert_eq!( ParsedLocator::try_from("aa"), Ok(ParsedLocator::from(Locator::new_entry(PackageID::Usr, ResourceID::Weak(name.clone()).into(), VarEntryType::Environment))) );
    assert!( ParsedLocator::try_from("").is_err() );
    assert_eq!( ParsedLocator::try_from("$aa"), Ok(Locator::new_entry(PackageID::Usr, ResourceID::Weak(name.clone()), VarEntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("$").is_err() );
    assert_eq!( ParsedLocator::try_from("%aa()"), Ok(Locator::new_func(PackageID::Usr, ResourceID::Weak(name.clone()) ).into()) ) ;
    assert_eq!( 
        ParsedLocator::try_from("%aa(x, y)"), 
        Ok(
            ParsedLocator::new(
                Locator::new_func(PackageID::Usr, ResourceID::Weak(name.clone())),
                Some("x, y".to_string())
            )
        )
    );
    assert!( ParsedLocator::try_from("%aa").is_err() );
    assert!( ParsedLocator::try_from("%").is_err() );

    assert_eq!( ParsedLocator::try_from("usr::aa"), Ok(Locator::new_entry(PackageID::Usr, ResourceID::Weak(name.clone()), VarEntryType::Environment.into()).into() ) );
    assert!( ParsedLocator::try_from("usr::").is_err() );
    assert_eq!( ParsedLocator::try_from("usr::$aa"), Ok(Locator::new_entry(PackageID::Usr, ResourceID::Weak(name.clone()), VarEntryType::Variable.into() ).into() ) );
    assert!( ParsedLocator::try_from("usr::$").is_err() );
    assert_eq!( ParsedLocator::try_from("usr::%aa()"), Ok(Locator::new_func(PackageID::Usr, ResourceID::Weak(name.clone()) ).into()) ) ;
    assert!( ParsedLocator::try_from("usr::%aa").is_err() );
    assert!( ParsedLocator::try_from("usr::%").is_err() );

    assert_eq!( ParsedLocator::try_from("std::aa"), Ok(Locator::new_entry(PackageID::Std, ResourceID::Weak(name.clone()), VarEntryType::Environment.into()).into()) );
    assert!( ParsedLocator::try_from("std::").is_err() );
    assert_eq!( ParsedLocator::try_from("std::$aa"), Ok(Locator::new_entry(PackageID::Std, ResourceID::Weak(name.clone()), VarEntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("std::$").is_err() );
    assert_eq!( ParsedLocator::try_from("std::%aa()"), Ok(Locator::new_func(PackageID::Std, ResourceID::Weak(name.clone()) ).into()) ) ;
    assert!( ParsedLocator::try_from("std::%aa").is_err() );
    assert!( ParsedLocator::try_from("std::%").is_err() );

    assert_eq!( ParsedLocator::try_from("*::aa"), Ok(Locator::new_entry(PackageID::Any, ResourceID::Weak(name.clone()), VarEntryType::Environment.into()).into()) );
    assert!( ParsedLocator::try_from("*::").is_err() );
    assert_eq!( ParsedLocator::try_from("*::$aa"), Ok(Locator::new_entry(PackageID::Any, ResourceID::Weak(name.clone()), VarEntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("**::$").is_err() );
    assert_eq!( ParsedLocator::try_from("*::%aa()"), Ok(Locator::new_func(PackageID::Any, ResourceID::Weak(name.clone()) ).into()) ) ;
    assert!( ParsedLocator::try_from("*::%aa").is_err() );
    assert!( ParsedLocator::try_from("*::%").is_err() );

    let foo = Name::validate("foo".to_string()).unwrap();
    assert_eq!( ParsedLocator::try_from("foo::aa"), Ok(Locator::new_entry(PackageID::Weak(foo.clone()), ResourceID::Weak(name.clone()), VarEntryType::Environment.into()).into()) );
    assert!( ParsedLocator::try_from("foo::").is_err() );
    assert_eq!( ParsedLocator::try_from("foo::$aa"), Ok(Locator::new_entry(PackageID::Weak(foo.clone()), ResourceID::Weak(name.clone()), VarEntryType::Variable.into() ).into()) );
    assert!( ParsedLocator::try_from("foo::$").is_err() );
    assert_eq!( ParsedLocator::try_from("foo::%aa()"), Ok(Locator::new_func(PackageID::Weak(foo.clone()), ResourceID::Weak(name.clone()) ).into()) ) ;
    assert!( ParsedLocator::try_from("foo::%aa").is_err() );
    assert!( ParsedLocator::try_from("foo::%").is_err() );

    assert!( ParsedLocator::try_from("::").is_err() );
    }

    #[test]
    fn numerical_id() {
        let a = STD_ID;
        let b = USR_ID;

        let c = NumericalResourceID::new(a, 0);
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
            let c = PackageID::Strong(Name::usr_name(), NumericalPackID::new(2));
            let d = PackageID::Weak(Name::usr_name());

            assert_eq!(a, b);
            assert_eq!(b, c);
            assert_eq!(c, d);
            assert_eq!(a, d); //Since the weak is the 'std' string, it will detect that and mark it as the STD package.

            assert_eq!(a, PackageID::Any);
        }

        {
            let a = PackageID::Std;
            let b = PackageID::Num(NumericalPackID::new(1));
            let c = PackageID::Strong(Name::std_name(), NumericalPackID::new(1));
            let d = PackageID::Weak(Name::std_name());

            assert_eq!(a, b);
            assert_eq!(b, c);
            assert_eq!(c, d);
            assert_eq!(a, d); //Since the weak is the 'std' string, it will detect that and mark it as the STD package.

            assert_eq!(a, PackageID::Any);
        }

        {
            let name = Name::validate("hello".to_string()).unwrap();
            let a = PackageID::Weak(name.clone());
            let b = PackageID::Strong(name.clone(), NumericalPackID::new(4));
            let c = PackageID::Num(NumericalPackID::new(4));

            assert_eq!(a, b); //Weak to Strong
            assert_eq!(b, a); //Any order
            assert_eq!(b, c); //Strong to Numerical
            assert_eq!(c, b); //Any order
            assert_ne!(a, c); //Weak to Numerical, no connection
        }

        {
            let name = Name::validate("hello".to_string()).unwrap();
            let a = PackageID::Weak(name.clone());
            let b = PackageID::Weak(name.clone());
            assert_eq!(a, b); //Weak to weak
        }


        {
            let name = Name::validate("hello".to_string()).unwrap();
            let a = PackageID::Strong(name.clone(), ANY_ID);
            assert_eq!(a, a.clone());
        }

        {
            let a = PackageID::Num(USR_ID);
            assert_eq!(a, a.clone());
        }

    }

    #[test]
    fn resource_id() {
        let name = Name::validate("hello".to_string()).unwrap();

        let a = ResourceID::Numeric(4);
        let b = ResourceID::Strong(name.clone(), 4);
        let c = ResourceID::Weak(name.clone());
        let d = ResourceID::Numeric(5);
        let e = ResourceID::Strong(name.clone(), 5);
        
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
