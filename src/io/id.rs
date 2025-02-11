use crate::core::errors::FormattingError;
use super::entry::EntryType;

use std::fmt::Display;
use crate::core::is_string_whitespace;

pub enum PackageIdentifier {
    Spec(String),
    Any,
    Usr,
    Std
}
impl Display for PackageIdentifier {
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

pub struct Identifier {
    pack: PackageIdentifier,
    entry_kind: Option<EntryType>, //If this is none, then arguments is not none, and vice versa.
    entry_name: String,
    arguments: Option<Vec<String>>
}
impl TryFrom<String> for Identifier {
    type Error = FormattingError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
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

        let parent: PackageIdentifier;
        let raw_child: &str;

        match splits.len() {
            1 => {
                parent = PackageIdentifier::Usr;
                raw_child = splits[0];
            },
            2 => {
                match splits[0] {
                    "usr" => parent = PackageIdentifier::Usr,
                    "std" => parent = PackageIdentifier::Std,
                    "*" => parent = PackageIdentifier::Any,
                    a if !a.is_empty() && !is_string_whitespace(a) => parent = PackageIdentifier::Spec(a.trim().to_string()),
                    b => return Err(FormattingError::new(&b, "the parent package must not be just whitespace or empty"))
                }
                raw_child = splits[1];
            },
            _ => return Err(FormattingError::new(&value, "there can only be one or two values before and after the '::'"))
        }

        let raw_child = raw_child.trim();
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

                let split_args: Vec<String> = args.split(',').map(|x| x.trim().to_string()).collect();

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
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(a) = self.arguments.as_deref() {
            write!(f, "{}::%{}({})", &self.pack, &self.entry_name, a.join(", "))
        }
        else {
            write!(f, "{}::{}{}", &self.pack, &self.entry_name, self.entry_kind.unwrap().symbol())
        }
    }
}
impl Identifier {
    pub fn new_entry(package: PackageIdentifier, kind: EntryType, name: String) -> Self {
        Self {
            pack: package,
            entry_kind: Some(kind),
            entry_name: name,
            arguments: None
        }
    }
    pub fn new_func(package: PackageIdentifier, name: String, arguments: Vec<String>) -> Self {
        Self {
            pack: package,
            entry_kind: None,
            entry_name: name,
            arguments: Some(arguments)
        }
    }

    pub fn package(&self) -> &PackageIdentifier {
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