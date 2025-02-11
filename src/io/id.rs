use crate::core::errors::FormattingError;
use super::entry::EntryType;

use std::fmt::Display;

pub enum PackageIdentifyer {
    Spec(String),
    Any,
    Usr,
    Std
}
impl Display for PackageIdentifyer {
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
    pack: PackageIdentifyer,
    entry_kind: EntryType,
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
            2: If a function is called, the parethesis are required, no matter what
            3: 
         */

        todo!()
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(a) = self.arguments.as_deref() {
            write!(f, "{}::%{}({})", &self.pack, &self.entry_name, a.join(", "))
        }
        else {
            write!(f, "{}::{}{}", &self.pack, &self.entry_name, self.entry_kind.symbol())
        }
    }
}