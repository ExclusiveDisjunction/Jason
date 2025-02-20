use super::var::VariableEntry;
use super::func::FunctionEntry;
use super::base::IOEntry;
use super::super::id::{ResourceKind, NumericalResourceID};

use crate::core::errors::NamingError;

use std::fmt::{Display, Debug};

pub enum PackageEntry {
    Var(VariableEntry),
    Func(FunctionEntry)
}

impl From<FunctionEntry> for PackageEntry {
    fn from(value: FunctionEntry) -> Self {
        Self::Func(value)
    }
}
impl From<VariableEntry> for PackageEntry {
    fn from(value: VariableEntry) -> Self {
        Self::Var(value)
    }
}
impl Into<Option<VariableEntry>> for PackageEntry {
    fn into(self) -> Option<VariableEntry> {
        match self {
            Self::Var(v) => Some(v),
            _ => None
        }
    }
}
impl Into<Option<FunctionEntry>> for PackageEntry {
    fn into(self) -> Option<FunctionEntry> {
        match self {
            Self::Func(f) => Some(f),
            _ => None
        }
    }
}

impl Display for PackageEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Display = match self {
            Self::Var(v) => v,
            Self::Func(f) => f
        };

        x.fmt(f)
    }
}
impl Debug for PackageEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x: &dyn Debug = match self {
            Self::Var(v) => v,
            Self::Func(f) => f
        };

        x.fmt(f)
    }
}

impl PartialEq for PackageEntry {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(a), Self::Var(b)) => a == b,
            (Self::Func(a), Self::Func(b)) => a == b,
            _ => false
        }
    }
}

impl IOEntry for PackageEntry {
    fn name(&self) -> &str {
        match self {
            Self::Var(v) => v.name(),
            Self::Func(f) => f.name()
        }
    }
    fn set_name(&mut self, new: String) -> Result<(), NamingError> {
        match self {
            Self::Var(v) => v.set_name(new),
            Self::Func(f) => f.set_name(new) 
        }
    }

    fn resource_kind(&self) -> ResourceKind {
        match self {
            Self::Var(v) => v.resource_kind(),
            Self::Func(f) => f.resource_kind() 
        }
    }
    fn id(&self) -> NumericalResourceID {
        match self {
            Self::Var(v) => v.id(),
            Self::Func(f) => f.id()
        }
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID {
        match self {
            Self::Var(v) => v.id_mut(),
            Self::Func(f) => f.id_mut() 
        }
    }
}