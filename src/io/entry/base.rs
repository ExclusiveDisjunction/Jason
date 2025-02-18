pub use crate::calc::{VariableUnion, VariableUnionRef, VariableUnionRefMut, VariableData};
use crate::core::errors::NamingError;
use super::super::id::{Locator, NumericalPackID, NumericalResourceID, PackageID, ResourceID, ResourceKind};

use std::fmt::{Display, Debug};
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use serde::{Deserialize, Serialize};

#[derive(Clone, PartialEq)]
pub struct IOPoisonError {
    target: NumericalResourceID,
    message: String
}
impl Debug for IOPoisonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "when accessing resource at id {}, got a poison error '{}'", &self.target, &self.message)
    }
}
impl Display for IOPoisonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self as &dyn Debug).fmt(f)
    }
}
impl IOPoisonError {
    pub fn new<T: Display>(target: NumericalResourceID, x: T) -> Self {
        Self {
            target, 
            message: x.to_string()
        }
    }
}

pub type PoisonResult<T> = Result<T, IOPoisonError>;

pub struct ReadGuard<'a, T> {
    inner: Result<RwLockReadGuard<'a, T>, IOPoisonError>
}
impl<'a, T> Display for ReadGuard<'a, T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.access() {
            Some(v) => v.fmt(f),
            None => write!(f, "(unable to access-poison)")
        }
    }
}
impl<'a, T> Debug for ReadGuard<'a, T> where T: Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner.as_ref() {
            Ok(t) => t.fmt(f),
            Err(e) => write!(f, "(unable to access '{e}')")
        }
    }
}
impl<'a, T> PartialEq for ReadGuard<'a, T> where T: PartialEq {
    fn eq(&self, other: &Self) -> bool {
        match (self.access(), other.access()) {
            (Some(a), Some(b)) => a == b,
            _ => false
        }
    }
}
impl<'a, T> PartialEq<T> for ReadGuard<'a, T> where T: PartialEq {
    fn eq(&self, other: &T) -> bool {
        self.access()
        .map(|x| x == other)
        .unwrap_or(false)
    }
}
impl<'a, T> Deref for ReadGuard<'a, T> {
    type Target = Result<RwLockReadGuard<'a, T>, IOPoisonError>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<'a, T> From<IOPoisonError> for ReadGuard<'a, T> {
    fn from(value: IOPoisonError) -> Self {
        Self {
            inner: Err(value)
        }
    }
}
impl<'a, T> From<RwLockReadGuard<'a, T>> for ReadGuard<'a, T> {
    fn from(value: RwLockReadGuard<'a, T>) -> Self {
        Self {
            inner: Ok(value)
        }
    }
}
impl<'a, T> From<PoisonResult<RwLockReadGuard<'a, T>>> for ReadGuard<'a, T> {
    fn from(value: PoisonResult<RwLockReadGuard<'a, T>>) -> Self {
        Self {
            inner: value
        }
    }
}
impl<'a, T> ReadGuard<'a, T> {
    pub fn access(&'a self) -> Option<&'a T> {
        match self.inner.as_ref() {
            Ok(v) => Some(v.deref()),
            Err(_) => None
        }
    }
    pub fn access_error(&'a self) -> Option<&'a IOPoisonError> {
        match self.inner.as_ref() {
            Ok(_) => None,
            Err(e) => Some(e)
        }
    }

    pub fn get_err(self) -> Option<IOPoisonError> {
        match self.inner {
            Ok(_) => None,
            Err(e) => Some(e)
        }
    }
    pub fn get_lock(self) -> Option<RwLockReadGuard<'a, T>> {
        match self.inner {
            Ok(v) => Some(v),
            Err(_) => None
        }
    }
}

pub struct WriteGuard<'a, T> {
    inner: Result<RwLockWriteGuard<'a, T>, IOPoisonError>
}
impl<'a, T> Deref for WriteGuard<'a, T> {
    type Target = Result<RwLockWriteGuard<'a, T>, IOPoisonError>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<'a, T> DerefMut for WriteGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner   
    }
}
impl<'a, T> From<IOPoisonError> for WriteGuard<'a, T> {
    fn from(value: IOPoisonError) -> Self {
        Self {
            inner: Err(value)
        }
    }
}
impl<'a, T> From<RwLockWriteGuard<'a, T>> for WriteGuard<'a, T> {
    fn from(value: RwLockWriteGuard<'a, T>) -> Self {
        Self {
            inner: Ok(value)
        }
    }
}
impl<'a, T> From<PoisonResult<RwLockWriteGuard<'a, T>>> for WriteGuard<'a, T> {
    fn from(value: PoisonResult<RwLockWriteGuard<'a, T>>) -> Self {
        Self {
            inner: value
        }
    }
}
impl<'a, T> WriteGuard<'a, T> {
    pub fn from_ok(data: RwLockWriteGuard<'a, T>) -> Self {
        Self {
            inner: Ok(data)
        }
    }
    pub fn from_err(data: IOPoisonError) -> Self {
        Self {
            inner: Err(data)
        }
    }
    pub fn new(data: Result<RwLockWriteGuard<'a, T>, IOPoisonError>) -> Self {
        Self {
            inner: data
        }
    }

    pub fn access(&'a mut self) -> Option<&'a mut T> {
        match self.inner.as_mut() {
            Ok(v) => Some(v.deref_mut()),
            Err(_) => None
        }
    }
}

pub trait IOEntry: Serialize + for<'a> Deserialize<'a> + PartialEq + Display + Debug {
    fn name(&self) -> &str;
    fn set_name(&mut self, new: String) -> Result<(), NamingError>;

    fn accepts_id(&self, id: NumericalResourceID) -> bool {
        self.id() == id
    }
    fn accepts_locator(&self, id: &Locator) -> bool {
        if id.kind() != self.resource_kind() { return false; }

        if id.resource().is_weak() {
            id.resource().name() == Some(self.name())
        }
        else {
            id == &self.id()
        }
    }

    fn resource_kind(&self) -> ResourceKind;
    fn id(&self) -> NumericalResourceID;
    fn package_id(&self) -> NumericalPackID {
        self.id().package()
    }
    fn id_mut(&mut self) -> &mut NumericalResourceID;
    fn strong_locator(&self) -> Locator {
        Locator::new(
            PackageID::Num(self.package_id()),
            ResourceID::Strong(self.name().to_string(), self.id().resx()),
            self.resource_kind()
        )
    }
}

pub trait IOStorage: IOEntry {
    type Holding: Serialize + for<'a> Deserialize<'a>;
    
    fn get_arc(&self) -> &Arc<RwLock<Self::Holding>>;
    fn clone_arc(&self) -> Arc<RwLock<Self::Holding>> {
        Arc::clone(self.get_arc())
    }
    fn get_data(&self) -> ReadGuard<'_, Self::Holding> {
        self.get_arc()
        .read()
        .map_err(|x| IOPoisonError::new(self.id(), x))
        .into()
    }
    fn get_data_mut(&self) -> WriteGuard<'_, Self::Holding> {
        self.get_arc()
        .write()
        .map_err(|x| IOPoisonError::new(self.id(), x))
        .into()
    }
    fn set_data(&self, new: Self::Holding) -> PoisonResult<()> {
        let mut lock = match self.get_arc().write() {
            Ok(l) => l,
            Err(e) => return Err(IOPoisonError::new(self.id(), e.to_string()))
        };

        *lock = new;

        Ok(())
    }
}
