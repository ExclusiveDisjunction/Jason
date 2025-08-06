use crate::prelude::{Name, NameRef};
use crate::expr::raw::ResourceKind;

use std::fmt::{Display, Debug};
use std::sync::{Arc, RwLock};
use serde::{Deserialize, Serialize};

use exdisj::log_warning;
use exdisj::io::lock::{ReadGuard, WriteGuard};
use exdisj::error::PoisonError;

pub trait IOEntry: PartialEq + Display + Debug {
    fn name(&self) -> NameRef<'_>;
    fn set_name(&mut self, new: Name);

    fn kind(&self) -> ResourceKind;
}

#[derive(Debug)]
pub struct EntryHandle<T> {
    inner: Arc<RwLock<T>>
}
impl<T> Clone for EntryHandle<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner)
        }
    }
}
impl<T> EntryHandle<T> {
    pub fn new(arc: Arc<RwLock<T>>) -> Self {
        Self {
            inner: arc
        }
    }

    pub fn read(&self) -> ReadGuard<'_, T> {
        self.inner
            .read()
            .map_err(PoisonError::new)
            .into()
    }
    pub fn write(&self) -> WriteGuard<'_, T> {
        self.inner
            .write()
            .map_err(PoisonError::new)
            .into()
    }
    pub fn set(&self, new: T) {
        let mut lock = match self.inner.write() {
            Ok(l) => l,
            Err(e) => {
                log_warning!("While accessing element, a poison was detected: '{}'", &e);
                e.into_inner()
            }
        };

        *lock = new;
    }
}

pub trait IOStorage: IOEntry + Serialize + for<'a> Deserialize<'a> {
    type Holding: Serialize + for<'a> Deserialize<'a>;
    
    fn access(&self) -> EntryHandle<Self::Holding>;
}
