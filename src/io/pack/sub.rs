use std::fs::File;
use std::io::Write;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

use serde::{Serialize, Deserialize};
use serde_json::json;

use crate::calc::func::Function;
use crate::io::entry::PackageEntry;

use super::header::PackageHeader;

