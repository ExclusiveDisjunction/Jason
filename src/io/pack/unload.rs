use crate::calc::func::Function;
use super::header::PackageHeader;
use super::super::entry::*;
use crate::core::errors::Error;

use std::sync::{Arc, RwLock};
use std::path::{Path, PathBuf};
use std::fs::File;


pub struct Package {
    pack_id: u32,
    current_id: u32,
    name: String,
    header: PackageHeader,
    entries: Vec<Arc<RwLock<PackageEntry>>>,
    func: Vec<Arc<Function>>,

    header_file: File,
    entries_file: File,
    func_file: File,
    location: PathBuf
}
impl Package {

    pub fn open_from_directory(path: &Path, id: u32) -> Result<Self, Error> {
        todo!()
    }
    
    fn get_next_id(&mut self) -> Option<u32> {
        if self.current_id == u32::MAX {
            None
        }
        else {
            let result = self.current_id;
            self.current_id += 1;
            Some(result)
        }
    }

    pub fn location(&self) -> &Path {
        &self.location
    }
    pub fn id(&self) -> u32 {
        self.pack_id
    }
    pub fn header(&self) -> &PackageHeader {
        &self.header
    }
    pub fn header_mut(&mut self) -> &mut PackageHeader {
        &mut self.header
    }

    pub fn compress_to(&self, location: &Path) -> Result<(), Error> {
        todo!()
    }
    pub fn save(&self) -> std::io::Result<()> {
        todo!()
    }
    pub fn close(&mut self) {

    }
}