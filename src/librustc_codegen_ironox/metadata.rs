extern crate ar;

use rustc::middle::cstore::{MetadataLoader, METADATA_FILENAME};
use rustc_data_structures::owning_ref::OwningRef;
use rustc_target::spec::Target;

use std::fs::File;
use std::io::Read;
use std::path::Path;

pub use rustc_data_structures::sync::MetadataRef;

pub struct IronOxMetadataLoader;

impl MetadataLoader for IronOxMetadataLoader {
    fn get_rlib_metadata(&self, _: &Target, filename: &Path)
        -> Result<MetadataRef, String> {
        unimplemented!("get_rlib_metadata {:?}", filename);
    }

    fn get_dylib_metadata(&self, _target: &Target, filename: &Path)
        -> Result<MetadataRef, String> {
        unimplemented!("get_dylib_metadata {:?}", filename);
    }
}
