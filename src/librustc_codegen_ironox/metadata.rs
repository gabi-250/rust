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
        let input_file =
            File::open(filename)
                .map_err(|_e| format!("failed to read {}", filename.display()))?;
        let mut archive = ar::Archive::new(input_file);
        let mut buf = vec![];
        while let Some(entry) = archive.next_entry() {
            let mut entry = entry.expect("failed to parse archive entry");
            if entry.header().identifier() == METADATA_FILENAME.to_string().as_bytes() {
                entry.read_to_end(&mut buf).expect("failed to read metadata file");
            }
        }
        let buf = OwningRef::new(box buf);
        let buf: OwningRef<_, [u8]> = buf.map(|v| &v[..]);
        Ok(rustc_erase_owner!(buf))
    }

    fn get_dylib_metadata(&self, _target: &Target, filename: &Path)
        -> Result<MetadataRef, String> {
        unimplemented!("get_dylib_metadata {:?}", filename);
    }
}
