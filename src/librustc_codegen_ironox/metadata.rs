extern crate ar;
extern crate goblin;

use rustc::middle::cstore::{metadata_section_name, MetadataLoader, METADATA_FILENAME};
use rustc_data_structures::owning_ref::OwningRef;
use rustc_target::spec::Target;

use std::fs::File;
use std::io::Read;
use std::path::Path;

pub use rustc_data_structures::sync::MetadataRef;

pub struct IronOxMetadataLoader;

impl MetadataLoader for IronOxMetadataLoader {
    fn get_rlib_metadata(&self, _: &Target, filename: &Path) -> Result<MetadataRef, String> {
        let input_file =
            File::open(filename).map_err(|_e| format!("failed to read {}", filename.display()))?;
        let mut archive = ar::Archive::new(input_file);
        let mut buf = vec![];
        while let Some(entry) = archive.next_entry() {
            let mut entry = entry.expect("failed to parse archive entry");
            if entry.header().identifier() == METADATA_FILENAME.to_string().as_bytes() {
                entry
                    .read_to_end(&mut buf)
                    .expect("failed to read metadata file");
            }
        }
        let buf = OwningRef::new(box buf);
        let buf: OwningRef<_, [u8]> = buf.map(|v| &v[..]);
        Ok(rustc_erase_owner!(buf))
    }

    fn get_dylib_metadata(&self, target: &Target, filename: &Path) -> Result<MetadataRef, String> {
        let mut input_file =
            File::open(filename).map_err(|_e| format!("failed to read {}", filename.display()))?;
        let mut buf = vec![];
        input_file
            .read_to_end(&mut buf)
            .map_err(|_| format!("failed to read {}", filename.display()))?;
        let buf = OwningRef::new(box buf);
        let buf = buf.try_map(|buf| search_meta_section(&buf, target, filename))?;
        return Ok(rustc_erase_owner!(buf));
    }
}

fn search_meta_section<'a>(
    bytes: &'a [u8],
    target: &Target,
    filename: &Path,
) -> Result<&'a [u8], String> {
    let elf = goblin::elf::Elf::parse(&bytes).map_err(|_| "failed to parse ELF")?;
    for sh in &elf.section_headers {
        if elf
            .shdr_strtab
            .get(sh.sh_name)
            .map_or(false, |r| r.ok() == Some(metadata_section_name(target)))
        {
            let start_index = sh.sh_offset as usize;
            let end_index = (sh.sh_offset + sh.sh_size) as usize;
            return Ok(&bytes[start_index..end_index]);
        }
    }
    Err(format!("metadata not found: '{}'", filename.display()))
}
