use encoding_rs_io::DecodeReaderBytes;
use std::convert::TryInto;
use std::env;
use std::ffi::OsString;
use std::fs::{self, DirEntry, Metadata};
use std::io::{self, Read};
use std::path::{Path as StdPath, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/*const*/
pub fn is_windows() -> bool {
    env::consts::OS == "windows"
}

pub fn process_cwd() -> String {
    env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap()
}

pub struct Dirent {
    dir_entry: DirEntry,
}

impl Dirent {
    pub fn new(dir_entry: DirEntry) -> Self {
        Self { dir_entry }
    }

    pub fn name(&self) -> String {
        self.dir_entry.file_name().into_string().unwrap()
    }

    pub fn is_symbolic_link(&self) -> bool {
        matches!(
            self.dir_entry.file_type(),
            Ok(file_type) if file_type.is_symlink()
        )
    }
}

pub trait StatLike {
    fn is_file(&self) -> bool;
    fn is_directory(&self) -> bool;
}

impl StatLike for Dirent {
    fn is_file(&self) -> bool {
        matches!(
            self.dir_entry.file_type(),
            Ok(file_type) if file_type.is_file()
        )
    }

    fn is_directory(&self) -> bool {
        matches!(
            self.dir_entry.file_type(),
            Ok(file_type) if file_type.is_dir()
        )
    }
}

pub fn fs_readdir_sync<TPath: AsRef<StdPath>>(path: TPath) -> io::Result<Vec<OsString>> {
    let path = path.as_ref();
    fs::read_dir(path).map(|entries| {
        entries
            .filter_map(|entry| entry.map(|entry| entry.file_name()).ok())
            .collect()
    })
}

pub fn fs_readdir_sync_with_file_types(path: &str) -> io::Result<Vec<Dirent>> {
    fs::read_dir(path).map(|entries| {
        entries
            .filter_map(|entry| entry.map(|entry| Dirent::new(entry)).ok())
            .collect()
    })
}

pub struct Stats {
    metadata: Metadata,
}

impl Stats {
    pub fn new(metadata: Metadata) -> Self {
        Self { metadata }
    }

    pub fn size(&self) -> usize {
        self.metadata.len().try_into().unwrap()
    }
}

impl StatLike for Stats {
    fn is_file(&self) -> bool {
        self.metadata.is_file()
    }

    fn is_directory(&self) -> bool {
        self.metadata.is_dir()
    }
}

pub fn fs_stat_sync<TPath: AsRef<StdPath>>(path: TPath) -> Option<Stats> {
    fs::metadata(path).map(|metadata| Stats::new(metadata)).ok()
}

pub fn fs_exists_sync<TPath: AsRef<StdPath>>(path: TPath) -> bool {
    path.as_ref().exists()
}

pub fn read_file_and_strip_leading_byte_order_mark(file_name: &str) -> io::Result<String> {
    fs::read(file_name)
        .and_then(|contents| {
            let mut string_buffer = String::with_capacity(contents.len());
            let mut decoder = DecodeReaderBytes::new(&*contents);
            let result = decoder.read_to_string(&mut string_buffer);
            result.map(|_| string_buffer)
        })
        .map(|contents| {
            let mut contents_chars = contents.chars();
            if contents_chars.next() == Some('\u{FEFF}') {
                contents_chars.collect()
            } else {
                contents
            }
        })
}

pub fn path_join(paths: &[&StdPath]) -> PathBuf {
    assert!(!paths.is_empty());
    let mut ret = paths[0].to_owned();
    for path in &paths[1..] {
        ret.push(path);
    }
    ret
}

pub fn millis_since_epoch_to_system_time(millis: u128) -> SystemTime {
    // TODO: I saw u128 being used to represent milliseconds since epoch somewhere but this only
    // accepts u64, is that sufficient (and if so should I use that everywhere)?
    UNIX_EPOCH + Duration::from_millis(millis.try_into().unwrap())
}
