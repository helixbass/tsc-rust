use std::env;
use std::fs::{self, DirEntry, Metadata};
use std::io;

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
}

impl StatLike for Stats {
    fn is_file(&self) -> bool {
        self.metadata.is_file()
    }

    fn is_directory(&self) -> bool {
        self.metadata.is_dir()
    }
}

pub fn fs_stat_sync(path: &str) -> Option<Stats> {
    fs::metadata(path).map(|metadata| Stats::new(metadata)).ok()
}
