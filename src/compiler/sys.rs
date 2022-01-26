use std::borrow::Cow;
use std::fs::Metadata;
use std::path::Path;
use std::{env, fs};

pub(crate) enum FileSystemEntryKind {
    File,
    Directory,
}

pub trait System {
    fn args(&self) -> &Vec<String>;
    fn read_file(&self, path: &str) -> Option<String>;
    fn get_executing_file_path(&self) -> Cow<'static, str>;
    fn get_current_directory(&self) -> String;
    fn resolve_path(&self, path: &str) -> String;
    fn file_exists(&self, path: &str) -> bool;
}

struct SystemConcrete {
    args: Vec<String>,
}

impl SystemConcrete {
    fn stat_sync(&self, path: &str) -> Option<Stats> {
        Some(Stats::new(fs::metadata(Path::new(path)).ok()?))
    }

    fn read_file_worker(&self, file_name: &str) -> Option<String> {
        fs::read_to_string(file_name).ok()
    }

    fn file_system_entry_exists(&self, path: &str, entry_kind: FileSystemEntryKind) -> bool {
        let stat = self.stat_sync(path);
        if stat.is_none() {
            return false;
        }
        let stat = stat.unwrap();
        match entry_kind {
            FileSystemEntryKind::File => stat.is_file(),
            FileSystemEntryKind::Directory => stat.is_directory(),
            _ => false,
        }
    }
}

impl System for SystemConcrete {
    fn args(&self) -> &Vec<String> {
        &self.args
    }

    fn read_file(&self, file_name: &str) -> Option<String> {
        self.read_file_worker(file_name)
    }

    fn resolve_path(&self, path: &str) -> String {
        Path::new(path)
            .canonicalize()
            .unwrap()
            .into_os_string()
            .into_string()
            .unwrap()
    }

    fn get_executing_file_path(&self) -> Cow<'static, str> {
        Cow::Borrowed(file!())
    }

    fn get_current_directory(&self) -> String {
        env::current_dir()
            .unwrap()
            .into_os_string()
            .into_string()
            .unwrap()
    }

    fn file_exists(&self, path: &str) -> bool {
        self.file_system_entry_exists(path, FileSystemEntryKind::File)
    }
}

lazy_static! {
    static ref SYS: SystemConcrete = SystemConcrete {
        args: env::args().skip(1).collect(),
    };
}

pub fn get_sys() -> &'static impl System {
    &*SYS
}

pub struct Stats {
    metadata: Metadata,
}

impl Stats {
    pub fn new(metadata: Metadata) -> Self {
        Self { metadata }
    }

    pub fn is_file(&self) -> bool {
        self.metadata.is_file()
    }

    pub fn is_directory(&self) -> bool {
        self.metadata.is_dir()
    }
}
