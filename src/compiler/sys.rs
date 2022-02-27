use std::borrow::Cow;
use std::fs::Metadata;
use std::path::Path;
use std::process;
use std::{env, fs};

use crate::ExitStatus;

pub(crate) enum FileSystemEntryKind {
    File,
    Directory,
}

pub trait System {
    fn args(&self) -> &[String];
    fn new_line(&self) -> &str;
    fn write(&self, s: &str);
    fn write_output_is_tty(&self) -> Option<bool> {
        None
    }
    fn get_width_of_terminal(&self) -> Option<usize> {
        None
    }
    fn read_file(&self, path: &str) -> Option<String>;
    fn get_executing_file_path(&self) -> Cow<'static, str>;
    fn get_current_directory(&self) -> String;
    fn resolve_path(&self, path: &str) -> String;
    fn file_exists(&self, path: &str) -> bool;
    fn exit(&self, exit_code: Option<ExitStatus>) -> !;
    fn disable_cpu_profiler(&self /*, continuation: &dyn FnMut()*/);
    fn get_environment_variable(&self, name: &str) -> String;
    fn try_enable_source_maps_for_host(&self) {}
    fn set_blocking(&self) {}
}

struct SystemConcrete {
    args: Vec<String>,
}

impl SystemConcrete {
    pub fn new(args: Vec<String>) -> Self {
        Self { args }
    }

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
    fn args(&self) -> &[String] {
        &self.args
    }

    fn new_line(&self) -> &str {
        LINE_ENDING
    }

    fn write(&self, s: &str) {
        print!("{}", s);
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

    fn exit(&self, exit_code: Option<ExitStatus>) -> ! {
        self.disable_cpu_profiler();
        process::exit(exit_code.map_or(0, |exit_code| exit_code as i32))
    }

    fn disable_cpu_profiler(&self) {}

    fn get_environment_variable(&self, name: &str) -> String {
        env::var_os(name)
            .map(|os_string| os_string.to_str().unwrap().to_owned())
            .unwrap_or_else(|| "".to_owned())
    }
}

lazy_static! {
    static ref SYS: SystemConcrete = SystemConcrete::new(env::args().skip(1).collect(),);
}

pub fn get_sys() -> &'static impl System {
    &*SYS
}

#[cfg(windows)]
const LINE_ENDING: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static str = "\n";

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
