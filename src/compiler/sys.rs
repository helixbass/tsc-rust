use std::borrow::Cow;
use std::fs::Metadata;
use std::path::Path;
use std::process;
use std::rc::Rc;
use std::{env, fs};

use crate::{ConvertToTSConfigHost, ExitStatus};

pub(crate) enum FileSystemEntryKind {
    File,
    Directory,
}

pub trait System: ConvertToTSConfigHost {
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
    fn is_watch_file_supported(&self) -> bool;
    fn is_watch_directory_supported(&self) -> bool;
    fn resolve_path(&self, path: &str) -> String;
    fn file_exists(&self, path: &str) -> bool;
    fn directory_exists(&self, path: &str) -> bool;
    fn get_executing_file_path(&self) -> Cow<'static, str>;
    fn is_get_modified_time_supported(&self) -> bool;
    fn is_set_modified_time_supported(&self) -> bool;
    fn is_delete_file_supported(&self) -> bool;
    fn exit(&self, exit_code: Option<ExitStatus>) -> !;
    fn enable_cpu_profiler(&self, path: &str, continuation: &mut dyn FnMut()) {
        continuation()
    }
    fn disable_cpu_profiler(&self /*, continuation: &dyn FnMut()*/) {}
    fn get_environment_variable(&self, name: &str) -> String;
    fn try_enable_source_maps_for_host(&self) {}
    fn is_clear_screen_implemented(&self) -> bool;
    fn clear_screen(&self) {}
    fn set_blocking(&self) {}
    fn as_convert_to_tsconfig_host(&self) -> &dyn ConvertToTSConfigHost;
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

impl ConvertToTSConfigHost for SystemConcrete {
    fn use_case_sensitive_file_names(&self) -> bool {
        // TODO: this isn't right
        false
    }

    fn get_current_directory(&self) -> String {
        env::current_dir()
            .unwrap()
            .into_os_string()
            .into_string()
            .unwrap()
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

    fn is_watch_file_supported(&self) -> bool {
        false
    }

    fn is_watch_directory_supported(&self) -> bool {
        false
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

    fn is_get_modified_time_supported(&self) -> bool {
        false
    }

    fn is_set_modified_time_supported(&self) -> bool {
        false
    }

    fn is_delete_file_supported(&self) -> bool {
        false
    }

    fn file_exists(&self, path: &str) -> bool {
        self.file_system_entry_exists(path, FileSystemEntryKind::File)
    }

    fn directory_exists(&self, path: &str) -> bool {
        self.file_system_entry_exists(path, FileSystemEntryKind::Directory)
    }

    fn exit(&self, exit_code: Option<ExitStatus>) -> ! {
        self.disable_cpu_profiler();
        process::exit(exit_code.map_or(0, |exit_code| exit_code as i32))
    }

    fn get_environment_variable(&self, name: &str) -> String {
        env::var_os(name)
            .map(|os_string| os_string.to_str().unwrap().to_owned())
            .unwrap_or_else(|| "".to_owned())
    }

    fn is_clear_screen_implemented(&self) -> bool {
        unimplemented!()
    }

    fn as_convert_to_tsconfig_host(&self) -> &dyn ConvertToTSConfigHost {
        self
    }
}

thread_local! {
    static SYS: Rc<dyn System> = Rc::new(SystemConcrete::new(env::args().skip(1).collect(),));
}

pub fn get_sys() -> Rc<dyn System> {
    SYS.with(|sys| sys.clone())
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
