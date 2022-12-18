use gc::{Finalize, Trace};
use std::borrow::Cow;
use std::env;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::Path;
use std::process;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    combine_paths, empty_file_system_entries, fs_readdir_sync_with_file_types, fs_stat_sync,
    is_windows, match_files, process_cwd, read_file_and_strip_leading_byte_order_mark,
    ConvertToTSConfigHost, ExitStatus, FileSystemEntries, RequireResult, StatLike, Stats,
    WatchFileKind, WatchOptions,
};

pub fn generate_djb2_hash(data: &str) -> String {
    unimplemented!()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FileWatcherEventKind {
    Created,
    Changed,
    Deleted,
}

pub type FileWatcherCallback = Rc<dyn Fn(&str, FileWatcherEventKind)>;

pub type DirectoryWatcherCallback = Rc<dyn Fn(&str)>;

pub(crate) fn missing_file_modified_time() -> SystemTime {
    UNIX_EPOCH
}

lazy_static! {
    pub(crate) static ref ignored_paths: Vec<&'static str> =
        vec!["/node_modules/.", "/.git", "/.#"];
}

pub(crate) enum FileSystemEntryKind {
    File,
    Directory,
}

pub type Buffer = Vec<u8>;

pub trait System: ConvertToTSConfigHost + Trace + Finalize {
    fn args(&self) -> &[String];
    fn new_line(&self) -> &str;
    fn write(&self, s: &str);
    fn write_output_is_tty(&self) -> Option<bool> {
        None
    }
    fn get_width_of_terminal(&self) -> Option<usize> {
        None
    }
    fn read_file(&self, path: &str) -> io::Result<Option<String>>;
    fn get_file_size(&self, path: &str) -> Option<usize>;
    fn write_file(
        &self,
        path: &str,
        data: &str,
        write_byte_order_mark: Option<bool>,
    ) -> io::Result<()>;
    fn is_watch_file_supported(&self) -> bool;
    fn watch_file(
        &self,
        path: &str,
        callback: FileWatcherCallback,
        polling_interval: Option<u32>,
        options: Option<&WatchOptions>,
    ) -> Rc<dyn FileWatcher>;
    fn is_watch_directory_supported(&self) -> bool;
    fn watch_directory(
        &self,
        path: &str,
        callback: DirectoryWatcherCallback,
        recursive: Option<bool>,
        options: Option<&WatchOptions>,
    ) -> Rc<dyn FileWatcher>;
    fn resolve_path(&self, path: &str) -> String;
    fn file_exists(&self, path: &str) -> bool;
    fn directory_exists(&self, path: &str) -> bool;
    fn create_directory(&self, path: &str);
    fn get_executing_file_path(&self) -> Cow<'static, str>;
    fn get_directories(&self, path: &str) -> Vec<String>;
    fn read_directory(
        &self,
        path: &str,
        extensions: Option<&[&str]>,
        excludes: Option<&[String]>,
        includes: Option<&[String]>,
        depth: Option<usize>,
    ) -> Vec<String>;
    fn is_get_modified_time_supported(&self) -> bool;
    fn get_modified_time(&self, path: &str) -> Option<SystemTime> {
        panic!("Shouldn't call get_modified_time() unless supported")
    }
    fn is_set_modified_time_supported(&self) -> bool;
    fn set_modified_time(&self, path: &str, time: SystemTime) {
        panic!("Shouldn't call set_modified_time() unless supported")
    }
    fn is_delete_file_supported(&self) -> bool;
    fn delete_file(&self, path: &str) {
        panic!("Shouldn't call delete_file() unless supported")
    }
    fn is_create_hash_supported(&self) -> bool;
    fn create_hash(&self, data: &str) -> String {
        panic!("Shouldn't call create_hash() unless supported")
    }
    fn create_sha256_hash(&self, data: &str) -> Option<String> {
        None
    }
    fn get_memory_usage(&self) -> Option<usize> {
        None
    }
    fn exit(&self, exit_code: Option<ExitStatus>) -> !;
    fn enable_cpu_profiler(&self, path: &str, continuation: &mut dyn FnMut()) {
        continuation()
    }
    fn disable_cpu_profiler(&self /*, continuation: &dyn FnMut()*/) {}
    fn cpu_profiling_enabled(&self) -> Option<bool> {
        None
    }
    fn realpath(&self, path: &str) -> Option<String> {
        None
    }
    fn is_realpath_supported(&self) -> bool;
    fn get_environment_variable(&self, name: &str) -> String;
    fn try_enable_source_maps_for_host(&self) {}
    fn debug_mode(&self) -> Option<bool> {
        None
    }
    // TODO: need to be able to return different types? How to do this without a generic trait method?
    fn set_timeout(&self, callback: &dyn FnOnce(), ms: u32) {}
    fn clear_timeout(&self, timeout_id: u32) {}
    fn is_clear_screen_supported(&self) -> bool;
    fn clear_screen(&self) {
        panic!("Shouldn't call clear_screen() unless supported")
    }
    fn set_blocking(&self) {}
    fn base64_decode(&self, input: &str) -> Option<String> {
        None
    }
    fn base64_encode(&self, input: &str) -> Option<String> {
        None
    }
    fn buffer_from(&self, input: String, encoding: Option<&str>) -> Option<Buffer> {
        None
    }
    fn now(&self) -> Option<SystemTime> {
        None
    }
    fn disable_use_file_version_as_signature(&self) -> Option<bool> {
        None
    }
    fn require(&self, base_dir: &str, module_name: &str) -> Option<RequireResult> {
        None
    }
    fn default_watch_file_kind(&self) -> Option<WatchFileKind> {
        None
    }
    fn as_convert_to_tsconfig_host(&self) -> &dyn ConvertToTSConfigHost;
}

pub trait FileWatcher {
    fn close(&self);
}

const byte_order_mark_indicator: &'static str = "\u{FEFF}";

#[derive(Trace, Finalize)]
struct SystemConcrete {
    args: Vec<String>,
    use_case_sensitive_file_names: bool,
}

impl SystemConcrete {
    pub fn new(args: Vec<String>, use_case_sensitive_file_names: bool) -> Self {
        Self {
            args,
            use_case_sensitive_file_names,
        }
    }

    fn realpath_sync(&self, path: &str) -> io::Result<String> {
        fs::canonicalize(path).map(|path_buf| path_buf.into_os_string().into_string().unwrap())
    }

    fn stat_sync(&self, path: &str) -> Option<Stats> {
        fs_stat_sync(path)
    }

    fn read_file_worker(&self, file_name: &str) -> io::Result<Option<String>> {
        // I think the idea should be (so that behavior higher up in eg
        // create_compiler_host_worker() -> get_source_file() can more closely match the Typescript
        // version) that Err just corresponds to a thrown exception in JS, which
        // if doesn't look like readFileWorker() typically does
        Ok(read_file_and_strip_leading_byte_order_mark(file_name).ok())
    }

    fn get_accessible_file_system_entries(&self, path: &str) -> FileSystemEntries {
        // perfLogger.logEvent("ReadDir: " + (path || "."));
        let entries = fs_readdir_sync_with_file_types(if path.is_empty() { "." } else { path });
        if entries.is_err() {
            return empty_file_system_entries.clone();
        }
        let entries = entries.unwrap();
        let mut files: Vec<String> = vec![];
        let mut directories: Vec<String> = vec![];
        for dirent in &entries {
            let entry = dirent.name();

            if matches!(&*entry, "." | "..") {
                continue;
            }

            let stat: &dyn StatLike;
            let mut maybe_stats: Option<Stats> = None;
            if dirent.is_symbolic_link() {
                let name = combine_paths(path, &[Some(&entry)]);

                maybe_stats = self.stat_sync(&name);
                if maybe_stats.is_none() {
                    continue;
                }
                stat = maybe_stats.as_ref().unwrap();
            } else {
                stat = dirent;
            }

            if stat.is_file() {
                files.push(entry);
            } else if stat.is_directory() {
                directories.push(entry);
            }
        }
        files.sort();
        directories.sort();
        FileSystemEntries { files, directories }
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

    fn buffer_from(&self, input: String, encoding: Option<&str>) -> Option<Buffer> {
        Some(input.into_bytes())
    }

    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
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

    fn read_directory(
        &self,
        path: &str,
        extensions: Option<&[&str]>,
        excludes: Option<&[String]>,
        includes: Option<&[String]>,
        depth: Option<usize>,
    ) -> Vec<String> {
        match_files(
            path,
            extensions,
            excludes,
            includes,
            self.use_case_sensitive_file_names,
            &process_cwd(),
            depth,
            |path| self.get_accessible_file_system_entries(path),
            |path| self.realpath(path).unwrap(),
        )
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

    fn realpath(&self, path: &str) -> Option<String> {
        Some(self.realpath_sync(path).unwrap_or_else(|_| path.to_owned()))
    }

    fn is_realpath_supported(&self) -> bool {
        true
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

    fn is_clear_screen_supported(&self) -> bool {
        unimplemented!()
    }

    fn as_convert_to_tsconfig_host(&self) -> &dyn ConvertToTSConfigHost {
        self
    }

    fn get_file_size(&self, path: &str) -> Option<usize> {
        // try {
        let stat = self.stat_sync(path);
        Some(
            stat.filter(|stat| stat.is_file())
                .map_or(0, |stat| stat.size()),
        )
        // }
        // catch { /*ignore*/ }
    }

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: Option<bool>,
    ) -> io::Result<()> {
        // perfLogger.logEvent("WriteFile: " + fileName);
        let mut data: Cow<'_, str> = Cow::Borrowed(data);
        if write_byte_order_mark == Some(true) {
            data = Cow::Owned(format!("{}{}", byte_order_mark_indicator, data));
        }

        // try {
        let mut fd = File::create(file_name /*, "w"*/)?;
        write!(fd, "{}", data /*, "utf8"*/)?;
        Ok(())
        // }
        // finally {
        //     if (fd !== undefined) {
        //         _fs.closeSync(fd);
        //     }
        // }
    }

    fn watch_file(
        &self,
        path: &str,
        callback: FileWatcherCallback,
        polling_interval: Option<u32>,
        options: Option<&WatchOptions>,
    ) -> Rc<dyn FileWatcher> {
        unimplemented!()
    }

    fn watch_directory(
        &self,
        path: &str,
        callback: DirectoryWatcherCallback,
        recursive: Option<bool>,
        options: Option<&WatchOptions>,
    ) -> Rc<dyn FileWatcher> {
        unimplemented!()
    }

    fn create_directory(&self, path: &str) {
        unimplemented!()
    }

    fn get_directories(&self, path: &str) -> Vec<String> {
        self.get_accessible_file_system_entries(path)
            .directories
            .clone()
    }

    fn is_create_hash_supported(&self) -> bool {
        unimplemented!()
    }
}

thread_local! {
    static SYS: Rc<dyn System> = Rc::new(SystemConcrete::new(
        env::args().skip(1).collect(),
        is_file_system_case_sensitive(),
    ));
}

pub fn get_sys() -> Rc<dyn System> {
    SYS.with(|sys| sys.clone())
}

/*const*/
fn is_file_system_case_sensitive() -> bool {
    !is_windows()
    // return !fileExists(swapCase(__filename));
}

#[cfg(windows)]
const LINE_ENDING: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static str = "\n";
