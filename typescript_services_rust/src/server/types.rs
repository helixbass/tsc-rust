use gc::{Trace, Finalize};
use std::rc::Rc;

use typescript_rust::{
    DirectoryWatcherCallback, FileWatcher, FileWatcherCallback, System, WatchOptions,
};

pub struct CompressedData {
    pub length: usize,
    pub compression_kind: String,
    pub data: (),
}

pub enum RequireResult {
    Module(()),
    Error(RequireResultError),
}

pub struct RequireResultError {
    pub stack: Option<String>,
    pub message: Option<String>,
}

pub type TimeoutId = ();

pub trait ServerHost: System + Trace + Finalize {
    fn watch_file(
        &self,
        path: &str,
        callback: FileWatcherCallback,
        polling_interval: Option<usize>,
        options: Option<Rc<WatchOptions>>,
    ) -> Rc<dyn FileWatcher>;
    fn watch_directory(
        &self,
        path: &str,
        callback: DirectoryWatcherCallback,
        recursive: Option<bool>,
        options: Option<Rc<WatchOptions>>,
    ) -> Rc<dyn FileWatcher>;
    fn set_timeout(
        &self,
        callback: Rc<dyn Fn(/*...args: any[]*/)>,
        ms: usize, /*, ...args: any[]*/
    ) -> TimeoutId /*any*/;
    fn clear_timeout(&self, timeout_id: TimeoutId);
    fn set_immediate(
        &self,
        callback: Rc<dyn Fn(/*...args: any[]*/)>,
        ms: usize, /*, ...args: any[]*/
    ) -> TimeoutId /*any*/;
    fn clear_immediate(&self, timeout_id: TimeoutId);
    fn gc(&self) {}
    fn trace(&self, _s: &str) {}
    fn require(&self, _initial_path: &str, _module_name: &str) -> Option<RequireResult> {
        None
    }
}
