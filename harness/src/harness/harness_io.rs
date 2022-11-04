use regex::Regex;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub trait IO {
    fn list_files(
        &self,
        path: &str,
        filter: Option<&Regex>,
        options: Option<ListFilesOptions>,
    ) -> Vec<String>;
}

pub struct ListFilesOptions {
    pub recursive: Option<bool>,
}

thread_local! {
    static IO_: RefCell<Option<Rc<dyn IO>>> = RefCell::new(None);
}

pub fn with_io<TReturn, TCallback: FnMut(&dyn IO) -> TReturn>(mut callback: TCallback) -> TReturn {
    IO_.with(|io| callback(io.borrow().as_deref().unwrap()))
}

pub const user_specified_root: &'static str = "";

pub struct FileBasedTest {
    pub file: String,
    pub configurations: Option<Vec<FileBasedTestConfiguration>>,
}

pub type FileBasedTestConfiguration = HashMap<String, String>;
