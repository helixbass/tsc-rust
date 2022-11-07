pub mod vfs {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::time::SystemTime;
    use typescript_rust::Comparison;

    use crate::{collections, documents, vpath};

    pub const built_folder: &'static str = "/.ts";

    pub const test_lib_folder: &'static str = "/.lib";

    pub const src_folder: &'static str = "/.src";

    pub struct FileSystem {
        pub ignore_case: bool,
        pub string_comparer: Rc<dyn Fn(&str, &str) -> Comparison>,

        _time: RefCell<TimestampOrNowOrSystemTimeOrCallback>,
    }

    impl FileSystem {
        pub fn meta(&self) -> &collections::Metadata<String> {
            unimplemented!()
        }

        pub fn meta_mut(&self) -> &mut collections::Metadata<String> {
            unimplemented!()
        }

        pub fn is_readonly(&self) -> bool {
            unimplemented!()
        }

        pub fn shadow(self: Rc<Self>, ignore_case: Option<bool>) -> Self {
            let ignore_case = ignore_case.unwrap_or(self.ignore_case);
            unimplemented!()
        }

        pub fn set_time<TValue: Into<TimestampOrNowOrSystemTimeOrCallback>>(&self, value: TValue) {
            let value = value.into();
            *self._time.borrow_mut() = value;
        }

        pub fn filemeta_mut(
            &self,
            path: &str,
        ) -> &mut collections::Metadata<Rc<documents::TextDocument>> {
            unimplemented!()
        }

        pub fn cwd(&self) -> &str {
            unimplemented!()
        }

        pub fn chdir(&self, path: &str) {
            unimplemented!()
        }

        pub fn apply(&self, files: &FileSet) {
            unimplemented!()
        }

        pub fn mkdirp_sync(&self, path: &str) {
            unimplemented!()
        }

        pub fn symlink_sync(&self, target: &str, linkpath: &str) {
            unimplemented!()
        }

        pub fn write_file_sync(
            &self,
            path: &str,
            data: &str, /*string | Buffer*/
            encoding: Option<&str>,
        ) {
            unimplemented!()
        }
    }

    pub enum TimestampOrNowOrSystemTimeOrCallback {
        Timestamp(u128),
        Now,
        SystemTime(SystemTime),
        Callback(Rc<dyn Fn() -> TimestampOrNowOrSystemTime>),
    }

    impl From<u128> for TimestampOrNowOrSystemTimeOrCallback {
        fn from(value: u128) -> Self {
            Self::Timestamp(value)
        }
    }

    impl From<SystemTime> for TimestampOrNowOrSystemTimeOrCallback {
        fn from(value: SystemTime) -> Self {
            Self::SystemTime(value)
        }
    }

    impl From<Rc<dyn Fn() -> TimestampOrNowOrSystemTime>> for TimestampOrNowOrSystemTimeOrCallback {
        fn from(value: Rc<dyn Fn() -> TimestampOrNowOrSystemTime>) -> Self {
            Self::Callback(value)
        }
    }

    pub enum TimestampOrNowOrSystemTime {
        Timestamp(u128),
        Now,
        SystemTime(SystemTime),
    }

    impl From<u128> for TimestampOrNowOrSystemTime {
        fn from(value: u128) -> Self {
            Self::Timestamp(value)
        }
    }

    impl From<SystemTime> for TimestampOrNowOrSystemTime {
        fn from(value: SystemTime) -> Self {
            Self::SystemTime(value)
        }
    }

    #[derive(Default)]
    pub struct FileSystemCreateOptions {
        pub time: Option<TimestampOrNowOrSystemTimeOrCallback>,
        pub files: Option<FileSet>,
        pub cwd: Option<String>,
        pub meta: Option<HashMap<String, String>>,
        pub documents: Option<Vec<Rc<documents::TextDocument>>>,
    }

    pub trait FileSystemResolverHost {}

    pub fn create_from_file_system(
        host: Rc<dyn FileSystemResolverHost>,
        ignore_case: bool,
        options: Option<FileSystemCreateOptions>,
    ) -> FileSystem {
        let FileSystemCreateOptions {
            documents,
            files,
            cwd,
            time,
            meta,
        } = options.unwrap_or_default();
        let fs = get_built_local(host, ignore_case).shadow(None);
        if let Some(meta) = meta {
            for key in meta.keys() {
                fs.meta_mut()
                    .set(key.clone(), meta.get(key).unwrap().clone());
            }
        }
        if let Some(time) = time {
            fs.set_time(time);
        }
        if let Some(cwd) = cwd.filter(|cwd| !cwd.is_empty()) {
            fs.mkdirp_sync(&cwd);
            fs.chdir(&cwd);
        }
        if let Some(documents) = documents {
            for document in documents {
                fs.mkdirp_sync(&vpath::dirname(&document.file));
                fs.write_file_sync(&document.file, &document.text, Some("utf8"));
                fs.filemeta_mut(&document.file)
                    .set("document".to_owned(), document.clone());
                let symlink = document.meta.get("symlink");
                if let Some(symlink) = symlink {
                    for link in symlink.split(",").map(|link| link.trim()) {
                        fs.mkdirp_sync(&vpath::dirname(link));
                        fs.symlink_sync(&vpath::resolve(fs.cwd(), &[Some(&document.file)]), link);
                    }
                }
            }
        }
        if let Some(files) = files {
            fs.apply(&files);
        }
        fs
    }

    pub type FileSet = HashMap<String, Option<FileSetValue>>;

    #[derive(Clone)]
    pub enum FileSetValue {
        // DirectoryLike
        FileSet(FileSet),
        Directory(Directory),
        // FileLike
        File(File),
        // Buffer(Buffer),
        String(String),
        Link(Link),
        Symlink(Symlink),
        Mount(Mount),
        Rmdir(Rmdir),
        Unlink(Unlink),
    }

    impl From<Symlink> for FileSetValue {
        fn from(value: Symlink) -> Self {
            Self::Symlink(value)
        }
    }

    pub type Directory = ();

    pub type File = ();

    pub type Link = ();

    pub type Rmdir = ();

    pub type Unlink = ();

    #[derive(Clone)]
    pub struct Symlink {
        pub symlink: String,
        pub meta: Option<()>,
    }

    impl Symlink {
        pub fn new(symlink: String, options: Option<SymlinkNewOptions>) -> Self {
            Self {
                symlink,
                meta: options.and_then(|options| options.meta),
            }
        }
    }

    pub struct SymlinkNewOptions {
        meta: Option<()>,
    }

    pub type Mount = ();

    fn get_built_local(host: Rc<dyn FileSystemResolverHost>, ignore_case: bool) -> Rc<FileSystem> {
        unimplemented!()
    }
}
