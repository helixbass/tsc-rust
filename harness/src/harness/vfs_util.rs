pub mod vfs {
    use std::collections::HashMap;
    use std::rc::Rc;
    use typescript_rust::Comparison;

    use crate::{collections, documents};

    pub const built_folder: &'static str = "/.ts";

    pub const test_lib_folder: &'static str = "/.lib";

    pub const src_folder: &'static str = "/.src";

    pub struct FileSystem {
        pub ignore_case: bool,
        pub string_comparer: Rc<dyn Fn(&str, &str) -> Comparison>,
    }

    impl FileSystem {
        pub fn meta(&self) -> &collections::Metadata<String> {
            unimplemented!()
        }

        pub fn is_readonly(&self) -> bool {
            unimplemented!()
        }

        pub fn shadow(self: Rc<Self>, ignore_case: Option<bool>) -> Self {
            let ignore_case = ignore_case.unwrap_or(self.ignore_case);
            unimplemented!()
        }

        pub fn apply(&self, files: &FileSet) {
            unimplemented!()
        }
    }

    #[derive(Default)]
    pub struct FileSystemCreateOptions {
        pub time: Option<()>,
        pub files: Option<FileSet>,
        pub cwd: Option<String>,
        pub meta: Option<HashMap<String, ()>>,
        pub documents: Option<Vec<documents::TextDocument>>,
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
        unimplemented!()
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
}
