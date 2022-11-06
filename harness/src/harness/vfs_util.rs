pub mod vfs {
    use std::collections::HashMap;

    pub const built_folder: &'static str = "/.ts";

    pub const test_lib_folder: &'static str = "/.lib";

    pub const src_folder: &'static str = "/.src";

    pub type FileSet = HashMap<String, Option<FileSetValue>>;

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
