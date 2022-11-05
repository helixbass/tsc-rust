pub mod vfs {
    use std::collections::HashMap;

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

    pub type Directory = ();

    pub type File = ();

    pub type Link = ();

    pub type Rmdir = ();

    pub type Unlink = ();

    pub type Symlink = ();

    pub type Mount = ();
}
