pub mod vfs {
    use std::borrow::Cow;
    use std::cell::{Ref, RefCell, RefMut};
    use std::collections::HashMap;
    use std::iter::FromIterator;
    use std::rc::Rc;
    use std::time::SystemTime;
    use typescript_rust::Comparison;

    use crate::{collections, documents, vpath};

    pub const built_folder: &'static str = "/.ts";

    pub const projects_folder: &'static str = "/.projects";

    pub const test_lib_folder: &'static str = "/.lib";

    pub const src_folder: &'static str = "/.src";

    pub struct FileSystem {
        pub ignore_case: bool,
        pub string_comparer: Rc<dyn Fn(&str, &str) -> Comparison>,
        _lazy: FileSystemLazy,

        _cwd: Option<String>,
        _time: RefCell<TimestampOrNowOrSystemTimeOrCallback>,
        _shadow_root: Option<Rc<FileSystem>>,
        _dir_stack: Option<Vec<String>>,
    }

    impl FileSystem {
        pub fn new(ignore_case: bool, options: Option<FileSystemOptions>) -> Self {
            let options = options.unwrap_or_default();
            let FileSystemOptions {
                time,
                files,
                meta,
                cwd: options_cwd,
            } = options;
            let time = time.unwrap_or(TimestampOrNowOrSystemTimeOrCallback::Now);

            let mut ret = Self {
                ignore_case,
                string_comparer: Rc::new(move |a: &str, b: &str| {
                    if ignore_case {
                        vpath::compare_case_insensitive(a, b)
                    } else {
                        vpath::compare_case_sensitive(a, b)
                    }
                }),
                _time: RefCell::new(time),
                _cwd: None,
                _dir_stack: None,
                _lazy: Default::default(),
                _shadow_root: None,
            };

            if let Some(meta) = meta {
                for (key, value) in meta {
                    ret.meta_mut().set(&key, value);
                }
            }

            if let Some(files) = files {
                ret._apply_files(&files, "");
            }

            let mut cwd = options_cwd;
            if match cwd.as_ref() {
                None => true,
                Some(cwd) => cwd.is_empty() || !vpath::is_root(cwd),
            } {
                if let Some(lazy_links) = ret._lazy.links.as_ref() {
                    for name in lazy_links.keys() {
                        cwd = Some(if let Some(cwd_present) = cwd {
                            vpath::resolve(name, &[Some(&cwd_present)])
                        } else {
                            name.clone()
                        });
                        break;
                    }
                }
            }

            if let Some(cwd) = cwd.as_ref().filter(|cwd| !cwd.is_empty()) {
                vpath::validate(cwd, Some(vpath::ValidationFlags::Absolute));
                ret.mkdirp_sync(cwd);
            }

            ret._cwd = Some(cwd.unwrap_or_else(|| "".to_owned()));

            ret
        }

        pub fn meta(&self) -> Ref<collections::Metadata<String>> {
            unimplemented!()
        }

        pub fn meta_rc(&self) -> Rc<RefCell<collections::Metadata<String>>> {
            unimplemented!()
        }

        pub fn meta_mut(&self) -> RefMut<collections::Metadata<String>> {
            RefMut::map(self._lazy.meta.borrow_mut(), |lazy_meta| {
                lazy_meta.get_or_insert_with(|| {
                    collections::Metadata::new(
                        self._shadow_root
                            .as_ref()
                            .map(|_shadow_root| _shadow_root.meta_rc()),
                    )
                })
            })
        }

        pub fn is_readonly(&self) -> bool {
            unimplemented!()
        }

        pub fn make_readonly(&self) -> &Self {
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

        pub fn pushd(&self, path: Option<&str>) {
            unimplemented!()
        }

        pub fn popd(&self) {
            unimplemented!()
        }

        pub fn apply(&self, files: &FileSet) {
            unimplemented!()
        }

        pub fn mount_sync(&self, source: &str, target: &str, resolver: Rc<FileSystemResolver>) {
            unimplemented!()
        }

        pub fn rimraf_sync(&self, path: &str) {
            unimplemented!()
        }

        pub fn mkdirp_sync(&self, path: &str) {
            unimplemented!()
        }

        pub fn link_sync(&self, oldpath: &str, newpath: &str) {
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

        fn _apply_files(&self, files: &FileSet, dirname: &str) {
            let mut deferred: Vec<(FileSetValue /*Symlink | Link | Mount*/, String)> = vec![];
            self._apply_files_worker(files, dirname, &mut deferred);
            for (entry, path) in deferred {
                self.mkdirp_sync(&vpath::dirname(&path));
                self.pushd(Some(&vpath::dirname(&path)));
                match entry {
                    FileSetValue::Symlink(entry) => {
                        if (self.string_comparer)(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be symbolic links.");
                        }
                        self.symlink_sync(&vpath::resolve(dirname, &[Some(&entry.symlink)]), &path);
                        self._apply_file_extended_options(&path, entry.meta.as_ref());
                    }
                    FileSetValue::Link(entry) => {
                        if (self.string_comparer)(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be hard links.");
                        }
                        self.link_sync(&entry.path, &path);
                    }
                    FileSetValue::Mount(entry) => {
                        self.mount_sync(&entry.source, &path, entry.resolver.clone());
                        self._apply_file_extended_options(&path, entry.meta.as_ref());
                    }
                    _ => unreachable!(),
                }
                self.popd();
            }
        }

        fn _apply_file_extended_options(
            &self,
            path: &str,
            entry_meta: Option<&HashMap<String, ()>>,
        ) {
            unimplemented!()
        }

        fn _apply_files_worker(
            &self,
            files: &FileSet,
            dirname: &str,
            deferred: &mut Vec<(FileSetValue /*Symlink | Link | Mount*/, String)>,
        ) {
            for (key, value) in files {
                let value = normalize_file_set_entry(value.as_ref());
                let path = if !dirname.is_empty() {
                    vpath::resolve(dirname, &[Some(key)])
                } else {
                    key.clone()
                };
                vpath::validate(&path, Some(vpath::ValidationFlags::Absolute));

                match value.as_deref() {
                    None | Some(FileSetValue::Rmdir(_)) | Some(FileSetValue::Unlink(_)) => {
                        if (self.string_comparer)(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be deleted.");
                        }
                        self.rimraf_sync(&path);
                    }
                    Some(FileSetValue::File(value)) => {
                        if (self.string_comparer)(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be files.");
                        }
                        self.mkdirp_sync(&vpath::dirname(&path));
                        self.write_file_sync(&path, &value.data, value.encoding.as_deref());
                        self._apply_file_extended_options(&path, value.meta.as_ref());
                    }
                    Some(FileSetValue::Directory(value)) => {
                        self.mkdirp_sync(&path);
                        self._apply_file_extended_options(&path, value.meta.as_ref());
                        self._apply_files_worker(&value.files, &path, deferred);
                    }
                    Some(value) => {
                        deferred.push((value.clone(), path));
                    }
                }
            }
        }
    }

    #[derive(Default)]
    struct FileSystemLazy {
        links: Option<collections::SortedMap<String, Inode>>,
        shadows: Option<HashMap<usize, Inode>>,
        meta: Rc<RefCell<Option<collections::Metadata<String>>>>,
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
    pub struct FileSystemOptions {
        pub time: Option<TimestampOrNowOrSystemTimeOrCallback>,
        pub files: Option<FileSet>,
        pub cwd: Option<String>,
        pub meta: Option<HashMap<String, String>>,
    }

    #[derive(Default)]
    pub struct FileSystemCreateOptions {
        pub time: Option<TimestampOrNowOrSystemTimeOrCallback>,
        pub files: Option<FileSet>,
        pub cwd: Option<String>,
        pub meta: Option<HashMap<String, String>>,
        pub documents: Option<Vec<Rc<documents::TextDocument>>>,
    }

    pub struct FileSystemResolver {
        pub host: Rc<dyn FileSystemResolverHost>,
    }

    pub trait FileSystemResolverHost {
        fn get_workspace_root(&self) -> String;
    }

    pub fn create_resolver(host: Rc<dyn FileSystemResolverHost>) -> FileSystemResolver {
        FileSystemResolver { host }
    }

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
                fs.meta_mut().set(key, meta.get(key).unwrap().clone());
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
                    .set("document", document.clone());
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

    impl From<FileSet> for FileSetValue {
        fn from(value: FileSet) -> Self {
            Self::FileSet(value)
        }
    }

    impl From<Directory> for FileSetValue {
        fn from(value: Directory) -> Self {
            Self::Directory(value)
        }
    }

    impl From<File> for FileSetValue {
        fn from(value: File) -> Self {
            Self::File(value)
        }
    }

    impl From<String> for FileSetValue {
        fn from(value: String) -> Self {
            Self::String(value)
        }
    }

    impl From<Link> for FileSetValue {
        fn from(value: Link) -> Self {
            Self::Link(value)
        }
    }

    impl From<Symlink> for FileSetValue {
        fn from(value: Symlink) -> Self {
            Self::Symlink(value)
        }
    }

    impl From<Mount> for FileSetValue {
        fn from(value: Mount) -> Self {
            Self::Mount(value)
        }
    }

    impl From<Rmdir> for FileSetValue {
        fn from(value: Rmdir) -> Self {
            Self::Rmdir(value)
        }
    }

    impl From<Unlink> for FileSetValue {
        fn from(value: Unlink) -> Self {
            Self::Unlink(value)
        }
    }

    #[derive(Clone)]
    pub struct Directory {
        pub files: FileSet,
        pub meta: Option<HashMap<String, ()>>,
    }

    impl Directory {
        pub fn new(files: FileSet, options: Option<DirectoryOptions>) -> Self {
            let options = options.unwrap_or_default();
            let DirectoryOptions { meta } = options;
            Self { files, meta }
        }
    }

    #[derive(Default)]
    pub struct DirectoryOptions {
        pub meta: Option<HashMap<String, ()>>,
    }

    #[derive(Clone)]
    pub struct File {
        pub data: String, /*Buffer | string*/
        pub encoding: Option<String>,
        pub meta: Option<HashMap<String, ()>>,
    }

    impl File {
        pub fn new(data: String /*Buffer | string*/, options: Option<FileOptions>) -> Self {
            let options = options.unwrap_or_default();
            let FileOptions { meta, encoding } = options;
            Self {
                data,
                encoding,
                meta,
            }
        }
    }

    #[derive(Default)]
    pub struct FileOptions {
        pub encoding: Option<String>,
        pub meta: Option<HashMap<String, ()>>,
    }

    #[derive(Clone)]
    pub struct Link {
        pub path: String,
    }

    impl Link {
        pub fn new(path: String) -> Self {
            Self { path }
        }
    }

    #[derive(Clone)]
    pub struct Rmdir {}

    #[derive(Clone)]
    pub struct Unlink {}

    #[derive(Clone)]
    pub struct Symlink {
        pub symlink: String,
        pub meta: Option<HashMap<String, ()>>,
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
        meta: Option<HashMap<String, ()>>,
    }

    #[derive(Clone)]
    pub struct Mount {
        pub source: String,
        pub resolver: Rc<FileSystemResolver>,
        pub meta: Option<HashMap<String, ()>>,
    }

    impl Mount {
        pub fn new(
            source: String,
            resolver: Rc<FileSystemResolver>,
            options: Option<MountOptions>,
        ) -> Self {
            let options = options.unwrap_or_default();
            let MountOptions { meta } = options;
            Self {
                source,
                resolver,
                meta,
            }
        }
    }

    #[derive(Default)]
    pub struct MountOptions {
        pub meta: Option<HashMap<String, ()>>,
    }

    enum Inode {
        FileInode(FileInode),
        DirectoryInode(DirectoryInode),
        SymlinkInode(SymlinkInode),
    }

    struct FileInode {}

    struct DirectoryInode {}

    struct SymlinkInode {}

    thread_local! {
        static built_local_host_: RefCell<Option<Rc<dyn FileSystemResolverHost>>> = RefCell::new(None);
        static built_local_ci_: RefCell<Option<Rc<FileSystem>>> = RefCell::new(None);
        static built_local_cs_: RefCell<Option<Rc<FileSystem>>> = RefCell::new(None);
    }

    fn maybe_built_local_host() -> Option<Rc<dyn FileSystemResolverHost>> {
        built_local_host_.with(|built_local_host| built_local_host.borrow().clone())
    }

    fn set_built_local_host(value: Option<Rc<dyn FileSystemResolverHost>>) {
        built_local_host_.with(|built_local_host| {
            *built_local_host.borrow_mut() = value;
        })
    }

    fn maybe_built_local_ci() -> Option<Rc<FileSystem>> {
        built_local_ci_.with(|built_local_ci| built_local_ci.borrow().clone())
    }

    fn set_built_local_ci(value: Option<Rc<FileSystem>>) {
        built_local_ci_.with(|built_local_ci| {
            *built_local_ci.borrow_mut() = value;
        })
    }

    fn maybe_built_local_cs() -> Option<Rc<FileSystem>> {
        built_local_cs_.with(|built_local_cs| built_local_cs.borrow().clone())
    }

    fn set_built_local_cs(value: Option<Rc<FileSystem>>) {
        built_local_cs_.with(|built_local_cs| {
            *built_local_cs.borrow_mut() = value;
        })
    }

    fn get_built_local(host: Rc<dyn FileSystemResolverHost>, ignore_case: bool) -> Rc<FileSystem> {
        if !matches!(
            maybe_built_local_host().as_ref(),
            Some(built_local_host) if Rc::ptr_eq(
                built_local_host,
                &host,
            )
        ) {
            set_built_local_ci(None);
            set_built_local_cs(None);
            set_built_local_host(Some(host.clone()));
        }
        if maybe_built_local_ci().is_none() {
            let resolver = Rc::new(create_resolver(host.clone()));
            set_built_local_ci(Some(Rc::new(FileSystem::new(
                true,
                Some(FileSystemOptions {
                    files: Some(FileSet::from_iter(IntoIterator::into_iter([
                        (
                            built_folder.to_owned(),
                            Some(
                                Mount::new(
                                    vpath::resolve(
                                        &host.get_workspace_root(),
                                        &[Some("built/local")],
                                    ),
                                    resolver.clone(),
                                    None,
                                )
                                .into(),
                            ),
                        ),
                        (
                            test_lib_folder.to_owned(),
                            Some(
                                Mount::new(
                                    vpath::resolve(
                                        &host.get_workspace_root(),
                                        &[Some("tests/lib")],
                                    ),
                                    resolver.clone(),
                                    None,
                                )
                                .into(),
                            ),
                        ),
                        (
                            projects_folder.to_owned(),
                            Some(
                                Mount::new(
                                    vpath::resolve(
                                        &host.get_workspace_root(),
                                        &[Some("tests/projects")],
                                    ),
                                    resolver.clone(),
                                    None,
                                )
                                .into(),
                            ),
                        ),
                        (src_folder.to_owned(), Some(FileSet::new().into())),
                    ]))),
                    cwd: Some(src_folder.to_owned()),
                    meta: Some(HashMap::from_iter(IntoIterator::into_iter([(
                        "defaultLibLocation".to_owned(),
                        built_folder.to_owned(),
                    )]))),
                    time: None,
                }),
            ))));
            maybe_built_local_ci().unwrap().make_readonly();
        }
        if ignore_case {
            return maybe_built_local_ci().unwrap();
        }
        if maybe_built_local_cs().is_none() {
            set_built_local_cs(Some(Rc::new(
                maybe_built_local_ci().unwrap().shadow(Some(false)),
            )));
            maybe_built_local_cs().unwrap().make_readonly();
        }
        maybe_built_local_cs().unwrap()
    }

    fn normalize_file_set_entry(value: Option<&FileSetValue>) -> Option<Cow<'_, FileSetValue>> {
        let value = value?;
        if matches!(
            value,
            FileSetValue::Directory(_)
                | FileSetValue::File(_)
                | FileSetValue::Link(_)
                | FileSetValue::Symlink(_)
                | FileSetValue::Mount(_)
                | FileSetValue::Rmdir(_)
                | FileSetValue::Unlink(_)
        ) {
            return Some(Cow::Borrowed(value));
        }
        Some(match value {
            FileSetValue::String(value) /*|| Buffer.isBuffer(value)*/ => Cow::Owned(File::new(value.clone(), None).into()),
            FileSetValue::FileSet(value) => Cow::Owned(Directory::new(value.clone(), None).into()),
            _ => unreachable!(),
        })
    }
}
