pub mod vfs {
    use local_macros::enum_unwrapped;
    use std::borrow::Cow;
    use std::cell::{Cell, Ref, RefCell, RefMut};
    use std::collections::HashMap;
    use std::iter::FromIterator;
    use std::rc::Rc;
    use std::time::{SystemTime, UNIX_EPOCH};
    use typescript_rust::Comparison;

    use crate::{collections, documents, vpath};

    pub const built_folder: &'static str = "/.ts";

    pub const projects_folder: &'static str = "/.projects";

    pub const test_lib_folder: &'static str = "/.lib";

    pub const src_folder: &'static str = "/.src";

    const S_IFMT: u32 = 0o170000;
    const S_IFSOCK: u32 = 0o140000;
    const S_IFLNK: u32 = 0o120000;
    const S_IFREG: u32 = 0o100000;
    const S_IFBLK: u32 = 0o060000;
    const S_IFDIR: u32 = 0o040000;
    const S_IFCHR: u32 = 0o020000;
    const S_IFIFO: u32 = 0o010000;

    thread_local! {
        static dev_count_: Cell<u32> = Cell::new(0);
    }

    fn incremented_dev_count() -> u32 {
        dev_count_.with(|dev_count| {
            let dev_count_new = dev_count.get() + 1;
            dev_count.set(dev_count_new);
            dev_count_new
        })
    }

    thread_local! {
        static ino_count_: Cell<u32> = Cell::new(0);
    }

    fn incremented_ino_count() -> u32 {
        ino_count_.with(|ino_count| {
            let ino_count_new = ino_count.get() + 1;
            ino_count.set(ino_count_new);
            ino_count_new
        })
    }

    pub struct FileSystem {
        pub ignore_case: bool,
        pub string_comparer: Rc<dyn Fn(&str, &str) -> Comparison>,
        _lazy: FileSystemLazy,

        _cwd: RefCell<Option<String>>,
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
                _cwd: RefCell::new(None),
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
                if let Some(lazy_links) = ret._lazy.links.borrow().as_ref() {
                    for name in lazy_links.borrow().keys() {
                        cwd = Some(if let Some(cwd_present) = cwd {
                            vpath::resolve(name, &[Some(&cwd_present)])
                        } else {
                            name.to_owned()
                        });
                        break;
                    }
                }
            }

            if let Some(cwd) = cwd.as_ref().filter(|cwd| !cwd.is_empty()) {
                vpath::validate(cwd, Some(vpath::ValidationFlags::Absolute));
                ret.mkdirp_sync(cwd);
            }

            ret.set_cwd(Some(cwd.unwrap_or_else(|| "".to_owned())));

            ret
        }

        pub fn maybe_cwd(&self) -> Ref<Option<String>> {
            self._cwd.borrow()
        }

        pub fn set_cwd(&self, _cwd: Option<String>) {
            *self._cwd.borrow_mut() = _cwd;
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

        pub fn time(&self) -> u128 {
            let result = match self._time.borrow().clone() {
                TimestampOrNowOrSystemTimeOrCallback::Callback(_time) => _time(),
                TimestampOrNowOrSystemTimeOrCallback::Timestamp(_time) => _time.into(),
                TimestampOrNowOrSystemTimeOrCallback::Now => TimestampOrNowOrSystemTime::Now,
                TimestampOrNowOrSystemTimeOrCallback::SystemTime(_time) => _time.into(),
            };
            match result {
                TimestampOrNowOrSystemTime::Timestamp(_time) => _time,
                TimestampOrNowOrSystemTime::Now => SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_millis(),
                TimestampOrNowOrSystemTime::SystemTime(_time) => {
                    _time.duration_since(UNIX_EPOCH).unwrap().as_millis()
                }
            }
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
            let path = self._resolve(path);
            let result = self
                ._walk(
                    &path,
                    Some(true),
                    Some(|error: &NodeJSErrnoException, result: WalkResult| {
                        if error.code.as_deref() == Some("ENOENT") {
                            self._mkdir(result);
                            return OnErrorReturn::Retry;
                        }
                        OnErrorReturn::Throw
                    }),
                )
                .unwrap();

            if result.node.is_none() {
                self._mkdir(result);
            }
        }

        pub fn _mkdir(
            &self,
            WalkResult {
                parent,
                links,
                node: existing_node,
                basename,
                ..
            }: WalkResult,
        ) {
            if existing_node.is_some() {
                // throw createIOError("EEXIST");
                panic!("EEXIST");
            }
            let time = self.time();
            let node = self._mknod(
                if let Some(parent) = parent.as_ref() {
                    parent.dev()
                } else {
                    incremented_dev_count()
                },
                S_IFDIR,
                0o777,
                Some(time),
            );
            self._add_link(
                parent.as_deref(),
                &mut links.borrow_mut(),
                &basename,
                Rc::new(node),
                Some(time),
            );
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

        fn _mknod(&self, dev: u32, type_: u32, mode: u32, time: Option<u128>) -> Inode {
            let time = time.unwrap_or_else(|| self.time());
            match type_ {
                S_IFREG => FileInode::new(
                    dev,
                    incremented_ino_count(),
                    (mode & !S_IFMT & !0o022 & 0o7777) | (type_ & S_IFMT),
                    time,
                    time,
                    time,
                    time,
                    0,
                )
                .into(),
                S_IFDIR => DirectoryInode::new(
                    dev,
                    incremented_ino_count(),
                    (mode & !S_IFMT & !0o022 & 0o7777) | (type_ & S_IFMT),
                    time,
                    time,
                    time,
                    time,
                    0,
                )
                .into(),
                S_IFLNK => SymlinkInode::new(
                    dev,
                    incremented_ino_count(),
                    (mode & !S_IFMT & !0o022 & 0o7777) | (type_ & S_IFMT),
                    time,
                    time,
                    time,
                    time,
                    0,
                )
                .into(),
                _ => unreachable!(),
            }
        }

        fn _add_link(
            &self,
            parent: Option<&Inode /*DirectoryInode*/>,
            links: &mut collections::SortedMap<String, Rc<Inode>>,
            name: &str,
            node: Rc<Inode>,
            time: Option<u128>,
        ) {
            let time = time.unwrap_or_else(|| self.time());
            links.set(name.to_owned(), node.clone());
            node.increment_nlink();
            node.set_ctime_ms(time);
            if let Some(parent) = parent {
                parent.set_mtime_ms(time);
            }
            if parent.is_none()
                && self
                    .maybe_cwd()
                    .as_ref()
                    .filter(|_cwd| !_cwd.is_empty())
                    .is_none()
            {
                self.set_cwd(Some(name.to_owned()));
            }
        }

        fn _get_root_links(&self) -> Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>> {
            self._lazy
                .links
                .borrow_mut()
                .get_or_insert_with(|| {
                    let mut _lazy_links = collections::SortedMap::new(
                        collections::SortOptions {
                            comparer: {
                                let string_comparer = self.string_comparer.clone();
                                Rc::new(move |a: &String, b: &String| string_comparer(a, b))
                            },
                            sort: None,
                        },
                        Option::<HashMap<String, Rc<Inode>>>::None,
                    );
                    if let Some(_shadow_root) = self._shadow_root.as_ref() {
                        self._copy_shadow_links(
                            _shadow_root._get_root_links().borrow().entries(),
                            &mut _lazy_links,
                        );
                    }
                    Rc::new(RefCell::new(_lazy_links))
                })
                .clone()
        }

        fn _get_links(
            &self,
            node: &Inode, /*DirectoryInode*/
        ) -> Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>> {
            let node_as_directory_inode = node.as_directory_inode();
            if node_as_directory_inode.maybe_links().is_none() {
                let mut links = collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: Rc::new({
                            let string_comparer = self.string_comparer.clone();
                            move |a: &String, b: &String| string_comparer(a, b)
                        }),
                        sort: None,
                    },
                    Option::<HashMap<String, Rc<Inode>>>::None,
                );
                let source = node_as_directory_inode.maybe_source();
                let resolver = node_as_directory_inode.maybe_resolver();
                if let (Some(source), Some(resolver)) = (source, resolver) {
                    node_as_directory_inode.set_source(None);
                    node_as_directory_inode.set_resolver(None);
                    for name in resolver.readdir_sync(&source) {
                        let path = vpath::combine(&source, &[Some(&name)]);
                        let stats = resolver.stat_sync(&path);
                        match stats.mode & S_IFMT {
                            S_IFDIR => {
                                let dir =
                                    self._mknod(node_as_directory_inode.dev, S_IFDIR, 0o777, None);
                                dir.as_directory_inode()
                                    .set_source(Some(vpath::combine(&source, &[Some(&name)])));
                                dir.as_directory_inode()
                                    .set_resolver(Some(resolver.clone()));
                                self._add_link(Some(node), &mut links, &name, Rc::new(dir), None);
                            }
                            S_IFREG => {
                                let mut file =
                                    self._mknod(node_as_directory_inode.dev, S_IFREG, 0o666, None);
                                file.as_file_inode()
                                    .set_source(Some(vpath::combine(&source, &[Some(&name)])));
                                file.as_file_inode().set_resolver(Some(resolver.clone()));
                                file.as_file_inode_mut().size = Some(stats.size);
                                self._add_link(Some(node), &mut links, &name, Rc::new(file), None);
                            }
                            _ => (),
                        }
                    }
                } else if let (Some(_shadow_root), Some(node_shadow_root)) = (
                    self._shadow_root.as_ref(),
                    node_as_directory_inode.shadow_root.as_ref(),
                ) {
                    self._copy_shadow_links(
                        _shadow_root._get_links(node_shadow_root).borrow().entries(),
                        &mut links,
                    );
                }
                node_as_directory_inode.set_links(Some(Rc::new(RefCell::new(links))));
            }
            node_as_directory_inode.maybe_links().unwrap()
        }

        fn _copy_shadow_links<
            'source,
            TSource: IntoIterator<Item = (&'source String, &'source Rc<Inode>)>,
        >(
            &self,
            source: TSource,
            target: &mut collections::SortedMap<String, Rc<Inode>>,
        ) {
            unimplemented!()
        }

        fn _walk<TOnError: FnMut(&NodeJSErrnoException, WalkResult) -> OnErrorReturn>(
            &self,
            path: &str,
            no_follow: Option<bool>,
            mut on_error: Option<TOnError>,
        ) -> Option<WalkResult> {
            let mut links = self._get_root_links();
            let mut parent: Option<Rc<Inode>> = None;
            let mut components = vpath::parse(path, None);
            let mut step = 0;
            let mut depth = 0;
            let mut retry = false;
            loop {
                if depth >= 40 {
                    // throw createIOError("ELOOP");
                    panic!("ELOOP");
                }
                let last_step = step == components.len() - 1;
                let mut basename = components[step].clone();
                let node = {
                    let links = links.borrow();
                    let link_entry = links.get_entry(&basename);
                    if let Some(link_entry) = link_entry {
                        components[step] = link_entry.0.clone();
                        basename = link_entry.0.clone();
                    }
                    link_entry.map(|link_entry| link_entry.1.clone())
                };
                if last_step && (no_follow == Some(true) || !is_symlink(node.as_deref())) {
                    return Some(WalkResult {
                        realpath: vpath::format(&components),
                        basename,
                        parent,
                        links: links.clone(),
                        node: node.clone(),
                    });
                }
                if node.is_none() {
                    if self.trap_error(
                        &components,
                        step,
                        &mut retry,
                        on_error.as_mut(),
                        parent.clone(),
                        links.clone(),
                        create_io_error(IOErrorCode::ENOENT, None),
                        node.clone(),
                    ) {
                        continue;
                    }
                    return None;
                }
                let ref node = node.unwrap();
                if is_symlink(Some(node)) {
                    let dirname = vpath::format(&components[..step]);
                    let symlink =
                        vpath::resolve(&dirname, &[Some(node.as_symlink_inode().symlink())]);
                    links = self._get_root_links();
                    parent = None;
                    let mut components_new = vpath::parse(&symlink, None);
                    components_new.extend_from_slice(&components[step + 1..]);
                    components = components_new;
                    step = 0;
                    depth += 1;
                    retry = false;
                    continue;
                }
                if is_directory(Some(node)) {
                    links = self._get_links(node);
                    parent = Some(node.clone());
                    step += 1;
                    retry = false;
                    continue;
                }
                if self.trap_error(
                    &components,
                    step,
                    &mut retry,
                    on_error.as_mut(),
                    parent.clone(),
                    links.clone(),
                    create_io_error(IOErrorCode::ENOTDIR, None),
                    Some(node.clone()),
                ) {
                    continue;
                }
                return None;
            }
        }

        fn trap_error<TOnError: FnMut(&NodeJSErrnoException, WalkResult) -> OnErrorReturn>(
            &self,
            components: &[String],
            step: usize,
            retry: &mut bool,
            on_error: Option<&mut TOnError>,
            parent: Option<Rc<Inode /*DirectoryInode*/>>,
            links: Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>>,
            error: NodeJSErrnoException,
            node: Option<Rc<Inode>>,
        ) -> bool {
            let realpath = vpath::format(&components[..step + 1]);
            let basename = &components[step];
            let result = if !*retry && on_error.is_some() {
                (on_error.unwrap())(
                    &error,
                    WalkResult {
                        realpath,
                        basename: basename.clone(),
                        parent: parent.clone(),
                        links,
                        node,
                    },
                )
            } else {
                OnErrorReturn::Throw
            };
            if result == OnErrorReturn::Stop {
                return false;
            }
            if result == OnErrorReturn::Retry {
                *retry = true;
                return true;
            }
            panic!("{}", error.message);
        }

        fn _resolve(&self, path: &str) -> String {
            if let Some(_cwd) = self.maybe_cwd().as_ref().filter(|_cwd| !_cwd.is_empty()) {
                vpath::resolve(
                    _cwd,
                    &[Some(&vpath::validate(
                        path,
                        Some(
                            vpath::ValidationFlags::RelativeOrAbsolute
                                | vpath::ValidationFlags::AllowWildcard,
                        ),
                    ))],
                )
            } else {
                vpath::validate(
                    path,
                    Some(vpath::ValidationFlags::Absolute | vpath::ValidationFlags::AllowWildcard),
                )
            }
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

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum OnErrorReturn {
        Stop,
        Retry,
        Throw,
    }

    #[derive(Default)]
    struct FileSystemLazy {
        links: RefCell<Option<Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>>>>,
        shadows: Option<HashMap<usize, Rc<Inode>>>,
        meta: Rc<RefCell<Option<collections::Metadata<String>>>>,
    }

    #[derive(Clone)]
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

    impl FileSystemResolver {
        pub fn stat_sync(&self, path: &str) -> FileSystemResolverStats {
            unimplemented!()
        }

        pub fn readdir_sync(&self, path: &str) -> Vec<String> {
            unimplemented!()
        }
    }

    pub struct FileSystemResolverStats {
        pub mode: u32,
        pub size: usize,
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

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum IOErrorCode {
        EACCES,
        EIO,
        ENOENT,
        EEXIST,
        ELOOP,
        ENOTDIR,
        EISDIR,
        EBADF,
        EINVAL,
        ENOTEMPTY,
        EPERM,
        EROFS,
    }

    impl IOErrorCode {
        pub fn message(&self) -> &'static str {
            match self {
                Self::EACCES => "access denied",
                Self::EIO => "an I/O error occurred",
                Self::ENOENT => "no such file or directory",
                Self::EEXIST => "file already exists",
                Self::ELOOP => "too many symbolic links encountered",
                Self::ENOTDIR => "no such directory",
                Self::EISDIR => "path is a directory",
                Self::EBADF => "invalid file descriptor",
                Self::EINVAL => "invalid value",
                Self::ENOTEMPTY => "directory not empty",
                Self::EPERM => "operation not permitted",
                Self::EROFS => "file system is read-only",
            }
        }
    }

    pub fn create_io_error(code: IOErrorCode, details: Option<String>) -> NodeJSErrnoException {
        let details = details.unwrap_or_else(|| "".to_owned());
        let err = NodeJSErrnoException {
            message: format!("{code:?}: {} {}", code.message(), details),
            code: Some(format!("{code:?}")),
        };
        // if (Error.captureStackTrace) Error.captureStackTrace(err, createIOError);
        err
    }

    pub struct NodeJSErrnoException {
        pub message: String,
        pub code: Option<String>,
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

    #[derive(Clone)]
    pub enum Inode {
        FileInode(FileInode),
        DirectoryInode(DirectoryInode),
        SymlinkInode(SymlinkInode),
    }

    impl Inode {
        pub fn dev(&self) -> u32 {
            unimplemented!()
        }

        pub fn mode(&self) -> u32 {
            unimplemented!()
        }

        pub fn increment_nlink(&self) {
            unimplemented!()
        }

        pub fn set_ctime_ms(&self, ctime_ms: u128) {
            unimplemented!()
        }

        pub fn set_mtime_ms(&self, mtime_ms: u128) {
            unimplemented!()
        }

        pub fn as_file_inode(&self) -> &FileInode {
            enum_unwrapped!(self, [Inode, FileInode])
        }

        pub fn as_file_inode_mut(&mut self) -> &mut FileInode {
            match self {
                Self::FileInode(value) => value,
                _ => unreachable!(),
            }
        }

        pub fn as_directory_inode(&self) -> &DirectoryInode {
            enum_unwrapped!(self, [Inode, DirectoryInode])
        }

        pub fn as_symlink_inode(&self) -> &SymlinkInode {
            enum_unwrapped!(self, [Inode, SymlinkInode])
        }
    }

    impl From<FileInode> for Inode {
        fn from(value: FileInode) -> Self {
            Self::FileInode(value)
        }
    }

    impl From<DirectoryInode> for Inode {
        fn from(value: DirectoryInode) -> Self {
            Self::DirectoryInode(value)
        }
    }

    impl From<SymlinkInode> for Inode {
        fn from(value: SymlinkInode) -> Self {
            Self::SymlinkInode(value)
        }
    }

    #[derive(Clone)]
    pub struct FileInode {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub atime_ms: u128,
        pub mtime_ms: u128,
        pub ctime_ms: u128,
        pub birthtime_ms: u128,
        pub nlink: usize,
        pub size: Option<usize>,
        // buffer?: Buffer;
        source: RefCell<Option<String>>,
        resolver: RefCell<Option<Rc<FileSystemResolver>>>,
        shadow_root: Option<Rc<Inode /*FileInode*/>>,
        meta: Option<Rc<collections::Metadata<()>>>,
    }

    impl FileInode {
        pub fn new(
            dev: u32,
            ino: u32,
            mode: u32,
            atime_ms: u128,
            mtime_ms: u128,
            ctime_ms: u128,
            birthtime_ms: u128,
            nlink: usize,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                atime_ms,
                mtime_ms,
                ctime_ms,
                birthtime_ms,
                nlink,
                size: None,
                source: RefCell::new(None),
                resolver: RefCell::new(None),
                shadow_root: None,
                meta: None,
            }
        }

        pub fn maybe_source(&self) -> Option<String> {
            self.source.borrow().clone()
        }

        pub fn set_source(&self, source: Option<String>) {
            *self.source.borrow_mut() = source;
        }

        pub fn maybe_resolver(&self) -> Option<Rc<FileSystemResolver>> {
            self.resolver.borrow().clone()
        }

        pub fn set_resolver(&self, resolver: Option<Rc<FileSystemResolver>>) {
            *self.resolver.borrow_mut() = resolver;
        }
    }

    #[derive(Clone)]
    pub struct DirectoryInode {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub atime_ms: u128,
        pub mtime_ms: u128,
        pub ctime_ms: u128,
        pub birthtime_ms: u128,
        pub nlink: usize,
        links: RefCell<Option<Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>>>>,
        source: RefCell<Option<String>>,
        resolver: RefCell<Option<Rc<FileSystemResolver>>>,
        pub shadow_root: Option<Rc<Inode /*DirectoryInode*/>>,
        meta: Option<Rc<collections::Metadata<()>>>,
    }

    impl DirectoryInode {
        pub fn new(
            dev: u32,
            ino: u32,
            mode: u32,
            atime_ms: u128,
            mtime_ms: u128,
            ctime_ms: u128,
            birthtime_ms: u128,
            nlink: usize,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                atime_ms,
                mtime_ms,
                ctime_ms,
                birthtime_ms,
                nlink,
                links: RefCell::new(None),
                source: RefCell::new(None),
                resolver: RefCell::new(None),
                shadow_root: None,
                meta: None,
            }
        }

        pub fn maybe_links(
            &self,
        ) -> Option<Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>>> {
            self.links.borrow().clone()
        }

        pub fn set_links(
            &self,
            links: Option<Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>>>,
        ) {
            *self.links.borrow_mut() = links;
        }

        pub fn maybe_source(&self) -> Option<String> {
            self.source.borrow().clone()
        }

        pub fn set_source(&self, source: Option<String>) {
            *self.source.borrow_mut() = source;
        }

        pub fn maybe_resolver(&self) -> Option<Rc<FileSystemResolver>> {
            self.resolver.borrow().clone()
        }

        pub fn set_resolver(&self, resolver: Option<Rc<FileSystemResolver>>) {
            *self.resolver.borrow_mut() = resolver;
        }
    }

    #[derive(Clone)]
    pub struct SymlinkInode {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub atime_ms: u128,
        pub mtime_ms: u128,
        pub ctime_ms: u128,
        pub birthtime_ms: u128,
        pub nlink: usize,
        pub symlink: Option<String>,
        pub shadow_root: Option<Rc<Inode /*SymlinkInode*/>>,
        meta: Option<Rc<collections::Metadata<()>>>,
    }

    impl SymlinkInode {
        pub fn new(
            dev: u32,
            ino: u32,
            mode: u32,
            atime_ms: u128,
            mtime_ms: u128,
            ctime_ms: u128,
            birthtime_ms: u128,
            nlink: usize,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                atime_ms,
                mtime_ms,
                ctime_ms,
                birthtime_ms,
                nlink,
                symlink: None,
                shadow_root: None,
                meta: None,
            }
        }

        pub fn symlink(&self) -> &String {
            self.symlink.as_ref().unwrap()
        }
    }

    fn is_directory(node: Option<&Inode>) -> bool {
        matches!(
            node,
            Some(node) if node.mode() & S_IFMT == S_IFDIR
        )
    }

    fn is_symlink(node: Option<&Inode>) -> bool {
        matches!(
            node,
            Some(node) if node.mode() & S_IFMT == S_IFLNK
        )
    }

    pub struct WalkResult {
        pub realpath: String,
        pub basename: String,
        pub parent: Option<Rc<Inode>>,
        pub links: Rc<RefCell<collections::SortedMap<String, Rc<Inode>>>>,
        pub node: Option<Rc<Inode>>,
    }

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
