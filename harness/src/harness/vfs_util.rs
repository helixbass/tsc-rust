pub mod vfs {
    use std::{
        borrow::Cow,
        cell::{Cell, Ref, RefCell, RefMut},
        collections::HashMap,
        io,
        iter::FromIterator,
        time::{SystemTime, UNIX_EPOCH},
    };

    use derive_builder::Builder;
    use gc::{Finalize, Trace};
    use local_macros::enum_unwrapped;
    use typescript_rust::{
        debug_cell, get_sys, id_arena::Id, io_error_from_name, is_option_str_empty,
        millis_since_epoch_to_system_time, AllArenas, Buffer, Comparison, FileSystemEntries,
        HasArena, InArena, Node,
    };

    use crate::{
        collections, collections::SortOptionsComparer, documents, vpath, AllArenasHarness,
        HasArenaHarness, IdForFileSystemResolverHost, InArenaHarness, OptionInArenaHarness,
    };

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

    pub trait StringComparer: Trace + Finalize {
        fn call(&self, a: &str, b: &str) -> Comparison;
    }

    #[derive(Trace, Finalize)]
    pub struct StringComparerFileSystem {
        ignore_case: bool,
    }

    impl StringComparerFileSystem {
        pub fn new(ignore_case: bool) -> Self {
            Self { ignore_case }
        }
    }

    impl StringComparer for StringComparerFileSystem {
        fn call(&self, a: &str, b: &str) -> Comparison {
            if self.ignore_case {
                vpath::compare_case_insensitive(a, b)
            } else {
                vpath::compare_case_sensitive(a, b)
            }
        }
    }

    #[derive(Trace, Finalize)]
    pub struct SortOptionsComparerFromStringComparer {
        string_comparer: Id<Box<dyn StringComparer>>,
    }

    impl SortOptionsComparerFromStringComparer {
        pub fn new(string_comparer: Id<Box<dyn StringComparer>>) -> Self {
            Self { string_comparer }
        }
    }

    impl SortOptionsComparer<String> for SortOptionsComparerFromStringComparer {
        fn call(&self, a: &String, b: &String) -> Comparison {
            self.string_comparer.ref_(self).call(a, b)
        }
    }

    impl HasArena for SortOptionsComparerFromStringComparer {
        fn arena(&self) -> &AllArenas {
            unimplemented!()
        }
    }

    impl HasArenaHarness for SortOptionsComparerFromStringComparer {
        fn arena_harness(&self) -> &AllArenasHarness {
            unimplemented!()
        }
    }

    #[derive(Trace, Finalize)]
    pub struct FileSystem {
        pub ignore_case: bool,
        pub string_comparer: Id<Box<dyn StringComparer>>,
        _lazy: FileSystemLazy,

        #[unsafe_ignore_trace]
        _cwd: RefCell<Option<String>>,
        #[unsafe_ignore_trace]
        _time: RefCell<TimestampOrNowOrSystemTimeOrCallback>,
        _shadow_root: Option<Id<FileSystem>>,
        #[unsafe_ignore_trace]
        _dir_stack: RefCell<Option<Vec<String>>>,
        #[unsafe_ignore_trace]
        _is_readonly: Cell<bool>,
    }

    impl FileSystem {
        pub fn new(
            ignore_case: bool,
            options: Option<FileSystemOptions>,
            arena: &impl HasArenaHarness,
        ) -> io::Result<Self> {
            let options = options.unwrap_or_default();
            let FileSystemOptions {
                time,
                files,
                meta,
                cwd: options_cwd,
            } = options;
            let time = time.unwrap_or(TimestampOrNowOrSystemTimeOrCallback::Now);

            let ret = Self {
                ignore_case,
                string_comparer: arena
                    .alloc_string_comparer(Box::new(StringComparerFileSystem::new(ignore_case))),
                _time: RefCell::new(time),
                _cwd: Default::default(),
                _dir_stack: Default::default(),
                _lazy: Default::default(),
                _shadow_root: Default::default(),
                _is_readonly: Default::default(),
            };

            if let Some(meta) = meta {
                for (key, value) in meta {
                    ret.meta_mut().set(&key, value);
                }
            }

            if let Some(files) = files {
                ret._apply_files(&files, "")?;
            }

            let mut cwd = options_cwd;
            if match cwd.as_ref() {
                None => true,
                Some(cwd) => cwd.is_empty() || !vpath::is_root(cwd),
            } {
                if let Some(lazy_links) = ret._lazy.links.get() {
                    for name in lazy_links.ref_(arena).keys() {
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
                ret.mkdirp_sync(cwd)?;
            }

            ret.set_cwd(Some(cwd.unwrap_or_else(|| "".to_owned())));

            Ok(ret)
        }

        fn maybe_cwd(&self) -> Ref<Option<String>> {
            self._cwd.borrow()
        }

        fn set_cwd(&self, _cwd: Option<String>) {
            *self._cwd.borrow_mut() = _cwd;
        }

        fn maybe_dir_stack_mut(&self) -> RefMut<Option<Vec<String>>> {
            self._dir_stack.borrow_mut()
        }

        pub fn meta_id(&self) -> Id<collections::Metadata<String>> {
            if self._lazy.meta.get().is_none() {
                self._lazy.meta.set(Some(
                    self.alloc_metadata_string(collections::Metadata::new(
                        self._shadow_root
                            .as_ref()
                            .map(|_shadow_root| _shadow_root.ref_(self).meta_id()),
                    )),
                ));
            }
            self._lazy.meta.get().unwrap()
        }

        pub fn meta(&self) -> debug_cell::Ref<collections::Metadata<String>> {
            self.meta_id().ref_(self)
        }

        pub fn meta_mut(&self) -> debug_cell::RefMut<collections::Metadata<String>> {
            self.meta_id().ref_mut(self)
        }

        pub fn is_readonly(&self) -> bool {
            // return Object.isFrozen(this);
            self._is_readonly.get()
        }

        pub fn make_readonly(&self) -> &Self {
            // Object.freeze(this);
            self._is_readonly.set(true);
            self
        }

        pub fn shadow_root(&self) -> Option<Id<FileSystem>> {
            self._shadow_root.clone()
        }

        pub fn shadow(
            this: Id<Self>,
            ignore_case: Option<bool>,
            arena: &impl HasArenaHarness,
        ) -> io::Result<Self> {
            let ignore_case = ignore_case.unwrap_or(this.ref_(arena).ignore_case);
            if !this.ref_(arena).is_readonly() {
                panic!("Cannot shadow a mutable file system.");
            }
            if ignore_case && !this.ref_(arena).ignore_case {
                panic!("Cannot create a case-insensitive file system from a case-sensitive one.");
            }
            let mut fs = FileSystem::new(
                ignore_case,
                Some(FileSystemOptions {
                    time: Some(this.ref_(arena)._time.borrow().clone()),
                    files: None,
                    cwd: None,
                    meta: None,
                }),
                arena,
            )?;
            fs._shadow_root = Some(this.clone());
            fs.set_cwd(this.ref_(arena).maybe_cwd().clone());
            Ok(fs)
        }

        pub fn time(&self) -> u128 {
            let result = match self._time.borrow().clone() {
                TimestampOrNowOrSystemTimeOrCallback::Callback(_time) => _time.ref_(self).call(),
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

        pub fn filemeta(&self, path: &str) -> io::Result<Id<collections::Metadata<MetaValue>>> {
            let WalkResult { node, .. } = self
                ._walk(
                    &self._resolve(path),
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if node.is_none() {
                return io_error_from_name("ENOENT");
            }
            let ref node = node.unwrap();
            Ok(self._filemeta(&node.ref_(self)))
        }

        pub fn _filemeta(&self, node: &Inode) -> Id<collections::Metadata<MetaValue>> {
            if node.meta().is_none() {
                node.set_meta({
                    let parent_meta = if let (Some(node_shadow_root), Some(_shadow_root)) = (
                        node.maybe_shadow_root().as_ref(),
                        self._shadow_root.as_ref(),
                    ) {
                        Some(
                            _shadow_root
                                .ref_(self)
                                ._filemeta(&node_shadow_root.ref_(self)),
                        )
                    } else {
                        None
                    };
                    Some(self.alloc_metadata_metavalue(collections::Metadata::new(parent_meta)))
                });
            }
            node.meta().unwrap()
        }

        pub fn cwd(&self) -> io::Result<String> {
            let _cwd = self._cwd.borrow();
            if is_option_str_empty(_cwd.as_deref()) {
                panic!("The current working directory has not been set.");
            }
            let _cwd = _cwd.as_ref().unwrap();
            let WalkResult { node, .. } = self
                ._walk(
                    _cwd,
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if node.is_none() {
                return io_error_from_name("ENOENT");
            }
            let node = node.unwrap();
            if !is_directory(Some(&node.ref_(self))) {
                return io_error_from_name("ENOTDIR");
            }
            Ok(_cwd.clone())
        }

        pub fn chdir(&self, path: &str) -> io::Result<()> {
            if self.is_readonly() {
                return io_error_from_name("EPERM");
            }
            let path = self._resolve(path);
            let WalkResult { node, .. } = self
                ._walk(
                    &path,
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if node.is_none() {
                return io_error_from_name("ENOENT");
            }
            let node = node.unwrap();
            if !is_directory(Some(&node.ref_(self))) {
                return io_error_from_name("ENOTDIR");
            }
            self.set_cwd(Some(path));
            Ok(())
        }

        pub fn pushd(&self, path: Option<&str>) -> io::Result<()> {
            if self.is_readonly() {
                return io_error_from_name("EPERM");
            }
            let path = path
                .filter(|path| !path.is_empty())
                .map(|path| self._resolve(path));
            if let Some(_cwd) = self.maybe_cwd().as_ref().filter(|_cwd| !_cwd.is_empty()) {
                self.maybe_dir_stack_mut()
                    .get_or_insert_with(|| vec![])
                    .push(_cwd.clone());
            }
            if let Some(path) = path.filter(|path| {
                !path.is_empty()
                    && !matches!(
                        self.maybe_cwd().as_ref(),
                        Some(_cwd) if path == _cwd
                    )
            }) {
                self.chdir(&path)?;
            }
            Ok(())
        }

        pub fn popd(&self) -> io::Result<()> {
            if self.is_readonly() {
                return io_error_from_name("EPERM");
            }
            let mut _dir_stack = self._dir_stack.borrow_mut();
            let path = _dir_stack.as_mut().and_then(|_dir_stack| _dir_stack.pop());
            if let Some(path) = path {
                self.chdir(&path)?;
            }
            Ok(())
        }

        pub fn apply(&self, files: &FileSet) -> io::Result<()> {
            self._apply_files(
                files,
                {
                    let value = self.maybe_cwd().clone();
                    value
                }
                .as_deref()
                .unwrap(),
            )?;

            Ok(())
        }

        pub fn mount_sync(
            &self,
            source: &str,
            target: &str,
            resolver: Id<FileSystemResolver>,
        ) -> io::Result<()> {
            if self.is_readonly() {
                return io_error_from_name("EROFS");
            }

            let source = vpath::validate(source, Some(vpath::ValidationFlags::Absolute));

            let WalkResult {
                parent,
                links,
                node: existing_node,
                basename,
                ..
            } = self
                ._walk(
                    &self._resolve(target),
                    Some(true),
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if existing_node.is_some() {
                return io_error_from_name("EEXIST");
            }

            let time = self.time();
            let node = self._mknod(
                if let Some(parent) = parent.as_ref() {
                    parent.ref_(self).dev()
                } else {
                    incremented_dev_count()
                },
                S_IFDIR,
                0o777,
                Some(time),
            );
            node.as_directory_inode().set_source(Some(source));
            node.as_directory_inode().set_resolver(Some(resolver));
            self._add_link(
                parent.refed(self).as_deref(),
                &mut links.ref_mut(self),
                &basename,
                self.alloc_inode(node),
                Some(time),
            );
            Ok(())
        }

        pub fn rimraf_sync(&self, _path: &str) {
            unimplemented!()
        }

        pub fn mkdirp_sync(&self, path: &str) -> io::Result<()> {
            let path = self._resolve(path);
            let result = self
                ._walk(
                    &path,
                    Some(true),
                    Some(|error: &NodeJSErrnoException, result: WalkResult| {
                        if error.code.as_deref() == Some("ENOENT") {
                            self._mkdir(result)?;
                            return Ok(OnErrorReturn::Retry);
                        }
                        Ok(OnErrorReturn::Throw)
                    }),
                )?
                .unwrap();

            if result.node.is_none() {
                self._mkdir(result)?;
            }

            Ok(())
        }

        pub fn exists_sync(&self, path: &str) -> io::Result<bool> {
            let result = self._walk(
                &self._resolve(path),
                Some(true),
                Some(|_: &NodeJSErrnoException, _: WalkResult| Ok(OnErrorReturn::Stop)),
            )?;
            Ok(matches!(
                result,
                Some(result) if result.node.is_some()
            ))
        }

        pub fn stat_sync(&self, path: &str) -> io::Result<Stats> {
            self._stat(
                self._walk(
                    &self._resolve(path),
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap(),
            )
        }

        pub fn utimes_sync(&self, _path: &str, _atime: SystemTime, _mtime: SystemTime) {
            unimplemented!()
        }

        fn _stat(&self, entry: WalkResult) -> io::Result<Stats> {
            let node = entry.node.as_ref();
            if node.is_none() {
                return io_error_from_name("ENOENT");
            }
            let node = node.unwrap();
            Ok(Stats::new(
                node.ref_(self).dev(),
                node.ref_(self).ino(),
                node.ref_(self).mode(),
                node.ref_(self).nlink(),
                0,
                if is_file(Some(&node.ref_(self))) {
                    self._get_size(&node.ref_(self))
                } else if is_symlink(Some(&node.ref_(self))) {
                    node.ref_(self)
                        .as_symlink_inode()
                        .symlink
                        .as_ref()
                        .unwrap()
                        .len()
                } else {
                    0
                },
                4096,
                0,
                node.ref_(self).atime_ms(),
                node.ref_(self).mtime_ms(),
                node.ref_(self).ctime_ms(),
                node.ref_(self).birthtime_ms(),
            ))
        }

        pub fn readdir_sync(&self, path: &str) -> io::Result<Vec<String>> {
            let WalkResult { node, .. } = self
                ._walk(
                    &self._resolve(path),
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if node.is_none() {
                return io_error_from_name("ENOENT");
            }
            let node = node.unwrap();
            if !is_directory(Some(&node.ref_(self))) {
                return io_error_from_name("ENOTDIR");
            }
            Ok(self
                ._get_links(&node.ref_(self))
                .ref_(self)
                .keys()
                .map(ToOwned::to_owned)
                .collect())
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
        ) -> io::Result<()> {
            if existing_node.is_some() {
                return io_error_from_name("EEXIST");
            }
            let time = self.time();
            let node = self._mknod(
                if let Some(parent) = parent.as_ref() {
                    parent.ref_(self).dev()
                } else {
                    incremented_dev_count()
                },
                S_IFDIR,
                0o777,
                Some(time),
            );
            self._add_link(
                parent.refed(self).as_deref(),
                &mut links.ref_mut(self),
                &basename,
                self.alloc_inode(node),
                Some(time),
            );
            Ok(())
        }

        pub fn link_sync(&self, _oldpath: &str, _newpath: &str) {
            unimplemented!()
        }

        pub fn unlink_sync(&self, _path: &str) {
            unimplemented!()
        }

        pub fn symlink_sync(&self, target: &str, linkpath: &str) -> io::Result<()> {
            if self.is_readonly() {
                return io_error_from_name("EROFS");
            }

            let WalkResult {
                parent,
                links,
                node: existing_node,
                basename,
                ..
            } = self
                ._walk(
                    &self._resolve(linkpath),
                    Some(true),
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if parent.is_none() {
                return io_error_from_name("EPERM");
            }
            let parent = parent.unwrap();
            if existing_node.is_some() {
                return io_error_from_name("EEXIST");
            }

            let time = self.time();
            let mut node = self._mknod(parent.ref_(self).dev(), S_IFLNK, 0o666, Some(time));
            node.as_symlink_inode_mut().symlink = Some(vpath::validate(
                target,
                Some(vpath::ValidationFlags::RelativeOrAbsolute),
            ));
            self._add_link(
                Some(&parent.ref_(self)),
                &mut links.ref_mut(self),
                &basename,
                self.alloc_inode(node),
                Some(time),
            );
            Ok(())
        }

        pub fn realpath_sync(&self, path: &str) -> io::Result<String> {
            let WalkResult { realpath, .. } = self
                ._walk(
                    &self._resolve(path),
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            Ok(realpath)
        }

        pub fn read_file_sync(
            &self,
            path: &str,
            encoding: Option<&str /*BufferEncoding*/>,
        ) -> io::Result<StringOrBuffer> {
            let WalkResult { node, .. } = self
                ._walk(
                    &self._resolve(path),
                    None,
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if node.is_none() {
                return io_error_from_name("ENOENT");
            }
            let ref node = node.unwrap();
            if is_directory(Some(&node.ref_(self))) {
                return io_error_from_name("EISDIR");
            }
            if !is_file(Some(&node.ref_(self))) {
                return io_error_from_name("EBADF");
            }

            let buffer = self._get_buffer(&node.ref_(self)).clone();
            Ok(if let Some(encoding) = encoding {
                if encoding != "utf8" {
                    unimplemented!()
                }
                String::from_utf8(buffer).unwrap().into()
            } else {
                buffer.into()
            })
        }

        pub fn write_file_sync<'a>(
            &self,
            path: &str,
            data: impl Into<StringOrRefBuffer<'a>>,
            encoding: Option<&str>,
        ) -> io::Result<()> {
            let data = data.into();
            if self.is_readonly() {
                return io_error_from_name("EROFS");
            }

            let WalkResult {
                parent,
                links,
                node: existing_node,
                basename,
                ..
            } = self
                ._walk(
                    &self._resolve(path),
                    Some(false),
                    Option::<fn(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>>::None,
                )?
                .unwrap();
            if parent.is_none() {
                return io_error_from_name("EPERM");
            }
            let ref parent = parent.unwrap();

            let time = self.time();
            let ref node = existing_node.unwrap_or_else(|| {
                let node = self.alloc_inode(self._mknod(
                    parent.ref_(self).dev(),
                    S_IFREG,
                    0o666,
                    Some(time),
                ));
                self._add_link(
                    Some(&parent.ref_(self)),
                    &mut links.ref_mut(self),
                    &basename,
                    node.clone(),
                    Some(time),
                );
                node
            });

            if is_directory(Some(&node.ref_(self))) {
                return io_error_from_name("EISDIR");
            }
            if !is_file(Some(&node.ref_(self))) {
                return io_error_from_name("EBADF");
            }

            node.ref_(self).as_file_inode().set_buffer(Some(match data {
                StringOrRefBuffer::Buffer(data) => data.clone(),
                StringOrRefBuffer::String(data) => get_sys(self)
                    .ref_(self)
                    .buffer_from(data, Some(encoding.unwrap_or("utf8")))
                    .unwrap(),
            }));
            node.ref_(self).as_file_inode().set_size(Some(
                node.ref_(self)
                    .as_file_inode()
                    .maybe_buffer()
                    .as_ref()
                    .unwrap()
                    .len(),
            ));
            node.ref_(self).set_mtime_ms(time);
            node.ref_(self).set_ctime_ms(time);
            Ok(())
        }

        fn _mknod(&self, dev: u32, type_: u32, mode: u32, time: Option<u128>) -> Inode {
            let time = time.unwrap_or_else(|| self.time());
            Inode::new(
                type_,
                dev,
                incremented_ino_count(),
                (mode & !S_IFMT & !0o022 & 0o7777) | (type_ & S_IFMT),
                time,
                time,
                time,
                time,
                0,
                None,
            )
        }

        fn _add_link(
            &self,
            parent: Option<&Inode /*DirectoryInode*/>,
            links: &mut collections::SortedMap<String, Id<Inode>>,
            name: &str,
            node: Id<Inode>,
            time: Option<u128>,
        ) {
            let time = time.unwrap_or_else(|| self.time());
            links.set(name.to_owned(), node.clone());
            node.ref_(self).increment_nlink();
            node.ref_(self).set_ctime_ms(time);
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

        fn _get_root_links(&self) -> Id<collections::SortedMap<String, Id<Inode>>> {
            if self._lazy.links.get().is_none() {
                self._lazy.links.set({
                    let mut _lazy_links = collections::SortedMap::new(
                        collections::SortOptions {
                            comparer: self.alloc_sort_options_comparer_string(Box::new(
                                SortOptionsComparerFromStringComparer::new(
                                    self.string_comparer.clone(),
                                ),
                            )),
                            sort: None,
                        },
                        Option::<HashMap<String, Id<Inode>>>::None,
                    );
                    if let Some(_shadow_root) = self._shadow_root.as_ref() {
                        self._copy_shadow_links(
                            _shadow_root
                                .ref_(self)
                                ._get_root_links()
                                .ref_(self)
                                .entries(),
                            &mut _lazy_links,
                        );
                    }
                    Some(self.alloc_links(_lazy_links))
                });
            }
            self._lazy.links.get().unwrap()
        }

        fn _get_links(
            &self,
            node: &Inode, /*DirectoryInode*/
        ) -> Id<collections::SortedMap<String, Id<Inode>>> {
            let node_as_directory_inode = node.as_directory_inode();
            if node_as_directory_inode.maybe_links().is_none() {
                let mut links = collections::SortedMap::new(
                    collections::SortOptions {
                        comparer: self.alloc_sort_options_comparer_string(Box::new(
                            SortOptionsComparerFromStringComparer::new(
                                self.string_comparer.clone(),
                            ),
                        )),
                        sort: None,
                    },
                    Option::<HashMap<String, Id<Inode>>>::None,
                );
                let source = node_as_directory_inode.maybe_source();
                let resolver = node_as_directory_inode.maybe_resolver();
                if let (Some(source), Some(resolver)) = (source, resolver) {
                    node_as_directory_inode.set_source(None);
                    node_as_directory_inode.set_resolver(None);
                    for name in resolver.ref_(self).readdir_sync(&source) {
                        let path = vpath::combine(&source, &[Some(&name)]);
                        let stats = resolver.ref_(self).stat_sync(&path);
                        match stats.mode & S_IFMT {
                            S_IFDIR => {
                                let dir =
                                    self._mknod(node_as_directory_inode.dev, S_IFDIR, 0o777, None);
                                dir.as_directory_inode()
                                    .set_source(Some(vpath::combine(&source, &[Some(&name)])));
                                dir.as_directory_inode()
                                    .set_resolver(Some(resolver.clone()));
                                self._add_link(
                                    Some(node),
                                    &mut links,
                                    &name,
                                    self.alloc_inode(dir),
                                    None,
                                );
                            }
                            S_IFREG => {
                                let file =
                                    self._mknod(node_as_directory_inode.dev, S_IFREG, 0o666, None);
                                file.as_file_inode()
                                    .set_source(Some(vpath::combine(&source, &[Some(&name)])));
                                file.as_file_inode().set_resolver(Some(resolver.clone()));
                                file.as_file_inode().set_size(Some(stats.size));
                                self._add_link(
                                    Some(node),
                                    &mut links,
                                    &name,
                                    self.alloc_inode(file),
                                    None,
                                );
                            }
                            _ => (),
                        }
                    }
                } else if let (Some(_shadow_root), Some(node_shadow_root)) = (
                    self._shadow_root.as_ref(),
                    node_as_directory_inode.shadow_root.as_ref(),
                ) {
                    self._copy_shadow_links(
                        _shadow_root
                            .ref_(self)
                            ._get_links(&node_shadow_root.ref_(self))
                            .ref_(self)
                            .entries(),
                        &mut links,
                    );
                }
                node_as_directory_inode.set_links(Some(self.alloc_links(links)));
            }
            node_as_directory_inode.maybe_links().unwrap()
        }

        fn _get_shadow(&self, root: Id<Inode>) -> Id<Inode> {
            self._lazy
                .shadows
                .borrow_mut()
                .get_or_insert_with(|| Default::default())
                .entry(root.ref_(self).ino())
                .or_insert_with(|| {
                    let mut shadow = Inode::new(
                        root.ref_(self).canonical_type(),
                        root.ref_(self).dev(),
                        root.ref_(self).ino(),
                        root.ref_(self).mode(),
                        root.ref_(self).atime_ms(),
                        root.ref_(self).mtime_ms(),
                        root.ref_(self).ctime_ms(),
                        root.ref_(self).birthtime_ms(),
                        root.ref_(self).nlink(),
                        Some(root.clone()),
                    );

                    if is_symlink(Some(&root.ref_(self))) {
                        shadow.as_symlink_inode_mut().symlink =
                            root.ref_(self).as_symlink_inode().symlink.clone();
                    }

                    self.alloc_inode(shadow)
                })
                .clone()
        }

        fn _copy_shadow_links<
            'source,
            TSource: IntoIterator<Item = (&'source String, &'source Id<Inode>)>,
        >(
            &self,
            source: TSource,
            target: &mut collections::SortedMap<String, Id<Inode>>,
        ) {
            let source = source.into_iter();
            for (name, root) in source {
                target.set(name.clone(), self._get_shadow(root.clone()));
            }
        }

        fn _get_size(&self, node: &Inode /*FileInode*/) -> usize {
            let node = node.as_file_inode();
            if let Some(node_buffer) = node.buffer.borrow().as_ref() {
                return node_buffer.len();
            }
            if let Some(node_size) = node.size.get() {
                return node_size;
            }
            if let (Some(node_source), Some(node_resolver)) =
                (node.maybe_source(), node.maybe_resolver())
            {
                let ret = node_resolver.ref_(self).stat_sync(&node_source).size;
                node.set_size(Some(ret));
                return ret;
            }
            if let (Some(_shadow_root), Some(node_shadow_root)) =
                (self._shadow_root.as_ref(), node.shadow_root.as_ref())
            {
                let ret = _shadow_root
                    .ref_(self)
                    ._get_size(&node_shadow_root.ref_(self));
                node.set_size(Some(ret));
                return ret;
            }
            0
        }

        fn _get_buffer<'node>(&self, node: &'node Inode /*FileInode*/) -> Ref<'node, Buffer> {
            let node = node.as_file_inode();
            node.buffer.borrow_mut().get_or_insert_with(|| {
                let source = node.maybe_source();
                let resolver = node.maybe_resolver();
                if let (Some(source), Some(resolver)) =
                    (source.filter(|source| !source.is_empty()), resolver)
                {
                    node.set_source(None);
                    node.set_resolver(None);
                    node.set_size(None);
                    resolver.ref_(self).read_file_sync(&source)
                } else if let (Some(_shadow_root), Some(node_shadow_root)) =
                    (self._shadow_root.as_ref(), node.shadow_root.as_ref())
                {
                    _shadow_root
                        .ref_(self)
                        ._get_buffer(&node_shadow_root.ref_(self))
                        .clone()
                } else {
                    Buffer::new() /*.allocUnsafe(0)*/
                }
            });
            Ref::map(node.buffer.borrow(), |node_buffer| {
                node_buffer.as_ref().unwrap()
            })
        }

        fn _walk(
            &self,
            path: &str,
            no_follow: Option<bool>,
            mut on_error: Option<
                impl FnMut(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>,
            >,
        ) -> io::Result<Option<WalkResult>> {
            let mut links = self._get_root_links();
            let mut parent: Option<Id<Inode>> = None;
            let mut components = vpath::parse(path, None);
            let mut step = 0;
            let mut depth = 0;
            let mut retry = false;
            loop {
                if depth >= 40 {
                    return io_error_from_name("ELOOP");
                }
                let last_step = step == components.len() - 1;
                let mut basename = components[step].clone();
                let node = {
                    let links = links.ref_(self);
                    let link_entry = links.get_entry(&basename);
                    if let Some(link_entry) = link_entry {
                        components[step] = link_entry.0.clone();
                        basename = link_entry.0.clone();
                    }
                    link_entry.map(|link_entry| link_entry.1.clone())
                };
                if last_step
                    && (no_follow == Some(true) || !is_symlink(node.refed(self).as_deref()))
                {
                    return Ok(Some(WalkResult {
                        realpath: vpath::format(&components),
                        basename,
                        parent,
                        links: links.clone(),
                        node: node.clone(),
                    }));
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
                    )? {
                        continue;
                    }
                    return Ok(None);
                }
                let ref node = node.unwrap();
                if is_symlink(Some(&node.ref_(self))) {
                    let dirname = vpath::format(&components[..step]);
                    let symlink = vpath::resolve(
                        &dirname,
                        &[Some(node.ref_(self).as_symlink_inode().symlink())],
                    );
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
                if is_directory(Some(&node.ref_(self))) {
                    links = self._get_links(&node.ref_(self));
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
                )? {
                    continue;
                }
                return Ok(None);
            }
        }

        fn trap_error(
            &self,
            components: &[String],
            step: usize,
            retry: &mut bool,
            on_error: Option<
                &mut impl FnMut(&NodeJSErrnoException, WalkResult) -> io::Result<OnErrorReturn>,
            >,
            parent: Option<Id<Inode /*DirectoryInode*/>>,
            links: Id<collections::SortedMap<String, Id<Inode>>>,
            error: NodeJSErrnoException,
            node: Option<Id<Inode>>,
        ) -> io::Result<bool> {
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
                )?
            } else {
                OnErrorReturn::Throw
            };
            if result == OnErrorReturn::Stop {
                return Ok(false);
            }
            if result == OnErrorReturn::Retry {
                *retry = true;
                return Ok(true);
            }
            io_error_from_name(&error.message)
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

        fn _apply_files(&self, files: &FileSet, dirname: &str) -> io::Result<()> {
            let mut deferred: Vec<(FileSetValue /*Symlink | Link | Mount*/, String)> = vec![];
            self._apply_files_worker(files, dirname, &mut deferred)?;
            for (entry, path) in deferred {
                self.mkdirp_sync(&vpath::dirname(&path))?;
                self.pushd(Some(&vpath::dirname(&path)))?;
                match entry {
                    FileSetValue::Symlink(entry) => {
                        if self
                            .string_comparer
                            .ref_(self)
                            .call(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be symbolic links.");
                        }
                        self.symlink_sync(
                            &vpath::resolve(dirname, &[Some(&entry.symlink)]),
                            &path,
                        )?;
                        self._apply_file_extended_options(&path, entry.meta.as_ref())?;
                    }
                    FileSetValue::Link(entry) => {
                        if self
                            .string_comparer
                            .ref_(self)
                            .call(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be hard links.");
                        }
                        self.link_sync(&entry.path, &path);
                    }
                    FileSetValue::Mount(entry) => {
                        self.mount_sync(&entry.source, &path, entry.resolver.clone())?;
                        self._apply_file_extended_options(&path, entry.meta.as_ref())?;
                    }
                    _ => unreachable!(),
                }
                self.popd()?;
            }

            Ok(())
        }

        fn _apply_file_extended_options(
            &self,
            path: &str,
            entry_meta: Option<&HashMap<String, Id<documents::TextDocument>>>,
        ) -> io::Result<()> {
            if let Some(meta) = entry_meta {
                let filemeta = self.filemeta(path)?;
                let mut filemeta = filemeta.ref_mut(self);
                for (key, value) in meta {
                    filemeta.set(key, value.clone().into());
                }
            }

            Ok(())
        }

        fn _apply_files_worker(
            &self,
            files: &FileSet,
            dirname: &str,
            deferred: &mut Vec<(FileSetValue /*Symlink | Link | Mount*/, String)>,
        ) -> io::Result<()> {
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
                        if self
                            .string_comparer
                            .ref_(self)
                            .call(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be deleted.");
                        }
                        self.rimraf_sync(&path);
                    }
                    Some(FileSetValue::File(value)) => {
                        if self
                            .string_comparer
                            .ref_(self)
                            .call(&vpath::dirname(&path), &path)
                            == Comparison::EqualTo
                        {
                            panic!("Roots cannot be files.");
                        }
                        self.mkdirp_sync(&vpath::dirname(&path))?;
                        self.write_file_sync(&path, value.data.clone(), value.encoding.as_deref())?;
                        self._apply_file_extended_options(&path, value.meta.as_ref())?;
                    }
                    Some(FileSetValue::Directory(value)) => {
                        self.mkdirp_sync(&path)?;
                        self._apply_file_extended_options(&path, value.meta.as_ref())?;
                        self._apply_files_worker(&value.files, &path, deferred)?;
                    }
                    Some(value) => {
                        deferred.push((value.clone(), path));
                    }
                }
            }

            Ok(())
        }
    }

    impl HasArena for FileSystem {
        fn arena(&self) -> &AllArenas {
            unimplemented!()
        }
    }

    impl HasArenaHarness for FileSystem {
        fn arena_harness(&self) -> &AllArenasHarness {
            unimplemented!()
        }
    }

    #[derive(Clone, Trace, Finalize)]
    pub enum MetaValue {
        RcTextDocument(Id<documents::TextDocument>),
        Node(Id<Node>),
    }

    impl MetaValue {
        pub fn as_node(&self) -> &Id<Node> {
            enum_unwrapped!(self, [MetaValue, Node])
        }
    }

    impl From<Id<documents::TextDocument>> for MetaValue {
        fn from(value: Id<documents::TextDocument>) -> Self {
            Self::RcTextDocument(value)
        }
    }

    impl From<Id<Node>> for MetaValue {
        fn from(value: Id<Node>) -> Self {
            Self::Node(value)
        }
    }

    pub enum StringOrRefBuffer<'buffer> {
        String(String),
        Buffer(&'buffer Buffer),
    }

    impl<'buffer> From<String> for StringOrRefBuffer<'buffer> {
        fn from(value: String) -> Self {
            Self::String(value)
        }
    }

    impl<'buffer> From<&'buffer Buffer> for StringOrRefBuffer<'buffer> {
        fn from(value: &'buffer Buffer) -> Self {
            Self::Buffer(value)
        }
    }

    pub enum StringOrBuffer {
        String(String),
        Buffer(Buffer),
    }

    impl StringOrBuffer {
        pub fn as_string_owned(self) -> String {
            enum_unwrapped!(self, [StringOrBuffer, String])
        }
    }

    impl From<String> for StringOrBuffer {
        fn from(value: String) -> Self {
            Self::String(value)
        }
    }

    impl From<Buffer> for StringOrBuffer {
        fn from(value: Buffer) -> Self {
            Self::Buffer(value)
        }
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum OnErrorReturn {
        Stop,
        Retry,
        Throw,
    }

    #[derive(Default, Trace, Finalize)]
    struct FileSystemLazy {
        #[unsafe_ignore_trace]
        links: Cell<Option<Id<collections::SortedMap<String, Id<Inode>>>>>,
        #[unsafe_ignore_trace]
        shadows: RefCell<Option<HashMap<u32, Id<Inode>>>>,
        #[unsafe_ignore_trace]
        meta: Cell<Option<Id<collections::Metadata<String>>>>,
    }

    // TODO: revisit this, SystemTime Trace wasn't implemented and unsafe_ignore_trace didn't seem
    // to work I guess on an enum variant?
    #[derive(Clone /*, Trace, Finalize*/)]
    pub enum TimestampOrNowOrSystemTimeOrCallback {
        Timestamp(u128),
        Now,
        // #[unsafe_ignore_trace]
        SystemTime(SystemTime),
        Callback(Id<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>>),
    }
    unsafe impl Trace for TimestampOrNowOrSystemTimeOrCallback {
        gc::unsafe_empty_trace!();
    }
    impl Finalize for TimestampOrNowOrSystemTimeOrCallback {}

    pub trait TimestampOrNowOrSystemTimeOrCallbackCallback: Trace + Finalize {
        fn call(&self) -> TimestampOrNowOrSystemTime;
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

    impl From<Id<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>>>
        for TimestampOrNowOrSystemTimeOrCallback
    {
        fn from(value: Id<Box<dyn TimestampOrNowOrSystemTimeOrCallbackCallback>>) -> Self {
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

    #[derive(Builder, Default)]
    #[builder(default, setter(strip_option, into))]
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
        pub documents: Option<Vec<Id<documents::TextDocument>>>,
    }

    #[derive(Trace, Finalize)]
    pub struct FileSystemResolver {
        #[unsafe_ignore_trace]
        pub host: IdForFileSystemResolverHost,
    }

    impl FileSystemResolver {
        pub fn stat_sync(&self, path: &str) -> FileSystemResolverStats {
            if self.host.ref_(self).directory_exists(path) {
                FileSystemResolverStats {
                    mode: S_IFDIR | 0o777,
                    size: 0,
                }
            } else if self.host.ref_(self).file_exists(path) {
                FileSystemResolverStats {
                    mode: S_IFREG | 0o666,
                    size: self.host.ref_(self).get_file_size(path),
                }
            } else {
                panic!("ENOENT: path does not exist");
            }
        }

        pub fn readdir_sync(&self, path: &str) -> Vec<String> {
            let FileSystemEntries {
                mut files,
                mut directories,
            } = self
                .host
                .ref_(self)
                .get_accessible_file_system_entries(path);
            directories.append(&mut files);
            directories
        }

        pub fn read_file_sync(&self, path: &str) -> Buffer {
            get_sys(self)
                .ref_(self)
                .buffer_from(self.host.ref_(self).read_file(path).unwrap(), Some("utf8"))
                .unwrap()
        }
    }

    impl HasArena for FileSystemResolver {
        fn arena(&self) -> &AllArenas {
            unimplemented!()
        }
    }

    impl HasArenaHarness for FileSystemResolver {
        fn arena_harness(&self) -> &AllArenasHarness {
            unimplemented!()
        }
    }

    pub struct FileSystemResolverStats {
        pub mode: u32,
        pub size: usize,
    }

    pub trait FileSystemResolverHost: Trace + Finalize {
        fn get_accessible_file_system_entries(&self, path: &str) -> FileSystemEntries;
        fn directory_exists(&self, path: &str) -> bool;
        fn file_exists(&self, path: &str) -> bool;
        fn get_file_size(&self, path: &str) -> usize;
        fn read_file(&self, path: &str) -> Option<String>;
        fn get_workspace_root(&self) -> String;
    }

    pub fn create_resolver(host: IdForFileSystemResolverHost) -> FileSystemResolver {
        FileSystemResolver { host }
    }

    pub fn create_from_file_system(
        host: impl Into<IdForFileSystemResolverHost>,
        ignore_case: bool,
        options: Option<FileSystemCreateOptions>,
        arena: &impl HasArenaHarness,
    ) -> io::Result<FileSystem> {
        let host: IdForFileSystemResolverHost = host.into();
        let FileSystemCreateOptions {
            documents,
            files,
            cwd,
            time,
            meta,
        } = options.unwrap_or_default();
        let fs = FileSystem::shadow(get_built_local(host, ignore_case, arena)?, None, arena)?;
        if let Some(meta) = meta {
            for key in meta.keys() {
                fs.meta_mut().set(key, meta.get(key).unwrap().clone());
            }
        }
        if let Some(time) = time {
            fs.set_time(time);
        }
        if let Some(cwd) = cwd.filter(|cwd| !cwd.is_empty()) {
            fs.mkdirp_sync(&cwd)?;
            fs.chdir(&cwd)?;
        }
        if let Some(documents) = documents {
            for document in documents {
                fs.mkdirp_sync(&vpath::dirname(&document.ref_(arena).file))?;
                fs.write_file_sync(
                    &document.ref_(arena).file,
                    document.ref_(arena).text.clone(),
                    Some("utf8"),
                )?;
                fs.filemeta(&document.ref_(arena).file)?
                    .ref_mut(arena)
                    .set("document", document.clone().into());
                let document_ref = document.ref_(arena);
                let symlink = document_ref.meta.get("symlink");
                if let Some(symlink) = symlink {
                    for link in symlink.split(",").map(|link| link.trim()) {
                        fs.mkdirp_sync(&vpath::dirname(link))?;
                        fs.symlink_sync(
                            &vpath::resolve(&fs.cwd()?, &[Some(&document.ref_(arena).file)]),
                            link,
                        )?;
                    }
                }
            }
        }
        if let Some(files) = files {
            fs.apply(&files)?;
        }
        Ok(fs)
    }

    pub struct Stats {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub nlink: usize,
        pub uid: u32,
        pub gid: u32,
        pub rdev: u32,
        pub size: usize,
        pub blksize: usize,
        pub blocks: usize,
        pub atime_ms: u128,
        pub mtime_ms: u128,
        pub ctime_ms: u128,
        pub birthtime_ms: u128,
        pub atime: SystemTime,
        pub mtime: SystemTime,
        pub ctime: SystemTime,
        pub birthtime: SystemTime,
    }

    impl Stats {
        pub fn new(
            dev: u32,
            ino: u32,
            mode: u32,
            nlink: usize,
            rdev: u32,
            size: usize,
            blksize: usize,
            blocks: usize,
            atime_ms: u128,
            mtime_ms: u128,
            ctime_ms: u128,
            birthtime_ms: u128,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                nlink,
                uid: 0,
                gid: 0,
                rdev,
                size,
                blksize,
                blocks,
                atime_ms,
                mtime_ms,
                ctime_ms,
                birthtime_ms,
                atime: millis_since_epoch_to_system_time(atime_ms),
                mtime: millis_since_epoch_to_system_time(mtime_ms),
                ctime: millis_since_epoch_to_system_time(ctime_ms),
                birthtime: millis_since_epoch_to_system_time(birthtime_ms),
            }
        }

        pub fn is_file(&self) -> bool {
            self.mode & S_IFMT == S_IFREG
        }

        pub fn is_directory(&self) -> bool {
            self.mode & S_IFMT == S_IFDIR
        }

        pub fn is_symbolic_link(&self) -> bool {
            self.mode & S_IFMT == S_IFLNK
        }

        pub fn is_block_device(&self) -> bool {
            self.mode & S_IFMT == S_IFBLK
        }

        pub fn is_character_device(&self) -> bool {
            self.mode & S_IFMT == S_IFCHR
        }

        pub fn is_fifo(&self) -> bool {
            self.mode & S_IFMT == S_IFIFO
        }

        pub fn is_socket(&self) -> bool {
            self.mode & S_IFMT == S_IFSOCK
        }
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
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
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
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
    }

    #[derive(Clone)]
    pub struct File {
        pub data: String, /*Buffer | string*/
        pub encoding: Option<String>,
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
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
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
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
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
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
        meta: Option<HashMap<String, Id<documents::TextDocument>>>,
    }

    #[derive(Clone)]
    pub struct Mount {
        pub source: String,
        pub resolver: Id<FileSystemResolver>,
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
    }

    impl Mount {
        pub fn new(
            source: String,
            resolver: Id<FileSystemResolver>,
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
        pub meta: Option<HashMap<String, Id<documents::TextDocument>>>,
    }

    #[derive(Clone, Trace, Finalize)]
    pub enum Inode {
        FileInode(FileInode),
        DirectoryInode(DirectoryInode),
        SymlinkInode(SymlinkInode),
    }

    impl Inode {
        pub fn new(
            type_: u32,
            dev: u32,
            ino: u32,
            mode: u32,
            atime_ms: u128,
            mtime_ms: u128,
            ctime_ms: u128,
            birthtime_ms: u128,
            nlink: usize,
            shadow_root: Option<Id<Inode>>,
        ) -> Self {
            match type_ {
                S_IFREG => FileInode::new(
                    dev,
                    ino,
                    mode,
                    atime_ms,
                    mtime_ms,
                    ctime_ms,
                    birthtime_ms,
                    nlink,
                    shadow_root,
                )
                .into(),
                S_IFDIR => DirectoryInode::new(
                    dev,
                    ino,
                    mode,
                    atime_ms,
                    mtime_ms,
                    ctime_ms,
                    birthtime_ms,
                    nlink,
                    shadow_root,
                )
                .into(),
                S_IFLNK => SymlinkInode::new(
                    dev,
                    ino,
                    mode,
                    atime_ms,
                    mtime_ms,
                    ctime_ms,
                    birthtime_ms,
                    nlink,
                    shadow_root,
                )
                .into(),
                _ => unreachable!(),
            }
        }

        pub fn canonical_type(&self) -> u32 {
            match self {
                Self::FileInode(_) => S_IFREG,
                Self::DirectoryInode(_) => S_IFDIR,
                Self::SymlinkInode(_) => S_IFLNK,
            }
        }

        pub fn dev(&self) -> u32 {
            match self {
                Self::FileInode(value) => value.dev,
                Self::DirectoryInode(value) => value.dev,
                Self::SymlinkInode(value) => value.dev,
            }
        }

        pub fn ino(&self) -> u32 {
            match self {
                Self::FileInode(value) => value.ino,
                Self::DirectoryInode(value) => value.ino,
                Self::SymlinkInode(value) => value.ino,
            }
        }

        pub fn mode(&self) -> u32 {
            match self {
                Self::FileInode(value) => value.mode,
                Self::DirectoryInode(value) => value.mode,
                Self::SymlinkInode(value) => value.mode,
            }
        }

        pub fn atime_ms(&self) -> u128 {
            match self {
                Self::FileInode(value) => value.atime_ms,
                Self::DirectoryInode(value) => value.atime_ms,
                Self::SymlinkInode(value) => value.atime_ms,
            }
        }

        pub fn ctime_ms(&self) -> u128 {
            match self {
                Self::FileInode(value) => value.ctime_ms(),
                Self::DirectoryInode(value) => value.ctime_ms(),
                Self::SymlinkInode(value) => value.ctime_ms(),
            }
        }

        pub fn mtime_ms(&self) -> u128 {
            match self {
                Self::FileInode(value) => value.mtime_ms(),
                Self::DirectoryInode(value) => value.mtime_ms(),
                Self::SymlinkInode(value) => value.mtime_ms(),
            }
        }

        pub fn birthtime_ms(&self) -> u128 {
            match self {
                Self::FileInode(value) => value.birthtime_ms,
                Self::DirectoryInode(value) => value.birthtime_ms,
                Self::SymlinkInode(value) => value.birthtime_ms,
            }
        }

        pub fn nlink(&self) -> usize {
            match self {
                Self::FileInode(value) => value.nlink(),
                Self::DirectoryInode(value) => value.nlink(),
                Self::SymlinkInode(value) => value.nlink(),
            }
        }

        pub fn increment_nlink(&self) {
            match self {
                Self::FileInode(value) => value.increment_nlink(),
                Self::DirectoryInode(value) => value.increment_nlink(),
                Self::SymlinkInode(value) => value.increment_nlink(),
            }
        }

        pub fn set_ctime_ms(&self, ctime_ms: u128) {
            match self {
                Self::FileInode(value) => value.set_ctime_ms(ctime_ms),
                Self::DirectoryInode(value) => value.set_ctime_ms(ctime_ms),
                Self::SymlinkInode(value) => value.set_ctime_ms(ctime_ms),
            }
        }

        pub fn set_mtime_ms(&self, mtime_ms: u128) {
            match self {
                Self::FileInode(value) => value.set_mtime_ms(mtime_ms),
                Self::DirectoryInode(value) => value.set_mtime_ms(mtime_ms),
                Self::SymlinkInode(value) => value.set_mtime_ms(mtime_ms),
            }
        }

        pub fn meta(&self) -> Option<Id<collections::Metadata<MetaValue>>> {
            match self {
                Self::FileInode(value) => value.meta(),
                Self::DirectoryInode(value) => value.meta(),
                Self::SymlinkInode(value) => value.meta(),
            }
        }

        pub fn set_meta(&self, meta: Option<Id<collections::Metadata<MetaValue>>>) {
            match self {
                Self::FileInode(value) => value.set_meta(meta),
                Self::DirectoryInode(value) => value.set_meta(meta),
                Self::SymlinkInode(value) => value.set_meta(meta),
            }
        }

        pub fn maybe_shadow_root(&self) -> Option<Id<Inode>> {
            match self {
                Self::FileInode(value) => value.shadow_root.clone(),
                Self::DirectoryInode(value) => value.shadow_root.clone(),
                Self::SymlinkInode(value) => value.shadow_root.clone(),
            }
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

        pub fn as_symlink_inode_mut(&mut self) -> &mut SymlinkInode {
            match self {
                Self::SymlinkInode(value) => value,
                _ => unreachable!(),
            }
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

    #[derive(Clone, Trace, Finalize)]
    pub struct FileInode {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub atime_ms: u128,
        #[unsafe_ignore_trace]
        pub mtime_ms: Cell<u128>,
        #[unsafe_ignore_trace]
        pub ctime_ms: Cell<u128>,
        pub birthtime_ms: u128,
        #[unsafe_ignore_trace]
        nlink: Cell<usize>,
        #[unsafe_ignore_trace]
        size: Cell<Option<usize>>,
        #[unsafe_ignore_trace]
        buffer: RefCell<Option<Buffer>>,
        #[unsafe_ignore_trace]
        source: RefCell<Option<String>>,
        #[unsafe_ignore_trace]
        resolver: Cell<Option<Id<FileSystemResolver>>>,
        pub shadow_root: Option<Id<Inode /*FileInode*/>>,
        #[unsafe_ignore_trace]
        meta: Cell<Option<Id<collections::Metadata<MetaValue>>>>,
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
            shadow_root: Option<Id<Inode>>,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                atime_ms,
                mtime_ms: Cell::new(mtime_ms),
                ctime_ms: Cell::new(ctime_ms),
                birthtime_ms,
                nlink: Cell::new(nlink),
                size: Default::default(),
                buffer: Default::default(),
                source: Default::default(),
                resolver: Default::default(),
                shadow_root,
                meta: Default::default(),
            }
        }

        pub fn mtime_ms(&self) -> u128 {
            self.mtime_ms.get()
        }

        pub fn set_mtime_ms(&self, mtime_ms: u128) {
            self.mtime_ms.set(mtime_ms);
        }

        pub fn ctime_ms(&self) -> u128 {
            self.ctime_ms.get()
        }

        pub fn set_ctime_ms(&self, ctime_ms: u128) {
            self.ctime_ms.set(ctime_ms);
        }

        pub fn nlink(&self) -> usize {
            self.nlink.get()
        }

        pub fn increment_nlink(&self) {
            self.nlink.set(self.nlink.get() + 1);
        }

        pub fn set_size(&self, size: Option<usize>) {
            self.size.set(size);
        }

        pub fn maybe_source(&self) -> Option<String> {
            self.source.borrow().clone()
        }

        pub fn maybe_buffer(&self) -> Ref<Option<Buffer>> {
            self.buffer.borrow()
        }

        pub fn set_buffer(&self, buffer: Option<Buffer>) {
            *self.buffer.borrow_mut() = buffer;
        }

        pub fn set_source(&self, source: Option<String>) {
            *self.source.borrow_mut() = source;
        }

        pub fn maybe_resolver(&self) -> Option<Id<FileSystemResolver>> {
            self.resolver.get()
        }

        pub fn set_resolver(&self, resolver: Option<Id<FileSystemResolver>>) {
            self.resolver.set(resolver);
        }

        pub fn meta(&self) -> Option<Id<collections::Metadata<MetaValue>>> {
            self.meta.get()
        }

        pub fn set_meta(&self, meta: Option<Id<collections::Metadata<MetaValue>>>) {
            self.meta.set(meta);
        }
    }

    #[derive(Clone, Trace, Finalize)]
    pub struct DirectoryInode {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub atime_ms: u128,
        #[unsafe_ignore_trace]
        pub mtime_ms: Cell<u128>,
        #[unsafe_ignore_trace]
        pub ctime_ms: Cell<u128>,
        pub birthtime_ms: u128,
        #[unsafe_ignore_trace]
        nlink: Cell<usize>,
        #[unsafe_ignore_trace]
        links: Cell<Option<Id<collections::SortedMap<String, Id<Inode>>>>>,
        #[unsafe_ignore_trace]
        source: RefCell<Option<String>>,
        #[unsafe_ignore_trace]
        resolver: Cell<Option<Id<FileSystemResolver>>>,
        pub shadow_root: Option<Id<Inode /*DirectoryInode*/>>,
        #[unsafe_ignore_trace]
        meta: Cell<Option<Id<collections::Metadata<MetaValue>>>>,
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
            shadow_root: Option<Id<Inode>>,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                atime_ms,
                mtime_ms: Cell::new(mtime_ms),
                ctime_ms: Cell::new(ctime_ms),
                birthtime_ms,
                nlink: Cell::new(nlink),
                links: Default::default(),
                source: Default::default(),
                resolver: Default::default(),
                shadow_root,
                meta: Default::default(),
            }
        }

        pub fn mtime_ms(&self) -> u128 {
            self.mtime_ms.get()
        }

        pub fn set_mtime_ms(&self, mtime_ms: u128) {
            self.mtime_ms.set(mtime_ms);
        }

        pub fn ctime_ms(&self) -> u128 {
            self.ctime_ms.get()
        }

        pub fn set_ctime_ms(&self, ctime_ms: u128) {
            self.ctime_ms.set(ctime_ms);
        }

        pub fn nlink(&self) -> usize {
            self.nlink.get()
        }

        pub fn increment_nlink(&self) {
            self.nlink.set(self.nlink.get() + 1);
        }

        pub fn maybe_links(&self) -> Option<Id<collections::SortedMap<String, Id<Inode>>>> {
            self.links.get()
        }

        pub fn set_links(&self, links: Option<Id<collections::SortedMap<String, Id<Inode>>>>) {
            self.links.set(links);
        }

        pub fn maybe_source(&self) -> Option<String> {
            self.source.borrow().clone()
        }

        pub fn set_source(&self, source: Option<String>) {
            *self.source.borrow_mut() = source;
        }

        pub fn maybe_resolver(&self) -> Option<Id<FileSystemResolver>> {
            self.resolver.get()
        }

        pub fn set_resolver(&self, resolver: Option<Id<FileSystemResolver>>) {
            self.resolver.set(resolver);
        }

        pub fn meta(&self) -> Option<Id<collections::Metadata<MetaValue>>> {
            self.meta.get()
        }

        pub fn set_meta(&self, meta: Option<Id<collections::Metadata<MetaValue>>>) {
            self.meta.set(meta);
        }
    }

    #[derive(Clone, Trace, Finalize)]
    pub struct SymlinkInode {
        pub dev: u32,
        pub ino: u32,
        pub mode: u32,
        pub atime_ms: u128,
        #[unsafe_ignore_trace]
        pub mtime_ms: Cell<u128>,
        #[unsafe_ignore_trace]
        ctime_ms: Cell<u128>,
        pub birthtime_ms: u128,
        #[unsafe_ignore_trace]
        nlink: Cell<usize>,
        pub symlink: Option<String>,
        pub shadow_root: Option<Id<Inode /*SymlinkInode*/>>,
        #[unsafe_ignore_trace]
        meta: Cell<Option<Id<collections::Metadata<MetaValue>>>>,
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
            shadow_root: Option<Id<Inode>>,
        ) -> Self {
            Self {
                dev,
                ino,
                mode,
                atime_ms,
                mtime_ms: Cell::new(mtime_ms),
                ctime_ms: Cell::new(ctime_ms),
                birthtime_ms,
                nlink: Cell::new(nlink),
                symlink: Default::default(),
                shadow_root,
                meta: Default::default(),
            }
        }

        pub fn mtime_ms(&self) -> u128 {
            self.mtime_ms.get()
        }

        pub fn set_mtime_ms(&self, mtime_ms: u128) {
            self.mtime_ms.set(mtime_ms);
        }

        pub fn ctime_ms(&self) -> u128 {
            self.ctime_ms.get()
        }

        pub fn set_ctime_ms(&self, ctime_ms: u128) {
            self.ctime_ms.set(ctime_ms);
        }

        pub fn nlink(&self) -> usize {
            self.nlink.get()
        }

        pub fn increment_nlink(&self) {
            self.nlink.set(self.nlink.get() + 1);
        }

        pub fn symlink(&self) -> &String {
            self.symlink.as_ref().unwrap()
        }

        pub fn meta(&self) -> Option<Id<collections::Metadata<MetaValue>>> {
            self.meta.get()
        }

        pub fn set_meta(&self, meta: Option<Id<collections::Metadata<MetaValue>>>) {
            self.meta.set(meta);
        }
    }

    fn is_file(node: Option<&Inode>) -> bool {
        matches!(
            node,
            Some(node) if node.mode() & S_IFMT == S_IFREG
        )
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
        pub parent: Option<Id<Inode>>,
        pub links: Id<collections::SortedMap<String, Id<Inode>>>,
        pub node: Option<Id<Inode>>,
    }

    thread_local! {
        static BUILT_LOCAL_HOST_PER_ARENA: RefCell<HashMap<*const AllArenasHarness, IdForFileSystemResolverHost>> = RefCell::new(HashMap::new());
        static BUILT_LOCAL_CI_PER_ARENA: RefCell<HashMap<*const AllArenasHarness, Id<FileSystem>>> = RefCell::new(HashMap::new());
        static BUILT_LOCAL_CS_PER_ARENA: RefCell<HashMap<*const AllArenasHarness, Id<FileSystem>>> = RefCell::new(HashMap::new());
    }

    fn maybe_built_local_host(arena: &impl HasArenaHarness) -> Option<IdForFileSystemResolverHost> {
        BUILT_LOCAL_HOST_PER_ARENA.with(|per_arena| {
            let per_arena = per_arena.borrow();
            let arena_ptr: *const AllArenasHarness = arena.arena_harness();
            per_arena.get(&arena_ptr).copied()
        })
    }

    fn set_built_local_host(
        value: Option<IdForFileSystemResolverHost>,
        arena: &impl HasArenaHarness,
    ) {
        BUILT_LOCAL_HOST_PER_ARENA.with(|per_arena| {
            let mut per_arena = per_arena.borrow_mut();
            let arena_ptr: *const AllArenasHarness = arena.arena_harness();
            match value {
                Some(value) => {
                    per_arena.insert(arena_ptr, value);
                }
                None => {
                    per_arena.remove(&arena_ptr);
                }
            }
        });
    }

    fn maybe_built_local_ci(arena: &impl HasArenaHarness) -> Option<Id<FileSystem>> {
        BUILT_LOCAL_CI_PER_ARENA.with(|per_arena| {
            let per_arena = per_arena.borrow();
            let arena_ptr: *const AllArenasHarness = arena.arena_harness();
            per_arena.get(&arena_ptr).copied()
        })
    }

    fn set_built_local_ci(value: Option<Id<FileSystem>>, arena: &impl HasArenaHarness) {
        BUILT_LOCAL_CI_PER_ARENA.with(|per_arena| {
            let mut per_arena = per_arena.borrow_mut();
            let arena_ptr: *const AllArenasHarness = arena.arena_harness();
            match value {
                Some(value) => {
                    per_arena.insert(arena_ptr, value);
                }
                None => {
                    per_arena.remove(&arena_ptr);
                }
            }
        });
    }

    fn maybe_built_local_cs(arena: &impl HasArenaHarness) -> Option<Id<FileSystem>> {
        BUILT_LOCAL_CS_PER_ARENA.with(|per_arena| {
            let per_arena = per_arena.borrow(;
            let arena_ptr: *const AllArenasHarness = arena.arena_harness();
            per_arena.get(&arena_ptr).copied()
        })
    }

    fn set_built_local_cs(value: Option<Id<FileSystem>>, arena: &impl HasArenaHarness) {
        BUILT_LOCAL_CS_PER_ARENA.with(|per_arena| {
            let mut per_arena = per_arena.borrow_mut();
            let arena_ptr: *const AllArenasHarness = arena.arena_harness();
            match value {
                Some(value) => {
                    per_arena.insert(arena_ptr, value);
                }
                None => {
                    per_arena.remove(&arena_ptr);
                }
            }
        });
    }

    fn get_built_local(
        host: IdForFileSystemResolverHost,
        ignore_case: bool,
        arena: &impl HasArenaHarness,
    ) -> io::Result<Id<FileSystem>> {
        if !matches!(
            maybe_built_local_host(arena),
            Some(built_local_host) if built_local_host == host
        ) {
            set_built_local_ci(None, arena);
            set_built_local_cs(None, arena);
            set_built_local_host(Some(host.clone()), arena);
        }
        if maybe_built_local_ci(arena).is_none() {
            let resolver = arena.alloc_file_system_resolver(create_resolver(host));
            set_built_local_ci(
                Some(
                    arena.alloc_file_system(FileSystem::new(
                        true,
                        Some(FileSystemOptions {
                            files: Some(FileSet::from_iter(IntoIterator::into_iter([
                                (
                                    built_folder.to_owned(),
                                    Some(
                                        Mount::new(
                                            // vpath::resolve(
                                            //     &host.get_workspace_root(),
                                            //     &[Some("built/local")],
                                            // ),
                                            "/Users/jrosse/prj/TypeScript/built/local/".to_owned(),
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
                                            // vpath::resolve(
                                            //     &host.get_workspace_root(),
                                            //     &[Some("tests/lib")],
                                            // ),
                                            "/Users/jrosse/prj/TypeScript/tests/lib".to_owned(),
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
                                                &host.ref_(arena).get_workspace_root(),
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
                        arena,
                    )?),
                ),
                arena,
            );
            maybe_built_local_ci(arena)
                .unwrap()
                .ref_(arena)
                .make_readonly();
        }
        if ignore_case {
            return Ok(maybe_built_local_ci(arena).unwrap());
        }
        if maybe_built_local_cs(arena).is_none() {
            set_built_local_cs(
                Some(arena.alloc_file_system(FileSystem::shadow(
                    maybe_built_local_ci(arena).unwrap(),
                    Some(false),
                    arena,
                )?)),
                arena,
            );
            maybe_built_local_cs(arena)
                .unwrap()
                .ref_(arena)
                .make_readonly();
        }
        Ok(maybe_built_local_cs(arena).unwrap())
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
