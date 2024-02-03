use typescript_rust::{
    debug_cell::{Ref, RefCell, RefMut},
    id_arena::{Arena, Id},
    AllArenas, HasArena,
};

use crate::{documents::TextDocument, harness::harness_io::NodeIO, vfs::FileSystemResolverHost};

pub struct AllArenasHarness {
    all_arenas: AllArenas,
    node_ios: RefCell<Arena<NodeIO>>,
    file_system_resolver_hosts: RefCell<Arena<Box<dyn FileSystemResolverHost>>>,
    text_documents: RefCell<Arena<TextDocument>>,
}

pub trait HasArenaHarness: HasArena {
    fn arena_harness(&self) -> &AllArenasHarness;

    fn node_io(&self, node_io: Id<NodeIO>) -> Ref<NodeIO> {
        self.arena_harness().node_io(node_io)
    }

    fn alloc_node_io(&self, node_io: NodeIO) -> Id<NodeIO> {
        self.arena_harness().alloc_node_io(node_io)
    }

    fn file_system_resolver_host(
        &self,
        file_system_resolver_host: Id<Box<dyn FileSystemResolverHost>>,
    ) -> Ref<Box<dyn FileSystemResolverHost>> {
        self.arena_harness()
            .file_system_resolver_host(file_system_resolver_host)
    }

    fn alloc_file_system_resolver_host(
        &self,
        file_system_resolver_host: Box<dyn FileSystemResolverHost>,
    ) -> Id<Box<dyn FileSystemResolverHost>> {
        self.arena_harness()
            .alloc_file_system_resolver_host(file_system_resolver_host)
    }

    fn text_document(&self, text_document: Id<TextDocument>) -> Ref<TextDocument> {
        self.arena_harness().text_document(text_document)
    }

    fn alloc_text_document(&self, text_document: TextDocument) -> Id<TextDocument> {
        self.arena_harness().alloc_text_document(text_document)
    }
}

impl HasArena for AllArenasHarness {
    fn arena(&self) -> &AllArenas {
        &self.all_arenas
    }
}

impl HasArenaHarness for AllArenasHarness {
    fn arena_harness(&self) -> &AllArenasHarness {
        self
    }

    fn node_io(&self, node_io: Id<NodeIO>) -> Ref<NodeIO> {
        Ref::map(self.node_ios.borrow(), |node_ios| &node_ios[node_io])
    }

    fn alloc_node_io(&self, node_io: NodeIO) -> Id<NodeIO> {
        let id = self.node_ios.borrow_mut().alloc(node_io);
        id
    }

    fn file_system_resolver_host(
        &self,
        file_system_resolver_host: Id<Box<dyn FileSystemResolverHost>>,
    ) -> Ref<Box<dyn FileSystemResolverHost>> {
        Ref::map(
            self.file_system_resolver_hosts.borrow(),
            |file_system_resolver_hosts| &file_system_resolver_hosts[file_system_resolver_host],
        )
    }

    fn alloc_file_system_resolver_host(
        &self,
        file_system_resolver_host: Box<dyn FileSystemResolverHost>,
    ) -> Id<Box<dyn FileSystemResolverHost>> {
        let id = self
            .file_system_resolver_hosts
            .borrow_mut()
            .alloc(file_system_resolver_host);
        id
    }

    fn text_document(&self, text_document: Id<TextDocument>) -> Ref<TextDocument> {
        Ref::map(self.text_documents.borrow(), |text_documents| {
            &text_documents[text_document]
        })
    }

    fn alloc_text_document(&self, text_document: TextDocument) -> Id<TextDocument> {
        let id = self.text_documents.borrow_mut().alloc(text_document);
        id
    }
}

pub trait InArenaHarness {
    type Item: ?Sized;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Self::Item>;
    fn ref_mut<'a>(&self, _has_arena: &'a impl HasArenaHarness) -> RefMut<'a, Self::Item> {
        unimplemented!()
    }
}

impl InArenaHarness for Id<NodeIO> {
    type Item = NodeIO;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, NodeIO> {
        has_arena.node_io(*self)
    }
}

impl InArenaHarness for Id<Box<dyn FileSystemResolverHost>> {
    type Item = Box<dyn FileSystemResolverHost>;

    fn ref_<'a>(
        &self,
        has_arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, Box<dyn FileSystemResolverHost>> {
        has_arena.file_system_resolver_host(*self)
    }
}

impl InArenaHarness for Id<TextDocument> {
    type Item = TextDocument;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, TextDocument> {
        has_arena.text_document(*self)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum IdForFileSystemResolverHost {
    FileSystemResolverHost(Id<Box<dyn FileSystemResolverHost>>),
    NodeIO(Id<NodeIO>),
}

impl InArenaHarness for IdForFileSystemResolverHost {
    type Item = dyn FileSystemResolverHost;

    fn ref_<'a>(
        &self,
        arena: &'a impl HasArenaHarness,
    ) -> Ref<'a, dyn FileSystemResolverHost + 'static> {
        match self {
            Self::FileSystemResolverHost(value) => Ref::map(value.ref_(arena), |value| &**value),
            Self::NodeIO(value) => Ref::map(value.ref_(arena), |value| {
                value as &dyn FileSystemResolverHost
            }),
        }
    }
}

impl From<Id<Box<dyn FileSystemResolverHost>>> for IdForFileSystemResolverHost {
    fn from(value: Id<Box<dyn FileSystemResolverHost>>) -> Self {
        Self::FileSystemResolverHost(value)
    }
}

impl From<Id<NodeIO>> for IdForFileSystemResolverHost {
    fn from(value: Id<NodeIO>) -> Self {
        Self::NodeIO(value)
    }
}
