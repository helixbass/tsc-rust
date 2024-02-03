use std::cell::RefCell;

use typescript_rust::{AllArenas, debug_cell::{RefCell, Ref, RefMut}, id_arena::{Arena, Id}, HasArena};

use crate::harness::harness_io::NodeIO;

pub struct AllArenasHarness {
    all_arenas: AllArenas,
    node_ios: RefCell<Arena<NodeIO>>,
}

pub trait HasArenaHarness: HasArena {
    fn arena_harness(&self) -> &AllArenasHarness;

    fn node_io(&self, node_io: Id<NodeIO>) -> Ref<NodeIO> {
        self.arena_harness().node_io(node_io)
    }

    fn alloc_node_io(&self, node_io: NodeIO) -> Id<NodeIO> {
        self.arena_harness().alloc_node_io(node_io)
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
}

pub trait InArenaHarness {
    type Item: ?Sized;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, Self::Item>;
    fn ref_mut<'a>(&self, has_arena: &'a impl HasArenaHarness) -> RefMut<'a, Self::Item> {
        unimplemented!()
    }
}

impl InArenaHarness for Id<NodeIO> {
    type Item = NodeIO;

    fn ref_<'a>(&self, has_arena: &'a impl HasArenaHarness) -> Ref<'a, NodeIO> {
        has_arena.node_io(*self)
    }
}
