use typescript_rust::{debug_cell::Ref, id_arena::Id};

use crate::{HasArenaHarness, Inode};

pub trait OptionInArenaHarness {
    type Item;

    #[track_caller]
    fn refed<'a>(self, has_arena: &'a impl HasArenaHarness) -> Option<Ref<'a, Self::Item>>;
}

impl OptionInArenaHarness for Option<Id<Inode>> {
    type Item = Inode;

    fn refed<'a>(self, has_arena: &'a impl HasArenaHarness) -> Option<Ref<'a, Inode>> {
        self.map(|inode| has_arena.inode(inode))
    }
}
