use typescript_rust::{debug_cell::Ref, id_arena::Id};

mod all_arenas_harness;
pub use all_arenas_harness::*;
mod eq_arena;
pub use eq_arena::*;
mod has_arena_harness;
pub use has_arena_harness::*;
mod in_arena_harness;
pub use in_arena_harness::*;
mod option_in_arena_harness;
pub use option_in_arena_harness::*;

use crate::{harness::harness_io::NodeIO, vfs::FileSystemResolverHost};

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

#[macro_export]
macro_rules! impl_has_arena_harness {
    ($type:ty $(,)?) => {
        impl typescript_rust::HasArena for $type {
            fn arena(&self) -> &typescript_rust::AllArenas {
                unsafe { &(*self.arena).all_arenas }
            }
        }

        impl $crate::HasArenaHarness for $type {
            fn arena_harness(&self) -> &$crate::AllArenasHarness {
                unsafe { &*self.arena }
            }
        }
    };
}
