use std::any::Any;

use debug_cell::Ref;
use id_arena::Id;

use crate::{EmitHost, ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, Transformer};

mod all_arenas;
pub use all_arenas::*;
mod arena_alloc;
pub use arena_alloc::*;
mod has_arena;
pub use has_arena::*;
mod in_arena;
pub use in_arena::*;
mod option_in_arena;
pub use option_in_arena::*;

pub fn downcast_transformer_ref<TTransformer: Any>(
    transformer: Transformer,
    arena: &impl HasArena,
) -> Ref<'_, TTransformer> {
    Ref::map(transformer.ref_(arena), |transformer| {
        transformer
            .as_dyn_any()
            .downcast_ref::<TTransformer>()
            .unwrap()
    })
}

pub enum IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    EmitHost(Id<Box<dyn EmitHost>>),
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory(
        Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>,
    ),
}

impl From<Id<Box<dyn EmitHost>>> for IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    fn from(value: Id<Box<dyn EmitHost>>) -> Self {
        Self::EmitHost(value)
    }
}

impl From<Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>>
    for IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory
{
    fn from(value: Id<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>) -> Self {
        Self::ModuleSpecifierResolutionHostAndGetCommonSourceDirectory(value)
    }
}

impl InArena for IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory {
    type Item = dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory;

    fn ref_<'a>(
        &self,
        arena: &'a impl HasArena,
    ) -> Ref<'a, dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory + 'static> {
        match self {
            Self::EmitHost(value) => Ref::map(value.ref_(arena), |value| {
                value.as_module_specifier_resolution_host_and_get_common_source_directory()
            }),
            Self::ModuleSpecifierResolutionHostAndGetCommonSourceDirectory(value) => {
                Ref::map(value.ref_(arena), |value| &**value)
            }
        }
    }
}

#[macro_export]
macro_rules! per_arena {
    ($type:ty, $arena:expr, $initializer:expr $(,)?) => {{
        use std::cell::RefCell;
        use std::collections::HashMap;
        use $crate::id_arena::Id;
        use $crate::AllArenasId;

        thread_local! {
            static PER_ARENA: RefCell<HashMap<AllArenasId, Id<$type>>> = RefCell::new(HashMap::new());
        }

        PER_ARENA.with(|per_arena| {
            let mut per_arena = per_arena.borrow_mut();
            let arena_id = $arena.all_arenas_id();
            *per_arena.entry(arena_id).or_insert_with(|| $initializer)
        })
    }}
}

#[macro_export]
macro_rules! impl_has_arena {
    ($type:ty $(,)?) => {
        impl $crate::HasArena for $type {
            fn arena(&self) -> &$crate::AllArenas {
                unsafe { &*self.arena }
            }
        }
    };
}

#[macro_export]
macro_rules! released {
    ($expr:expr $(,)?) => {{
        let value = $expr;
        value
    }};
}
