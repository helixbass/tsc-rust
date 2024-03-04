use std::collections::HashMap;

use id_arena::Id;

use crate::{
    HasArena, ModeAwareCache, Path, PerModuleNameCache, ResolvedModuleWithFailedLookupLocations,
    ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
};

pub trait ArenaAlloc: Sized {
    #[track_caller]
    fn alloc(self, arena: &impl HasArena) -> Id<Self>;
}

impl ArenaAlloc for ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>> {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations(self)
    }
}

impl ArenaAlloc for ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>> {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations(
            self,
        )
    }
}

impl ArenaAlloc for PerModuleNameCache {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_per_module_name_cache(self)
    }
}

impl ArenaAlloc
    for HashMap<
        String,
        Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
    >
{
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(self)
    }
}

impl ArenaAlloc
    for HashMap<
        Path,
        Id<
            HashMap<
                String,
                Id<ModeAwareCache<Id<ResolvedTypeReferenceDirectiveWithFailedLookupLocations>>>,
            >,
        >,
    >
{
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_path_mode_aware_cache_resolved_type_reference_directive_with_failed_lookup_locations_map(self)
    }
}

impl ArenaAlloc
    for HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>
{
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(self)
    }
}

impl ArenaAlloc
    for HashMap<
        Path,
        Id<HashMap<String, Id<ModeAwareCache<Id<ResolvedModuleWithFailedLookupLocations>>>>>,
    >
{
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_path_mode_aware_cache_resolved_module_with_failed_lookup_locations_map(self)
    }
}

impl ArenaAlloc for HashMap<String, Id<PerModuleNameCache>> {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_per_module_name_cache_map(self)
    }
}

impl ArenaAlloc for HashMap<Path, Id<HashMap<String, Id<PerModuleNameCache>>>> {
    fn alloc(self, arena: &impl HasArena) -> Id<Self> {
        arena.alloc_path_per_module_name_cache_map(self)
    }
}
