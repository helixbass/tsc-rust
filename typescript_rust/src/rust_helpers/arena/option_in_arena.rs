use std::collections::HashMap;

use debug_cell::Ref;
use id_arena::Id;

use crate::{
    CommandLineOption, CompilerOptions, ExternalModuleInfo, FileIncludeReason, HasArena,
    ModuleResolutionCache, Node, NodeArray, PackageJsonInfo, PackageJsonInfoCache,
    PerModuleNameCache, Program, ResolvedModuleFull, Symbol, TypeReferenceDirectiveResolutionCache,
};

pub trait OptionInArena {
    type Item;

    #[track_caller]
    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Self::Item>>;
}

impl OptionInArena for Option<Id<Node>> {
    type Item = Node;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Node>> {
        // self.map(|node| has_arena.node(node))
        match self {
            None => None,
            Some(node) => Some(has_arena.node(node)),
        }
    }
}

impl OptionInArena for Option<Id<FileIncludeReason>> {
    type Item = FileIncludeReason;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, FileIncludeReason>> {
        self.map(|file_include_reason| has_arena.file_include_reason(file_include_reason))
    }
}

impl OptionInArena for Option<Id<CompilerOptions>> {
    type Item = CompilerOptions;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, CompilerOptions>> {
        // self.map(|compiler_options| has_arena.compiler_options(compiler_options))
        match self {
            None => None,
            Some(compiler_options) => Some(has_arena.compiler_options(compiler_options)),
        }
    }
}

impl OptionInArena for Option<Id<Program>> {
    type Item = Program;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Program>> {
        self.map(|program| has_arena.program(program))
    }
}

impl OptionInArena for Option<Id<NodeArray>> {
    type Item = NodeArray;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, NodeArray>> {
        // self.map(|node_array| has_arena.node_array(node_array))
        match self {
            None => None,
            Some(node_array) => Some(has_arena.node_array(node_array)),
        }
    }
}

impl OptionInArena for Option<Id<TypeReferenceDirectiveResolutionCache>> {
    type Item = TypeReferenceDirectiveResolutionCache;

    fn refed<'a>(
        self,
        has_arena: &'a impl HasArena,
    ) -> Option<Ref<'a, TypeReferenceDirectiveResolutionCache>> {
        self.map(|type_reference_directive_resolution_cache| {
            has_arena.type_reference_directive_resolution_cache(
                type_reference_directive_resolution_cache,
            )
        })
    }
}

impl OptionInArena for Option<Id<ModuleResolutionCache>> {
    type Item = ModuleResolutionCache;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, ModuleResolutionCache>> {
        self.map(|module_resolution_cache| {
            has_arena.module_resolution_cache(module_resolution_cache)
        })
    }
}

impl OptionInArena for Option<Id<ExternalModuleInfo>> {
    type Item = ExternalModuleInfo;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, ExternalModuleInfo>> {
        self.map(|external_module_info| has_arena.external_module_info(external_module_info))
    }
}

impl OptionInArena for Option<Id<Box<dyn PackageJsonInfoCache>>> {
    type Item = Box<dyn PackageJsonInfoCache>;

    fn refed<'a>(
        self,
        has_arena: &'a impl HasArena,
    ) -> Option<Ref<'a, Box<dyn PackageJsonInfoCache>>> {
        self.map(|package_json_info_cache| {
            has_arena.package_json_info_cache(package_json_info_cache)
        })
    }
}

impl OptionInArena for Option<Id<PerModuleNameCache>> {
    type Item = PerModuleNameCache;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, PerModuleNameCache>> {
        self.map(|per_module_name_cache| has_arena.per_module_name_cache(per_module_name_cache))
    }
}

impl OptionInArena for Option<Id<PackageJsonInfo>> {
    type Item = PackageJsonInfo;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, PackageJsonInfo>> {
        self.map(|package_json_info| has_arena.package_json_info(package_json_info))
    }
}

impl OptionInArena for Option<Id<CommandLineOption>> {
    type Item = CommandLineOption;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, CommandLineOption>> {
        // self.map(|command_line_option| has_arena.command_line_option(command_line_option))
        match self {
            None => None,
            Some(command_line_option) => Some(has_arena.command_line_option(command_line_option)),
        }
    }
}

impl OptionInArena for Option<Id<HashMap<String, Id<CommandLineOption>>>> {
    type Item = HashMap<String, Id<CommandLineOption>>;

    fn refed<'a>(
        self,
        has_arena: &'a impl HasArena,
    ) -> Option<Ref<'a, HashMap<String, Id<CommandLineOption>>>> {
        self.map(|command_line_options_map| {
            has_arena.command_line_options_map(command_line_options_map)
        })
    }
}

impl OptionInArena for Option<Id<Vec<Id<Symbol>>>> {
    type Item = Vec<Id<Symbol>>;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, Vec<Id<Symbol>>>> {
        self.map(|vec_symbol| has_arena.vec_symbol(vec_symbol))
    }
}

impl OptionInArena for Option<Id<ResolvedModuleFull>> {
    type Item = ResolvedModuleFull;

    fn refed<'a>(self, has_arena: &'a impl HasArena) -> Option<Ref<'a, ResolvedModuleFull>> {
        self.map(|resolved_module_full| has_arena.resolved_module_full(resolved_module_full))
    }
}
