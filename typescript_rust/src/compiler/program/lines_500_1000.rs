use std::{
    cell::{Ref, RefCell, RefMut, Cell},
    collections::{HashMap, HashSet},
    convert::TryInto,
    io, mem,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;
use local_macros::enum_unwrapped;

use super::{
    create_compiler_host, get_module_name_string_literal_at,
    parse_config_host_from_compiler_host_like,
    update_host_for_use_source_of_project_reference_redirect, CompilerHostLikeRcDynCompilerHost,
    GetResolvedProjectReferences, HostForUseSourceOfProjectReferenceRedirect,
    UpdateHostForUseSourceOfProjectReferenceRedirectReturn,
};
use crate::{
    change_extension, clone, combine_paths, create_diagnostic_collection, create_file_diagnostic,
    create_module_resolution_cache, create_multi_map,
    create_type_reference_directive_resolution_cache, extension_from_path, file_extension_is,
    file_extension_is_one_of, gc_cell_ref_mut_unwrapped, get_automatic_type_directive_names,
    get_common_source_directory_of_config, get_directory_path, get_emit_module_kind,
    get_emit_module_resolution_kind, get_output_declaration_file_name, get_package_scope_for_path,
    get_supported_extensions, get_supported_extensions_with_json_if_resolve_json_module,
    is_import_call, is_import_equals_declaration, is_logging, map_defined, maybe_for_each,
    options_have_changes, out_file, ref_mut_unwrapped, ref_unwrapped, resolve_module_name,
    resolve_type_reference_directive, skip_trivia, source_file_affecting_compiler_options,
    stable_sort, to_file_name_lower_case, try_maybe_for_each, walk_up_parenthesized_expressions,
    AsDoubleDeref, AutomaticTypeDirectiveFile, CompilerHost, CompilerOptions, CreateProgramOptions,
    Debug_, Diagnostic, DiagnosticCollection, Extension, FileIncludeKind, FileIncludeReason,
    FilePreprocessingDiagnostics, FilePreprocessingDiagnosticsKind, GetOrInsertDefault,
    GetProgramBuildInfo, LibFile, ModuleKind, ModuleResolutionCache, ModuleResolutionHost,
    ModuleResolutionHostOverrider, ModuleResolutionKind, ModuleSpecifierResolutionHost, MultiMap,
    Node, NodeInterface, NonEmpty, OptionTry, PackageId, PackageJsonInfoCache, ParseConfigFileHost,
    ParsedCommandLine, Path, Program, ProjectReference, ProjectReferenceFile, ReadonlyTextRange,
    RedirectTargetsMap, ReferencedFile, ResolvedModuleFull, ResolvedProjectReference,
    ResolvedTypeReferenceDirective, RootFile, ScriptReferenceHost, SourceFile, SourceFileLike,
    SourceFileMayBeEmittedHost, SourceOfProjectReferenceRedirect, StructureIsReused, SymlinkCache,
    TextRange, TypeCheckerHost, TypeCheckerHostDebuggable, TypeReferenceDirectiveResolutionCache,
    VecExt,
    HasArena, AllArenas, InArena, static_arena, OptionInArena,
};

pub trait LoadWithLocalCacheLoader<TValue>: Trace + Finalize {
    fn call(
        &self,
        name: &str,
        containing_file: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<TValue>;
}

#[derive(Trace, Finalize)]
pub struct LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    options: Id<CompilerOptions>,
    host: Id<Box<dyn CompilerHost>>,
    type_reference_directive_resolution_cache: Option<Id<TypeReferenceDirectiveResolutionCache>>,
}

impl LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    pub fn new(
        options: Id<CompilerOptions>,
        host: Id<Box<dyn CompilerHost>>,
        type_reference_directive_resolution_cache: Option<
            Id<TypeReferenceDirectiveResolutionCache>,
        >,
    ) -> Self {
        Self {
            options,
            host,
            type_reference_directive_resolution_cache,
        }
    }
}

impl LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>
    for LoadWithLocalCacheLoaderResolveTypeReferenceDirective
{
    fn call(
        &self,
        types_ref: &str,
        containing_file: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Id<ResolvedTypeReferenceDirective>> {
        Ok(resolve_type_reference_directive(
            types_ref,
            Some(containing_file),
            self.options.clone(),
            self.host.ref_(self).as_dyn_module_resolution_host(),
            redirected_reference,
            self.type_reference_directive_resolution_cache.clone(),
            self,
        )?
        .ref_(self).resolved_type_reference_directive
        .clone()
        .unwrap())
    }
}

impl HasArena for LoadWithLocalCacheLoaderResolveTypeReferenceDirective {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub(crate) fn load_with_local_cache<TValue: Clone>(
    names: &[String],
    containing_file: &str,
    redirected_reference: Option<Id<ResolvedProjectReference>>,
    loader: &dyn LoadWithLocalCacheLoader<TValue>,
) -> io::Result<Vec<TValue>> {
    if names.is_empty() {
        return Ok(Default::default());
    }
    let mut resolutions: Vec<TValue> = Default::default();
    let mut cache: HashMap<String, TValue> = HashMap::new();
    for name in names {
        let result: TValue;
        if cache.contains_key(name) {
            result = cache.get(name).cloned().unwrap();
        } else {
            result = loader.call(name, containing_file, redirected_reference.clone())?;
            cache.insert(name.clone(), result.clone());
        }
        resolutions.push(result);
    }
    Ok(resolutions)
}

pub(crate) trait SourceFileImportsList {
    fn maybe_imports(&self) -> GcCellRef<Option<Vec<Id<Node>>>>;
    fn imports(&self) -> GcCellRef<Vec<Id<Node>>>;
    fn maybe_module_augmentations(&self) -> GcCellRef<Option<Vec<Id<Node>>>>;
    fn module_augmentations(&self) -> GcCellRef<Vec<Id<Node>>>;
    fn maybe_implied_node_format(&self) -> Option<ModuleKind>;
}

impl SourceFileImportsList for SourceFile {
    fn maybe_imports(&self) -> GcCellRef<Option<Vec<Id<Node>>>> {
        self.maybe_imports()
    }

    fn imports(&self) -> GcCellRef<Vec<Id<Node>>> {
        self.imports()
    }

    fn maybe_module_augmentations(&self) -> GcCellRef<Option<Vec<Id<Node>>>> {
        self.maybe_module_augmentations()
    }

    fn module_augmentations(&self) -> GcCellRef<Vec<Id<Node>>> {
        self.module_augmentations()
    }

    fn maybe_implied_node_format(&self) -> Option<ModuleKind> {
        self.maybe_implied_node_format()
    }
}

pub(crate) fn get_mode_for_resolution_at_index(
    file: &impl SourceFileImportsList,
    index: usize,
    arena: &impl HasArena,
) -> Option<ModuleKind> {
    if file.maybe_implied_node_format().is_none() {
        return None;
    }
    get_mode_for_usage_location(
        file.maybe_implied_node_format(),
        get_module_name_string_literal_at(file, index, arena),
        arena,
    )
}

pub(crate) fn get_mode_for_usage_location(
    implied_node_format: Option<ModuleKind>,
    usage: Id<Node>, /*StringLiteralLike*/
    arena: &impl HasArena,
) -> Option<ModuleKind> {
    let implied_node_format = implied_node_format?;
    if implied_node_format != ModuleKind::ESNext {
        return Some(
            if is_import_call(walk_up_parenthesized_expressions(usage.ref_(arena).parent(), arena).unwrap(), arena) {
                ModuleKind::ESNext
            } else {
                ModuleKind::CommonJS
            },
        );
    }
    let expr_parent_parent =
        walk_up_parenthesized_expressions(usage.ref_(arena).parent(), arena).and_then(|node| node.ref_(arena).maybe_parent());
    Some(
        if matches!(
            expr_parent_parent,
            Some(expr_parent_parent) if is_import_equals_declaration(&expr_parent_parent.ref_(arena))
        ) {
            ModuleKind::CommonJS
        } else {
            ModuleKind::ESNext
        },
    )
}

pub trait LoadWithModeAwareCacheLoader<TValue>: Trace + Finalize {
    fn call(
        &self,
        name: &str,
        resolver_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        containing_file_name: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<TValue>;
}

#[derive(Trace, Finalize)]
pub struct LoadWithModeAwareCacheLoaderResolveModuleName {
    options: Id<CompilerOptions>,
    host: Id<Box<dyn CompilerHost>>,
    module_resolution_cache: Option<Id<ModuleResolutionCache>>,
}

impl LoadWithModeAwareCacheLoaderResolveModuleName {
    pub fn new(
        options: Id<CompilerOptions>,
        host: Id<Box<dyn CompilerHost>>,
        module_resolution_cache: Option<Id<ModuleResolutionCache>>,
    ) -> Self {
        Self {
            options,
            host,
            module_resolution_cache,
        }
    }
}

impl LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>
    for LoadWithModeAwareCacheLoaderResolveModuleName
{
    fn call(
        &self,
        module_name: &str,
        resolver_mode: Option<ModuleKind /*ModuleKind.CommonJS | ModuleKind.ESNext*/>,
        containing_file_name: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Option<Id<ResolvedModuleFull>>> {
        Ok(resolve_module_name(
            module_name,
            containing_file_name,
            self.options.clone(),
            self.host.ref_(self).as_dyn_module_resolution_host(),
            self.module_resolution_cache.clone(),
            redirected_reference,
            resolver_mode,
            self,
        )?
        .ref_(self).resolved_module
        .clone())
    }
}

impl HasArena for LoadWithModeAwareCacheLoaderResolveModuleName
{
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub(crate) fn load_with_mode_aware_cache<TValue: Clone>(
    names: &[String],
    containing_file: Id<Node>, /*SourceFile*/
    containing_file_name: &str,
    redirected_reference: Option<Id<ResolvedProjectReference>>,
    loader: &dyn LoadWithModeAwareCacheLoader<TValue>,
    arena: &impl HasArena,
) -> io::Result<Vec<TValue>> {
    if names.is_empty() {
        return Ok(vec![]);
    }
    let mut resolutions: Vec<TValue> = Default::default();
    let mut cache: HashMap<String, TValue> = Default::default();
    let mut i = 0;
    let containing_file_ref = containing_file.ref_(arena);
    let containing_file_as_source_file = containing_file_ref.as_source_file();
    for name in names {
        let result: TValue;
        let mode = get_mode_for_resolution_at_index(containing_file_as_source_file, i, arena);
        i += 1;
        let cache_key = if let Some(mode) = mode {
            format!("{:?}|{}", mode, name)
        } else {
            name.clone()
        };
        if cache.contains_key(&cache_key) {
            result = cache.get(&cache_key).unwrap().clone();
        } else {
            result = loader.call(
                name,
                mode,
                containing_file_name,
                redirected_reference.clone(),
            )?;
            cache.insert(cache_key, result.clone());
        }
        resolutions.push(result);
    }
    Ok(resolutions)
}

pub fn for_each_resolved_project_reference<TReturn>(
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    mut cb: impl FnMut(Id<ResolvedProjectReference>, Option<Id<ResolvedProjectReference>>) -> Option<TReturn>,
    arena: &impl HasArena,
) -> Option<TReturn> {
    for_each_project_reference(
        None,
        resolved_project_references,
        |resolved_ref: Option<Id<ResolvedProjectReference>>,
         parent: Option<Id<ResolvedProjectReference>>,
         _| { resolved_ref.and_then(|resolved_ref| cb(resolved_ref, parent)) },
        Option::<
            fn(
                Option<&[Rc<ProjectReference>]>,
                Option<Id<ResolvedProjectReference>>,
            ) -> Option<TReturn>,
        >::None,
        arena,
    )
}

pub fn for_each_project_reference<TReturn>(
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    mut cb_resolved_ref: impl FnMut(
        Option<Id<ResolvedProjectReference>>,
        Option<Id<ResolvedProjectReference>>,
        usize,
    ) -> Option<TReturn>,
    cb_ref: Option<
        impl Fn(Option<&[Rc<ProjectReference>]>, Option<Id<ResolvedProjectReference>>) -> Option<TReturn>,
    >,
    arena: &impl HasArena,
) -> Option<TReturn> {
    let mut seen_resolved_refs: Option<HashSet<Path>> = None;

    for_each_project_reference_worker(
        cb_ref.as_ref(),
        &mut cb_resolved_ref,
        &mut seen_resolved_refs,
        project_references,
        resolved_project_references,
        None,
        arena,
    )
}

pub fn try_for_each_project_reference<TReturn, TError>(
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    mut cb_resolved_ref: impl FnMut(
        Option<Id<ResolvedProjectReference>>,
        Option<Id<ResolvedProjectReference>>,
        usize,
    ) -> Result<Option<TReturn>, TError>,
    cb_ref: Option<
        impl Fn(
            Option<&[Rc<ProjectReference>]>,
            Option<Id<ResolvedProjectReference>>,
        ) -> Result<Option<TReturn>, TError>,
    >,
    arena: &impl HasArena,
) -> Result<Option<TReturn>, TError> {
    let mut seen_resolved_refs: Option<HashSet<Path>> = None;

    for_each_project_reference_worker_fallible(
        cb_ref.as_ref(),
        &mut cb_resolved_ref,
        &mut seen_resolved_refs,
        project_references,
        resolved_project_references,
        None,
        arena,
    )
}

#[allow(dead_code)]
pub fn for_each_project_reference_bool(
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    mut cb_resolved_ref: impl FnMut(
        Option<Id<ResolvedProjectReference>>,
        Option<Id<ResolvedProjectReference>>,
        usize,
    ) -> bool,
    cb_ref: Option<
        impl Fn(Option<&[Rc<ProjectReference>]>, Option<Id<ResolvedProjectReference>>) -> bool,
    >,
    arena: &impl HasArena,
) -> bool {
    for_each_project_reference(
        project_references,
        resolved_project_references,
        |a: Option<Id<ResolvedProjectReference>>,
         b: Option<Id<ResolvedProjectReference>>,
         c: usize| { cb_resolved_ref(a, b, c).then_some(()) },
        cb_ref.as_ref().map(|cb_ref| {
            |a: Option<&[Rc<ProjectReference>]>, b: Option<Id<ResolvedProjectReference>>| {
                cb_ref(a, b).then_some(())
            }
        }),
        arena,
    )
    .is_some()
}

pub fn try_for_each_project_reference_bool<TError>(
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    mut cb_resolved_ref: impl FnMut(
        Option<Id<ResolvedProjectReference>>,
        Option<Id<ResolvedProjectReference>>,
        usize,
    ) -> Result<bool, TError>,
    cb_ref: Option<
        impl Fn(
            Option<&[Rc<ProjectReference>]>,
            Option<Id<ResolvedProjectReference>>,
        ) -> Result<bool, TError>,
    >,
    arena: &impl HasArena,
) -> Result<bool, TError> {
    Ok(try_for_each_project_reference(
        project_references,
        resolved_project_references,
        |a: Option<Id<ResolvedProjectReference>>,
         b: Option<Id<ResolvedProjectReference>>,
         c: usize| { Ok(cb_resolved_ref(a, b, c)?.then_some(())) },
        cb_ref.as_ref().map(|cb_ref| {
            |a: Option<&[Rc<ProjectReference>]>, b: Option<Id<ResolvedProjectReference>>| {
                Ok(cb_ref(a, b)?.then_some(()))
            }
        }),
        arena,
    )?
    .is_some())
}

fn for_each_project_reference_worker<TReturn>(
    cb_ref: Option<
        &impl Fn(Option<&[Rc<ProjectReference>]>, Option<Id<ResolvedProjectReference>>) -> Option<TReturn>,
    >,
    cb_resolved_ref: &mut impl FnMut(
        Option<Id<ResolvedProjectReference>>,
        Option<Id<ResolvedProjectReference>>,
        usize,
    ) -> Option<TReturn>,
    seen_resolved_refs: &mut Option<HashSet<Path>>,
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    parent: Option<Id<ResolvedProjectReference>>,
    arena: &impl HasArena,
) -> Option<TReturn> {
    if let Some(cb_ref) = cb_ref {
        let result = cb_ref(project_references, parent);
        if result.is_some() {
            return result;
        }
    }

    maybe_for_each(
        resolved_project_references,
        |resolved_ref: &Option<Id<ResolvedProjectReference>>, index| {
            if matches!(
                resolved_ref.as_ref(),
                Some(resolved_ref) if matches!(
                    seen_resolved_refs,
                    Some(seen_resolved_refs) if seen_resolved_refs.contains(
                        &*resolved_ref.ref_(arena).source_file.ref_(arena).as_source_file().path()
                    )
                )
            ) {
                return None;
            }

            let result = cb_resolved_ref(resolved_ref.clone(), parent, index);
            if result.is_some() || resolved_ref.is_none() {
                return result;
            }
            let resolved_ref = resolved_ref.unwrap();

            seen_resolved_refs
                .get_or_insert_default_()
                .insert(resolved_ref.ref_(arena).source_file.ref_(arena).as_source_file().path().clone());
            for_each_project_reference_worker(
                cb_ref,
                cb_resolved_ref,
                seen_resolved_refs,
                resolved_ref.ref_(arena).command_line.ref_(arena).project_references.as_deref(),
                resolved_ref.ref_(arena).maybe_references().as_deref(),
                Some(resolved_ref),
                arena,
            )
        },
    )
}

fn for_each_project_reference_worker_fallible<TReturn, TError>(
    cb_ref: Option<
        &impl Fn(
            Option<&[Rc<ProjectReference>]>,
            Option<Id<ResolvedProjectReference>>,
        ) -> Result<Option<TReturn>, TError>,
    >,
    cb_resolved_ref: &mut impl FnMut(
        Option<Id<ResolvedProjectReference>>,
        Option<Id<ResolvedProjectReference>>,
        usize,
    ) -> Result<Option<TReturn>, TError>,
    seen_resolved_refs: &mut Option<HashSet<Path>>,
    project_references: Option<&[Rc<ProjectReference>]>,
    resolved_project_references: Option<&[Option<Id<ResolvedProjectReference>>]>,
    parent: Option<Id<ResolvedProjectReference>>,
    arena: &impl HasArena,
) -> Result<Option<TReturn>, TError> {
    if let Some(cb_ref) = cb_ref {
        let result = cb_ref(project_references, parent)?;
        if result.is_some() {
            return Ok(result);
        }
    }

    try_maybe_for_each(
        resolved_project_references,
        |&resolved_ref: &Option<Id<ResolvedProjectReference>>, index| {
            if matches!(
                resolved_ref,
                Some(resolved_ref) if matches!(
                    seen_resolved_refs,
                    Some(seen_resolved_refs) if seen_resolved_refs.contains(
                        &*resolved_ref.ref_(arena).source_file.ref_(arena).as_source_file().path()
                    )
                )
            ) {
                return Ok(None);
            }

            let result = cb_resolved_ref(resolved_ref.clone(), parent, index)?;
            if result.is_some() || resolved_ref.is_none() {
                return Ok(result);
            }
            let resolved_ref = resolved_ref.unwrap();

            seen_resolved_refs
                .get_or_insert_default_()
                .insert(resolved_ref.ref_(arena).source_file.ref_(arena).as_source_file().path().clone());
            for_each_project_reference_worker_fallible(
                cb_ref,
                cb_resolved_ref,
                seen_resolved_refs,
                resolved_ref.ref_(arena).command_line.ref_(arena).project_references.as_deref(),
                resolved_ref.ref_(arena).maybe_references().as_deref(),
                Some(resolved_ref),
                arena,
            )
        },
    )
}

pub(crate) const inferred_types_containing_file: &str = "__inferred type names__.ts";

#[derive(Trace, Finalize)]
pub(crate) struct DiagnosticCache {
    pub per_file: Option<HashMap<Path, Vec<Id<Diagnostic>>>>,
    pub all_diagnostics: Option<Vec<Id<Diagnostic>>>,
}

impl Default for DiagnosticCache {
    fn default() -> Self {
        Self {
            per_file: None,
            all_diagnostics: None,
        }
    }
}

pub(crate) fn is_referenced_file(reason: Option<&FileIncludeReason>) -> bool {
    matches!(reason, Some(FileIncludeReason::ReferencedFile(_)))
}

#[derive(Debug)]
pub(crate) struct ReferenceFileLocation {
    pub file: Id<Node /*SourceFile*/>,
    pub pos: isize,
    pub end: isize,
    pub package_id: Option<PackageId>,
}

#[derive(Debug)]
pub(crate) struct SyntheticReferenceFileLocation {
    pub file: Id<Node /*SourceFile*/>,
    pub package_id: Option<PackageId>,
    pub text: String,
}

pub(crate) fn is_reference_file_location(
    location: &ReferenceFileLocationOrSyntheticReferenceFileLocation,
) -> bool {
    matches!(
        location,
        ReferenceFileLocationOrSyntheticReferenceFileLocation::ReferenceFileLocation(_)
    )
}

#[derive(Debug)]
pub(crate) enum ReferenceFileLocationOrSyntheticReferenceFileLocation {
    ReferenceFileLocation(ReferenceFileLocation),
    SyntheticReferenceFileLocation(SyntheticReferenceFileLocation),
}

impl ReferenceFileLocationOrSyntheticReferenceFileLocation {
    pub fn maybe_package_id(&self) -> Option<&PackageId> {
        match self {
            Self::ReferenceFileLocation(location) => location.package_id.as_ref(),
            Self::SyntheticReferenceFileLocation(location) => location.package_id.as_ref(),
        }
    }

    pub fn file(&self) -> Id<Node> {
        match self {
            Self::ReferenceFileLocation(location) => location.file.clone(),
            Self::SyntheticReferenceFileLocation(location) => location.file.clone(),
        }
    }

    pub fn as_reference_file_location(&self) -> &ReferenceFileLocation {
        enum_unwrapped!(
            self,
            [
                ReferenceFileLocationOrSyntheticReferenceFileLocation,
                ReferenceFileLocation
            ]
        )
    }
}

impl From<ReferenceFileLocation> for ReferenceFileLocationOrSyntheticReferenceFileLocation {
    fn from(value: ReferenceFileLocation) -> Self {
        Self::ReferenceFileLocation(value)
    }
}

impl From<SyntheticReferenceFileLocation>
    for ReferenceFileLocationOrSyntheticReferenceFileLocation
{
    fn from(value: SyntheticReferenceFileLocation) -> Self {
        Self::SyntheticReferenceFileLocation(value)
    }
}

pub(crate) fn get_referenced_file_location(
    mut get_source_file_by_path: impl FnMut(&Path) -> Option<Id<Node /*SourceFile*/>>,
    ref_: &ReferencedFile,
    arena: &impl HasArena,
) -> ReferenceFileLocationOrSyntheticReferenceFileLocation {
    let file = Debug_.check_defined(get_source_file_by_path(&ref_.file), None);
    let kind = ref_.kind;
    let index = ref_.index;
    let pos: Option<isize>;
    let end: Option<isize>;
    let mut package_id: Option<PackageId> = None;
    let file_ref = file.ref_(arena);
    let file_as_source_file = file_ref.as_source_file();
    match kind {
        FileIncludeKind::Import => {
            let import_literal = get_module_name_string_literal_at(file_as_source_file, index, arena);
            let import_literal_ref = import_literal.ref_(arena);
            let import_literal_text = import_literal_ref.as_literal_like_node().text();
            package_id = file_as_source_file
                .maybe_resolved_modules()
                .as_ref()
                .and_then(|file_resolved_modules| {
                    file_resolved_modules
                        .get(
                            &import_literal_text,
                            get_mode_for_resolution_at_index(file_as_source_file, index, arena),
                        )
                        .flatten()
                })
                .and_then(|resolved_module| resolved_module.ref_(arena).package_id.clone());
            if import_literal.ref_(arena).pos() == -1 {
                return SyntheticReferenceFileLocation {
                    file,
                    package_id,
                    text: import_literal_text.clone(),
                }
                .into();
            }
            pos = Some(skip_trivia(
                &file_as_source_file.text_as_chars(),
                import_literal.ref_(arena).pos(),
                None,
                None,
                None,
            ));
            end = Some(import_literal.ref_(arena).end());
        }
        FileIncludeKind::ReferenceFile => {
            let file_referenced_files = file_as_source_file.referenced_files();
            let file_referenced_files = (*file_referenced_files).borrow();
            pos = Some(file_referenced_files[index].pos());
            end = Some(file_referenced_files[index].end());
        }
        FileIncludeKind::TypeReferenceDirective => {
            let file_type_reference_directives = file_as_source_file.type_reference_directives();
            let file_type_reference_directives = (*file_type_reference_directives).borrow();
            pos = Some(file_type_reference_directives[index].pos());
            end = Some(file_type_reference_directives[index].end());
            package_id = file_as_source_file
                .maybe_resolved_type_reference_directive_names()
                .as_ref()
                .and_then(|file_resolved_type_reference_directive_names| {
                    file_resolved_type_reference_directive_names
                        .get(
                            &to_file_name_lower_case(
                                &file_type_reference_directives[index].file_name,
                            ),
                            file_as_source_file.maybe_implied_node_format(),
                        )
                        .flatten()
                })
                .and_then(|resolved| resolved.ref_(arena).package_id.clone());
        }
        FileIncludeKind::LibReferenceDirective => {
            let file_lib_reference_directives = file_as_source_file.lib_reference_directives();
            let file_lib_reference_directives = (*file_lib_reference_directives).borrow();
            pos = Some(file_lib_reference_directives[index].pos());
            end = Some(file_lib_reference_directives[index].end());
        }
        _ => {
            Debug_.assert_never(kind, None);
        }
    }
    ReferenceFileLocation {
        file,
        pos: pos.unwrap(),
        end: end.unwrap(),
        package_id,
    }
    .into()
}

pub fn get_config_file_parsing_diagnostics(
    config_file_parse_result: &ParsedCommandLine,
    arena: &impl HasArena,
) -> Vec<Id<Diagnostic>> {
    if let Some(config_file_parse_result_options_config_file) =
        config_file_parse_result.options.ref_(arena).config_file.as_ref()
    {
        config_file_parse_result_options_config_file
            .ref_(arena).as_source_file()
            .parse_diagnostics()
            .ref_(arena)
            .clone()
            .and_extend(config_file_parse_result.errors.ref_(arena).iter().cloned())
    } else {
        config_file_parse_result.errors.ref_(arena).clone()
    }
}

pub fn get_implied_node_format_for_file(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: Id<CompilerOptions>,
    arena: &impl HasArena,
) -> Option<ModuleKind /*ModuleKind.ESNext | ModuleKind.CommonJS*/> {
    match get_emit_module_resolution_kind(&options.ref_(arena)) {
        ModuleResolutionKind::Node12 | ModuleResolutionKind::NodeNext => {
            if file_extension_is_one_of(
                file_name,
                &[Extension::Dmts, Extension::Mts, Extension::Mjs],
            ) {
                Some(ModuleKind::ESNext)
            } else if file_extension_is_one_of(
                file_name,
                &[Extension::Dcts, Extension::Cts, Extension::Cjs],
            ) {
                Some(ModuleKind::CommonJS)
            } else if file_extension_is_one_of(
                file_name,
                &[
                    Extension::Dts,
                    Extension::Ts,
                    Extension::Tsx,
                    Extension::Js,
                    Extension::Jsx,
                ],
            ) {
                Some(lookup_from_package_json(
                    file_name,
                    package_json_info_cache,
                    host,
                    options,
                    arena,
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn lookup_from_package_json(
    file_name: &Path,
    package_json_info_cache: Option<&dyn PackageJsonInfoCache>,
    host: &dyn ModuleResolutionHost,
    options: Id<CompilerOptions>,
    arena: &impl HasArena,
) -> ModuleKind /*ModuleKind.ESNext | ModuleKind.CommonJS*/ {
    let scope = get_package_scope_for_path(file_name, package_json_info_cache, host, options, arena);
    if matches!(
        scope,
        Some(scope) if matches!(
            scope.ref_(arena).package_json_content.as_object().and_then(|scope_package_json_content| {
                scope_package_json_content.get("type_")
            }),
            Some(serde_json::Value::String(value)) if &**value == "module"
        )
    ) {
        ModuleKind::ESNext
    } else {
        ModuleKind::CommonJS
    }
}

fn should_program_create_new_source_files(
    program: Option<&Program>,
    new_options: &CompilerOptions,
    arena: &impl HasArena,
) -> bool {
    let Some(program) = program else {
        return false;
    };
    options_have_changes(
        &program.get_compiler_options().ref_(program),
        new_options,
        &source_file_affecting_compiler_options(arena).ref_(arena),
        arena,
    )
}

pub fn create_program(root_names_or_options: CreateProgramOptions, arena: &impl HasArena) -> io::Result<Id<Program>> {
    let create_program_options = root_names_or_options;
    let program = Program::new(create_program_options, arena);
    program.ref_(arena).create()?;
    Ok(program)
}

impl Program {
    pub fn new(
        create_program_options: CreateProgramOptions,
        arena: &impl HasArena,
        // options: Id<CompilerOptions>,
        // files: Vec<Id<Node>>,
        // current_directory: String,
        // host: Id<Box<dyn CompilerHost>>,
    ) -> Id<Self> {
        let options = create_program_options.options.clone();
        let max_node_module_js_depth = options.ref_(arena).max_node_module_js_depth.unwrap_or(0);
        let ret =
            arena.alloc_program(Self {
                _arena_id: Default::default(),
                create_program_options: RefCell::new(Some(create_program_options)),
                root_names: Default::default(),
                options,
                config_file_parsing_diagnostics: Default::default(),
                project_references: Default::default(),
                processing_default_lib_files: Default::default(),
                processing_other_files: Default::default(),
                files: Default::default(),
                symlinks: Default::default(),
                common_source_directory: Default::default(),
                diagnostics_producing_type_checker: Default::default(),
                no_diagnostics_type_checker: Default::default(),
                classifiable_names: Default::default(),
                ambient_module_name_to_unmodified_file_name: Default::default(),
                file_reasons: Cell::new(arena.alloc_file_reasons(create_multi_map())),
                cached_bind_and_check_diagnostics_for_file: Default::default(),
                cached_declaration_diagnostics_for_file: Default::default(),

                resolved_type_reference_directives: Cell::new(arena.alloc_resolved_type_reference_directives_map(Default::default())),
                file_processing_diagnostics: Default::default(),

                max_node_module_js_depth,
                current_node_modules_depth: Default::default(),

                modules_with_elided_imports: Default::default(),

                source_files_found_searching_node_modules: Default::default(),

                old_program: Default::default(),
                host: Default::default(),
                config_parsing_host: Default::default(),

                skip_default_lib: Default::default(),
                get_default_library_file_name_memoized: Default::default(),
                default_library_path: Default::default(),
                program_diagnostics: Default::default(),
                current_directory: Default::default(),
                supported_extensions: Default::default(),
                supported_extensions_with_json_if_resolve_json_module: Default::default(),
                has_emit_blocking_diagnostics: Default::default(),
                _compiler_options_object_literal_syntax: Default::default(),
                module_resolution_cache: Default::default(),
                type_reference_directive_resolution_cache: Default::default(),
                actual_resolve_module_names_worker: Default::default(),
                actual_resolve_type_reference_directive_names_worker: Default::default(),
                package_id_to_source_file: Default::default(),
                source_file_to_package_name: Default::default(),
                redirect_targets_map: RefCell::new(Rc::new(RefCell::new(create_multi_map()))),
                uses_uri_style_node_core_modules: Default::default(),
                files_by_name: Default::default(),
                missing_file_paths: Default::default(),
                files_by_name_ignore_case: Default::default(),
                resolved_project_references: Default::default(),
                project_reference_redirects: Default::default(),
                map_from_file_to_project_reference_redirects: Default::default(),
                map_from_to_project_reference_redirect_source: Default::default(),
                use_source_of_project_reference_redirect: Default::default(),
                file_exists_rc: Default::default(),
                directory_exists_rc: Default::default(),
                should_create_new_source_file: Default::default(),
                structure_is_reused: Default::default(),
                get_program_build_info: Default::default(),
            });
        ret.ref_(arena).set_arena_id(ret);
        ret
    }

    pub fn create(&self) -> io::Result<()> {
        let CreateProgramOptions {
            root_names,
            config_file_parsing_diagnostics,
            project_references,
            old_program,
            host,
            ..
        } = self.create_program_options.borrow_mut().take().unwrap();
        *self.root_names.borrow_mut() = Some(root_names);
        *self.config_file_parsing_diagnostics.borrow_mut() = config_file_parsing_diagnostics;
        *self.project_references.borrow_mut() = project_references;
        *self.old_program.borrow_mut() = old_program;

        // tracing?.push(tracing.Phase.Program, "createProgram", { configFilePath: options.configFilePath, rootDir: options.rootDir }, /*separateBeginAndEnd*/ true);
        // performance.mark("beforeProgram");

        *self.host.borrow_mut() = Some(host.unwrap_or_else(|| {
            self.alloc_compiler_host(Box::new(create_compiler_host(self.options.clone(), None, self)))
        }));
        *self.config_parsing_host.borrow_mut() = Some(self.alloc_parse_config_file_host(Box::new(
            parse_config_host_from_compiler_host_like(
                self.alloc_compiler_host_like(Box::new(CompilerHostLikeRcDynCompilerHost::new(
                    self.host(),
                ))),
                None,
                self,
            ),
        )));

        self.skip_default_lib.set(self.options.ref_(self).no_lib);
        *self.default_library_path.borrow_mut() =
            Some(self.host().ref_(self).get_default_lib_location()?.try_unwrap_or_else(
                || -> io::Result<_> {
                    Ok(get_directory_path(&self.get_default_library_file_name()?))
                },
            )?);
        *self.program_diagnostics.borrow_mut() = Some(create_diagnostic_collection(&*static_arena()));
        *self.current_directory.borrow_mut() =
            Some(CompilerHost::get_current_directory(&**self.host().ref_(self))?);
        *self.supported_extensions.borrow_mut() =
            Some(get_supported_extensions(Some(&self.options.ref_(self)), None));
        *self
            .supported_extensions_with_json_if_resolve_json_module
            .borrow_mut() = Some(get_supported_extensions_with_json_if_resolve_json_module(
            Some(&self.options.ref_(self)),
            &self.supported_extensions(),
        ));

        *self.has_emit_blocking_diagnostics.borrow_mut() = Some(HashMap::new());

        if self.host().ref_(self).is_resolve_module_names_supported() {
            *self.actual_resolve_module_names_worker.borrow_mut() = Some(self.alloc_actual_resolve_module_names_worker(Box::new(
                ActualResolveModuleNamesWorkerHost::new(self.host(), self.options.clone()),
            )));
            *self.module_resolution_cache.borrow_mut() = self.host().ref_(self).get_module_resolution_cache();
        } else {
            *self.module_resolution_cache.borrow_mut() =
                Some(self.alloc_module_resolution_cache(create_module_resolution_cache(
                    &self.current_directory(),
                    self.get_canonical_file_name_rc(),
                    Some(self.options.clone()),
                    None,
                    None,
                    self,
                )));
            let loader = LoadWithModeAwareCacheLoaderResolveModuleName::new(
                self.options.clone(),
                self.host(),
                self.maybe_module_resolution_cache().clone(),
            );
            *self.actual_resolve_module_names_worker.borrow_mut() = Some(self.alloc_actual_resolve_module_names_worker(Box::new(
                ActualResolveModuleNamesWorkerLoadWithModeAwareCache::new(self.alloc_load_with_mode_aware_cache_loader(Box::new(
                    loader,
                ))),
            )));
        }

        if self.host().ref_(self).is_resolve_type_reference_directives_supported() {
            *self
                .actual_resolve_type_reference_directive_names_worker
                .borrow_mut() = Some(self.alloc_actual_resolve_type_reference_directive_names_worker(Box::new(
                ActualResolveTypeReferenceDirectiveNamesWorkerHost::new(
                    self.host(),
                    self.options.clone(),
                ),
            )));
        } else {
            *self.type_reference_directive_resolution_cache.borrow_mut() =
                Some(self.alloc_type_reference_directive_resolution_cache(create_type_reference_directive_resolution_cache(
                    &self.current_directory(),
                    self.get_canonical_file_name_rc(),
                    None,
                    self.maybe_module_resolution_cache()
                        .as_ref()
                        .map(|module_resolution_cache| {
                            module_resolution_cache.ref_(self).get_package_json_info_cache()
                        }),
                    None,
                    self,
                )));
            let loader = LoadWithLocalCacheLoaderResolveTypeReferenceDirective::new(
                self.options.clone(),
                self.host(),
                self.maybe_type_reference_directive_resolution_cache()
                    .clone(),
            );
            *self
                .actual_resolve_type_reference_directive_names_worker
                .borrow_mut() = Some(self.alloc_actual_resolve_type_reference_directive_names_worker(Box::new(
                ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache::new(self.alloc_load_with_local_cache_loader(
                    Box::new(loader),
                )),
            )));
        }

        *self.package_id_to_source_file.borrow_mut() = Some(HashMap::new());
        *self.source_file_to_package_name.borrow_mut() = Some(HashMap::new());
        self.uses_uri_style_node_core_modules.set(Some(false));

        *self.files_by_name.borrow_mut() = Some(HashMap::new());
        *self.files_by_name_ignore_case.borrow_mut() =
            if CompilerHost::use_case_sensitive_file_names(&**self.host().ref_(self)) {
                Some(HashMap::new())
            } else {
                None
            };

        self.use_source_of_project_reference_redirect.set(Some(
            self.host().ref_(self).use_source_of_project_reference_redirect() == Some(true)
                && self.options.ref_(self).disable_source_of_project_reference_redirect != Some(true),
        ));
        let UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
            on_program_create_complete,
            file_exists,
            directory_exists,
        } = update_host_for_use_source_of_project_reference_redirect(
            HostForUseSourceOfProjectReferenceRedirect {
                compiler_host: self.host(),
                get_symlink_cache: self.get_symlink_cache_id(),
                use_source_of_project_reference_redirect: self
                    .use_source_of_project_reference_redirect(),
                to_path: self.to_path_rc(),
                get_resolved_project_references: self.alloc_get_resolved_project_references(Box::new(
                    ProgramGetResolvedProjectReferences::new(self.arena_id()),
                )),
                for_each_resolved_project_reference: self.for_each_resolved_project_reference_id(),
            },
            self,
        );
        *self.file_exists_rc.borrow_mut() = Some(file_exists);
        *self.directory_exists_rc.borrow_mut() = directory_exists;

        // tracing?.push(tracing.Phase.Program, "shouldProgramCreateNewSourceFiles", { hasOldProgram: !!oldProgram });
        self.should_create_new_source_file
            .set(Some(should_program_create_new_source_files(
                self.maybe_old_program().refed(self).as_deref(),
                &self.options.ref_(self),
                self,
            )));
        // tracing?.pop();
        // tracing?.push(tracing.Phase.Program, "tryReuseStructureFromOldProgram", {});
        self.set_structure_is_reused(self.try_reuse_structure_from_old_program()?);
        // tracing?.pop();
        if self.structure_is_reused() != StructureIsReused::Completely {
            *self.processing_default_lib_files.borrow_mut() = Some(vec![]);
            *self.processing_other_files.borrow_mut() = Some(vec![]);

            if let Some(project_references) = self.maybe_project_references().as_ref() {
                {
                    self.maybe_resolved_project_references_mut()
                        .try_get_or_insert_with(|| {
                            project_references
                                .into_iter()
                                .map(|project_reference| {
                                    self.parse_project_reference_config_file(project_reference)
                                })
                                .collect()
                        })?;
                }
                if !self.root_names().is_empty() {
                    self.maybe_resolved_project_references().as_ref().try_map(|resolved_project_references| -> io::Result<_> {
                        resolved_project_references.iter().enumerate().try_for_each(|(index, parsed_ref)| -> io::Result<_> {
                            if parsed_ref.is_none() {
                                return Ok(());
                            }
                            let parsed_ref = parsed_ref.as_ref().unwrap();
                            let parsed_ref_command_line_options_ref = parsed_ref.ref_(self).command_line.ref_(self).options.ref_(self);
                            let out = out_file(
                                &parsed_ref_command_line_options_ref,
                            );
                            if self.use_source_of_project_reference_redirect() {
                                if out.is_non_empty() || get_emit_module_kind(&parsed_ref.ref_(self).command_line.ref_(self).options.ref_(self)) == ModuleKind::None {
                                    for file_name in &parsed_ref.ref_(self).command_line.ref_(self).file_names {
                                        self.process_project_reference_file(
                                            file_name,
                                            self.alloc_file_include_reason(FileIncludeReason::ProjectReferenceFile(
                                                ProjectReferenceFile {
                                                    kind: FileIncludeKind::SourceFromProjectReference,
                                                    index,
                                                }
                                            ))
                                        )?;
                                    }
                                }
                            } else {
                                if let Some(out) = out.non_empty() {
                                    self.process_project_reference_file(
                                        &change_extension(out, ".d.ts"),
                                        self.alloc_file_include_reason(FileIncludeReason::ProjectReferenceFile(
                                            ProjectReferenceFile {
                                                kind: FileIncludeKind::OutputFromProjectReference,
                                                index,
                                            }
                                        ))
                                    )?;
                                } else if get_emit_module_kind(&parsed_ref.ref_(self).command_line.ref_(self).options.ref_(self)) == ModuleKind::None {
                                    let mut got_common_source_directory: Option<String> = Default::default();
                                    for file_name in &parsed_ref.ref_(self).command_line.ref_(self).file_names {
                                        if !file_extension_is(file_name, Extension::Dts.to_str()) && !file_extension_is(file_name, Extension::Json.to_str()) {
                                            self.process_project_reference_file(
                                                &get_output_declaration_file_name(
                                                    file_name,
                                                    &parsed_ref.ref_(self).command_line.ref_(self),
                                                    !CompilerHost::use_case_sensitive_file_names(&**self.host().ref_(self)),
                                                    Some(&mut || {
                                                        got_common_source_directory.get_or_insert_with(|| {
                                                            get_common_source_directory_of_config(
                                                                &parsed_ref.ref_(self).command_line.ref_(self),
                                                                !CompilerHost::use_case_sensitive_file_names(&**self.host().ref_(self)),
                                                                self,
                                                            )
                                                        }).clone()
                                                    }),
                                                    self,
                                                ),
                                                self.alloc_file_include_reason(FileIncludeReason::ProjectReferenceFile(
                                                    ProjectReferenceFile {
                                                        kind: FileIncludeKind::OutputFromProjectReference,
                                                        index,
                                                    }
                                                ))
                                            )?;
                                        }
                                    }
                                }
                            }

                            Ok(())
                        })?;

                        Ok(())
                    })?;
                }
            }

            // tracing?.push(tracing.Phase.Program, "processRootFiles", { count: rootNames.length });
            try_for_each(&*self.root_names(), |name, index| -> io::Result<_> {
                self.process_root_file(
                    name,
                    false,
                    false,
                    self.alloc_file_include_reason(FileIncludeReason::RootFile(RootFile {
                        kind: FileIncludeKind::RootFile,
                        index,
                    })),
                )?;
                Ok(Option::<()>::None)
            })?;
            // tracing?.pop();

            let type_references = if !self.root_names().is_empty() {
                get_automatic_type_directive_names(
                    &self.options.ref_(self),
                    self.host().ref_(self).as_dyn_module_resolution_host(),
                    self,
                )?
            } else {
                vec![]
            };

            if !type_references.is_empty() {
                // tracing?.push(tracing.Phase.Program, "processTypeReferences", { count: typeReferences.length });
                let containing_directory = if let Some(options_config_file_path) =
                    self.options.ref_(self).config_file_path.as_ref()
                {
                    get_directory_path(options_config_file_path)
                } else {
                    CompilerHost::get_current_directory(&**self.host().ref_(self))?
                };
                let containing_filename = combine_paths(
                    &containing_directory,
                    &[Some(inferred_types_containing_file)],
                );
                let resolutions = self.resolve_type_reference_directive_names_worker(
                    &type_references,
                    containing_filename,
                )?;
                for i in 0..type_references.len() {
                    self.process_type_reference_directive(
                        &type_references[i],
                        resolutions.get(i).and_then(|resolution| resolution.clone()),
                        self.alloc_file_include_reason(FileIncludeReason::AutomaticTypeDirectiveFile(
                            AutomaticTypeDirectiveFile {
                                kind: FileIncludeKind::AutomaticTypeDirectiveFile,
                                type_reference: type_references[i].clone(),
                                package_id: resolutions
                                    .get(i)
                                    .cloned()
                                    .flatten()
                                    .and_then(|resolution| resolution.ref_(self).package_id.clone()),
                            },
                        )),
                    )?;
                }
                // tracing?.pop();
            }

            if !self.root_names().is_empty() && self.maybe_skip_default_lib() != Some(true) {
                let default_library_file_name = self.get_default_library_file_name()?;
                if self.options.ref_(self).lib.is_none() && *default_library_file_name != "" {
                    self.process_root_file(
                        &default_library_file_name,
                        true,
                        false,
                        self.alloc_file_include_reason(FileIncludeReason::LibFile(LibFile {
                            kind: FileIncludeKind::LibFile,
                            index: None,
                        })),
                    )?;
                } else {
                    try_maybe_for_each(
                        self.options.ref_(self).lib.as_ref(),
                        |lib_file_name: &String, index| -> io::Result<Option<()>> {
                            self.process_root_file(
                                &self.path_for_lib_file(lib_file_name)?,
                                true,
                                false,
                                self.alloc_file_include_reason(FileIncludeReason::LibFile(LibFile {
                                    kind: FileIncludeKind::LibFile,
                                    index: Some(index),
                                })),
                            )?;
                            Ok(None)
                        },
                    )?;
                }
            }

            *self.missing_file_paths.borrow_mut() = Some(map_defined(
                Some(&*self.files_by_name()),
                |(path, file), _| {
                    if matches!(file, FilesByNameValue::Undefined,) {
                        Some(path.clone().into())
                    } else {
                        None
                    }
                },
            ));

            if is_logging {
                println!(
                    "processing_default_lib_files: {:?}",
                    self.processing_default_lib_files
                        .borrow()
                        .as_ref()
                        .unwrap()
                        .into_iter()
                        .map(|file| file.ref_(self).as_source_file().file_name().clone())
                        .collect::<Vec<_>>()
                );
                println!(
                    "processing_other_files: {:?}",
                    self.processing_other_files
                        .borrow()
                        .as_ref()
                        .unwrap()
                        .into_iter()
                        .map(|file| file.ref_(self).as_source_file().file_name().clone())
                        .collect::<Vec<_>>()
                );
            }
            *self.files.borrow_mut() = Some({
                let mut files: Vec<Id<Node>> = stable_sort(
                    self.processing_default_lib_files.borrow().as_ref().unwrap(),
                    |&a, &b| self.compare_default_lib_files(a, b),
                )
                .into();
                let mut processing_other_files =
                    self.processing_other_files.borrow().clone().unwrap();
                files.append(&mut processing_other_files);
                files
            });
            // println!("files: {:#?}", &*self.files());
            *self.processing_default_lib_files.borrow_mut() = None;
            *self.processing_other_files.borrow_mut() = None;
        }

        Debug_.assert(self.maybe_missing_file_paths().is_some(), None);

        if let Some(_old_program) = self.maybe_old_program().as_ref() {
            if self.host().ref_(self).is_on_release_old_source_file_supported() {
                unimplemented!()
            }
        }

        if let Some(_old_program) = self.maybe_old_program().as_ref() {
            if self.host().ref_(self).is_on_release_parsed_command_line_supported() {
                unimplemented!()
            }
        }

        *self.type_reference_directive_resolution_cache.borrow_mut() = None;

        self.set_old_program(None);

        on_program_create_complete();

        self.maybe_file_processing_diagnostics().as_ref().map(|file_processing_diagnostics| {
            for diagnostic in file_processing_diagnostics {
                match diagnostic.ref_(self).kind() {
                    FilePreprocessingDiagnosticsKind::FilePreprocessingFileExplainingDiagnostic => {
                        let diagnostic_ref = diagnostic.ref_(self);
                        let diagnostic_as_file_explaining_diagnostic = diagnostic_ref.as_file_explaining_diagnostic();
                        self.program_diagnostics_mut().add(
                            self.create_diagnostic_explaining_file(
                                diagnostic_as_file_explaining_diagnostic.file.as_ref().and_then(|diagnostic_file| {
                                    self.get_source_file_by_path(diagnostic_file)
                                }),
                                Some(diagnostic_as_file_explaining_diagnostic.file_processing_reason.clone()),
                                diagnostic_as_file_explaining_diagnostic.diagnostic,
                                Some(diagnostic_as_file_explaining_diagnostic.args.clone().unwrap_or_else(|| vec![]))
                            )
                        );
                    }
                    FilePreprocessingDiagnosticsKind::FilePreprocessingReferencedDiagnostic => {
                        let diagnostic_ref = diagnostic.ref_(self);
                        let diagnostic_as_referenced_diagnostic = diagnostic_ref.as_referenced_diagnostic();
                        let referenced_file_location = get_referenced_file_location(
                            |path: &Path| self.get_source_file_by_path(path),
                            &diagnostic_as_referenced_diagnostic.reason,
                            self,
                        );
                        let referenced_file_location_as_reference_file_location = referenced_file_location.as_reference_file_location();
                        let file = referenced_file_location_as_reference_file_location.file;
                        let pos = referenced_file_location_as_reference_file_location.pos;
                        let end = referenced_file_location_as_reference_file_location.end;
                        self.program_diagnostics_mut().add(
                            self.alloc_diagnostic(
                                create_file_diagnostic(
                                    file,
                                    /*Debug.checkDefined(*/pos.try_into().unwrap()/*)*/,
                                    /*Debug.checkDefined(*/(end/*)*/ - pos).try_into().unwrap(),
                                    diagnostic_as_referenced_diagnostic.diagnostic,
                                    Some(diagnostic_as_referenced_diagnostic.args.clone().unwrap_or_else(|| vec![]))
                                ).into()
                            )
                        );
                    }
                }
            }
        });

        self.verify_compiler_options();
        // performance.mark("afterProgram");
        // performance.measure("Program", "beforeProgram", "afterProgram");
        // tracing?.pop();

        Ok(())
    }

    pub fn set_arena_id(&self, id: Id<Self>) {
        *self._arena_id.borrow_mut() = Some(id);
    }

    pub fn arena_id(&self) -> Id<Self> {
        self._arena_id.borrow().clone().unwrap()
    }

    pub(super) fn root_names(&self) -> Ref<Vec<String>> {
        Ref::map(self.root_names.borrow(), |root_names| {
            root_names.as_ref().unwrap()
        })
    }

    pub(super) fn maybe_config_file_parsing_diagnostics(
        &self,
    ) -> GcCellRef<Option<Vec<Id<Diagnostic>>>> {
        self.config_file_parsing_diagnostics.borrow()
    }

    pub(super) fn maybe_project_references(&self) -> Ref<Option<Vec<Rc<ProjectReference>>>> {
        self.project_references.borrow()
    }

    pub(super) fn files(&self) -> GcCellRef<Vec<Id<Node>>> {
        GcCellRef::map(self.files.borrow(), |files| files.as_ref().unwrap())
    }

    pub(super) fn set_files(&self, files: Option<Vec<Id<Node>>>) {
        *self.files.borrow_mut() = files;
    }

    pub(super) fn maybe_common_source_directory_mut(&self) -> RefMut<Option<String>> {
        self.common_source_directory.borrow_mut()
    }

    pub(super) fn get_default_library_file_name(&self) -> io::Result<Ref<String>> {
        if self
            .get_default_library_file_name_memoized
            .borrow()
            .is_none()
        {
            *self.get_default_library_file_name_memoized.borrow_mut() =
                Some(self.host().ref_(self).get_default_lib_file_name(&self.options.ref_(self))?);
        }
        Ok(Ref::map(
            self.get_default_library_file_name_memoized.borrow(),
            |default_library_file_name_memoized| {
                default_library_file_name_memoized.as_ref().unwrap()
            },
        ))
    }

    pub(super) fn maybe_skip_default_lib(&self) -> Option<bool> {
        self.skip_default_lib.get()
    }

    pub(super) fn set_skip_default_lib(&self, skip_default_lib: Option<bool>) {
        self.skip_default_lib.set(skip_default_lib);
    }

    pub(super) fn default_library_path(&self) -> Ref<String> {
        Ref::map(self.default_library_path.borrow(), |default_library_path| {
            default_library_path.as_ref().unwrap()
        })
    }

    pub(super) fn current_node_modules_depth(&self) -> usize {
        self.current_node_modules_depth.get()
    }

    pub(super) fn set_current_node_modules_depth(&self, current_node_modules_depth: usize) {
        self.current_node_modules_depth
            .set(current_node_modules_depth);
    }

    pub(super) fn modules_with_elided_imports(&self) -> RefMut<HashMap<String, bool>> {
        self.modules_with_elided_imports.borrow_mut()
    }

    pub(super) fn source_files_found_searching_node_modules(&self) -> Ref<HashMap<String, bool>> {
        self.source_files_found_searching_node_modules.borrow()
    }

    pub(super) fn source_files_found_searching_node_modules_mut(
        &self,
    ) -> RefMut<HashMap<String, bool>> {
        self.source_files_found_searching_node_modules.borrow_mut()
    }

    pub(super) fn maybe_old_program(&self) -> Option<Id<Program>> {
        self.old_program.borrow().clone()
    }

    pub(super) fn set_old_program(&self, old_program: Option<Id<Program>>) {
        *self.old_program.borrow_mut() = old_program;
    }

    pub(super) fn host(&self) -> Id<Box<dyn CompilerHost>> {
        self.host.borrow().clone().unwrap()
    }

    pub(super) fn config_parsing_host(&self) -> Id<Box<dyn ParseConfigFileHost>> {
        self.config_parsing_host.borrow().clone().unwrap()
    }

    pub(super) fn symlinks(&self) -> GcCellRefMut<Option<Id<SymlinkCache>>> {
        self.symlinks.borrow_mut()
    }

    pub(super) fn ambient_module_name_to_unmodified_file_name(
        &self,
    ) -> RefMut<HashMap<String, String>> {
        self.ambient_module_name_to_unmodified_file_name
            .borrow_mut()
    }

    pub(super) fn file_reasons(&self) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        self.file_reasons.get()
    }

    pub(super) fn set_file_reasons(
        &self,
        file_reasons: Id<MultiMap<Path, Id<FileIncludeReason>>>,
    ) {
        self.file_reasons.set(file_reasons);
    }

    pub(super) fn cached_bind_and_check_diagnostics_for_file_mut(
        &self,
    ) -> GcCellRefMut<DiagnosticCache> {
        self.cached_bind_and_check_diagnostics_for_file.borrow_mut()
    }

    pub(super) fn cached_declaration_diagnostics_for_file_mut(
        &self,
    ) -> GcCellRefMut<DiagnosticCache> {
        self.cached_declaration_diagnostics_for_file.borrow_mut()
    }

    pub(super) fn maybe_file_processing_diagnostics(
        &self,
    ) -> GcCellRefMut<Option<Vec<Id<FilePreprocessingDiagnostics>>>> {
        self.file_processing_diagnostics.borrow_mut()
    }

    pub(super) fn resolved_type_reference_directives(
        &self,
    ) -> Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.resolved_type_reference_directives.get()
    }

    pub(super) fn set_resolved_type_reference_directives(
        &self,
        resolved_type_reference_directives: Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>>,
    ) {
        self.resolved_type_reference_directives.set(resolved_type_reference_directives);
    }

    pub(super) fn program_diagnostics(&self) -> GcCellRef<DiagnosticCollection> {
        GcCellRef::map(self.program_diagnostics.borrow(), |program_diagnostics| {
            program_diagnostics.as_ref().unwrap()
        })
    }

    pub(super) fn program_diagnostics_mut(
        &self,
    ) -> GcCellRefMut<Option<DiagnosticCollection>, DiagnosticCollection> {
        GcCellRefMut::map(
            self.program_diagnostics.borrow_mut(),
            |program_diagnostics| program_diagnostics.as_mut().unwrap(),
        )
    }

    pub(super) fn current_directory(&self) -> Ref<String> {
        Ref::map(self.current_directory.borrow(), |current_directory| {
            current_directory.as_ref().unwrap()
        })
    }

    pub(super) fn supported_extensions(&self) -> Ref<Vec<Vec<Extension>>> {
        Ref::map(self.supported_extensions.borrow(), |supported_extensions| {
            supported_extensions.as_ref().unwrap()
        })
    }

    pub(super) fn supported_extensions_with_json_if_resolve_json_module(
        &self,
    ) -> Ref<Vec<Vec<Extension>>> {
        Ref::map(
            self.supported_extensions_with_json_if_resolve_json_module
                .borrow(),
            |supported_extensions_with_json_if_resolve_json_module| {
                supported_extensions_with_json_if_resolve_json_module
                    .as_ref()
                    .unwrap()
            },
        )
    }

    pub(super) fn has_emit_blocking_diagnostics(&self) -> Ref<HashMap<Path, bool>> {
        ref_unwrapped(&self.has_emit_blocking_diagnostics)
    }

    pub(super) fn has_emit_blocking_diagnostics_mut(&self) -> RefMut<HashMap<Path, bool>> {
        ref_mut_unwrapped(&self.has_emit_blocking_diagnostics)
    }

    pub(super) fn maybe_compiler_options_object_literal_syntax(&self) -> Option<Option<Id<Node>>> {
        self._compiler_options_object_literal_syntax
            .borrow()
            .clone()
    }

    pub(super) fn set_compiler_options_object_literal_syntax(
        &self,
        compiler_options_object_literal_syntax: Option<Option<Id<Node>>>,
    ) {
        *self._compiler_options_object_literal_syntax.borrow_mut() =
            compiler_options_object_literal_syntax;
    }

    pub(super) fn maybe_module_resolution_cache(
        &self,
    ) -> GcCellRefMut<Option<Id<ModuleResolutionCache>>> {
        self.module_resolution_cache.borrow_mut()
    }

    pub(super) fn maybe_type_reference_directive_resolution_cache(
        &self,
    ) -> GcCellRefMut<Option<Id<TypeReferenceDirectiveResolutionCache>>> {
        self.type_reference_directive_resolution_cache.borrow_mut()
    }

    pub(super) fn actual_resolve_module_names_worker(
        &self,
    ) -> Id<Box<dyn ActualResolveModuleNamesWorker>> {
        self.actual_resolve_module_names_worker
            .borrow_mut()
            .clone()
            .unwrap()
    }

    pub(super) fn actual_resolve_type_reference_directive_names_worker(
        &self,
    ) -> Id<Box<dyn ActualResolveTypeReferenceDirectiveNamesWorker>> {
        self.actual_resolve_type_reference_directive_names_worker
            .borrow_mut()
            .clone()
            .unwrap()
    }

    pub(super) fn package_id_to_source_file(
        &self,
    ) -> GcCellRefMut<
        Option<HashMap<String, Id<Node /*SourceFile*/>>>,
        HashMap<String, Id<Node /*SourceFile*/>>,
    > {
        GcCellRefMut::map(
            self.package_id_to_source_file.borrow_mut(),
            |package_id_to_source_file| package_id_to_source_file.as_mut().unwrap(),
        )
    }

    pub(super) fn source_file_to_package_name(&self) -> RefMut<HashMap<Path, String>> {
        RefMut::map(
            self.source_file_to_package_name.borrow_mut(),
            |source_file_to_package_name| source_file_to_package_name.as_mut().unwrap(),
        )
    }

    pub(super) fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.redirect_targets_map.borrow().clone()
    }

    pub(super) fn set_redirect_targets_map(
        &self,
        redirect_targets_map: Rc<RefCell<RedirectTargetsMap>>,
    ) {
        *self.redirect_targets_map.borrow_mut() = redirect_targets_map;
    }

    pub(super) fn uses_uri_style_node_core_modules(&self) -> bool {
        self.uses_uri_style_node_core_modules.get().unwrap()
    }

    pub(super) fn set_uses_uri_style_node_core_modules(
        &self,
        uses_uri_style_node_core_modules: bool,
    ) {
        self.uses_uri_style_node_core_modules
            .set(Some(uses_uri_style_node_core_modules));
    }

    pub(super) fn files_by_name(&self) -> GcCellRef<HashMap<String, FilesByNameValue>> {
        GcCellRef::map(self.files_by_name.borrow(), |files_by_name| {
            files_by_name.as_ref().unwrap()
        })
    }

    pub(super) fn files_by_name_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<String, FilesByNameValue>>, HashMap<String, FilesByNameValue>>
    {
        GcCellRefMut::map(self.files_by_name.borrow_mut(), |files_by_name| {
            files_by_name.as_mut().unwrap()
        })
    }

    pub(super) fn maybe_missing_file_paths(&self) -> RefMut<Option<Vec<Path>>> {
        self.missing_file_paths.borrow_mut()
    }

    pub(super) fn files_by_name_ignore_case(
        &self,
    ) -> GcCellRefMut<Option<HashMap<String, Id<Node>>>, HashMap<String, Id<Node>>> {
        GcCellRefMut::map(
            self.files_by_name_ignore_case.borrow_mut(),
            |files_by_name_ignore_case| files_by_name_ignore_case.as_mut().unwrap(),
        )
    }

    pub(super) fn maybe_resolved_project_references(
        &self,
    ) -> GcCellRef<Option<Vec<Option<Id<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow()
    }

    pub(super) fn maybe_resolved_project_references_mut(
        &self,
    ) -> GcCellRefMut<Option<Vec<Option<Id<ResolvedProjectReference>>>>> {
        self.resolved_project_references.borrow_mut()
    }

    pub(super) fn maybe_project_reference_redirects(
        &self,
    ) -> GcCellRef<Option<HashMap<Path, Option<Id<ResolvedProjectReference>>>>> {
        self.project_reference_redirects.borrow()
    }

    pub(super) fn maybe_project_reference_redirects_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<Path, Option<Id<ResolvedProjectReference>>>>> {
        self.project_reference_redirects.borrow_mut()
    }

    pub(super) fn project_reference_redirects_mut(
        &self,
    ) -> GcCellRefMut<
        Option<HashMap<Path, Option<Id<ResolvedProjectReference>>>>,
        HashMap<Path, Option<Id<ResolvedProjectReference>>>,
    > {
        gc_cell_ref_mut_unwrapped(&self.project_reference_redirects)
    }

    pub(super) fn maybe_map_from_file_to_project_reference_redirects(
        &self,
    ) -> RefMut<Option<HashMap<Path, Path>>> {
        self.map_from_file_to_project_reference_redirects
            .borrow_mut()
    }

    pub(super) fn maybe_map_from_to_project_reference_redirect_source(
        &self,
    ) -> RefMut<Option<HashMap<Path, SourceOfProjectReferenceRedirect>>> {
        self.map_from_to_project_reference_redirect_source
            .borrow_mut()
    }

    pub(super) fn use_source_of_project_reference_redirect(&self) -> bool {
        self.use_source_of_project_reference_redirect.get().unwrap()
    }

    pub(super) fn file_exists_rc(&self) -> Id<Box<dyn ModuleResolutionHostOverrider>> {
        self.file_exists_rc.borrow().clone().unwrap()
    }

    pub(super) fn maybe_directory_exists_rc(
        &self,
    ) -> Option<Id<Box<dyn ModuleResolutionHostOverrider>>> {
        self.directory_exists_rc.borrow().clone()
    }

    pub(super) fn should_create_new_source_file(&self) -> bool {
        self.should_create_new_source_file.get().unwrap()
    }

    pub fn structure_is_reused(&self) -> StructureIsReused {
        self.structure_is_reused.get().unwrap()
    }

    pub(super) fn set_structure_is_reused(&self, structure_is_reused: StructureIsReused) {
        self.structure_is_reused.set(Some(structure_is_reused));
    }

    pub(super) fn maybe_get_program_build_info_id(
        &self,
    ) -> Option<Id<Box<dyn GetProgramBuildInfo>>> {
        self.get_program_build_info.borrow().clone()
    }
}

impl HasArena for Program {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct ProgramGetResolvedProjectReferences {
    program: Id<Program>,
}

impl ProgramGetResolvedProjectReferences {
    pub fn new(program: Id<Program>) -> Self {
        Self { program }
    }
}

impl GetResolvedProjectReferences for ProgramGetResolvedProjectReferences {
    fn call(&self) -> Option<Vec<Option<Id<ResolvedProjectReference>>>> {
        self.program.ref_(self).get_resolved_project_references().clone()
    }
}

impl HasArena for ProgramGetResolvedProjectReferences {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

mod _FilesByNameValueDeriveTraceScope {
    use local_macros::Trace;

    use super::*;

    #[derive(Clone, Trace, Finalize)]
    pub enum FilesByNameValue {
        SourceFile(Id<Node /*SourceFile*/>),
        False,
        Undefined,
    }

    impl FilesByNameValue {
        pub fn as_source_file(&self) -> Id<Node /*SourceFile*/> {
            *enum_unwrapped!(self, [FilesByNameValue, SourceFile])
        }
    }

    impl From<Id<Node /*SourceFile*/>> for FilesByNameValue {
        fn from(value: Id<Node /*SourceFile*/>) -> Self {
            Self::SourceFile(value)
        }
    }
}
pub use _FilesByNameValueDeriveTraceScope::FilesByNameValue;

use crate::try_for_each;

pub trait ActualResolveModuleNamesWorker: Trace + Finalize {
    fn call(
        &self,
        module_names: &[String],
        containing_file: Id<Node>, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Vec<Option<Id<ResolvedModuleFull>>>>;
}

#[derive(Trace, Finalize)]
struct ActualResolveModuleNamesWorkerHost {
    host: Id<Box<dyn CompilerHost>>,
    options: Id<CompilerOptions>,
}

impl ActualResolveModuleNamesWorkerHost {
    pub fn new(host: Id<Box<dyn CompilerHost>>, options: Id<CompilerOptions>) -> Self {
        Self { host, options }
    }
}

impl ActualResolveModuleNamesWorker for ActualResolveModuleNamesWorkerHost {
    fn call(
        &self,
        module_names: &[String],
        containing_file: Id<Node>, /*SourceFile*/
        containing_file_name: &str,
        reused_names: Option<&[String]>,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Vec<Option<Id<ResolvedModuleFull>>>> {
        Ok(self
            .host
            .ref_(self).resolve_module_names(
                /*Debug.checkEachDefined(*/ module_names, /*)*/
                containing_file_name,
                reused_names,
                redirected_reference,
                &self.options.ref_(self),
                Some(containing_file),
            )
            .unwrap()
            .into_iter()
            .map(|resolved| {
                if match resolved.as_ref() {
                    None => true,
                    Some(resolved) => resolved.ref_(self).extension.is_some(),
                } {
                    return resolved;
                }
                let resolved = resolved.unwrap();
                let mut with_extension = clone(&*resolved.ref_(self));
                with_extension.extension = Some(extension_from_path(&resolved.ref_(self).resolved_file_name));
                Some(self.alloc_resolved_module_full(with_extension))
            })
            .collect())
    }
}

impl HasArena for ActualResolveModuleNamesWorkerHost {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    loader: Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>>,
}

impl ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    pub fn new(
        loader: Id<Box<dyn LoadWithModeAwareCacheLoader<Option<Id<ResolvedModuleFull>>>>>,
    ) -> Self {
        Self { loader }
    }
}

impl ActualResolveModuleNamesWorker for ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    fn call(
        &self,
        module_names: &[String],
        containing_file: Id<Node>, /*SourceFile*/
        containing_file_name: &str,
        _reused_names: Option<&[String]>,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Vec<Option<Id<ResolvedModuleFull>>>> {
        load_with_mode_aware_cache(
            /*Debug.checkEachDefined(*/ module_names, /*)*/
            containing_file,
            containing_file_name,
            redirected_reference,
            &**self.loader.ref_(self),
            self,
        )
    }
}

impl HasArena for ActualResolveModuleNamesWorkerLoadWithModeAwareCache {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub trait ActualResolveTypeReferenceDirectiveNamesWorker: Trace + Finalize {
    fn call(
        &self,
        type_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Vec<Option<Id<ResolvedTypeReferenceDirective>>>>;
}

#[derive(Trace, Finalize)]
struct ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    host: Id<Box<dyn CompilerHost>>,
    options: Id<CompilerOptions>,
}

impl ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    pub fn new(host: Id<Box<dyn CompilerHost>>, options: Id<CompilerOptions>) -> Self {
        Self { host, options }
    }
}

impl ActualResolveTypeReferenceDirectiveNamesWorker
    for ActualResolveTypeReferenceDirectiveNamesWorkerHost
{
    fn call(
        &self,
        type_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Vec<Option<Id<ResolvedTypeReferenceDirective>>>> {
        Ok(self
            .host
            .ref_(self).resolve_type_reference_directives(
                /*Debug.checkEachDefined(*/ type_directive_names, /*)*/
                containing_file,
                redirected_reference,
                &self.options.ref_(self),
            )
            .unwrap())
    }
}

impl HasArena for ActualResolveTypeReferenceDirectiveNamesWorkerHost {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    loader: Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>>,
}

impl ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    pub fn new(
        loader: Id<Box<dyn LoadWithLocalCacheLoader<Id<ResolvedTypeReferenceDirective>>>>,
    ) -> Self {
        Self { loader }
    }
}

impl ActualResolveTypeReferenceDirectiveNamesWorker
    for ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache
{
    fn call(
        &self,
        type_reference_directive_names: &[String],
        containing_file: &str,
        redirected_reference: Option<Id<ResolvedProjectReference>>,
    ) -> io::Result<Vec<Option<Id<ResolvedTypeReferenceDirective>>>> {
        Ok(load_with_local_cache(
            /*Debug.checkEachDefined(*/ type_reference_directive_names, /*)*/
            containing_file,
            redirected_reference,
            &**self.loader.ref_(self),
        )?
        .into_iter()
        .map(Some)
        .collect())
    }
}

impl HasArena for ActualResolveTypeReferenceDirectiveNamesWorkerLoadWithLocalCache {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

impl ScriptReferenceHost for Program {
    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Id<Node /*SourceFile*/>> {
        self.get_source_file_(file_name)
    }

    fn get_source_file_by_path(&self, path: &Path) -> Option<Id<Node /*SourceFile*/>> {
        self.files_by_name()
            .get(&**path)
            .and_then(|value| match value {
                FilesByNameValue::SourceFile(value) => Some(value.clone()),
                _ => None,
            })
    }

    fn get_current_directory(&self) -> String {
        unimplemented!()
    }
}

impl ModuleSpecifierResolutionHost for Program {
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(self.use_case_sensitive_file_names())
    }

    fn file_exists(&self, path: &str) -> bool {
        self.file_exists_rc().ref_(self).file_exists(path)
    }

    fn get_current_directory(&self) -> String {
        self.get_current_directory()
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.maybe_directory_exists_rc()
            .and_then(|directory_exists_rc| directory_exists_rc.ref_(self).directory_exists(path))
    }

    fn read_file(&self, path: &str) -> Option<io::Result<Option<String>>> {
        Some(self.host().ref_(self).read_file(path))
    }

    fn is_read_file_supported(&self) -> bool {
        true
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.redirect_targets_map()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.is_source_of_project_reference_redirect_(file_name)
    }

    fn get_file_include_reasons(&self) -> Id<MultiMap<Path, Id<FileIncludeReason>>> {
        self.file_reasons()
    }

    fn is_get_nearest_ancestor_directory_with_package_json_supported(&self) -> bool {
        false
    }
}

impl TypeCheckerHost for Program {
    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.options.clone()
    }

    fn get_source_files(&self) -> GcCellRef<Vec<Id<Node>>> {
        self.files()
    }

    fn get_source_file(&self, file_name: &str) -> Option<Id<Node /*SourceFile*/>> {
        self.get_source_file_(file_name)
    }

    fn get_resolved_type_reference_directives(
        &self,
    ) -> Id<HashMap<String, Option<Id<ResolvedTypeReferenceDirective>>>> {
        self.resolved_type_reference_directives()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        self.get_project_reference_redirect_(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.use_source_of_project_reference_redirect()
            && self
                .get_resolved_project_reference_to_redirect(file_name)
                .is_some()
    }

    fn get_common_source_directory(&self) -> Option<String> {
        unimplemented!()
    }
}

impl TypeCheckerHostDebuggable for Program {}

impl SourceFileMayBeEmittedHost for Program {
    fn get_compiler_options(&self) -> Id<CompilerOptions> {
        self.get_compiler_options()
    }

    fn is_source_file_from_external_library(&self, file: Id<Node> /*SourceFile*/) -> bool {
        self.is_source_file_from_external_library(file)
    }

    fn get_resolved_project_reference_to_redirect(
        &self,
        file_name: &str,
    ) -> Option<Id<ResolvedProjectReference>> {
        self.get_resolved_project_reference_to_redirect(file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        self.is_source_of_project_reference_redirect_(file_name)
    }
}
