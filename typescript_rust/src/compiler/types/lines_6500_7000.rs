use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashMap,
    fmt, io,
    rc::Rc,
};

use bitflags::bitflags;
use derive_builder::Builder;
use gc::{Finalize, Gc, GcCellRef, Trace};
use local_macros::enum_unwrapped;

use super::{
    BaseTextRange, CompilerOptions, FileReference, Node, RedirectTargetsMap,
    ResolvedProjectReference, ScriptReferenceHost, ScriptTarget, SyntaxKind, SynthesizedComment,
    TextRange,
};
use crate::{
    ref_unwrapped, AllArenas, CancellationToken, Cloneable, ModuleResolutionCache,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, ParseConfigHost, ParsedCommandLine,
    Path, ProgramBuildInfo, ReadonlyTextRange, ResolveModuleNameResolutionHost, SourceFileLike,
    SourceTextAsChars, StringOrNumber, SymlinkCache,
    HasArena, InArena,
};

pub trait ModuleResolutionHost {
    fn file_exists(&self, file_name: &str) -> bool;
    fn file_exists_non_overridden(&self, file_name: &str) -> bool;
    fn set_overriding_file_exists(
        &self,
        overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn read_file(&self, file_name: &str) -> io::Result<Option<String>>;
    fn set_overriding_read_file(
        &self,
        overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn read_file_non_overridden(&self, file_name: &str) -> io::Result<Option<String>>;
    fn trace(&self, _s: &str) {}
    fn is_trace_supported(&self) -> bool;
    fn directory_exists(&self, _directory_name: &str) -> Option<bool> {
        None
    }
    fn is_directory_exists_supported(&self) -> bool;
    fn directory_exists_non_overridden(&self, _directory_name: &str) -> Option<bool> {
        None
    }
    fn set_overriding_directory_exists(
        &self,
        overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn realpath(&self, _path: &str) -> Option<String> {
        None
    }
    fn realpath_non_overridden(&self, _path: &str) -> Option<String> {
        None
    }
    fn is_realpath_supported(&self) -> bool;
    fn set_overriding_realpath(
        &self,
        overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn get_current_directory(&self) -> Option<io::Result<String>> {
        None
    }
    fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
        None
    }
    fn is_get_directories_supported(&self) -> bool;
    fn get_directories_non_overridden(&self, _path: &str) -> Option<Vec<String>> {
        None
    }
    fn set_overriding_get_directories(
        &self,
        overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        None
    }
}

impl<THost: ParseConfigHost> ModuleResolutionHost for THost {
    fn file_exists(&self, file_name: &str) -> bool {
        ParseConfigHost::file_exists(self, file_name)
    }

    fn file_exists_non_overridden(&self, _file_name: &str) -> bool {
        // this is a guess that we don't need any of these overriding things for these implementors
        // of ModuleResolutionHost
        unreachable!()
    }

    fn set_overriding_file_exists(
        &self,
        _overriding_file_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_directory_exists_supported(&self) -> bool {
        unreachable!()
    }

    fn set_overriding_directory_exists(
        &self,
        _overriding_directory_exists: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_realpath_supported(&self) -> bool {
        unreachable!()
    }

    fn set_overriding_realpath(
        &self,
        _overriding_realpath: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn is_get_directories_supported(&self) -> bool {
        unreachable!()
    }

    fn set_overriding_get_directories(
        &self,
        _overriding_get_directories: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn read_file(&self, file_name: &str) -> io::Result<Option<String>> {
        ParseConfigHost::read_file(self, file_name)
    }

    fn read_file_non_overridden(&self, _file_name: &str) -> io::Result<Option<String>> {
        unreachable!()
    }

    fn set_overriding_read_file(
        &self,
        _overriding_read_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    ) {
        unreachable!()
    }

    fn trace(&self, s: &str) {
        ParseConfigHost::trace(self, s)
    }

    fn is_trace_supported(&self) -> bool {
        ParseConfigHost::is_trace_supported(self)
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(ParseConfigHost::use_case_sensitive_file_names(self))
    }
}

pub trait ModuleResolutionHostOverrider: Trace + Finalize {
    fn file_exists(&self, file_name: &str) -> bool;
    fn read_file(&self, file_name: &str) -> io::Result<Option<String>>;
    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()>;
    fn directory_exists(&self, _directory_name: &str) -> Option<bool> {
        None
    }
    fn create_directory(&self, directory: &str) -> io::Result<()>;
    fn realpath(&self, _path: &str) -> Option<String> {
        None
    }
    fn get_directories(&self, _path: &str) -> Option<Vec<String>> {
        None
    }
}

#[derive(Builder, Clone, Debug, Trace, Finalize)]
#[builder(setter(into, strip_option))]
pub struct ResolvedModuleFull {
    pub resolved_file_name: String,
    #[builder(default)]
    pub is_external_library_import: Option<bool>,
    #[builder(default)]
    pub original_path: Option<String>,
    #[builder(default)]
    #[unsafe_ignore_trace]
    pub extension: Option<Extension>,
    #[builder(default)]
    #[unsafe_ignore_trace]
    pub package_id: Option<PackageId>,
}

impl ResolvedModuleFull {
    pub fn extension(&self) -> Extension {
        self.extension.unwrap()
    }
}

impl Cloneable for ResolvedModuleFull {
    fn cloned(&self) -> Self {
        self.clone()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PackageId {
    pub name: String,
    pub sub_module_name: String,
    pub version: String,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Extension {
    Ts,
    Tsx,
    Dts,
    Js,
    Jsx,
    Json,
    TsBuildInfo,
    Mjs,
    Mts,
    Dmts,
    Cjs,
    Cts,
    Dcts,
}

impl Extension {
    pub fn maybe_from_str(extension: &str) -> Option<Self> {
        let extension_lowercase = extension.to_lowercase();
        let extension: &str = &extension_lowercase;
        match extension {
            ".ts" => Some(Self::Ts),
            ".tsx" => Some(Self::Tsx),
            ".d.ts" => Some(Self::Dts),
            ".js" => Some(Self::Js),
            ".jsx" => Some(Self::Jsx),
            ".json" => Some(Self::Json),
            ".tsbuildinfo" => Some(Self::TsBuildInfo),
            ".mjs" => Some(Self::Mjs),
            ".mts" => Some(Self::Mts),
            ".d.mts" => Some(Self::Dmts),
            ".cjs" => Some(Self::Cjs),
            ".cts" => Some(Self::Cts),
            ".d.cts" => Some(Self::Dcts),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Ts => ".ts",
            Self::Tsx => ".tsx",
            Self::Dts => ".d.ts",
            Self::Js => ".js",
            Self::Jsx => ".jsx",
            Self::Json => ".json",
            Self::TsBuildInfo => ".tsbuildinfo",
            Self::Mjs => ".mjs",
            Self::Mts => ".mts",
            Self::Dmts => ".d.mts",
            Self::Cjs => ".cjs",
            Self::Cts => ".cts",
            Self::Dcts => ".d.cts",
        }
    }
}

impl AsRef<str> for Extension {
    fn as_ref(&self) -> &str {
        self.to_str()
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct ResolvedModuleWithFailedLookupLocations {
    pub resolved_module: Option<Id<ResolvedModuleFull>>,
    #[unsafe_ignore_trace]
    failed_lookup_locations: RefCell<Vec<String>>,
}

impl ResolvedModuleWithFailedLookupLocations {
    pub fn new(
        resolved_module: Option<Id<ResolvedModuleFull>>,
        failed_lookup_locations: Vec<String>,
    ) -> Self {
        Self {
            resolved_module,
            failed_lookup_locations: RefCell::new(failed_lookup_locations),
        }
    }

    pub fn failed_lookup_locations(&self) -> Ref<Vec<String>> {
        self.failed_lookup_locations.borrow()
    }

    pub fn failed_lookup_locations_mut(&self) -> RefMut<Vec<String>> {
        self.failed_lookup_locations.borrow_mut()
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct ResolvedTypeReferenceDirective {
    pub primary: bool,
    pub resolved_file_name: Option<String>,
    pub original_path: Option<String>,
    #[unsafe_ignore_trace]
    pub package_id: Option<PackageId>,
    pub is_external_library_import: Option<bool>,
}

#[derive(Trace, Finalize)]
pub struct ResolvedTypeReferenceDirectiveWithFailedLookupLocations {
    pub resolved_type_reference_directive: Option<Id<ResolvedTypeReferenceDirective>>,
    pub failed_lookup_locations: Vec<String>,
}

pub trait CompilerHost: ModuleResolutionHost + Trace + Finalize {
    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost;
    fn get_source_file(
        &self,
        file_name: &str,
        language_version: ScriptTarget,
        on_error: Option<&mut dyn FnMut(&str)>,
        should_create_new_source_file: Option<bool>,
    ) -> io::Result<Option<Id<Node /*SourceFile*/>>>;
    fn get_source_file_by_path(
        &self,
        _file_name: &str,
        _path: &Path,
        _language_version: ScriptTarget,
        _on_error: Option<&mut dyn FnMut(&str)>,
        _should_create_new_source_file: Option<bool>,
    ) -> Option<Id<Node /*SourceFile*/>> {
        None
    }
    fn is_get_source_file_by_path_supported(&self) -> bool;
    fn get_cancellation_token(&self) -> Option<Id<Box<dyn CancellationToken>>> {
        None
    }
    fn get_default_lib_file_name(&self, _options: &CompilerOptions) -> io::Result<String>;
    fn get_default_lib_location(&self) -> io::Result<Option<String>> {
        Ok(None)
    }
    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()>;
    fn write_file_non_overridden(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()>;
    fn is_write_file_supported(&self) -> bool;
    fn set_overriding_write_file(
        &self,
        overriding_write_file: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn get_current_directory(&self) -> io::Result<String>;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
    fn use_case_sensitive_file_names(&self) -> bool;
    fn get_new_line(&self) -> String;
    fn read_directory(
        &self,
        _root_dir: &str,
        _extensions: &[&str],
        _excludes: Option<&[String]>,
        _includes: &[String],
        _depth: Option<usize>,
    ) -> Option<io::Result<Vec<String>>> {
        None
    }
    fn is_read_directory_implemented(&self) -> bool;

    fn resolve_module_names(
        &self,
        _module_names: &[String],
        _containing_file: &str,
        _reused_names: Option<&[String]>,
        _redirected_reference: Option<Id<ResolvedProjectReference>>,
        _options: &CompilerOptions,
        _containing_source_file: Option<Id<Node> /*SourceFile*/>,
    ) -> Option<Vec<Option<Id<ResolvedModuleFull>>>> {
        None
    }
    fn is_resolve_module_names_supported(&self) -> bool;
    fn get_module_resolution_cache(&self) -> Option<Gc<ModuleResolutionCache>> {
        None
    }
    fn is_resolve_type_reference_directives_supported(&self) -> bool;
    fn resolve_type_reference_directives(
        &self,
        _type_reference_directive_names: &[String],
        _containing_file: &str,
        _redirected_reference: Option<Id<ResolvedProjectReference>>,
        _options: &CompilerOptions,
    ) -> Option<Vec<Option<Id<ResolvedTypeReferenceDirective>>>> {
        None
    }
    fn get_environment_variable(&self, _name: &str) -> Option<String> {
        None
    }
    fn on_release_old_source_file(
        &self,
        _old_source_file: Id<Node>, /*SourceFile*/
        _old_options: &CompilerOptions,
        _has_source_file_by_path: bool,
    ) {
    }
    fn is_on_release_old_source_file_supported(&self) -> bool;
    fn on_release_parsed_command_line(
        &self,
        _config_file_name: &str,
        _old_resolved_ref: Option<Id<ResolvedProjectReference>>,
        _option_options: &CompilerOptions,
    ) {
    }
    fn is_on_release_parsed_command_line_supported(&self) -> bool;
    fn has_invalidated_resolution(&self, _source_file: &Path) -> Option<bool> {
        None
    }
    fn has_changed_automatic_type_directive_names(&self) -> Option<bool> {
        None
    }
    fn create_hash(&self, _data: &str) -> Option<String> {
        None
    }
    fn get_parsed_command_line(&self, _file_name: &str) -> Option<ParsedCommandLine> {
        None
    }
    fn is_get_parsed_command_line_supported(&self) -> bool;
    fn use_source_of_project_reference_redirect(&self) -> Option<bool> {
        None
    }

    fn create_directory(&self, _directory: &str) -> io::Result<()> {
        Ok(())
    }
    fn create_directory_non_overridden(&self, _directory: &str) -> io::Result<()> {
        Ok(())
    }
    fn is_create_directory_supported(&self) -> bool;
    fn set_overriding_create_directory(
        &self,
        overriding_create_directory: Option<Id<Box<dyn ModuleResolutionHostOverrider>>>,
    );
    fn get_symlink_cache(&self) -> Option<Id<SymlinkCache>> {
        None
    }

    fn disable_use_file_version_as_signature(&self) -> Option<bool> {
        None
    }
}

#[derive(Clone)]
pub(crate) enum SourceOfProjectReferenceRedirect {
    String(String),
    True,
}

impl From<String> for SourceOfProjectReferenceRedirect {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

bitflags! {
    pub struct TransformFlags: u32 {
        const None = 0;

        const ContainsTypeScript = 1 << 0;

        const ContainsJsx = 1 << 1;
        const ContainsESNext = 1 << 2;
        const ContainsES2021 = 1 << 3;
        const ContainsES2020 = 1 << 4;
        const ContainsES2019 = 1 << 5;
        const ContainsES2018 = 1 << 6;
        const ContainsES2017 = 1 << 7;
        const ContainsES2016 = 1 << 8;
        const ContainsES2015 = 1 << 9;
        const ContainsGenerator = 1 << 10;
        const ContainsDestructuringAssignment = 1 << 11;

        const ContainsTypeScriptClassSyntax = 1 << 12;
        const ContainsLexicalThis = 1 << 13;
        const ContainsRestOrSpread = 1 << 14;
        const ContainsObjectRestOrSpread = 1 << 15;
        const ContainsComputedPropertyName = 1 << 16;
        const ContainsBlockScopedBinding = 1 << 17;
        const ContainsBindingPattern = 1 << 18;
        const ContainsYield = 1 << 19;
        const ContainsAwait = 1 << 20;
        const ContainsHoistedDeclarationOrCompletion = 1 << 21;
        const ContainsDynamicImport = 1 << 22;
        const ContainsClassFields = 1 << 23;
        const ContainsPossibleTopLevelAwait = 1 << 24;
        const ContainsLexicalSuper = 1 << 25;
        const ContainsUpdateExpressionForIdentifier = 1 << 26;
        const HasComputedFlags = 1 << 29;

        const AssertTypeScript = Self::ContainsTypeScript.bits;
        const AssertJsx = Self::ContainsJsx.bits;
        const AssertESNext = Self::ContainsESNext.bits;
        const AssertES2021 = Self::ContainsES2021.bits;
        const AssertES2020 = Self::ContainsES2020.bits;
        const AssertES2019 = Self::ContainsES2019.bits;
        const AssertES2018 = Self::ContainsES2018.bits;
        const AssertES2017 = Self::ContainsES2017.bits;
        const AssertES2016 = Self::ContainsES2016.bits;
        const AssertES2015 = Self::ContainsES2015.bits;
        const AssertGenerator = Self::ContainsGenerator.bits;
        const AssertDestructuringAssignment = Self::ContainsDestructuringAssignment.bits;

        const OuterExpressionExcludes = Self::HasComputedFlags.bits;
        const PropertyAccessExcludes = Self::OuterExpressionExcludes.bits;
        const NodeExcludes = Self::PropertyAccessExcludes.bits;
        const ArrowFunctionExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits | Self::ContainsHoistedDeclarationOrCompletion.bits | Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits |  Self::ContainsPossibleTopLevelAwait.bits;
        const FunctionExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits |  Self::ContainsHoistedDeclarationOrCompletion.bits |  Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits |  Self::ContainsPossibleTopLevelAwait.bits;
        const ConstructorExcludes = Self::NodeExcludes.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits |  Self::ContainsHoistedDeclarationOrCompletion.bits |  Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits |  Self::ContainsPossibleTopLevelAwait.bits;
        const MethodOrAccessorExcludes = Self::NodeExcludes.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsYield.bits | Self::ContainsAwait.bits |  Self::ContainsHoistedDeclarationOrCompletion.bits |  Self::ContainsBindingPattern.bits |  Self::ContainsObjectRestOrSpread.bits;
        const PropertyExcludes = Self::NodeExcludes.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits;
        const ClassExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsComputedPropertyName.bits;
        const ModuleExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits | Self::ContainsBlockScopedBinding.bits | Self::ContainsHoistedDeclarationOrCompletion.bits | Self::ContainsPossibleTopLevelAwait.bits;
        const TypeExcludes = !Self::ContainsTypeScript.bits;
        const ObjectLiteralExcludes = Self::NodeExcludes.bits | Self::ContainsTypeScriptClassSyntax.bits | Self::ContainsComputedPropertyName.bits | Self::ContainsObjectRestOrSpread.bits;
        const ArrayLiteralOrCallOrNewExcludes = Self::NodeExcludes.bits | Self::ContainsRestOrSpread.bits;
        const VariableDeclarationListExcludes = Self::NodeExcludes.bits | Self::ContainsBindingPattern.bits | Self::ContainsObjectRestOrSpread.bits;
        const ParameterExcludes = Self::NodeExcludes.bits;
        const CatchClauseExcludes = Self::NodeExcludes.bits | Self::ContainsObjectRestOrSpread.bits;
        const BindingPatternExcludes = Self::NodeExcludes.bits | Self::ContainsRestOrSpread.bits;
        const ContainsLexicalThisOrSuper = Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits;

        const PropertyNamePropagatingFlags = Self::ContainsLexicalThis.bits | Self::ContainsLexicalSuper.bits;
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct SourceMapRange {
    #[unsafe_ignore_trace]
    pos: Cell<isize>,
    #[unsafe_ignore_trace]
    end: Cell<isize>,
    pub source: Option<Gc<SourceMapSource>>,
}

impl SourceMapRange {
    pub fn new(pos: isize, end: isize, source: Option<Gc<SourceMapSource>>) -> Self {
        Self {
            pos: Cell::new(pos),
            end: Cell::new(end),
            source,
        }
    }
}

impl TextRange for SourceMapRange {
    fn pos(&self) -> isize {
        self.pos.get()
    }

    fn set_pos(&self, pos: isize) {
        self.pos.set(pos);
    }

    fn end(&self) -> isize {
        self.end.get()
    }

    fn set_end(&self, end: isize) {
        self.end.set(end);
    }
}

impl From<&Node> for SourceMapRange {
    fn from(value: &Node) -> Self {
        Self::new(value.pos(), value.end(), None)
    }
}

impl From<&BaseTextRange> for SourceMapRange {
    fn from(value: &BaseTextRange) -> Self {
        Self::new(value.pos(), value.end(), None)
    }
}

#[derive(Debug, Trace, Finalize)]
pub enum SourceMapSource {
    SourceFile(Id<Node /*SourceFile*/>),
    SourceMapSourceConcrete(SourceMapSourceConcrete),
}

impl SourceMapSource {
    pub fn ref_<'a>(&'a self, arena: &'a impl HasArena) -> SourceMapSourceRef<'a> {
        match self {
            SourceMapSource::SourceFile(value) => value.ref_(arena).into(),
            SourceMapSource::SourceMapSourceConcrete(value) => value.into(),
        }
    }
}

impl From<Id<Node /*SourceFile*/>> for SourceMapSource {
    fn from(value: Id<Node>) -> Self {
        Self::SourceFile(value)
    }
}

impl From<SourceMapSourceConcrete> for SourceMapSource {
    fn from(value: SourceMapSourceConcrete) -> Self {
        Self::SourceMapSourceConcrete(value)
    }
}

pub enum SourceMapSourceRef<'a> {
    SourceFile(debug_cell::Ref<'a, Node /*SourceFile*/>),
    SourceMapSourceConcrete(&'a SourceMapSourceConcrete),
}

impl<'a> SourceMapSourceRef<'a> {
    pub fn file_name(&self) -> String {
        match self {
            Self::SourceFile(value) => value.as_source_file().file_name().clone(),
            Self::SourceMapSourceConcrete(value) => value.file_name.clone(),
        }
    }

    pub fn skip_trivia(&self) -> Option<Gc<Box<dyn SkipTrivia>>> {
        match self {
            Self::SourceFile(_) => None,
            Self::SourceMapSourceConcrete(value) => value.skip_trivia.clone(),
        }
    }
}

impl<'a> SourceFileLike for SourceMapSourceRef<'a> {
    fn text(&self) -> Ref<String> {
        match self {
            Self::SourceFile(value) => value.as_source_file().text(),
            Self::SourceMapSourceConcrete(value) => value.text(),
        }
    }

    fn text_as_chars(&self) -> Ref<SourceTextAsChars> {
        match self {
            Self::SourceFile(value) => value.as_source_file().text_as_chars(),
            Self::SourceMapSourceConcrete(value) => value.text_as_chars(),
        }
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        match self {
            Self::SourceFile(value) => value.as_source_file().maybe_line_map(),
            Self::SourceMapSourceConcrete(value) => value.maybe_line_map(),
        }
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        match self {
            Self::SourceFile(value) => value.as_source_file().line_map(),
            Self::SourceMapSourceConcrete(value) => value.line_map(),
        }
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize> {
        match self {
            Self::SourceFile(value) => value
                .as_source_file()
                .maybe_get_position_of_line_and_character(line, character, allow_edits),
            Self::SourceMapSourceConcrete(value) => {
                value.maybe_get_position_of_line_and_character(line, character, allow_edits)
            }
        }
    }
}

impl<'a> From<debug_cell::Ref<'a, Node /*SourceFile*/>> for SourceMapSourceRef<'a> {
    fn from(value: debug_cell::Ref<'a, Node /*SourceFile*/>) -> Self {
        Self::SourceFile(value)
    }
}

impl<'a> From<&'a SourceMapSourceConcrete> for SourceMapSourceRef<'a> {
    fn from(value: &'a SourceMapSourceConcrete) -> Self {
        Self::SourceMapSourceConcrete(value)
    }
}

#[derive(Trace, Finalize)]
pub struct SourceMapSourceConcrete {
    pub file_name: String,
    #[unsafe_ignore_trace]
    text: RefCell<String>,
    #[unsafe_ignore_trace]
    text_as_chars: RefCell<SourceTextAsChars>,
    #[unsafe_ignore_trace]
    line_map: RefCell<Option<Vec<usize>>>,
    pub skip_trivia: Option<Gc<Box<dyn SkipTrivia>>>,
}

impl SourceMapSourceConcrete {
    pub fn new(
        file_name: String,
        text: String,
        text_as_chars: SourceTextAsChars,
        line_map: Option<Vec<usize>>,
        skip_trivia: Option<Gc<Box<dyn SkipTrivia>>>,
    ) -> Self {
        Self {
            file_name,
            text: RefCell::new(text),
            text_as_chars: RefCell::new(text_as_chars),
            line_map: RefCell::new(line_map),
            skip_trivia,
        }
    }
}

pub trait SkipTrivia: Trace + Finalize {
    fn call(&self, pos: isize) -> isize;
}

impl fmt::Debug for SourceMapSourceConcrete {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceMapSource").finish()
    }
}

impl SourceFileLike for SourceMapSourceConcrete {
    fn text(&self) -> Ref<String> {
        self.text.borrow()
    }

    fn text_as_chars(&self) -> Ref<SourceTextAsChars> {
        self.text_as_chars.borrow()
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        self.line_map.borrow_mut()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        ref_unwrapped(&self.line_map)
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        _line: usize,
        _character: usize,
        _allow_edits: Option<bool>,
    ) -> Option<usize> {
        None
    }
}

#[derive(Debug, Default, Trace, Finalize)]
pub struct EmitNode {
    pub annotated_nodes: Option<Vec<Id<Node>>>,
    #[unsafe_ignore_trace]
    pub flags: Option<EmitFlags>,
    #[unsafe_ignore_trace]
    pub leading_comments: Option<Vec<Rc<SynthesizedComment>>>,
    #[unsafe_ignore_trace]
    pub trailing_comments: Option<Vec<Rc<SynthesizedComment>>>,
    #[unsafe_ignore_trace]
    pub comment_range: Option<BaseTextRange>,
    pub source_map_range: Option<Id<SourceMapRange>>,
    pub token_source_map_ranges: Option<HashMap<SyntaxKind, Option<Id<SourceMapRange>>>>,
    #[unsafe_ignore_trace]
    pub constant_value: Option<StringOrNumber>,
    pub external_helpers_module_name: Option<Id<Node /*Identifier*/>>,
    pub external_helpers: Option<bool>,
    pub helpers: Option<Vec<Id<EmitHelper>>>,
    pub starts_on_new_line: Option<bool>,
    #[unsafe_ignore_trace]
    pub snippet_element: Option<SnippetElement>,
}

#[derive(Copy, Clone, Debug)]
pub struct SnippetElement {
    pub kind: SnippetKind,
    pub order: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SnippetKind {
    TabStop,
    Placeholder,
    Choice,
    Variable,
}

pub trait EmitHelperBase {
    fn name(&self) -> &str;
    fn scoped(&self) -> bool;
    fn text(&self) -> EmitHelperText;
    fn priority(&self) -> Option<usize>;
    fn dependencies(&self) -> Option<&[Id<EmitHelper>]>;
}

mod _EmitHelperTextDeriveTraceScope {
    use local_macros::Trace;

    use super::*;

    #[derive(Clone, Trace, Finalize)]
    pub enum EmitHelperText {
        String(String),
        Callback(Gc<Box<dyn EmitHelperTextCallback>>),
    }
}
pub use _EmitHelperTextDeriveTraceScope::EmitHelperText;
use id_arena::Id;

pub trait EmitHelperTextCallback: Trace + Finalize {
    fn call(&self, callback: &dyn Fn(&str) -> String) -> String;
}

impl fmt::Debug for EmitHelperText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("EmitHelperText").finish()
    }
}

impl From<String> for EmitHelperText {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Gc<Box<dyn EmitHelperTextCallback>>> for EmitHelperText {
    fn from(value: Gc<Box<dyn EmitHelperTextCallback>>) -> Self {
        Self::Callback(value)
    }
}

#[derive(Builder, Debug, Trace, Finalize)]
#[builder(setter(strip_option, into))]
pub struct ScopedEmitHelper {
    name: String,
    #[builder(setter(skip), default = "true")]
    scoped: bool, /*true*/
    text: EmitHelperText,
    #[builder(default)]
    priority: Option<usize>,
    #[builder(default)]
    dependencies: Option<Vec<Id<EmitHelper>>>,
}

impl ScopedEmitHelper {
    pub fn new(
        name: String,
        text: impl Into<EmitHelperText>,
        priority: Option<usize>,
        dependencies: Option<Vec<Id<EmitHelper>>>,
    ) -> Self {
        let text = text.into();
        Self {
            name,
            scoped: true,
            text,
            priority,
            dependencies,
        }
    }
}

impl EmitHelperBase for ScopedEmitHelper {
    fn name(&self) -> &str {
        &self.name
    }

    fn scoped(&self) -> bool {
        self.scoped
    }

    fn text(&self) -> EmitHelperText {
        self.text.clone()
    }

    fn priority(&self) -> Option<usize> {
        self.priority.clone()
    }

    fn dependencies(&self) -> Option<&[Id<EmitHelper>]> {
        self.dependencies.as_deref()
    }
}

#[derive(Builder, Debug, Trace, Finalize)]
#[builder(setter(strip_option, into))]
pub struct UnscopedEmitHelper {
    name: String,
    #[builder(setter(skip), default = "false")]
    scoped: bool, /*false*/
    text: String,
    #[builder(default)]
    priority: Option<usize>,
    #[builder(default)]
    dependencies: Option<Vec<Id<EmitHelper>>>,
    #[builder(default)]
    pub(crate) import_name: Option<String>,
}

impl EmitHelperBase for UnscopedEmitHelper {
    fn name(&self) -> &str {
        &self.name
    }

    fn scoped(&self) -> bool {
        self.scoped
    }

    fn text(&self) -> EmitHelperText {
        self.text.clone().into()
    }

    fn priority(&self) -> Option<usize> {
        self.priority
    }

    fn dependencies(&self) -> Option<&[Id<EmitHelper>]> {
        self.dependencies.as_deref()
    }
}

#[derive(Debug, Trace, Finalize)]
pub enum EmitHelper {
    ScopedEmitHelper(ScopedEmitHelper),
    UnscopedEmitHelper(UnscopedEmitHelper),
}

impl EmitHelper {
    pub fn as_unscoped_emit_helper(&self) -> &UnscopedEmitHelper {
        enum_unwrapped!(self, [EmitHelper, UnscopedEmitHelper])
    }
}

impl From<ScopedEmitHelper> for EmitHelper {
    fn from(value: ScopedEmitHelper) -> Self {
        Self::ScopedEmitHelper(value)
    }
}

impl From<UnscopedEmitHelper> for EmitHelper {
    fn from(value: UnscopedEmitHelper) -> Self {
        Self::UnscopedEmitHelper(value)
    }
}

impl EmitHelperBase for EmitHelper {
    fn name(&self) -> &str {
        match self {
            Self::ScopedEmitHelper(emit_helper) => emit_helper.name(),
            Self::UnscopedEmitHelper(emit_helper) => emit_helper.name(),
        }
    }

    fn scoped(&self) -> bool {
        match self {
            Self::ScopedEmitHelper(emit_helper) => emit_helper.scoped(),
            Self::UnscopedEmitHelper(emit_helper) => emit_helper.scoped(),
        }
    }

    fn text(&self) -> EmitHelperText {
        match self {
            Self::ScopedEmitHelper(emit_helper) => emit_helper.text(),
            Self::UnscopedEmitHelper(emit_helper) => emit_helper.text(),
        }
    }

    fn priority(&self) -> Option<usize> {
        match self {
            Self::ScopedEmitHelper(emit_helper) => emit_helper.priority(),
            Self::UnscopedEmitHelper(emit_helper) => emit_helper.priority(),
        }
    }

    fn dependencies(&self) -> Option<&[Id<EmitHelper>]> {
        match self {
            Self::ScopedEmitHelper(emit_helper) => emit_helper.dependencies(),
            Self::UnscopedEmitHelper(emit_helper) => emit_helper.dependencies(),
        }
    }
}

bitflags! {
    #[derive(Default)]
    pub struct EmitFlags: u32 {
        const None = 0;
        const SingleLine = 1 << 0;
        const AdviseOnEmitNode = 1 << 1;
        const NoSubstitution = 1 << 2;
        const CapturesThis = 1 << 3;
        const NoLeadingSourceMap = 1 << 4;
        const NoTrailingSourceMap = 1 << 5;
        const NoSourceMap = Self::NoLeadingSourceMap.bits | Self::NoTrailingSourceMap.bits;
        const NoNestedSourceMaps = 1 << 6;
        const NoTokenLeadingSourceMaps = 1 << 7;
        const NoTokenTrailingSourceMaps = 1 << 8;
        const NoTokenSourceMaps = Self::NoTokenLeadingSourceMaps.bits | Self::NoTokenTrailingSourceMaps.bits;
        const NoLeadingComments = 1 << 9;
        const NoTrailingComments = 1 << 10;
        const NoComments = Self::NoLeadingComments.bits | Self::NoTrailingComments.bits;
        const NoNestedComments = 1 << 11;
        const HelperName = 1 << 12;
        const ExportName = 1 << 13;
        const LocalName = 1 << 14;
        const InternalName = 1 << 15;
        const Indented = 1 << 16;
        const NoIndentation = 1 << 17;
        const AsyncFunctionBody = 1 << 18;
        const ReuseTempVariableScope = 1 << 19;
        const CustomPrologue = 1 << 20;
        const NoHoisting = 1 << 21;
        const HasEndOfDeclarationMarker = 1 << 22;
        const Iterator = 1 << 23;
        const NoAsciiEscaping = 1 << 24;
        const TypeScriptClassWrapper = 1 << 25;
        const NeverApplyImportHelper = 1 << 26;
        const IgnoreSourceNewlines = 1 << 27;
        const Immutable = 1 << 28;
        const IndirectCall = 1 << 29;
    }
}

bitflags! {
    pub(crate) struct ExternalEmitHelpers: u32 {
        const None = 0;
        const Extends = 1 << 0;
        const Assign = 1 << 1;
        const Rest = 1 << 2;
        const Decorate = 1 << 3;
        const Metadata = 1 << 4;
        const Param = 1 << 5;
        const Awaiter = 1 << 6;
        const Generator = 1 << 7;
        const Values = 1 << 8;
        const Read = 1 << 9;
        const SpreadArray = 1 << 10;
        const Await = 1 << 11;
        const AsyncGenerator = 1 << 12;
        const AsyncDelegator = 1 << 13;
        const AsyncValues = 1 << 14;
        const ExportStar = 1 << 15;
        const ImportStar = 1 << 16;
        const ImportDefault = 1 << 17;
        const MakeTemplateObject = 1 << 18;
        const ClassPrivateFieldGet = 1 << 19;
        const ClassPrivateFieldSet = 1 << 20;
        const ClassPrivateFieldIn = 1 << 21;
        const CreateBinding = 1 << 22;
        const FirstEmitHelper = Self::Extends.bits;
        const LastEmitHelper = Self::CreateBinding.bits;

        const ForOfIncludes = Self::Values.bits;

        const ForAwaitOfIncludes = Self::AsyncValues.bits;

        const AsyncGeneratorIncludes = Self::Await.bits | Self::AsyncGenerator.bits;

        const AsyncDelegatorIncludes = Self::Await.bits | Self::AsyncDelegator.bits | Self::AsyncValues.bits;

        const SpreadIncludes = Self::Read.bits | Self::SpreadArray.bits;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmitHint {
    SourceFile,
    Expression,
    IdentifierName,
    MappedTypeParameter,
    Unspecified,
    EmbeddedStatement,
    JsxAttributeValue,
}

pub trait SourceFileMayBeEmittedHost {
    fn get_compiler_options(&self) -> Id<CompilerOptions>;
    fn is_source_file_from_external_library(&self, file: Id<Node> /*SourceFile*/) -> bool;
    fn get_resolved_project_reference_to_redirect(
        &self,
        file_name: &str,
    ) -> Option<Id<ResolvedProjectReference>>;
    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool;
}

pub trait EmitHost:
    ScriptReferenceHost
    + ModuleSpecifierResolutionHostAndGetCommonSourceDirectory
    + SourceFileMayBeEmittedHost
    + ResolveModuleNameResolutionHost
    + Trace
    + Finalize
{
    // fn get_source_files(&self) -> &[Id<Node /*SourceFile*/>];
    fn get_source_files(&self) -> Vec<Id<Node /*SourceFile*/>>;
    fn use_case_sensitive_file_names(&self) -> bool;
    // fn get_current_directory(&self) -> String;

    fn get_lib_file_from_reference(&self, ref_: &FileReference) -> Option<Id<Node /*SourceFile*/>>;

    // fn get_canonical_file_name(&self, file_name: &str) -> String;
    fn get_new_line(&self) -> String;

    fn is_emit_blocked(&self, emit_file_name: &str) -> bool;

    fn get_prepend_nodes(&self) -> Vec<Id<Node /*InputFiles | UnparsedSource*/>>;

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Id<Node /*SourceFile*/>]>,
    ) -> io::Result<()>; // WriteFileCallback
    fn get_program_build_info(&self) -> Option<Id<ProgramBuildInfo>>;
    fn get_source_file_from_reference(
        &self,
        referencing_file: Id<Node>, /*SourceFile | UnparsedSource*/
        ref_: &FileReference,
    ) -> io::Result<Option<Id<Node /*SourceFile*/>>>;
    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>>;
    fn as_source_file_may_be_emitted_host(&self) -> &dyn SourceFileMayBeEmittedHost;
    fn as_module_specifier_resolution_host_and_get_common_source_directory(
        &self,
    ) -> &(dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory + 'static);
}
