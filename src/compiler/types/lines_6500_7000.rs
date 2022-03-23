#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::Cell;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

use super::{
    BaseTextRange, CompilerOptions, FileReference, ModuleSpecifierResolutionHost, Node,
    RedirectTargetsMap, ResolvedProjectReference, ScriptReferenceHost, ScriptTarget, SyntaxKind,
    SynthesizedComment, TextRange,
};
use crate::{
    CancellationToken, ModuleResolutionCache, ParseConfigHost, ParsedCommandLine, Path,
    ProgramBuildInfo, SymlinkCache,
};

pub trait ModuleResolutionHost {
    fn file_exists(&self, file_name: &str) -> bool;
    fn read_file(&self, file_name: &str) -> io::Result<String>;
    fn trace(&self, s: &str) {}
    fn directory_exists(&self, directory_name: &str) -> Option<bool> {
        None
    }
    fn realpath(&self, path: &str) -> Option<String> {
        None
    }
    fn get_current_directory(&self) -> Option<String> {
        None
    }
    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        None
    }
    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        None
    }
}

impl<THost: ParseConfigHost> ModuleResolutionHost for THost {
    fn file_exists(&self, file_name: &str) -> bool {
        ParseConfigHost::file_exists(self, file_name)
    }

    fn read_file(&self, file_name: &str) -> io::Result<String> {
        ParseConfigHost::read_file(self, file_name)
    }

    fn trace(&self, s: &str) {
        ParseConfigHost::trace(self, s)
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        Some(ParseConfigHost::use_case_sensitive_file_names(self))
    }
}

#[derive(Debug)]
pub struct ResolvedModuleFull {
    pub resolved_file_name: String,
    pub is_external_library_import: Option<bool>,
    pub original_path: Option<String>,
    pub extension: Option<Extension>,
    pub package_id: Option<PackageId>,
}

impl ResolvedModuleFull {
    pub fn extension(&self) -> Extension {
        self.extension.unwrap()
    }
}

#[derive(Clone, Debug)]
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

pub struct ResolvedModuleWithFailedLookupLocations {
    pub resolved_module: Option<ResolvedModuleFull>,
    pub failed_lookup_locations: Vec<String>,
}

#[derive(Debug)]
pub struct ResolvedTypeReferenceDirective {
    pub primary: bool,
    pub resolved_file_name: Option<String>,
    pub original_path: Option<String>,
    pub package_id: Option<PackageId>,
    pub is_external_library_import: Option<bool>,
}

pub trait CompilerHost: ModuleResolutionHost {
    fn get_source_file(
        &self,
        file_name: &str,
        language_version: ScriptTarget,
        on_error: Option<&mut dyn FnMut(&str)>,
        should_create_new_source_file: Option<bool>,
    ) -> Option<Rc<Node /*SourceFile*/>>;
    fn get_source_file_by_path(
        &self,
        file_name: &str,
        path: &Path,
        language_version: ScriptTarget,
        on_error: Option<&mut dyn FnMut(&str)>,
        should_create_new_source_file: Option<bool>,
    ) -> Option<Rc<Node /*SourceFile*/>> {
        None
    }
    fn get_cancellation_token(&self) -> Option<Rc<dyn CancellationToken>> {
        None
    }
    fn get_default_lib_file_name(&self, options: &CompilerOptions) -> String;
    fn get_default_lib_location(&self) -> Option<String> {
        None
    }
    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&mut dyn FnMut(&str)>,
        source_files: Option<&[Rc<Node /*SourceFile*/>]>,
    );
    fn get_current_directory(&self) -> String;
    fn get_canonical_file_name<'file_name>(&self, file_name: &'file_name str) -> String;
    fn use_case_sensitive_file_names(&self) -> bool;
    fn get_new_line(&self) -> String;
    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        None
    }

    fn resolve_module_names(
        &self,
        module_names: &[&str],
        containing_file: &str,
        reused_names: Option<&[&str]>,
        redirected_reference: Option<ResolvedProjectReference>,
        options: &CompilerOptions,
        containing_source_file: Option<Rc<Node /*SourceFile*/>>,
    ) -> Option<Vec<Option<ResolvedModuleFull>>> {
        None
    }
    fn get_module_resolution_cache(&self) -> Option<&dyn ModuleResolutionCache> {
        None
    }
    fn resolve_type_reference_directives(
        &self,
        type_reference_directive_names: &[&str],
        containing_file: &str,
        redirected_reference: Option<ResolvedProjectReference>,
        options: &CompilerOptions,
    ) -> Option<Vec<Option<Rc<ResolvedTypeReferenceDirective>>>> {
        None
    }
    fn get_environment_variable(&self, name: &str) -> Option<String> {
        None
    }
    fn on_release_old_source_file(
        &self,
        old_source_file: &Node, /*SourceFile*/
        old_options: &CompilerOptions,
        has_source_file_by_path: bool,
    ) {
    }
    fn on_release_parsed_command_line(
        &self,
        config_file_name: &str,
        old_resolved_ref: Option<ResolvedProjectReference>,
        option_options: &CompilerOptions,
    ) {
    }
    fn has_invalidated_resolution(&self, source_file: &Path) -> Option<bool> {
        None
    }
    fn has_changed_automatic_type_directive_names(&self) -> Option<bool> {
        None
    }
    fn create_hash(&self, data: &str) -> Option<String> {
        None
    }
    fn get_parsed_command_line(&self, file_name: &str) -> Option<ParsedCommandLine> {
        None
    }
    fn use_source_of_project_reference_redirect(&self) -> Option<bool> {
        None
    }

    fn create_directory(&self, directory: &str) {}
    fn get_symlink_cache(&self) -> Option<SymlinkCache> {
        None
    }

    fn disable_use_file_version_as_signature(&self) -> Option<bool> {
        None
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

#[derive(Debug)]
pub struct SourceMapRange {
    pos: Cell<isize>,
    end: Cell<isize>,
    source: Option<SourceMapSource>,
}

impl SourceMapRange {
    pub fn new(pos: isize, end: isize, source: Option<SourceMapSource>) -> Self {
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

#[derive(Debug)]
pub struct SourceMapSource {
    pub file_name: String,
    pub text: String,
    pub(crate) line_map: Vec<usize>,
    pub skip_trivia: Option<fn(usize) -> usize>,
}

#[derive(Clone, Debug)]
pub enum StringOrUsize {
    String(String),
    Usize(usize),
}

#[derive(Debug)]
pub struct EmitNode {
    pub annotated_nodes: Option<Vec<Rc<Node>>>,
    pub flags: Option<EmitFlags>,
    pub leading_comments: Option<Vec<Rc<SynthesizedComment>>>,
    pub trailing_comments: Option<Vec<Rc<SynthesizedComment>>>,
    pub comment_range: Option<BaseTextRange>,
    pub source_map_range: Option<Rc<SourceMapRange>>,
    pub token_source_map_ranges: Option<HashMap<SyntaxKind, Option<Rc<SourceMapRange>>>>,
    pub constant_value: Option<StringOrUsize>,
    pub external_helpers_module_name: Option<Rc<Node /*Identifier*/>>,
    pub external_helpers: Option<bool>,
    pub helpers: Option<Vec<Rc<EmitHelper>>>,
    pub starts_on_new_line: Option<bool>,
    pub snippet_element: Option<SnippetElement>,
}

impl Default for EmitNode {
    fn default() -> Self {
        Self {
            annotated_nodes: None,
            flags: None,
            leading_comments: None,
            trailing_comments: None,
            comment_range: None,
            source_map_range: None,
            token_source_map_ranges: None,
            constant_value: None,
            external_helpers_module_name: None,
            external_helpers: None,
            helpers: None,
            starts_on_new_line: None,
            snippet_element: None,
        }
    }
}

#[derive(Debug)]
pub enum SnippetElement {
    TabStop(TabStop),
    Placeholder(Placeholder),
}

#[derive(Debug)]
pub struct TabStop {
    pub kind: SnippetKind,
    pub order: usize,
}

#[derive(Debug)]
pub struct Placeholder {
    pub kind: SnippetKind,
    pub order: usize,
}

#[derive(Debug)]
pub enum SnippetKind {
    TabStop,
    Placeholder,
    Choice,
    Variable,
}

pub trait EmitHelperBase {
    fn name(&self) -> &str;
    fn scoped(&self) -> bool;
    fn text(&self) -> &str; // TODO: support callback value?
    fn priority(&self) -> Option<usize>;
    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]>;
}

#[derive(Debug)]
pub struct ScopedEmitHelper {
    name: String,
    scoped: bool, /*true*/
    text: String,
    priority: Option<usize>,
    dependencies: Option<Vec<Rc<EmitHelper>>>,
}

impl EmitHelperBase for ScopedEmitHelper {
    fn name(&self) -> &str {
        &self.name
    }

    fn scoped(&self) -> bool {
        self.scoped
    }

    fn text(&self) -> &str {
        &self.text
    }

    fn priority(&self) -> Option<usize> {
        self.priority.clone()
    }

    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]> {
        self.dependencies.as_deref()
    }
}

#[derive(Debug)]
pub struct UnscopedEmitHelper {
    name: String,
    scoped: bool, /*false*/
    text: String,
    priority: Option<usize>,
    dependencies: Option<Vec<Rc<EmitHelper>>>,
    pub(crate) import_name: Option<String>,
}

impl EmitHelperBase for UnscopedEmitHelper {
    fn name(&self) -> &str {
        &self.name
    }

    fn scoped(&self) -> bool {
        self.scoped
    }

    fn text(&self) -> &str {
        &self.text
    }

    fn priority(&self) -> Option<usize> {
        self.priority.clone()
    }

    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]> {
        self.dependencies.as_deref()
    }
}

#[derive(Debug)]
pub enum EmitHelper {
    ScopedEmitHelper(ScopedEmitHelper),
    UnscopedEmitHelper(UnscopedEmitHelper),
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

    fn text(&self) -> &str {
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

    fn dependencies(&self) -> Option<&[Rc<EmitHelper>]> {
        match self {
            Self::ScopedEmitHelper(emit_helper) => emit_helper.dependencies(),
            Self::UnscopedEmitHelper(emit_helper) => emit_helper.dependencies(),
        }
    }
}

bitflags! {
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
    Expression,
    Unspecified,
}

pub trait SourceFileMayBeEmittedHost {
    fn get_compiler_options(&self) -> Rc<CompilerOptions>;
    fn is_source_file_from_external_library(&self, file: &Node /*SourceFile*/) -> bool;
    fn get_resolved_project_reference_to_redirect(
        &self,
        file_name: &str,
    ) -> Option<ResolvedProjectReference>;
    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool;
}

pub trait EmitHost:
    ScriptReferenceHost + ModuleSpecifierResolutionHost + SourceFileMayBeEmittedHost
{
    fn get_source_files(&self) -> &[Rc<Node /*SourceFile*/>];
    fn use_case_sensitive_file_names(&self) -> bool;
    fn get_current_directory(&self) -> String;

    fn get_lib_file_from_reference(&self, ref_: &FileReference) -> Option<Rc<Node /*SourceFile*/>>;

    fn get_common_source_directory(&self) -> String;
    fn get_canonical_file_name(&self, file_name: &str) -> String;
    fn get_new_line(&self) -> String;

    fn is_emit_blocked(&self, emit_file_name: &str) -> bool;

    fn get_prepend_nodes(&self) -> Vec<Rc<Node /*InputFiles | UnparsedSource*/>>;

    fn write_file(
        &self,
        file_name: &str,
        data: &str,
        write_byte_order_mark: bool,
        on_error: Option<&dyn FnMut(&str)>,
        source_files: Option<&[Rc<Node /*SourceFile*/>]>,
    ); // WriteFileCallback
    fn get_program_build_info(&self) -> Option<ProgramBuildInfo>;
    fn get_source_file_from_reference(
        &self,
        referencing_file: &Node, /*SourceFile | UnparsedSource*/
        ref_: &FileReference,
    ) -> Option<Rc<Node /*SourceFile*/>>;
    fn redirect_targets_map(&self) -> &RedirectTargetsMap;
    fn as_source_file_may_be_emitted_host(&self) -> &dyn SourceFileMayBeEmittedHost;
}
