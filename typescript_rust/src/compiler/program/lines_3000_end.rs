use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::io;
use std::rc::Rc;

use super::{get_mode_for_resolution_at_index, SourceFileImportsList};
use crate::{
    compare_paths, contains_path, create_diagnostic_for_node_in_source_file, create_symlink_cache,
    file_extension_is, file_extension_is_one_of, get_allow_js_compiler_option, get_directory_path,
    get_property_assignment, get_spelling_suggestion, get_strict_option_value,
    is_declaration_file_name, is_in_js_file, lib_map, libs, maybe_for_each, out_file,
    remove_file_extension, remove_prefix, remove_suffix, resolution_extension_is_ts_or_json,
    resolve_config_file_project_name, set_resolved_module, supported_js_extensions_flat,
    to_file_name_lower_case, Comparison, CompilerHost, CompilerOptions,
    ConfigFileDiagnosticsReporter, Debug_, Diagnostic, DiagnosticInterface, DiagnosticMessage,
    Diagnostics, DirectoryStructureHost, Extension, FileIncludeKind, FileIncludeReason,
    FilePreprocessingDiagnostics, FilePreprocessingDiagnosticsKind,
    FilePreprocessingFileExplainingDiagnostic, FilePreprocessingReferencedDiagnostic,
    FileReference, ModuleResolutionHost, ModuleResolutionHostOverrider, NamedDeclarationInterface,
    Node, NodeFlags, NodeInterface, ParseConfigFileHost, ParseConfigHost, Path, Program,
    ProjectReference, ReferencedFile, ResolvedConfigFileName, ResolvedModuleFull,
    ResolvedProjectReference, ScriptReferenceHost, SymlinkCache, SyntaxKind,
};

impl Program {
    pub fn process_lib_reference_directives(&self, file: &Node /*SourceFile*/) {
        let file_as_source_file = file.as_source_file();
        maybe_for_each(
            file_as_source_file
                .maybe_lib_reference_directives()
                .as_ref(),
            |lib_reference: &FileReference, index| -> Option<()> {
                let lib_name = to_file_name_lower_case(&lib_reference.file_name);
                let lib_file_name = lib_map.with(|lib_map_| lib_map_.get(&&*lib_name).copied());
                if let Some(lib_file_name) = lib_file_name {
                    self.process_root_file(
                        &self.path_for_lib_file(lib_file_name),
                        true,
                        true,
                        &FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::LibReferenceDirective,
                            file: file_as_source_file.path().clone(),
                            index,
                        }),
                    );
                } else {
                    let unqualified_lib_name =
                        remove_suffix(remove_prefix(&lib_name, "lib."), ".d.ts");
                    let suggestion = libs.with(|libs_| {
                        get_spelling_suggestion(unqualified_lib_name, libs_, |lib| {
                            Some((*lib).to_owned())
                        })
                        .map(|suggestion| (*suggestion).to_owned())
                    });
                    let diagnostic = if suggestion.is_some() {
                        &*Diagnostics::Cannot_find_lib_definition_for_0_Did_you_mean_1
                    } else {
                        &*Diagnostics::Cannot_find_lib_definition_for_0
                    };
                    self.maybe_file_processing_diagnostics().get_or_insert_with(|| {
                        vec![]
                    }).push(
                        FilePreprocessingDiagnostics::FilePreprocessingReferencedDiagnostic(FilePreprocessingReferencedDiagnostic {
                            kind: FilePreprocessingDiagnosticsKind::FilePreprocessingReferencedDiagnostic,
                            reason: ReferencedFile {
                                kind: FileIncludeKind::LibReferenceDirective,
                                file: file_as_source_file.path().clone(),
                                index,
                            },
                            diagnostic,
                            args: if let Some(suggestion) = suggestion {
                                Some(vec![
                                    lib_name,
                                    suggestion,
                                ])
                            } else {
                                Some(vec![
                                    lib_name,
                                ])
                            }
                        })
                    );
                }
                None
            },
        );
    }

    pub fn get_canonical_file_name(&self, file_name: &str) -> String {
        self.host().get_canonical_file_name(file_name)
    }

    pub fn get_canonical_file_name_rc(&self) -> Rc<dyn Fn(&str) -> String> {
        let host = self.host();
        Rc::new(move |file_name| host.get_canonical_file_name(file_name))
    }

    pub fn process_imported_modules(&self, file: &Node /*SourceFile*/) {
        self.collect_external_module_references(file);
        let file_as_source_file = file.as_source_file();
        if !file_as_source_file
            .maybe_imports()
            .as_ref()
            .unwrap()
            .is_empty()
            || !file_as_source_file
                .maybe_module_augmentations()
                .as_ref()
                .unwrap()
                .is_empty()
        {
            let module_names = get_module_names(file);
            let resolutions = self.resolve_module_names_reusing_old_state(&module_names, file);
            Debug_.assert(resolutions.len() == module_names.len(), None);
            let options_for_file = if self.use_source_of_project_reference_redirect() {
                self.get_redirect_reference_for_resolution(file)
                    .map(|value| value.command_line.options.clone())
            } else {
                None
            }
            .unwrap_or_else(|| self.options.clone());
            for index in 0..module_names.len() {
                let resolution = resolutions[index].as_ref();
                set_resolved_module(
                    file,
                    &module_names[index],
                    resolution.cloned(),
                    get_mode_for_resolution_at_index(file_as_source_file, index),
                );

                if resolution.is_none() {
                    continue;
                }
                let resolution = resolution.unwrap();

                let is_from_node_modules_search = resolution.is_external_library_import;
                let is_js_file = !resolution_extension_is_ts_or_json(resolution.extension());
                let is_js_file_from_node_modules =
                    is_from_node_modules_search == Some(true) && is_js_file;
                let resolved_file_name = &resolution.resolved_file_name;

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() + 1);
                }

                let elide_import = is_js_file_from_node_modules
                    && self.current_node_modules_depth() > self.max_node_module_js_depth;
                let should_add_file = !resolved_file_name.is_empty()
                    && get_resolution_diagnostic(&options_for_file, resolution).is_none()
                    && options_for_file.no_resolve != Some(true)
                    && index < file_as_source_file.maybe_imports().as_ref().unwrap().len()
                    && !elide_import
                    && !(is_js_file && !get_allow_js_compiler_option(&options_for_file))
                    && (is_in_js_file(Some(
                        &*file_as_source_file.maybe_imports().as_ref().unwrap()[index],
                    )) || !file_as_source_file.maybe_imports().as_ref().unwrap()[index]
                        .flags()
                        .intersects(NodeFlags::JSDoc));

                if elide_import {
                    self.modules_with_elided_imports()
                        .insert(file_as_source_file.path().to_string(), true);
                } else if should_add_file {
                    self.find_source_file(
                        resolved_file_name,
                        false,
                        false,
                        &FileIncludeReason::ReferencedFile(ReferencedFile {
                            kind: FileIncludeKind::Import,
                            file: file_as_source_file.path().clone(),
                            index,
                        }),
                        resolution.package_id.as_ref(),
                    );
                }

                if is_from_node_modules_search == Some(true) {
                    self.set_current_node_modules_depth(self.current_node_modules_depth() - 1);
                }
            }
        } else {
            *file_as_source_file.maybe_resolved_modules() = None;
        }
    }

    pub fn parse_project_reference_config_file(
        &self,
        ref_: &ProjectReference,
    ) -> Option<Rc<ResolvedProjectReference>> {
        if self.maybe_project_reference_redirects().is_none() {
            *self.maybe_project_reference_redirects() = Some(HashMap::new());
        }

        let ref_path = resolve_project_reference_path(ref_);
        let source_file_path = self.to_path(&ref_path);
        let from_cache = self
            .maybe_project_reference_redirects()
            .as_ref()
            .unwrap()
            .get(&source_file_path)
            .cloned();
        if let Some(from_cache) = from_cache {
            return from_cache;
        }
        unimplemented!()
    }

    pub fn add_file_preprocessing_file_explaining_diagnostic<TFile: Borrow<Node>>(
        &self,
        file: Option<TFile>,
        file_processing_reason: &FileIncludeReason,
        diagnostic: &'static DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.maybe_file_processing_diagnostics().get_or_insert_with(|| vec![]).push(FilePreprocessingDiagnostics::FilePreprocessingFileExplainingDiagnostic(FilePreprocessingFileExplainingDiagnostic {
            kind: FilePreprocessingDiagnosticsKind::FilePreprocessingFileExplainingDiagnostic,
            file: file.map(|file| file.borrow().as_source_file().path().clone()),
            file_processing_reason: file_processing_reason.clone(),
            diagnostic,
            args,
        }))
    }

    pub fn create_option_diagnostic_in_object_literal_syntax(
        &self,
        object_literal: &Node, /*ObjectLiteralExpression*/
        on_key: bool,
        key1: &str,
        key2: Option<&str>,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> bool {
        let props = get_property_assignment(object_literal, key1, key2);
        for prop in &props {
            self.program_diagnostics().add(Rc::new(
                create_diagnostic_for_node_in_source_file(
                    self.options.config_file.as_ref().unwrap(),
                    &*if on_key {
                        prop.as_property_assignment().name()
                    } else {
                        prop.as_property_assignment().initializer.clone()
                    },
                    message,
                    args.clone(),
                )
                .into(),
            ));
        }
        !props.is_empty()
    }

    pub fn block_emitting_of_file(&self, emit_file_name: &str, diag: Rc<Diagnostic>) {
        self.has_emit_blocking_diagnostics()
            .insert(self.to_path(emit_file_name), true);
        self.program_diagnostics().add(diag);
    }

    pub fn is_emitted_file(&self, file: &str) -> bool {
        if self.options.no_emit == Some(true) {
            return false;
        }

        let file_path = self.to_path(file);
        if self.get_source_file_by_path(&file_path).is_some() {
            return false;
        }

        let out = out_file(&self.options);
        if let Some(out) = out {
            return self.is_same_file(&*file_path, out)
                || self.is_same_file(
                    &*file_path,
                    &format!("{}{}", remove_file_extension(out), Extension::Dts.to_str()),
                );
        }

        if matches!(
            self.options.declaration_dir.as_ref(),
            Some(options_declaration_dir) if contains_path(
                options_declaration_dir,
                &*file_path,
                Some(self.current_directory().clone()),
                Some(!CompilerHost::use_case_sensitive_file_names(&*self.host()))
            )
        ) {
            return true;
        }

        if let Some(options_out_dir) = self.options.out_dir.as_ref() {
            return contains_path(
                options_out_dir,
                &*file_path,
                Some(self.current_directory().clone()),
                Some(CompilerHost::use_case_sensitive_file_names(&*self.host())),
            );
        }

        if file_extension_is_one_of(&*file_path, &supported_js_extensions_flat)
            || file_extension_is(&*file_path, Extension::Dts.to_str())
        {
            let file_path_without_extension = remove_file_extension(&*file_path);
            return self
                .get_source_file_by_path(&Path::new(format!(
                    "{}{}",
                    file_path_without_extension,
                    Extension::Ts.to_str()
                )))
                .is_some()
                || self
                    .get_source_file_by_path(&Path::new(format!(
                        "{}{}",
                        file_path_without_extension,
                        Extension::Tsx.to_str()
                    )))
                    .is_some();
        }
        false
    }

    pub fn is_same_file(&self, file1: &str, file2: &str) -> bool {
        compare_paths(
            file1,
            file2,
            Some(self.current_directory().clone()),
            Some(!CompilerHost::use_case_sensitive_file_names(&*self.host())),
        ) == Comparison::EqualTo
    }

    pub fn get_symlink_cache(&self) -> Rc<SymlinkCache> {
        let host_symlink_cache = self.host().get_symlink_cache();
        if let Some(host_symlink_cache) = host_symlink_cache {
            return host_symlink_cache;
        }
        if self.symlinks().is_none() {
            let host = self.host();
            *self.symlinks() = Some(Rc::new(create_symlink_cache(
                &self.current_directory(),
                Rc::new(move |file_name: &str| host.get_canonical_file_name(file_name)),
            )));
        }
        let symlinks = self.symlinks().clone().unwrap();
        if
        /*files && resolvedTypeReferenceDirectives &&*/
        !symlinks.has_processed_resolutions() {
            symlinks.set_symlinks_from_resolutions(
                &self.files(),
                Some(&self.resolved_type_reference_directives()),
            );
        }
        symlinks
    }

    pub fn get_symlink_cache_rc(&self) -> Rc<dyn Fn() -> Rc<SymlinkCache>> {
        let self_clone = self.rc_wrapper();
        Rc::new(move || self_clone.get_symlink_cache())
    }
}

pub(super) struct HostForUseSourceOfProjectReferenceRedirect {
    pub compiler_host: Rc<dyn CompilerHost>,
    pub get_symlink_cache: Rc<dyn Fn() -> Rc<SymlinkCache>>,
    pub use_source_of_project_reference_redirect: bool,
    pub to_path: Rc<dyn Fn(&str) -> Path>,
    pub get_resolved_project_references:
        Rc<dyn Fn() -> Option<Vec<Option<Rc<ResolvedProjectReference>>>>>,
    pub for_each_resolved_project_reference:
        Rc<dyn Fn(&mut dyn FnMut(Rc<ResolvedProjectReference>))>,
}

pub(super) fn update_host_for_use_source_of_project_reference_redirect(
    host: HostForUseSourceOfProjectReferenceRedirect,
) -> UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
    let overrider: Rc<dyn ModuleResolutionHostOverrider> = Rc::new(
        UpdateHostForUseSourceOfProjectReferenceRedirectOverrider::new(
            host.compiler_host.clone(),
            host.get_symlink_cache.clone(),
            host.to_path.clone(),
            host.get_resolved_project_references.clone(),
            host.for_each_resolved_project_reference.clone(),
        ),
    );

    if !host.use_source_of_project_reference_redirect {
        return UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
            on_program_create_complete: Rc::new(|| {}),
            directory_exists: None,
            file_exists: overrider,
        };
    }

    host.compiler_host
        .set_overriding_file_exists(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_directory_exists(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_get_directories(Some(overrider.clone()));

    host.compiler_host
        .set_overriding_realpath(Some(overrider.clone()));

    UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
        on_program_create_complete: {
            let host_compiler_host_clone = host.compiler_host.clone();
            Rc::new(move || {
                host_compiler_host_clone.set_overriding_file_exists(None);
                host_compiler_host_clone.set_overriding_directory_exists(None);
                host_compiler_host_clone.set_overriding_get_directories(None);
            })
        },
        directory_exists: Some(overrider.clone()),
        file_exists: overrider,
    }
}

pub(super) struct UpdateHostForUseSourceOfProjectReferenceRedirectReturn {
    pub on_program_create_complete: Rc<dyn Fn()>,
    pub directory_exists: Option<Rc<dyn ModuleResolutionHostOverrider>>,
    pub file_exists: Rc<dyn ModuleResolutionHostOverrider>,
}

struct UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    pub host_compiler_host: Rc<dyn CompilerHost>,
    pub host_get_symlink_cache: Rc<dyn Fn() -> Rc<SymlinkCache>>,
    pub host_to_path: Rc<dyn Fn(&str) -> Path>,
    pub host_get_resolved_project_references:
        Rc<dyn Fn() -> Option<Vec<Option<Rc<ResolvedProjectReference>>>>>,
    pub host_for_each_resolved_project_reference:
        Rc<dyn Fn(&mut dyn FnMut(Rc<ResolvedProjectReference>))>,
    set_of_declaration_directories: RefCell<Option<HashSet<Path>>>,
}

impl UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    pub fn new(
        host_compiler_host: Rc<dyn CompilerHost>,
        host_get_symlink_cache: Rc<dyn Fn() -> Rc<SymlinkCache>>,
        host_to_path: Rc<dyn Fn(&str) -> Path>,
        host_get_resolved_project_references: Rc<
            dyn Fn() -> Option<Vec<Option<Rc<ResolvedProjectReference>>>>,
        >,
        host_for_each_resolved_project_reference: Rc<
            dyn Fn(&mut dyn FnMut(Rc<ResolvedProjectReference>)),
        >,
    ) -> Self {
        Self {
            host_compiler_host,
            host_get_symlink_cache,
            host_to_path,
            host_get_resolved_project_references,
            host_for_each_resolved_project_reference,
            set_of_declaration_directories: RefCell::new(None),
        }
    }

    fn handle_directory_could_be_symlink(&self, directory: &str) {
        unimplemented!()
    }

    fn file_or_directory_exists_using_source(
        &self,
        file_or_directory: &str,
        is_file: bool,
    ) -> bool {
        unimplemented!()
    }
}

impl ModuleResolutionHostOverrider for UpdateHostForUseSourceOfProjectReferenceRedirectOverrider {
    fn file_exists(&self, file: &str) -> bool {
        if self.host_compiler_host.file_exists_non_overridden(file) {
            return true;
        }
        if (self.host_get_resolved_project_references)().is_none() {
            return false;
        }
        if !is_declaration_file_name(file) {
            return false;
        }

        self.file_or_directory_exists_using_source(file, true)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        if !self.host_compiler_host.is_directory_exists_supported() {
            return None;
        }
        if self
            .host_compiler_host
            .directory_exists_non_overridden(path)
            == Some(true)
        {
            self.handle_directory_could_be_symlink(path);
            return Some(true);
        }

        if (self.host_get_resolved_project_references)().is_none() {
            return Some(false);
        }

        if self.set_of_declaration_directories.borrow().is_none() {
            let mut set_of_declaration_directories =
                self.set_of_declaration_directories.borrow_mut();
            *set_of_declaration_directories = Some(HashSet::new());
            let set_of_declaration_directories = set_of_declaration_directories.as_mut().unwrap();
            (self.host_for_each_resolved_project_reference)(&mut |ref_| {
                let out = out_file(&ref_.command_line.options);
                if let Some(out) = out {
                    set_of_declaration_directories
                        .insert(get_directory_path(&(self.host_to_path)(out)).into());
                } else {
                    let declaration_dir = ref_
                        .command_line
                        .options
                        .declaration_dir
                        .as_ref()
                        .or_else(|| ref_.command_line.options.out_dir.as_ref());
                    if let Some(declaration_dir) = declaration_dir {
                        set_of_declaration_directories.insert((self.host_to_path)(declaration_dir));
                    }
                }
            });
        }

        Some(self.file_or_directory_exists_using_source(path, false))
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        if !self.host_compiler_host.is_get_directories_supported() {
            return None;
        }
        if (self.host_get_resolved_project_references)().is_none()
            || self
                .host_compiler_host
                .directory_exists_non_overridden(path)
                == Some(true)
        {
            self.host_compiler_host.get_directories_non_overridden(path)
        } else {
            Some(vec![])
        }
    }

    fn realpath(&self, s: &str) -> Option<String> {
        if !self.host_compiler_host.is_realpath_supported() {
            return None;
        }
        (self.host_get_symlink_cache)()
            .get_symlinked_files()
            .as_ref()
            .and_then(|symlinked_files| symlinked_files.get(&(self.host_to_path)(s)).cloned())
            .or_else(|| self.host_compiler_host.realpath_non_overridden(s))
    }
}

pub(super) fn filter_semantic_diagnostics(
    diagnostic: Vec<Rc<Diagnostic>>,
    option: &CompilerOptions,
) -> Vec<Rc<Diagnostic>> {
    diagnostic
        .into_iter()
        .filter(|d| match d.maybe_skipped_on().as_ref() {
            None => true,
            Some(d_skipped_on) => option.get_value(d_skipped_on).as_option_bool() != Some(true),
        })
        .collect()
}

pub trait CompilerHostLike {
    fn use_case_sensitive_file_names(&self) -> bool;
    fn get_current_directory(&self) -> String;
    fn file_exists(&self, file_name: &str) -> bool;
    fn read_file(&self, file_name: &str) -> io::Result<String>;
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
    fn trace(&self, s: &str) {}
    fn is_trace_supported(&self) -> bool;
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {}

    // These exist to allow "forwarding" CompilerHost -> CompilerHostLike -> DirectoryStructureHost
    fn is_read_directory_implemented(&self) -> bool;
    fn realpath(&self, path: &str) -> Option<String>;
    fn create_directory(&self, path: &str);
    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>);
    fn directory_exists(&self, path: &str) -> Option<bool>;
    fn get_directories(&self, path: &str) -> Option<Vec<String>>;
}

pub struct CompilerHostLikeRcDynCompilerHost {
    host: Rc<dyn CompilerHost>,
}

impl CompilerHostLikeRcDynCompilerHost {
    pub fn new(host: Rc<dyn CompilerHost>) -> Self {
        Self { host }
    }
}

impl CompilerHostLike for CompilerHostLikeRcDynCompilerHost {
    fn use_case_sensitive_file_names(&self) -> bool {
        CompilerHost::use_case_sensitive_file_names(&*self.host)
    }

    fn get_current_directory(&self) -> String {
        CompilerHost::get_current_directory(&*self.host)
    }

    fn file_exists(&self, file_name: &str) -> bool {
        self.host.file_exists(file_name)
    }

    fn read_file(&self, file_name: &str) -> io::Result<String> {
        self.host.read_file(file_name)
    }

    fn read_directory(
        &self,
        root_dir: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        self.host
            .read_directory(root_dir, extensions, excludes, includes, depth)
    }

    fn trace(&self, s: &str) {
        self.host.trace(s)
    }

    fn is_trace_supported(&self) -> bool {
        self.host.is_trace_supported()
    }

    // fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {}
    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) {
        self.host.create_directory(path)
    }

    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {
        self.host
            .write_file(path, data, write_byte_order_mark.unwrap(), None, None)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.host.directory_exists(path)
    }

    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        self.host.get_directories(path)
    }
}

pub struct DirectoryStructureHostRcDynCompilerHostLike {
    host: Rc<dyn CompilerHostLike>,
}

impl DirectoryStructureHostRcDynCompilerHostLike {
    pub fn new(host: Rc<dyn CompilerHostLike>) -> Self {
        Self { host }
    }
}

impl DirectoryStructureHost for DirectoryStructureHostRcDynCompilerHostLike {
    fn file_exists(&self, path: &str) -> bool {
        self.host.file_exists(path)
    }

    fn read_file(&self, path: &str, encoding: Option<&str>) -> io::Result<String> {
        self.host.read_file(path)
    }

    fn directory_exists(&self, path: &str) -> Option<bool> {
        self.host.directory_exists(path)
    }
    fn get_directories(&self, path: &str) -> Option<Vec<String>> {
        self.host.get_directories(path)
    }
    fn read_directory(
        &self,
        path: &str,
        extensions: &[&str],
        exclude: Option<&[String]>,
        include: Option<&[String]>,
        depth: Option<usize>,
    ) -> Option<Vec<String>> {
        self.host
            .read_directory(path, extensions, exclude, include.unwrap(), depth)
    }
    fn is_read_directory_implemented(&self) -> bool {
        self.host.is_read_directory_implemented()
    }

    fn realpath(&self, path: &str) -> Option<String> {
        self.host.realpath(path)
    }

    fn create_directory(&self, path: &str) {
        self.host.create_directory(path)
    }

    fn write_file(&self, path: &str, data: &str, write_byte_order_mark: Option<bool>) {
        self.host.write_file(path, data, write_byte_order_mark)
    }
}

pub(crate) fn parse_config_host_from_compiler_host_like(
    host: Rc<dyn CompilerHostLike>,
    directory_structure_host: Option<Rc<dyn DirectoryStructureHost>>,
) -> ParseConfigHostFromCompilerHostLike {
    let directory_structure_host = directory_structure_host.unwrap_or_else(|| {
        Rc::new(DirectoryStructureHostRcDynCompilerHostLike::new(
            host.clone(),
        ))
    });
    ParseConfigHostFromCompilerHostLike::new(host, directory_structure_host)
}

pub struct ParseConfigHostFromCompilerHostLike {
    host: Rc<dyn CompilerHostLike>,
    directory_structure_host: Rc<dyn DirectoryStructureHost>,
}

impl ParseConfigHostFromCompilerHostLike {
    pub fn new(
        host: Rc<dyn CompilerHostLike>,
        directory_structure_host: Rc<dyn DirectoryStructureHost>,
    ) -> Self {
        Self {
            host,
            directory_structure_host,
        }
    }
}

impl ParseConfigFileHost for ParseConfigHostFromCompilerHostLike {
    fn get_current_directory(&self) -> String {
        self.host.get_current_directory()
    }
}

impl ParseConfigHost for ParseConfigHostFromCompilerHostLike {
    fn use_case_sensitive_file_names(&self) -> bool {
        self.host.use_case_sensitive_file_names()
    }

    fn read_directory(
        &self,
        root: &str,
        extensions: &[&str],
        excludes: Option<&[String]>,
        includes: &[String],
        depth: Option<usize>,
    ) -> Vec<String> {
        Debug_.assert(self.directory_structure_host.is_read_directory_implemented(), Some("'CompilerHost.readDirectory' must be implemented to correctly process 'projectReferences'"));
        self.directory_structure_host
            .read_directory(root, extensions, excludes, Some(includes), depth)
            .unwrap()
    }

    fn file_exists(&self, path: &str) -> bool {
        self.directory_structure_host.file_exists(path)
    }

    fn read_file(&self, path: &str) -> io::Result<String> {
        self.directory_structure_host.read_file(path, None)
    }

    fn trace(&self, s: &str) {
        self.host.trace(s)
    }

    fn is_trace_supported(&self) -> bool {
        self.host.is_trace_supported()
    }

    fn as_dyn_module_resolution_host(&self) -> &dyn ModuleResolutionHost {
        self
    }
}

impl ConfigFileDiagnosticsReporter for ParseConfigHostFromCompilerHostLike {
    fn on_un_recoverable_config_file_diagnostic(&self, diagnostic: Rc<Diagnostic>) {
        self.host
            .on_un_recoverable_config_file_diagnostic(diagnostic)
    }
}

pub fn resolve_project_reference_path(ref_: &ProjectReference) -> ResolvedConfigFileName {
    let passed_in_ref = ref_;
    resolve_config_file_project_name(&passed_in_ref.path)
}

pub fn get_resolution_diagnostic(
    options: &CompilerOptions,
    resolved_module: &ResolvedModuleFull,
) -> Option<&'static DiagnosticMessage> {
    let extension = resolved_module.extension();
    match extension {
        Extension::Ts | Extension::Dts => None,
        Extension::Tsx => need_jsx(options),
        Extension::Jsx => need_jsx(options).or_else(|| need_allow_js(options)),
        Extension::Js => need_allow_js(options),
        Extension::Json => need_resolve_json_module(options),
        _ => None,
    }
}

fn need_jsx(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if options.jsx.is_some() {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_jsx_is_not_set)
    }
}

fn need_allow_js(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if get_allow_js_compiler_option(options) || !get_strict_option_value(options, "noImplicitAny") {
        None
    } else {
        Some(&Diagnostics::Could_not_find_a_declaration_file_for_module_0_1_implicitly_has_an_any_type)
    }
}

fn need_resolve_json_module(options: &CompilerOptions) -> Option<&'static DiagnosticMessage> {
    if matches!(options.resolve_json_module, Some(true)) {
        None
    } else {
        Some(&Diagnostics::Module_0_was_resolved_to_1_but_resolveJsonModule_is_not_used)
    }
}

fn get_module_names(file: &Node /*SourceFile*/) -> Vec<String> {
    let file_as_source_file = file.as_source_file();
    let imports = file_as_source_file.maybe_imports();
    let imports = imports.as_ref().unwrap();
    let module_augmentations = file_as_source_file.maybe_module_augmentations();
    let module_augmentations = module_augmentations.as_ref().unwrap();
    let mut res: Vec<String> = imports
        .into_iter()
        .map(|i| i.as_literal_like_node().text().clone())
        .collect();
    for aug in module_augmentations {
        if aug.kind() == SyntaxKind::StringLiteral {
            res.push(aug.as_literal_like_node().text().clone());
        }
    }
    res
}

pub(crate) fn get_module_name_string_literal_at<TFile: SourceFileImportsList>(
    file: &TFile,
    index: usize,
) -> Rc<Node /*StringLiteralLike*/> {
    unimplemented!()
}
