use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use super::{create_brackets_map, TempFlags};
use crate::{
    combine_paths, compare_paths, compute_common_source_directory_of_filenames,
    directory_separator_str, factory, file_extension_is, file_extension_is_one_of,
    get_are_declaration_maps_enabled, get_base_file_name, get_declaration_emit_output_file_path,
    get_directory_path, get_emit_declarations, get_emit_module_kind_from_module_and_target,
    get_new_line_character, get_normalized_absolute_path, get_own_emit_output_file_path,
    get_relative_path_from_directory, get_source_files_to_emit, is_expression, is_identifier,
    is_incremental_compilation, is_json_source_file, is_option_str_empty, is_source_file,
    last_or_undefined, no_emit_notification, no_emit_substitution, normalize_slashes, out_file,
    remove_file_extension, resolve_path, with_synthetic_factory_and_factory,
    BaseNodeFactorySynthetic, BuildInfo, BundleFileInfo, BundleFileSection,
    BundleFileSectionInterface, BundleFileSectionKind, Comparison, CompilerOptions,
    CurrentParenthesizerRule, Debug_, DetachedCommentInfo, EmitBinaryExpression, EmitFileNames,
    EmitHint, EmitHost, EmitResolverDebuggable, EmitResult, EmitTextWriter, EmitTransformers,
    Extension, JsxEmit, ListFormat, Node, NodeArray, NodeId, NodeInterface, ParenthesizerRules,
    ParsedCommandLine, PrintHandlers, Printer, PrinterOptions, ScriptReferenceHost,
    SourceMapGenerator, SourceMapSource, SyntaxKind, TextRange,
};

lazy_static! {
    pub(super) static ref brackets: HashMap<ListFormat, (&'static str, &'static str)> =
        create_brackets_map();
}

pub(crate) fn is_build_info_file(file: &str) -> bool {
    file_extension_is(file, Extension::TsBuildInfo.to_str())
}

pub enum NodeOrVecNode {
    Node(Gc<Node>),
    VecNode(Vec<Gc<Node>>),
}

impl From<Vec<Gc<Node>>> for NodeOrVecNode {
    fn from(value: Vec<Gc<Node>>) -> Self {
        Self::VecNode(value)
    }
}

impl From<Gc<Node>> for NodeOrVecNode {
    fn from(value: Gc<Node>) -> Self {
        Self::Node(value)
    }
}

pub fn for_each_emitted_file_returns<TReturn>(
    host: &dyn EmitHost,
    mut action: impl FnMut(&EmitFileNames, Option<&Node /*SourceFile | Bundle*/>) -> Option<TReturn>,
    source_files_or_target_source_file: Option<impl Into<NodeOrVecNode>>,
    force_dts_emit: Option<bool>,
    only_build_info: Option<bool>,
    include_build_info: Option<bool>,
) -> Option<TReturn> {
    let force_dts_emit = force_dts_emit.unwrap_or(false);
    let source_files = match source_files_or_target_source_file.map(Into::into) {
        Some(NodeOrVecNode::VecNode(source_files_or_target_source_file)) => {
            source_files_or_target_source_file
        }
        Some(NodeOrVecNode::Node(source_files_or_target_source_file)) => get_source_files_to_emit(
            host,
            Some(&*source_files_or_target_source_file),
            Some(force_dts_emit),
        ),
        None => get_source_files_to_emit(host, Option::<&Node>::None, Some(force_dts_emit)),
    };
    let options = ScriptReferenceHost::get_compiler_options(host);
    if out_file(&options)
        .filter(|out_file| !out_file.is_empty())
        .is_some()
    {
        let prepends = host.get_prepend_nodes();
        if !source_files.is_empty() || !prepends.is_empty() {
            let bundle: Gc<Node> =
                with_synthetic_factory_and_factory(|synthetic_factory, factory_| {
                    factory_
                        .create_bundle(synthetic_factory, source_files, Some(prepends))
                        .into()
                });
            let result = action(
                &get_output_paths_for(&bundle, host, force_dts_emit),
                Some(&*bundle),
            );
            if result.is_some() {
                return result;
            }
        }
    } else {
        if only_build_info != Some(true) {
            for source_file in &source_files {
                let result = action(
                    &get_output_paths_for(source_file, host, force_dts_emit),
                    Some(&**source_file),
                );
                if result.is_some() {
                    return result;
                }
            }
        }
        if include_build_info == Some(true) {
            let build_info_path = get_ts_build_info_emit_output_file_path(&options);
            if let Some(build_info_path) =
                build_info_path.filter(|build_info_path| !build_info_path.is_empty())
            {
                return action(
                    &EmitFileNames {
                        build_info_path: Some(build_info_path),
                        ..Default::default()
                    },
                    None,
                );
            }
        }
    }
    None
}

pub fn for_each_emitted_file(
    host: &dyn EmitHost,
    mut action: impl FnMut(&EmitFileNames, Option<&Node /*SourceFile | Bundle*/>),
    source_files_or_target_source_file: Option<impl Into<NodeOrVecNode>>,
    force_dts_emit: Option<bool>,
    only_build_info: Option<bool>,
    include_build_info: Option<bool>,
) {
    for_each_emitted_file_returns(
        host,
        |emit_file_names, node| -> Option<()> {
            action(emit_file_names, node);
            None
        },
        source_files_or_target_source_file,
        force_dts_emit,
        only_build_info,
        include_build_info,
    );
}

pub fn get_ts_build_info_emit_output_file_path(options: &CompilerOptions) -> Option<String> {
    let config_file = options.config_file_path.as_ref();
    if !is_incremental_compilation(options) {
        return None;
    }
    if !is_option_str_empty(options.ts_build_info_file.as_deref()) {
        return options.ts_build_info_file.clone();
    }
    let out_path = out_file(options);
    let build_info_extension_less: String;
    if let Some(out_path) = out_path.filter(|out_path| !out_path.is_empty()) {
        build_info_extension_less = remove_file_extension(out_path).to_owned();
    } else {
        let config_file = config_file?;
        let config_file_extension_less = remove_file_extension(config_file);
        build_info_extension_less = if let Some(options_out_dir) = options
            .out_dir
            .as_ref()
            .filter(|options_out_dir| !options_out_dir.is_empty())
        {
            if let Some(options_root_dir) = options
                .root_dir
                .as_ref()
                .filter(|options_root_dir| !options_root_dir.is_empty())
            {
                resolve_path(
                    options_out_dir,
                    &[Some(&get_relative_path_from_directory(
                        options_root_dir,
                        config_file_extension_less,
                        Option::<fn(&str) -> String>::None,
                        Some(true),
                    ))],
                )
            } else {
                combine_paths(
                    options_out_dir,
                    &[Some(&get_base_file_name(
                        config_file_extension_less,
                        None,
                        None,
                    ))],
                )
            }
        } else {
            config_file_extension_less.to_owned()
        };
    }
    Some(format!(
        "{}{}",
        build_info_extension_less,
        Extension::TsBuildInfo.to_str()
    ))
}

pub(crate) fn get_output_paths_for_bundle(
    options: &CompilerOptions,
    force_dts_paths: bool,
) -> EmitFileNames {
    unimplemented!()
}

pub(crate) fn get_output_paths_for(
    source_file: &Node, /*SourceFile | Bundle*/
    host: &dyn EmitHost,
    force_dts_paths: bool,
) -> EmitFileNames {
    let options = ScriptReferenceHost::get_compiler_options(host);
    if source_file.kind() == SyntaxKind::Bundle {
        get_output_paths_for_bundle(&options, force_dts_paths)
    } else {
        let source_file_as_source_file = source_file.as_source_file();
        let own_output_file_path = get_own_emit_output_file_path(
            &source_file_as_source_file.file_name(),
            host,
            get_output_extension(&source_file_as_source_file.file_name(), &options).to_str(),
        );
        let is_json_file = is_json_source_file(source_file);
        let is_json_emitted_to_same_location = is_json_file
            && compare_paths(
                &source_file_as_source_file.file_name(),
                &own_output_file_path,
                Some(EmitHost::get_current_directory(host)),
                Some(!EmitHost::use_case_sensitive_file_names(host)),
            ) == Comparison::EqualTo;
        let js_file_path =
            if options.emit_declaration_only == Some(true) || is_json_emitted_to_same_location {
                None
            } else {
                Some(own_output_file_path)
            };
        let source_map_file_path = if js_file_path
            .as_ref()
            .filter(|js_file_path| !js_file_path.is_empty())
            .is_none()
            || is_json_source_file(source_file)
        {
            None
        } else {
            get_source_map_file_path(js_file_path.as_ref().unwrap(), &options)
        };
        let declaration_file_path =
            if force_dts_paths || get_emit_declarations(&options) && !is_json_file {
                Some(get_declaration_emit_output_file_path(
                    &source_file_as_source_file.file_name(),
                    host,
                ))
            } else {
                None
            };
        let declaration_map_path = declaration_file_path
            .as_ref()
            .filter(|declaration_file_path| {
                !declaration_file_path.is_empty() && get_are_declaration_maps_enabled(&options)
            })
            .map(|declaration_file_path| format!("{declaration_file_path}.map"));
        EmitFileNames {
            js_file_path,
            source_map_file_path,
            declaration_file_path,
            declaration_map_path,
            build_info_path: None,
        }
    }
}

fn get_source_map_file_path(js_file_path: &str, options: &CompilerOptions) -> Option<String> {
    if options.source_map == Some(true) && options.inline_source_map != Some(true) {
        Some(format!("{js_file_path}.map"))
    } else {
        None
    }
}

pub fn get_output_extension(file_name: &str, options: &CompilerOptions) -> Extension {
    if file_extension_is(file_name, Extension::Json.to_str()) {
        Extension::Json
    } else if options.jsx == Some(JsxEmit::Preserve)
        && file_extension_is_one_of(file_name, &[Extension::Jsx, Extension::Tsx])
    {
        Extension::Jsx
    } else if file_extension_is_one_of(file_name, &[Extension::Mts, Extension::Mjs]) {
        Extension::Mjs
    } else if file_extension_is_one_of(file_name, &[Extension::Cts, Extension::Cjs]) {
        Extension::Cjs
    } else {
        Extension::Js
    }
}

pub(crate) fn get_output_declaration_file_name<TGetCommonSourceDirectory: FnMut() -> String>(
    input_file_name: &str,
    config_file: &ParsedCommandLine,
    ignore_case: bool,
    get_common_source_directory: Option<&mut TGetCommonSourceDirectory>,
) -> String {
    unimplemented!()
}

pub(crate) fn get_common_source_directory<
    TEmittedFiles: FnMut() -> Vec<String>,
    TGetCanonicalFileName: FnMut(&str) -> String,
    TCheckSourceFilesBelongToPath: FnMut(&str),
>(
    options: &CompilerOptions,
    mut emitted_files: TEmittedFiles,
    current_directory: &str,
    get_canonical_file_name: TGetCanonicalFileName,
    check_source_files_belong_to_path: Option<TCheckSourceFilesBelongToPath>,
) -> String {
    let mut common_source_directory: String;
    if let Some(options_root_dir) = options
        .root_dir
        .as_ref()
        .filter(|options_root_dir| !options_root_dir.is_empty())
    {
        common_source_directory =
            get_normalized_absolute_path(options_root_dir, Some(current_directory));
        if let Some(mut check_source_files_belong_to_path) = check_source_files_belong_to_path {
            check_source_files_belong_to_path(options_root_dir);
        }
    } else if let Some(options_config_file_path) =
        options
            .config_file_path
            .as_ref()
            .filter(|options_config_file_path| {
                !options_config_file_path.is_empty() && options.composite == Some(true)
            })
    {
        common_source_directory = get_directory_path(&normalize_slashes(options_config_file_path));
        if let Some(mut check_source_files_belong_to_path) = check_source_files_belong_to_path {
            check_source_files_belong_to_path(&common_source_directory);
        }
    } else {
        common_source_directory = compute_common_source_directory_of_filenames(
            &emitted_files(),
            current_directory,
            get_canonical_file_name,
        );
    }

    if !common_source_directory.is_empty()
        && &common_source_directory[common_source_directory.len() - 1..] != directory_separator_str
    {
        common_source_directory.push_str(directory_separator_str);
    }
    common_source_directory
}

pub(crate) fn get_common_source_directory_of_config(
    command_line: &ParsedCommandLine,
    ignore_case: bool,
) -> String {
    let options = &command_line.options;
    let file_names = &command_line.file_names;
    unimplemented!()
}

pub(crate) fn emit_files(
    resolver: Gc<Box<dyn EmitResolverDebuggable>>,
    host: Gc<Box<dyn EmitHost>>,
    target_source_file: Option<&Node /*SourceFile*/>,
    EmitTransformers {
        script_transformers,
        declaration_transformers,
    }: EmitTransformers,
    emit_only_dts_files: Option<bool>,
    only_build_info: Option<bool>,
    force_dts_emit: Option<bool>,
) -> EmitResult {
    unimplemented!()
}

pub(crate) fn get_build_info(build_info_text: &str) -> Gc<BuildInfo> {
    unimplemented!()
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum PipelinePhase {
    Notification,
    Substitution,
    Comments,
    SourceMaps,
    Emit,
}

impl PipelinePhase {
    pub fn incremented(&self) -> Self {
        match self {
            Self::Notification => Self::Substitution,
            Self::Substitution => Self::Comments,
            Self::Comments => Self::SourceMaps,
            Self::SourceMaps => Self::Emit,
            Self::Emit => panic!("Called incremented() on PipelinePhase::Emit"),
        }
    }
}

pub fn create_printer(
    printer_options: PrinterOptions,
    handlers: Option<Gc<Box<dyn PrintHandlers>>>,
) -> Gc<Printer> {
    let handlers = handlers.unwrap_or_else(|| Gc::new(Box::new(DummyPrintHandlers)));
    let printer = Gc::new(Printer::new(printer_options, handlers));
    *printer._rc_wrapper.borrow_mut() = Some(printer.clone());
    printer.reset();
    *printer.emit_binary_expression.borrow_mut() =
        Some(Gc::new(printer.create_emit_binary_expression()));
    printer
}

#[derive(Trace, Finalize)]
pub(super) struct DummyPrintHandlers;

impl PrintHandlers for DummyPrintHandlers {
    fn is_on_emit_node_supported(&self) -> bool {
        false
    }

    fn is_substitute_node_supported(&self) -> bool {
        false
    }
}

impl Printer {
    pub fn new(printer_options: PrinterOptions, handlers: Gc<Box<dyn PrintHandlers>>) -> Self {
        let extended_diagnostics = printer_options.extended_diagnostics == Some(true);
        let new_line =
            get_new_line_character(printer_options.new_line, Option::<fn() -> String>::None);
        let module_kind = get_emit_module_kind_from_module_and_target(
            printer_options.module,
            printer_options.target,
        );
        let preserve_source_newlines = printer_options.preserve_source_newlines;
        let bundle_file_info = if printer_options.write_bundle_file_info == Some(true) {
            Some(BundleFileInfo {
                sections: vec![],
                sources: None,
            })
        } else {
            None
        };
        let relative_to_build_info = if bundle_file_info.is_some() {
            Some(Debug_.check_defined(printer_options.relative_to_build_info.clone(), None))
        } else {
            None
        };
        let record_internal_section = printer_options.record_internal_section;
        Self {
            _rc_wrapper: Default::default(),
            printer_options,
            handlers,
            extended_diagnostics,
            new_line,
            module_kind,
            current_source_file: Default::default(),
            bundled_helpers: Default::default(),
            node_id_to_generated_name: Default::default(),
            auto_generated_id_to_generated_name: Default::default(),
            generated_names: Default::default(),
            temp_flags_stack: Default::default(),
            temp_flags: Cell::new(TempFlags::Auto),
            reserved_names_stack: Default::default(),
            reserved_names: Default::default(),
            preserve_source_newlines: Cell::new(preserve_source_newlines),
            next_list_element_pos: Default::default(),
            writer: Default::default(),
            own_writer: Default::default(),
            write: Cell::new(Printer::write_base),
            is_own_file_emit: Default::default(),
            bundle_file_info: GcCell::new(bundle_file_info),
            relative_to_build_info,
            record_internal_section,
            source_file_text_pos: Default::default(),
            source_file_text_kind: Cell::new(BundleFileSectionKind::Text),

            source_maps_disabled: Cell::new(true),
            source_map_generator: Default::default(),
            source_map_source: Default::default(),
            source_map_source_index: Cell::new(-1),
            most_recently_added_source_map_source: Default::default(),
            most_recently_added_source_map_source_index: Cell::new(-1),

            container_pos: Cell::new(-1),
            container_end: Cell::new(-1),
            declaration_list_container_end: Cell::new(-1),
            current_line_map: Default::default(),
            detached_comments_info: Default::default(),
            has_written_comment: Default::default(),
            comments_disabled: Default::default(),
            last_substitution: Default::default(),
            current_parenthesizer_rule: Default::default(),
            // const { enter: enterComment, exit: exitComment } = performance.createTimerIf(extendedDiagnostics, "commentTime", "beforeComment", "afterComment");
            parenthesizer: factory.with(|factory_| factory_.parenthesizer()),
            emit_binary_expression: Default::default(),
        }
    }

    pub(super) fn rc_wrapper(&self) -> Gc<Printer> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_source_file(&self) -> Option<Gc<Node>> {
        self.current_source_file.borrow().clone()
    }

    pub(super) fn current_source_file(&self) -> Gc<Node> {
        self.current_source_file.borrow().clone().unwrap()
    }

    pub(super) fn set_current_source_file(&self, current_source_file: Option<Gc<Node>>) {
        *self.current_source_file.borrow_mut() = current_source_file;
    }

    pub(super) fn bundled_helpers(&self) -> Ref<HashMap<String, bool>> {
        self.bundled_helpers.borrow()
    }

    pub(super) fn bundled_helpers_mut(&self) -> RefMut<HashMap<String, bool>> {
        self.bundled_helpers.borrow_mut()
    }

    pub(super) fn node_id_to_generated_name_mut(&self) -> RefMut<HashMap<NodeId, String>> {
        self.node_id_to_generated_name.borrow_mut()
    }

    pub(super) fn set_node_id_to_generated_name(
        &self,
        node_id_to_generated_name: HashMap<NodeId, String>,
    ) {
        *self.node_id_to_generated_name.borrow_mut() = node_id_to_generated_name;
    }

    pub(super) fn auto_generated_id_to_generated_name_mut(&self) -> RefMut<HashMap<usize, String>> {
        self.auto_generated_id_to_generated_name.borrow_mut()
    }

    pub(super) fn set_auto_generated_id_to_generated_name(
        &self,
        auto_generated_id_to_generated_name: HashMap<usize, String>,
    ) {
        *self.auto_generated_id_to_generated_name.borrow_mut() =
            auto_generated_id_to_generated_name;
    }

    pub(super) fn generated_names(&self) -> Ref<HashSet<String>> {
        self.generated_names.borrow()
    }

    pub(super) fn generated_names_mut(&self) -> RefMut<HashSet<String>> {
        self.generated_names.borrow_mut()
    }

    pub(super) fn set_generated_names(&self, generated_names: HashSet<String>) {
        *self.generated_names.borrow_mut() = generated_names;
    }

    pub(super) fn temp_flags_stack_mut(&self) -> RefMut<Vec<TempFlags>> {
        self.temp_flags_stack.borrow_mut()
    }

    pub(super) fn set_temp_flags_stack(&self, temp_flags_stack: Vec<TempFlags>) {
        *self.temp_flags_stack.borrow_mut() = temp_flags_stack;
    }

    pub(super) fn temp_flags(&self) -> TempFlags {
        self.temp_flags.get()
    }

    pub(super) fn set_temp_flags(&self, temp_flags: TempFlags) {
        self.temp_flags.set(temp_flags);
    }

    pub(super) fn reserved_names_stack(&self) -> Ref<Vec<Option<Rc<RefCell<HashSet<String>>>>>> {
        self.reserved_names_stack.borrow()
    }

    pub(super) fn reserved_names_stack_mut(
        &self,
    ) -> RefMut<Vec<Option<Rc<RefCell<HashSet<String>>>>>> {
        self.reserved_names_stack.borrow_mut()
    }

    pub(super) fn set_reserved_names_stack(
        &self,
        reserved_names_stack: Vec<Option<Rc<RefCell<HashSet<String>>>>>,
    ) {
        *self.reserved_names_stack.borrow_mut() = reserved_names_stack;
    }

    pub(super) fn maybe_reserved_names(&self) -> Option<Rc<RefCell<HashSet<String>>>> {
        self.reserved_names.borrow().clone()
    }

    pub(super) fn maybe_reserved_names_mut(&self) -> RefMut<Option<Rc<RefCell<HashSet<String>>>>> {
        self.reserved_names.borrow_mut()
    }

    pub(super) fn set_reserved_names(&self, reserved_names: Option<Rc<RefCell<HashSet<String>>>>) {
        *self.reserved_names.borrow_mut() = reserved_names;
    }

    pub(super) fn maybe_preserve_source_newlines(&self) -> Option<bool> {
        self.preserve_source_newlines.get()
    }

    pub(super) fn set_preserve_source_newlines(&self, preserve_source_newlines: Option<bool>) {
        self.preserve_source_newlines.set(preserve_source_newlines);
    }

    pub(super) fn maybe_next_list_element_pos(&self) -> Option<isize> {
        self.next_list_element_pos.get()
    }

    pub(super) fn set_next_list_element_pos(&self, next_list_element_pos: Option<isize>) {
        self.next_list_element_pos.set(next_list_element_pos);
    }

    pub(super) fn maybe_writer(&self) -> Option<Gc<Box<dyn EmitTextWriter>>> {
        self.writer.borrow().clone()
    }

    pub(super) fn writer(&self) -> Gc<Box<dyn EmitTextWriter>> {
        self.writer.borrow().clone().unwrap()
    }

    pub(super) fn has_global_name(&self, name: &str) -> Option<bool> {
        self.handlers.has_global_name(name)
    }

    pub(super) fn on_emit_node(
        &self,
        hint: EmitHint,
        node: &Node,
        emit_callback: &dyn Fn(EmitHint, &Node),
    ) {
        if self.handlers.is_on_emit_node_supported() {
            self.handlers.on_emit_node(hint, node, emit_callback)
        } else {
            no_emit_notification(hint, node, emit_callback)
        }
    }

    pub(super) fn is_on_emit_node_no_emit_notification(&self) -> bool {
        !self.handlers.is_on_emit_node_supported()
    }

    pub(super) fn is_on_emit_node_supported(&self) -> bool {
        true
    }

    pub(super) fn is_emit_notification_enabled(&self, node: &Node) -> Option<bool> {
        self.handlers.is_emit_notification_enabled(node)
    }

    pub(super) fn substitute_node(&self, hint: EmitHint, node: &Node) -> Option<Gc<Node>> {
        Some(if self.handlers.is_substitute_node_supported() {
            self.handlers.substitute_node(hint, node).unwrap()
        } else {
            no_emit_substitution(hint, node)
        })
    }

    pub(super) fn is_substitute_node_no_emit_substitution(&self) -> bool {
        !self.handlers.is_substitute_node_supported()
    }

    pub(super) fn on_before_emit_node(&self, node: Option<&Node>) {
        self.handlers.on_before_emit_node(node)
    }

    pub(super) fn on_after_emit_node(&self, node: Option<&Node>) {
        self.handlers.on_after_emit_node(node)
    }

    pub(super) fn on_before_emit_node_array(&self, nodes: Option<&NodeArray>) {
        self.handlers.on_before_emit_node_array(nodes)
    }

    pub(super) fn on_after_emit_node_array(&self, nodes: Option<&NodeArray>) {
        self.handlers.on_after_emit_node_array(nodes)
    }

    pub(super) fn on_before_emit_token(&self, node: Option<&Node>) {
        self.handlers.on_before_emit_token(node)
    }

    pub(super) fn on_after_emit_token(&self, node: Option<&Node>) {
        self.handlers.on_after_emit_token(node)
    }

    pub(super) fn write(&self, text: &str) {
        (self.write.get())(self, text);
    }

    pub(super) fn is_own_file_emit(&self) -> bool {
        self.is_own_file_emit.get()
    }

    pub(super) fn set_is_own_file_emit(&self, is_own_file_emit: bool) {
        self.is_own_file_emit.set(is_own_file_emit);
    }

    pub(super) fn maybe_bundle_file_info(&self) -> GcCellRef<Option<BundleFileInfo>> {
        self.bundle_file_info.borrow()
    }

    pub(super) fn maybe_bundle_file_info_mut(&self) -> GcCellRefMut<Option<BundleFileInfo>> {
        self.bundle_file_info.borrow_mut()
    }

    pub(super) fn bundle_file_info(&self) -> GcCellRef<BundleFileInfo> {
        GcCellRef::map(self.bundle_file_info.borrow(), |bundle_file_info| {
            bundle_file_info.as_ref().unwrap()
        })
    }

    pub(super) fn bundle_file_info_mut(
        &self,
    ) -> GcCellRefMut<Option<BundleFileInfo>, BundleFileInfo> {
        GcCellRefMut::map(self.bundle_file_info.borrow_mut(), |bundle_file_info| {
            bundle_file_info.as_mut().unwrap()
        })
    }

    pub(super) fn relative_to_build_info(&self, value: &str) -> String {
        self.relative_to_build_info.clone().unwrap().call(value)
    }

    pub(super) fn source_file_text_pos(&self) -> usize {
        self.source_file_text_pos.get()
    }

    pub(super) fn set_source_file_text_pos(&self, source_file_text_pos: usize) {
        self.source_file_text_pos.set(source_file_text_pos);
    }

    pub(super) fn source_file_text_kind(&self) -> BundleFileSectionKind {
        self.source_file_text_kind.get()
    }

    pub(super) fn set_source_file_text_kind(&self, source_file_text_kind: BundleFileSectionKind) {
        self.source_file_text_kind.set(source_file_text_kind);
    }

    pub(super) fn set_source_map_generator(
        &self,
        source_map_generator: Option<Gc<Box<dyn SourceMapGenerator>>>,
    ) {
        *self.source_map_generator.borrow_mut() = source_map_generator;
    }

    pub(super) fn source_maps_disabled(&self) -> bool {
        self.source_maps_disabled.get()
    }

    pub(super) fn set_source_maps_disabled(&self, source_maps_disabled: bool) {
        self.source_maps_disabled.set(source_maps_disabled);
    }

    pub(super) fn source_map_generator(&self) -> Gc<Box<dyn SourceMapGenerator>> {
        self.source_map_generator.borrow().clone().unwrap()
    }

    pub(super) fn set_source_map_source_(&self, source_map_source: Option<Gc<SourceMapSource>>) {
        *self.source_map_source.borrow_mut() = source_map_source;
    }

    pub(super) fn source_map_source_index(&self) -> isize {
        self.source_map_source_index.get()
    }

    pub(super) fn set_source_map_source_index(&self, source_map_source_index: isize) {
        self.source_map_source_index.set(source_map_source_index);
    }

    pub(super) fn maybe_most_recently_added_source_map_source(
        &self,
    ) -> Option<Gc<SourceMapSource>> {
        self.most_recently_added_source_map_source.borrow().clone()
    }

    pub(super) fn set_most_recently_added_source_map_source(
        &self,
        most_recently_added_source_map_source: Option<Gc<SourceMapSource>>,
    ) {
        *self.most_recently_added_source_map_source.borrow_mut() =
            most_recently_added_source_map_source;
    }

    pub(super) fn most_recently_added_source_map_source_index(&self) -> isize {
        self.most_recently_added_source_map_source_index.get()
    }

    pub(super) fn set_most_recently_added_source_map_source_index(
        &self,
        most_recently_added_source_map_source_index: isize,
    ) {
        self.most_recently_added_source_map_source_index
            .set(most_recently_added_source_map_source_index);
    }

    pub(super) fn container_pos(&self) -> isize {
        self.container_pos.get()
    }

    pub(super) fn set_container_pos(&self, container_pos: isize) {
        self.container_pos.set(container_pos);
    }

    pub(super) fn container_end(&self) -> isize {
        self.container_end.get()
    }

    pub(super) fn set_container_end(&self, container_end: isize) {
        self.container_end.set(container_end);
    }

    pub(super) fn declaration_list_container_end(&self) -> isize {
        self.declaration_list_container_end.get()
    }

    pub(super) fn set_declaration_list_container_end(&self, declaration_list_container_end: isize) {
        self.declaration_list_container_end
            .set(declaration_list_container_end);
    }

    pub(super) fn maybe_current_line_map(&self) -> Ref<Option<Vec<usize>>> {
        self.current_line_map.borrow()
    }

    pub(super) fn current_line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.current_line_map.borrow(), |current_line_map| {
            current_line_map.as_ref().unwrap()
        })
    }

    pub(super) fn set_current_line_map(&self, current_line_map: Option<Vec<usize>>) {
        *self.current_line_map.borrow_mut() = current_line_map;
    }

    pub(super) fn maybe_detached_comments_info(&self) -> Ref<Option<Vec<DetachedCommentInfo>>> {
        self.detached_comments_info.borrow()
    }

    pub(super) fn detached_comments_info(&self) -> Ref<Vec<DetachedCommentInfo>> {
        Ref::map(
            self.detached_comments_info.borrow(),
            |detached_comments_info| detached_comments_info.as_ref().unwrap(),
        )
    }

    pub(super) fn detached_comments_info_mut(&self) -> RefMut<Vec<DetachedCommentInfo>> {
        RefMut::map(
            self.detached_comments_info.borrow_mut(),
            |detached_comments_info| detached_comments_info.as_mut().unwrap(),
        )
    }

    pub(super) fn set_detached_comments_info(
        &self,
        detached_comments_info: Option<Vec<DetachedCommentInfo>>,
    ) {
        *self.detached_comments_info.borrow_mut() = detached_comments_info;
    }

    pub(super) fn has_written_comment(&self) -> bool {
        self.has_written_comment.get()
    }

    pub(super) fn set_has_written_comment(&self, has_written_comment: bool) {
        self.has_written_comment.set(has_written_comment);
    }

    pub(super) fn comments_disabled(&self) -> bool {
        self.comments_disabled.get()
    }

    pub(super) fn set_comments_disabled(&self, comments_disabled: bool) {
        self.comments_disabled.set(comments_disabled);
    }

    pub(super) fn maybe_last_substitution(&self) -> Option<Gc<Node>> {
        self.last_substitution.borrow().clone()
    }

    pub(super) fn set_last_substitution(&self, last_substitution: Option<Gc<Node>>) {
        *self.last_substitution.borrow_mut() = last_substitution;
    }

    pub(super) fn maybe_current_parenthesizer_rule(
        &self,
    ) -> Option<Gc<Box<dyn CurrentParenthesizerRule>>> {
        self.current_parenthesizer_rule.borrow().clone()
    }

    pub(super) fn set_current_parenthesizer_rule(
        &self,
        current_parenthesizer_rule: Option<Gc<Box<dyn CurrentParenthesizerRule>>>,
    ) {
        *self.current_parenthesizer_rule.borrow_mut() = current_parenthesizer_rule;
    }

    pub(super) fn parenthesizer(
        &self,
    ) -> Gc<Box<dyn ParenthesizerRules<BaseNodeFactorySynthetic>>> {
        self.parenthesizer.clone()
    }

    pub(super) fn enter_comment(&self) {
        // unimplemented!()
    }

    pub(super) fn exit_comment(&self) {
        // unimplemented!()
    }

    pub(super) fn emit_binary_expression_rc(&self) -> Gc<EmitBinaryExpression> {
        self.emit_binary_expression.borrow().clone().unwrap()
    }

    pub(super) fn emit_binary_expression(&self, node: &Node /*BinaryExpression*/) {
        self.emit_binary_expression_rc().call(node)
    }

    pub fn print_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: &Node, /*SourceFile*/
    ) -> String {
        match hint {
            EmitHint::SourceFile => {
                Debug_.assert(is_source_file(node), Some("Expected a SourceFile node."));
            }
            EmitHint::IdentifierName => {
                Debug_.assert(is_identifier(node), Some("Expected an Identifier node."));
            }
            EmitHint::Expression => {
                Debug_.assert(is_expression(node), Some("Expected an Expression node."));
            }
            _ => (),
        }
        match node.kind() {
            SyntaxKind::SourceFile => {
                return self.print_file(node);
            }
            SyntaxKind::Bundle => {
                return self.print_bundle(node);
            }
            SyntaxKind::UnparsedSource => {
                return self.print_unparsed_source(node);
            }
            _ => (),
        }
        self.write_node(hint, node, Some(source_file), self.begin_print());
        self.end_print()
    }

    pub fn print_list(
        &self,
        format: ListFormat,
        nodes: &NodeArray,
        source_file: &Node, /*SourceFile*/
    ) -> String {
        self.write_list(format, nodes, Some(source_file), self.begin_print());
        self.end_print()
    }

    pub fn print_bundle(&self, bundle: &Node /*Bundle*/) -> String {
        self.write_bundle(bundle, self.begin_print(), None);
        self.end_print()
    }

    pub fn print_file(&self, source_file: &Node /*SourceFile*/) -> String {
        self.write_file(source_file, self.begin_print(), None);
        self.end_print()
    }

    pub fn print_unparsed_source(&self, unparsed: &Node /*UnparsedSource*/) -> String {
        self.write_unparsed_source(unparsed, self.begin_print());
        self.end_print()
    }

    pub fn write_node(
        &self,
        hint: EmitHint,
        node: &Node,
        source_file: Option<&Node /*SourceFile*/>,
        output: Gc<Box<dyn EmitTextWriter>>,
    ) {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        self.print(hint, node, source_file);
        self.reset();
        *self.writer.borrow_mut() = previous_writer;
    }

    pub fn write_list(
        &self,
        format: ListFormat,
        nodes: &NodeArray,
        source_file: Option<&Node /*SourceFile*/>,
        output: Gc<Box<dyn EmitTextWriter>>,
    ) {
        let previous_writer = self.maybe_writer();
        self.set_writer(Some(output), None);
        if source_file.is_some() {
            self.set_source_file(source_file);
        }
        self.emit_list(Option::<&Node>::None, Some(nodes), format, None, None, None);
        self.reset();
        *self.writer.borrow_mut() = previous_writer;
    }

    pub(super) fn get_text_pos_with_write_line(&self) -> usize {
        self.writer()
            .get_text_pos_with_write_line()
            .unwrap_or_else(|| self.writer().get_text_pos())
    }

    pub(super) fn update_or_push_bundle_file_text_like(
        &self,
        pos: isize,
        end: isize,
        kind: BundleFileSectionKind, /*BundleFileTextLikeKind*/
    ) {
        let mut bundle_file_info = self.bundle_file_info_mut();
        let last = last_or_undefined(&bundle_file_info.sections);
        if let Some(last) = last.filter(|last| last.kind() == kind) {
            last.set_end(end);
        } else {
            bundle_file_info
                .sections
                .push(Gc::new(BundleFileSection::new_text_like(
                    kind, None, pos, end,
                )));
        }
    }
}
