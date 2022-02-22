#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::{Borrow, Cow};
use std::cell::{Cell, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::{
    add_related_info, create_diagnostic_for_node, declaration_name_to_string,
    escape_leading_underscores, for_each_child_returns, get_assignment_declaration_kind,
    get_containing_class, get_emit_script_target, get_node_id, get_strict_option_value,
    get_symbol_name_for_private_identifier, get_text_of_identifier_or_literal, has_dynamic_name,
    has_syntactic_modifier, index_of, is_ambient_module, is_block, is_enum_const,
    is_export_specifier, is_global_scope_augmentation, is_jsdoc_construct_signature,
    is_module_block, is_named_declaration, is_private_identifier, is_signed_numeric_literal,
    is_source_file, is_string_or_numeric_literal_like, is_type_alias_declaration, length,
    maybe_for_each, maybe_set_parent, node_has_name, node_is_missing, set_parent_recursive,
    token_to_string, unescape_leading_underscores, AssignmentDeclarationKind, CompilerOptions,
    Debug_, Diagnostic, DiagnosticRelatedInformation, Diagnostics, FlowFlags, FlowNode, FlowStart,
    ModifierFlags, NodeFlags, NodeId, ScriptTarget, SignatureDeclarationInterface, Symbol,
    SymbolTable, SyntaxKind, __String, append_if_unique, create_symbol_table,
    get_escaped_text_of_identifier_or_literal, get_name_of_declaration, is_property_name_literal,
    object_allocator, set_parent, set_value_declaration, BaseSymbol, InternalSymbolName,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, SymbolFlags, SymbolInterface,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum ModuleInstanceState {
    NonInstantiated = 0,
    Instantiated = 1,
    ConstEnumOnly = 2,
}

#[derive(Debug)]
pub(super) struct ActiveLabel {
    pub next: Option<Rc<ActiveLabel>>,
    pub name: __String,
    break_target: Rc<FlowNode /*FlowLabel*/>,
    continue_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    referenced: Cell<bool>,
}

impl ActiveLabel {
    pub fn break_target(&self) -> Rc<FlowNode> {
        self.break_target.clone()
    }

    pub fn maybe_continue_target(&self) -> Option<Rc<FlowNode>> {
        self.continue_target.borrow().clone()
    }

    pub fn set_continue_target(&self, continue_target: Option<Rc<FlowNode>>) {
        *self.continue_target.borrow_mut() = continue_target;
    }

    pub fn set_referenced(&self, referenced: bool) {
        self.referenced.set(referenced)
    }
}

pub fn get_module_instance_state(
    node: &Node, /*ModuleDeclaration*/
    visited: Option<Rc<RefCell<HashMap<NodeId, Option<ModuleInstanceState>>>>>,
) -> ModuleInstanceState {
    let node_as_module_declaration = node.as_module_declaration();
    if let Some(node_body) = node_as_module_declaration.body.as_ref() {
        if node_body.maybe_parent().is_none() {
            set_parent(node_body, Some(node.node_wrapper()));
            set_parent_recursive(Some(&**node_body), false);
        }
    }
    if let Some(node_body) = node_as_module_declaration.body.as_ref() {
        get_module_instance_state_cached(node_body, visited)
    } else {
        ModuleInstanceState::Instantiated
    }
}

pub fn get_module_instance_state_cached(
    node: &Node, /*ModuleDeclaration*/
    visited: Option<Rc<RefCell<HashMap<NodeId, Option<ModuleInstanceState>>>>>,
) -> ModuleInstanceState {
    let visited = visited.unwrap_or_else(|| Rc::new(RefCell::new(HashMap::new())));
    let node_id = get_node_id(node);
    {
        let visited = (*visited).borrow();
        if visited.contains_key(&node_id) {
            return visited
                .get(&node_id)
                .unwrap()
                .unwrap_or(ModuleInstanceState::NonInstantiated);
        }
    }
    visited.borrow_mut().insert(node_id, None);
    let result = get_module_instance_state_worker(node, visited.clone());
    visited.borrow_mut().insert(node_id, Some(result));
    result
}

pub(super) fn get_module_instance_state_worker(
    node: &Node, /*ModuleDeclaration*/
    visited: Rc<RefCell<HashMap<NodeId, Option<ModuleInstanceState>>>>,
) -> ModuleInstanceState {
    match node.kind() {
        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeAliasDeclaration => {
            return ModuleInstanceState::NonInstantiated;
        }
        SyntaxKind::EnumDeclaration => {
            if is_enum_const(node) {
                return ModuleInstanceState::ConstEnumOnly;
            }
        }
        SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration => {
            if !has_syntactic_modifier(node, ModifierFlags::Export) {
                return ModuleInstanceState::NonInstantiated;
            }
        }
        SyntaxKind::ExportDeclaration => {
            let export_declaration = node.as_export_declaration();
            if export_declaration.module_specifier.is_none()
                && matches!(export_declaration.export_clause.as_ref(), Some(export_clause) if export_clause.kind() == SyntaxKind::NamedExports)
            {
                let mut state = ModuleInstanceState::NonInstantiated;
                for specifier in &export_declaration
                    .export_clause
                    .as_ref()
                    .unwrap()
                    .as_named_exports()
                    .elements
                {
                    let specifier_state =
                        get_module_instance_state_for_alias_target(specifier, visited.clone());
                    if specifier_state > state {
                        state = specifier_state;
                    }
                    if state == ModuleInstanceState::Instantiated {
                        return state;
                    }
                }
                return state;
            }
        }
        SyntaxKind::ModuleBlock => {
            let mut state = ModuleInstanceState::NonInstantiated;
            for_each_child_returns(
                node,
                |n| {
                    let child_state = get_module_instance_state_cached(n, Some(visited.clone()));
                    match child_state {
                        ModuleInstanceState::NonInstantiated => {
                            return None;
                        }
                        ModuleInstanceState::ConstEnumOnly => {
                            state = ModuleInstanceState::ConstEnumOnly;
                            return None;
                        }
                        ModuleInstanceState::Instantiated => {
                            state = ModuleInstanceState::Instantiated;
                            return Some(());
                        } // _ => Debug_.assert_never(child_state)
                    }
                },
                Option::<fn(&NodeArray) -> Option<()>>::None,
            );
            return state;
        }
        SyntaxKind::ModuleDeclaration => {
            return get_module_instance_state(node, Some(visited));
        }
        SyntaxKind::Identifier => {
            if matches!(
                node.as_identifier().maybe_is_in_jsdoc_namespace(),
                Some(true)
            ) {
                return ModuleInstanceState::NonInstantiated;
            }
        }
        _ => (),
    }
    ModuleInstanceState::Instantiated
}

pub(super) fn get_module_instance_state_for_alias_target(
    specifier: &Node, /*ExportSpecifier*/
    visited: Rc<RefCell<HashMap<NodeId, Option<ModuleInstanceState>>>>,
) -> ModuleInstanceState {
    let specifier_as_export_specifier = specifier.as_export_specifier();
    let name: Rc<Node> = specifier_as_export_specifier
        .property_name
        .as_ref()
        .map(|property_name| property_name.clone())
        .unwrap_or_else(|| specifier_as_export_specifier.name.clone());
    let mut p: Option<Rc<Node>> = specifier.maybe_parent();
    while let Some(p_present) = p {
        if is_block(&p_present) || is_module_block(&p_present) || is_source_file(&p_present) {
            let statements = p_present.as_has_statements().statements();
            let mut found: Option<ModuleInstanceState> = None;
            for statement in statements {
                if node_has_name(statement, &name) {
                    if statement.maybe_parent().is_none() {
                        set_parent(statement, Some(p_present.clone()));
                        set_parent_recursive(Some(&**statement), false);
                    }
                    let state = get_module_instance_state_cached(statement, Some(visited.clone()));
                    if match found {
                        None => true,
                        Some(found) if state > found => true,
                        _ => false,
                    } {
                        found = Some(state);
                    }
                    if matches!(found, Some(ModuleInstanceState::Instantiated)) {
                        return found.unwrap();
                    }
                }
            }
            if let Some(found) = found {
                return found;
            }
        }
        p = p_present.maybe_parent();
    }
    ModuleInstanceState::Instantiated
}

bitflags! {
    pub(super) struct ContainerFlags: u32 {
        const None = 0;

        const IsContainer = 1 << 0;

        const IsBlockScopedContainer = 1 << 1;

        const IsControlFlowContainer = 1 << 2;

        const IsFunctionLike = 1 << 3;
        const IsFunctionExpression = 1 << 4;
        const HasLocals = 1 << 5;
        const IsInterface = 1 << 6;
        const IsObjectLiteralOrClassExpressionMethodOrAccessor = 1 << 7;
    }
}

pub(super) fn init_flow_node(node: FlowNode) -> FlowNode {
    // Debug.attachFlowNodeDebugInfo(node);
    node
}

// lazy_static! {
//     static ref binder: BinderType = create_binder();
// }

pub fn bind_source_file(file: &Node /*SourceFile*/, options: Rc<CompilerOptions>) {
    let file_as_source_file = file.as_source_file();
    Debug_.log(&format!("binding: {}", file_as_source_file.file_name()));
    // tracing?.push(tracing.Phase.Bind, "bindSourceFile", { path: file.path }, /*separateBeginAndEnd*/ true);
    // performance.mark("beforeBind");
    // perfLogger.logStartBindFile("" + file.fileName);
    // binder.call(file, options);
    create_binder().call(file, options);
    // perfLogger.logStopBindFile();
    // performance.mark("afterBind");
    // performance.measure("Bind", "beforeBind", "afterBind");
    // tracing?.pop(;
}

#[allow(non_snake_case)]
pub(super) struct BinderType {
    pub(super) file: RefCell<Option<Rc</*SourceFile*/ Node>>>,
    pub(super) options: RefCell<Option<Rc<CompilerOptions>>>,
    pub(super) language_version: Cell<Option<ScriptTarget>>,
    pub(super) parent: RefCell<Option<Rc<Node>>>,
    pub(super) container: RefCell<Option<Rc<Node>>>,
    pub(super) this_parent_container: RefCell<Option<Rc<Node>>>,
    pub(super) block_scope_container: RefCell<Option<Rc<Node>>>,
    pub(super) last_container: RefCell<Option<Rc<Node>>>,
    pub(super) delayed_type_aliases:
        RefCell<Option<Vec<Rc<Node /*JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag*/>>>>,
    pub(super) seen_this_keyword: Cell<Option<bool>>,

    pub(super) current_flow: RefCell<Option<Rc<FlowNode>>>,
    pub(super) current_break_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pub(super) current_continue_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pub(super) current_return_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pub(super) current_true_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pub(super) current_false_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pub(super) current_exception_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pub(super) pre_switch_case_flow: RefCell<Option<Rc<FlowNode>>>,
    pub(super) active_label_list: RefCell<Option<Rc<ActiveLabel>>>,
    pub(super) has_explicit_return: Cell<Option<bool>>,

    pub(super) emit_flags: Cell<Option<NodeFlags>>,

    pub(super) in_strict_mode: Cell<Option<bool>>,

    pub(super) in_assignment_pattern: Cell<bool>,

    pub(super) symbol_count: Cell<usize>,

    pub(super) Symbol: RefCell<Option<fn(SymbolFlags, __String) -> BaseSymbol>>,
    pub(super) classifiable_names: RefCell<Option<Rc<RefCell<HashSet<__String>>>>>,

    pub(super) unreachable_flow: RefCell<Rc<FlowNode>>,
    pub(super) reported_unreachable_flow: RefCell<Rc<FlowNode>>,
    // bind_binary_expression_flow: RefCell<...>,
}

pub(super) fn create_binder() -> BinderType {
    BinderType {
        file: RefCell::new(None),
        options: RefCell::new(None),
        language_version: Cell::new(None),
        parent: RefCell::new(None),
        container: RefCell::new(None),
        this_parent_container: RefCell::new(None),
        block_scope_container: RefCell::new(None),
        last_container: RefCell::new(None),
        delayed_type_aliases: RefCell::new(None),
        seen_this_keyword: Cell::new(None),
        current_flow: RefCell::new(None),
        current_break_target: RefCell::new(None),
        current_continue_target: RefCell::new(None),
        current_return_target: RefCell::new(None),
        current_true_target: RefCell::new(None),
        current_false_target: RefCell::new(None),
        current_exception_target: RefCell::new(None),
        pre_switch_case_flow: RefCell::new(None),
        active_label_list: RefCell::new(None),
        has_explicit_return: Cell::new(None),
        // emit_flags: Cell::new(None),
        emit_flags: Cell::new(Some(NodeFlags::None)),
        in_strict_mode: Cell::new(None),
        in_assignment_pattern: Cell::new(false),
        symbol_count: Cell::new(0),
        Symbol: RefCell::new(None),
        classifiable_names: RefCell::new(None),
        unreachable_flow: RefCell::new(Rc::new(
            FlowStart::new(FlowFlags::Unreachable, None, None).into(),
        )),
        reported_unreachable_flow: RefCell::new(Rc::new(
            FlowStart::new(FlowFlags::Unreachable, None, None).into(),
        )),
    }
}

impl BinderType {
    pub(super) fn call(&self, f: &Node, opts: Rc<CompilerOptions>) {
        self.bind_source_file(f, opts);
    }

    pub(super) fn file(&self) -> Rc<Node> {
        self.file.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn set_file(&self, file: Option<Rc<Node>>) {
        *self.file.borrow_mut() = file;
    }

    pub(super) fn options(&self) -> Rc<CompilerOptions> {
        self.options.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn set_options(&self, options: Option<Rc<CompilerOptions>>) {
        *self.options.borrow_mut() = options;
    }

    pub(super) fn language_version(&self) -> ScriptTarget {
        self.language_version.get().unwrap()
    }

    pub(super) fn set_language_version(&self, language_version: Option<ScriptTarget>) {
        self.language_version.set(language_version);
    }

    pub(super) fn maybe_parent(&self) -> Option<Rc<Node>> {
        self.parent.borrow().as_ref().map(Clone::clone)
    }

    pub(super) fn parent(&self) -> Rc<Node> {
        self.parent.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn set_parent(&self, parent: Option<Rc<Node>>) {
        *self.parent.borrow_mut() = parent;
    }

    pub(super) fn container(&self) -> Rc<Node> {
        self.container.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn maybe_container(&self) -> Option<Rc<Node>> {
        self.container.borrow().as_ref().map(Clone::clone)
    }

    pub(super) fn set_container(&self, container: Option<Rc<Node>>) {
        *self.container.borrow_mut() = container;
    }

    pub(super) fn this_parent_container(&self) -> Rc<Node> {
        self.this_parent_container
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    pub(super) fn maybe_this_parent_container(&self) -> Option<Rc<Node>> {
        self.this_parent_container
            .borrow()
            .as_ref()
            .map(Clone::clone)
    }

    pub(super) fn set_this_parent_container(&self, this_parent_container: Option<Rc<Node>>) {
        *self.this_parent_container.borrow_mut() = this_parent_container;
    }

    pub(super) fn maybe_block_scope_container(&self) -> Option<Rc<Node>> {
        self.block_scope_container
            .borrow()
            .as_ref()
            .map(Clone::clone)
    }

    pub(super) fn block_scope_container(&self) -> Rc<Node> {
        self.block_scope_container
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    pub(super) fn set_block_scope_container(&self, block_scope_container: Option<Rc<Node>>) {
        *self.block_scope_container.borrow_mut() = block_scope_container;
    }

    pub(super) fn maybe_last_container(&self) -> Option<Rc<Node>> {
        self.last_container.borrow().as_ref().map(Clone::clone)
    }

    pub(super) fn last_container(&self) -> Rc<Node> {
        self.last_container.borrow().as_ref().unwrap().clone()
    }

    pub(super) fn set_last_container(&self, last_container: Option<Rc<Node>>) {
        *self.last_container.borrow_mut() = last_container;
    }

    pub(super) fn maybe_delayed_type_aliases(&self) -> RefMut<Option<Vec<Rc<Node>>>> {
        self.delayed_type_aliases.borrow_mut()
    }

    pub(super) fn delayed_type_aliases(&self) -> RefMut<Vec<Rc<Node>>> {
        RefMut::map(self.delayed_type_aliases.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    pub(super) fn set_delayed_type_aliases(&self, delayed_type_aliases: Option<Vec<Rc<Node>>>) {
        *self.delayed_type_aliases.borrow_mut() = delayed_type_aliases;
    }

    pub(super) fn maybe_seen_this_keyword(&self) -> Option<bool> {
        self.seen_this_keyword.get()
    }

    pub(super) fn set_seen_this_keyword(&self, seen_this_keyword: Option<bool>) {
        self.seen_this_keyword.set(seen_this_keyword);
    }

    pub(super) fn current_flow(&self) -> Rc<FlowNode> {
        self.current_flow.borrow().clone().unwrap()
    }

    pub(super) fn maybe_current_flow(&self) -> Option<Rc<FlowNode>> {
        self.current_flow.borrow().clone()
    }

    pub(super) fn set_current_flow(&self, current_flow: Option<Rc<FlowNode>>) {
        *self.current_flow.borrow_mut() = current_flow;
    }

    pub(super) fn maybe_current_break_target(&self) -> Option<Rc<FlowNode>> {
        self.current_break_target.borrow().clone()
    }

    pub(super) fn set_current_break_target(&self, current_break_target: Option<Rc<FlowNode>>) {
        *self.current_break_target.borrow_mut() = current_break_target;
    }

    pub(super) fn maybe_current_continue_target(&self) -> Option<Rc<FlowNode>> {
        self.current_continue_target.borrow().clone()
    }

    pub(super) fn set_current_continue_target(
        &self,
        current_continue_target: Option<Rc<FlowNode>>,
    ) {
        *self.current_continue_target.borrow_mut() = current_continue_target;
    }

    pub(super) fn maybe_current_return_target(&self) -> Option<Rc<FlowNode>> {
        self.current_return_target.borrow().clone()
    }

    pub(super) fn set_current_return_target(&self, current_return_target: Option<Rc<FlowNode>>) {
        *self.current_return_target.borrow_mut() = current_return_target;
    }

    pub(super) fn maybe_current_true_target(&self) -> Option<Rc<FlowNode>> {
        self.current_true_target.borrow().clone()
    }

    pub(super) fn set_current_true_target(&self, current_true_target: Option<Rc<FlowNode>>) {
        *self.current_true_target.borrow_mut() = current_true_target;
    }

    pub(super) fn maybe_current_false_target(&self) -> Option<Rc<FlowNode>> {
        self.current_false_target.borrow().clone()
    }

    pub(super) fn set_current_false_target(&self, current_false_target: Option<Rc<FlowNode>>) {
        *self.current_false_target.borrow_mut() = current_false_target;
    }

    pub(super) fn maybe_current_exception_target(&self) -> Option<Rc<FlowNode>> {
        self.current_exception_target.borrow().clone()
    }

    pub(super) fn set_current_exception_target(
        &self,
        current_exception_target: Option<Rc<FlowNode>>,
    ) {
        *self.current_exception_target.borrow_mut() = current_exception_target;
    }

    pub(super) fn maybe_active_label_list(&self) -> Option<Rc<ActiveLabel>> {
        self.active_label_list.borrow().clone()
    }

    pub(super) fn set_active_label_list(&self, active_label_list: Option<Rc<ActiveLabel>>) {
        *self.active_label_list.borrow_mut() = active_label_list;
    }

    pub(super) fn maybe_has_explicit_return(&self) -> Option<bool> {
        self.has_explicit_return.get()
    }

    pub(super) fn set_has_explicit_return(&self, has_explicit_return: Option<bool>) {
        self.has_explicit_return.set(has_explicit_return);
    }

    pub(super) fn maybe_emit_flags(&self) -> Option<NodeFlags> {
        self.emit_flags.get()
    }

    pub(super) fn emit_flags(&self) -> NodeFlags {
        self.emit_flags.get().unwrap()
    }

    pub(super) fn set_emit_flags(&self, emit_flags: Option<NodeFlags>) {
        self.emit_flags.set(emit_flags);
    }

    pub(super) fn set_in_strict_mode(&self, in_strict_mode: Option<bool>) {
        self.in_strict_mode.set(in_strict_mode);
    }

    pub(super) fn in_assignment_pattern(&self) -> bool {
        self.in_assignment_pattern.get()
    }

    pub(super) fn set_in_assignment_pattern(&self, in_assignment_pattern: bool) {
        self.in_assignment_pattern.set(in_assignment_pattern);
    }

    pub(super) fn symbol_count(&self) -> usize {
        self.symbol_count.get()
    }

    pub(super) fn increment_symbol_count(&self) {
        self.symbol_count.set(self.symbol_count() + 1);
    }

    pub(super) fn set_symbol_count(&self, symbol_count: usize) {
        self.symbol_count.set(symbol_count);
    }

    #[allow(non_snake_case)]
    pub(super) fn Symbol(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        self.Symbol.borrow().unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_Symbol(&self, Symbol: fn(SymbolFlags, __String) -> BaseSymbol) {
        *self.Symbol.borrow_mut() = Some(Symbol);
    }

    pub(super) fn classifiable_names(&self) -> Rc<RefCell<HashSet<__String>>> {
        self.classifiable_names
            .borrow()
            .as_ref()
            .map(|rc| rc.clone())
            .unwrap()
    }

    pub(super) fn set_classifiable_names(
        &self,
        classifiable_names: Option<Rc<RefCell<HashSet<__String>>>>,
    ) {
        *self.classifiable_names.borrow_mut() = classifiable_names;
    }

    pub(super) fn unreachable_flow(&self) -> Rc<FlowNode> {
        self.unreachable_flow.borrow().clone()
    }

    pub(super) fn bind_source_file(&self, f: &Node /*SourceFile*/, opts: Rc<CompilerOptions>) {
        self.set_file(Some(f.node_wrapper()));
        self.set_options(Some(opts.clone()));
        self.set_language_version(Some(get_emit_script_target(&opts)));
        self.set_in_strict_mode(Some(self.bind_in_strict_mode(f, &opts)));
        self.set_classifiable_names(Some(Rc::new(RefCell::new(HashSet::new()))));
        self.set_symbol_count(0);

        self.set_Symbol(object_allocator.get_symbol_constructor());

        // Debug.attachFlowNodeDebugInfo(unreachableFlow);
        // Debug.attachFlowNodeDebugInfo(reportedUnreachableFlow);

        let file = self.file();
        let file_as_source_file = file.as_source_file();
        if file_as_source_file.maybe_locals().is_none() {
            self.bind(Some(&*self.file()));
            file_as_source_file.set_symbol_count(self.symbol_count());
            file_as_source_file.set_classifiable_names(Some(self.classifiable_names()));
            self.delayed_bind_jsdoc_typedef_tag();
        }

        self.set_file(None);
        self.set_options(None);
        self.set_language_version(None);
        self.set_parent(None);
        self.set_container(None);
        self.set_this_parent_container(None);
        self.set_block_scope_container(None);
        self.set_last_container(None);
        self.set_delayed_type_aliases(None);
        self.set_seen_this_keyword(Some(false));
        self.set_current_flow(None);
        self.set_current_break_target(None);
        self.set_current_continue_target(None);
        self.set_current_return_target(None);
        self.set_current_true_target(None);
        self.set_current_false_target(None);
        self.set_current_exception_target(None);
        self.set_active_label_list(None);
        self.set_has_explicit_return(Some(false));
        self.set_in_assignment_pattern(false);
        self.set_emit_flags(Some(NodeFlags::None));
    }

    pub(super) fn bind_in_strict_mode(
        &self,
        file: &Node, /*SourceFile*/
        opts: &CompilerOptions,
    ) -> bool {
        let file_as_source_file = file.as_source_file();
        if get_strict_option_value(opts, "always_strict")
            && !file_as_source_file.is_declaration_file()
        {
            true
        } else {
            file_as_source_file
                .maybe_external_module_indicator()
                .is_some()
        }
    }

    pub(super) fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        self.increment_symbol_count();
        self.Symbol()(flags, name).into()
    }

    pub(super) fn add_declaration_to_symbol(
        &self,
        symbol: &Symbol,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
    ) {
        symbol.set_flags(symbol.flags() | symbol_flags);

        node.set_symbol(symbol.symbol_wrapper());
        let declarations = append_if_unique(
            symbol.maybe_declarations().as_ref().map(Clone::clone),
            node.node_wrapper(),
        );
        symbol.set_declarations(declarations);

        if symbol_flags.intersects(
            SymbolFlags::Class | SymbolFlags::Enum | SymbolFlags::Module | SymbolFlags::Variable,
        ) {
            let mut exports = symbol.maybe_exports();
            if exports.is_none() {
                *exports = Some(Rc::new(RefCell::new(create_symbol_table(None))));
            }
        }

        if symbol_flags.intersects(
            SymbolFlags::Class
                | SymbolFlags::Interface
                | SymbolFlags::TypeLiteral
                | SymbolFlags::ObjectLiteral,
        ) {
            let mut members = symbol.maybe_members();
            if members.is_none() {
                *members = Some(Rc::new(RefCell::new(create_symbol_table(None))));
            }
        }

        if matches!(symbol.maybe_const_enum_only_module(), Some(true))
            && symbol_flags
                .intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::RegularEnum)
        {
            symbol.set_const_enum_only_module(Some(false));
        }

        if symbol_flags.intersects(SymbolFlags::Value) {
            set_value_declaration(symbol, node);
        }
    }

    pub(super) fn get_declaration_name(&self, node: &Node) -> Option<__String> {
        if node.kind() == SyntaxKind::ExportAssignment {
            return Some(
                if matches!(node.as_export_assignment().is_export_equals, Some(true)) {
                    InternalSymbolName::ExportEquals()
                } else {
                    InternalSymbolName::Default()
                },
            );
        }

        let name = get_name_of_declaration(Some(node));
        if let Some(name) = name {
            if is_ambient_module(node) {
                let module_name = get_text_of_identifier_or_literal(&name);
                return Some(__String::new(if is_global_scope_augmentation(node) {
                    "__global".to_owned()
                } else {
                    format!("\"{}\"", module_name)
                }));
            }
            if name.kind() == SyntaxKind::ComputedPropertyName {
                let name_expression = &name.as_computed_property_name().expression;
                if is_string_or_numeric_literal_like(name_expression) {
                    return Some(escape_leading_underscores(
                        &name_expression.as_literal_like_node().text(),
                    ));
                }
                if is_signed_numeric_literal(name_expression) {
                    let name_expression_as_prefix_unary_expression =
                        name_expression.as_prefix_unary_expression();
                    return Some(__String::new(format!(
                        "{}{}",
                        token_to_string(name_expression_as_prefix_unary_expression.operator)
                            .unwrap(),
                        name_expression_as_prefix_unary_expression
                            .operand
                            .as_literal_like_node()
                            .text()
                    )));
                } else {
                    Debug_.fail(Some(
                        "Only computed properties with literal names have declaration names",
                    ));
                }
            }
            if is_private_identifier(&name) {
                let containing_class = get_containing_class(node)?;
                let containing_class_symbol = containing_class.symbol();
                return Some(get_symbol_name_for_private_identifier(
                    &containing_class_symbol,
                    &name.as_private_identifier().escaped_text,
                ));
            }
            return if is_property_name_literal(&name) {
                Some(get_escaped_text_of_identifier_or_literal(&name))
            } else {
                None
            };
        }
        match node.kind() {
            SyntaxKind::Constructor => Some(InternalSymbolName::Constructor()),
            SyntaxKind::FunctionType | SyntaxKind::CallSignature | SyntaxKind::JSDocSignature => {
                Some(InternalSymbolName::Constructor())
            }
            SyntaxKind::ConstructorType | SyntaxKind::ConstructSignature => {
                Some(InternalSymbolName::New())
            }
            SyntaxKind::IndexSignature => Some(InternalSymbolName::Index()),
            SyntaxKind::ExportDeclaration => Some(InternalSymbolName::ExportStar()),
            SyntaxKind::SourceFile => Some(InternalSymbolName::ExportEquals()),
            SyntaxKind::BinaryExpression => {
                if get_assignment_declaration_kind(node) == AssignmentDeclarationKind::ModuleExports
                {
                    return Some(InternalSymbolName::ExportEquals());
                }
                Debug_.fail(Some("Unknown binary declaration kind"));
            }
            SyntaxKind::JSDocFunctionType => Some(if is_jsdoc_construct_signature(node) {
                InternalSymbolName::New()
            } else {
                InternalSymbolName::Call()
            }),
            SyntaxKind::Parameter => {
                Debug_.assert(node.parent().kind() == SyntaxKind::JSDocFunctionType, Some("Impossible parameter parent kind")/*, () => `parent is: ${(ts as any).SyntaxKind ? (ts as any).SyntaxKind[node.parent.kind] : node.parent.kind}, expected JSDocFunction Type`);*/);
                let function_type = node.parent();
                let index = index_of(
                    &function_type.as_jsdoc_function_type().parameters(),
                    &node.node_wrapper(),
                    |a, b| Rc::ptr_eq(a, b),
                );
                return Some(__String::new(format!("arg{}", index)));
            }
            _ => None,
        }
    }

    pub(super) fn get_display_name(&self, node: &Node /*Declaration*/) -> Cow<'static, str> {
        if is_named_declaration(node) {
            declaration_name_to_string(node.as_named_declaration().maybe_name())
        } else {
            unescape_leading_underscores(
                Debug_.check_defined(self.get_declaration_name(node).as_ref(), None),
            )
            .into()
        }
    }

    pub(super) fn declare_symbol<TParent: Borrow<Symbol>>(
        &self,
        symbol_table: &mut SymbolTable,
        parent: Option<TParent>,
        node: &Node, /*Declaration*/
        includes: SymbolFlags,
        excludes: SymbolFlags,
        is_replaceable_by_method: Option<bool>,
        is_computed_name: Option<bool>,
    ) -> Rc<Symbol> {
        let is_replaceable_by_method = is_replaceable_by_method.unwrap_or(false);
        let is_computed_name = is_computed_name.unwrap_or(false);
        Debug_.assert(is_computed_name || !has_dynamic_name(node), None);

        let is_default_export = has_syntactic_modifier(node, ModifierFlags::Default)
            || is_export_specifier(node)
                && node
                    .as_export_specifier()
                    .name
                    .as_identifier()
                    .escaped_text
                    .eq_str("default");

        let name: Option<__String> = if is_computed_name {
            Some(InternalSymbolName::Computed())
        } else if is_default_export && parent.is_some() {
            Some(InternalSymbolName::Default())
        } else {
            self.get_declaration_name(node)
        };

        let mut symbol: Option<Rc<Symbol>> = None;
        match name {
            None => {
                symbol = Some(
                    self.create_symbol(SymbolFlags::None, InternalSymbolName::Missing())
                        .wrap(),
                );
            }
            Some(name) => {
                symbol = symbol_table.get(&name).map(Clone::clone);

                if includes.intersects(SymbolFlags::Classifiable) {
                    self.classifiable_names().borrow_mut().insert(name.clone());
                }

                match symbol.as_ref() {
                    None => {
                        symbol = Some(self.create_symbol(SymbolFlags::None, name.clone()).wrap());
                        symbol_table.insert(name, symbol.as_ref().unwrap().clone());
                        if is_replaceable_by_method {
                            symbol
                                .as_ref()
                                .unwrap()
                                .set_is_replaceable_by_method(Some(true));
                        }
                    }
                    Some(symbol)
                        if is_replaceable_by_method
                            && !matches!(symbol.maybe_is_replaceable_by_method(), Some(true)) =>
                    {
                        return symbol.clone();
                    }
                    Some(symbol_present) if symbol_present.flags().intersects(excludes) => {
                        if matches!(symbol_present.maybe_is_replaceable_by_method(), Some(true)) {
                            symbol =
                                Some(self.create_symbol(SymbolFlags::None, name.clone()).wrap());
                            symbol_table.insert(name, symbol.as_ref().unwrap().clone());
                        } else if !(includes.intersects(SymbolFlags::Variable)
                            && symbol_present.flags().intersects(SymbolFlags::Assignment))
                        {
                            if is_named_declaration(node) {
                                maybe_set_parent(
                                    node.as_named_declaration().maybe_name(),
                                    Some(node.node_wrapper()),
                                );
                            }
                            let mut message = if symbol_present
                                .flags()
                                .intersects(SymbolFlags::BlockScopedVariable)
                            {
                                &Diagnostics::Cannot_redeclare_block_scoped_variable_0
                            } else {
                                &Diagnostics::Duplicate_identifier_0
                            };
                            let mut message_needs_name = true;

                            if symbol_present.flags().intersects(SymbolFlags::Enum)
                                || includes.intersects(SymbolFlags::Enum)
                            {
                                message = &Diagnostics::Enum_declarations_can_only_merge_with_namespace_or_other_enum_declarations;
                                message_needs_name = false;
                            }

                            let mut multiple_default_exports = false;
                            if length(symbol_present.maybe_declarations().as_deref()) > 0 {
                                if is_default_export {
                                    message =
                                        &Diagnostics::A_module_cannot_have_multiple_default_exports;
                                    message_needs_name = false;
                                    multiple_default_exports = true;
                                } else {
                                    if matches!(symbol_present.maybe_declarations().as_ref(), Some(declarations) if !declarations.is_empty())
                                        && (node.kind() == SyntaxKind::ExportAssignment
                                            && !matches!(
                                                node.as_export_assignment().is_export_equals,
                                                Some(true)
                                            ))
                                    {
                                        message = &Diagnostics::A_module_cannot_have_multiple_default_exports;
                                        message_needs_name = false;
                                        multiple_default_exports = true;
                                    }
                                }
                            }

                            let mut related_information: Vec<Rc<DiagnosticRelatedInformation>> =
                                vec![];
                            if is_type_alias_declaration(node)
                                && node_is_missing(Some(&*node.as_type_alias_declaration().type_))
                                && has_syntactic_modifier(node, ModifierFlags::Export)
                                && symbol_present.flags().intersects(
                                    SymbolFlags::Alias | SymbolFlags::Type | SymbolFlags::Namespace,
                                )
                            {
                                related_information.push(Rc::new(
                                    create_diagnostic_for_node(
                                        node,
                                        &Diagnostics::Did_you_mean_0,
                                        Some(vec![format!(
                                            "export type {{ {} }}",
                                            unescape_leading_underscores(
                                                &node
                                                    .as_type_alias_declaration()
                                                    .name()
                                                    .as_identifier()
                                                    .escaped_text
                                            )
                                        )]),
                                    )
                                    .into(),
                                ));
                            }

                            let declaration_name: Rc<Node> = get_name_of_declaration(Some(node))
                                .unwrap_or_else(|| node.node_wrapper());
                            maybe_for_each(
                                symbol_present.maybe_declarations().as_ref(),
                                |declaration: &Rc<Node>, index| {
                                    let decl = get_name_of_declaration(Some(&**declaration))
                                        .unwrap_or_else(|| declaration.node_wrapper());
                                    let diag: Rc<Diagnostic> = Rc::new(
                                        create_diagnostic_for_node(
                                            &decl,
                                            message,
                                            if message_needs_name {
                                                Some(vec![self
                                                    .get_display_name(declaration)
                                                    .into_owned()])
                                            } else {
                                                None
                                            },
                                        )
                                        .into(),
                                    );
                                    self.file().as_source_file().bind_diagnostics().push(
                                        if multiple_default_exports {
                                            add_related_info(
                                                &diag,
                                                vec![Rc::new(
                                                create_diagnostic_for_node(
                                                    &declaration_name,
                                                    if index == 0 {
                                                        &Diagnostics::Another_export_default_is_here
                                                    } else {
                                                        &Diagnostics::and_here
                                                    },
                                                    None,
                                                )
                                                .into(),
                                            )],
                                            );
                                            diag
                                        } else {
                                            diag
                                        },
                                    );
                                    if multiple_default_exports {
                                        related_information.push(Rc::new(
                                            create_diagnostic_for_node(
                                                &decl,
                                                &Diagnostics::The_first_export_default_is_here,
                                                None,
                                            )
                                            .into(),
                                        ));
                                    }
                                    Option::<()>::None
                                },
                            );

                            let diag: Rc<Diagnostic> = Rc::new(
                                create_diagnostic_for_node(
                                    &declaration_name,
                                    message,
                                    if message_needs_name {
                                        Some(vec![self.get_display_name(node).into_owned()])
                                    } else {
                                        None
                                    },
                                )
                                .into(),
                            );
                            add_related_info(&diag, related_information);
                            self.file().as_source_file().bind_diagnostics().push(diag);

                            symbol = Some(self.create_symbol(SymbolFlags::None, name).wrap());
                        }
                    }
                    _ => (),
                }
            }
        }
        let symbol = symbol.unwrap();

        self.add_declaration_to_symbol(&symbol, node, includes);
        if let Some(symbol_parent) = symbol.maybe_parent() {
            Debug_.assert(matches!(parent, Some(parent) if Rc::ptr_eq(&symbol_parent, &parent.borrow().symbol_wrapper())), Some("Existing symbol parent should match new one"));
        } else {
            symbol.set_parent(parent.map(|parent| parent.borrow().symbol_wrapper()));
        }

        symbol
    }
}
