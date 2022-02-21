#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::{Borrow, Cow};
use std::cell::{Cell, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::{
    declaration_name_to_string, escape_leading_underscores, for_each_child_returns,
    get_assignment_declaration_kind, get_containing_class, get_emit_script_target, get_node_id,
    get_strict_option_value, get_symbol_name_for_private_identifier,
    get_text_of_identifier_or_literal, has_syntactic_modifier, index_of, is_ambient_module,
    is_block, is_enum_const, is_global_scope_augmentation, is_jsdoc_construct_signature,
    is_module_block, is_named_declaration, is_private_identifier, is_signed_numeric_literal,
    is_source_file, is_string_or_numeric_literal_like, node_has_name, set_parent_recursive,
    token_to_string, unescape_leading_underscores, AssignmentDeclarationKind, CompilerOptions,
    Debug_, FlowFlags, FlowNode, FlowStart, ModifierFlags, NodeFlags, NodeId, ScriptTarget,
    SignatureDeclarationInterface, Symbol, SymbolTable, SyntaxKind, __String, append_if_unique,
    create_symbol_table, for_each, for_each_child, get_escaped_text_of_identifier_or_literal,
    get_name_of_declaration, is_binding_pattern, is_block_or_catch_scoped,
    is_class_static_block_declaration, is_function_like, is_property_name_literal,
    object_allocator, set_parent, set_value_declaration, BaseSymbol, ExpressionStatement,
    IfStatement, InternalSymbolName, NamedDeclarationInterface, Node, NodeArray, NodeInterface,
    SymbolFlags, SymbolInterface,
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
    pub break_target: Rc<FlowNode /*FlowLabel*/>,
    pub continue_target: Option<Rc<FlowNode /*FlowLabel*/>>,
    pub referenced: bool,
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

fn get_module_instance_state_worker(
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

fn get_module_instance_state_for_alias_target(
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
    struct ContainerFlags: u32 {
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

fn init_flow_node(node: FlowNode) -> FlowNode {
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
struct BinderType {
    file: RefCell<Option<Rc</*SourceFile*/ Node>>>,
    options: RefCell<Option<Rc<CompilerOptions>>>,
    language_version: Cell<Option<ScriptTarget>>,
    parent: RefCell<Option<Rc<Node>>>,
    container: RefCell<Option<Rc<Node>>>,
    this_parent_container: RefCell<Option<Rc<Node>>>,
    block_scope_container: RefCell<Option<Rc<Node>>>,
    last_container: RefCell<Option<Rc<Node>>>,
    delayed_type_aliases:
        RefCell<Option<Vec<Rc<Node /*JSDocTypedefTag | JSDocCallbackTag | JSDocEnumTag*/>>>>,
    seen_this_keyword: Cell<Option<bool>>,

    current_flow: RefCell<Option<Rc<FlowNode>>>,
    current_break_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    current_continue_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    current_return_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    current_true_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    current_false_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    current_exception_target: RefCell<Option<Rc<FlowNode /*FlowLabel*/>>>,
    pre_switch_case_flow: RefCell<Option<Rc<FlowNode>>>,
    active_label_list: RefCell<Option<Rc<ActiveLabel>>>,
    has_explicit_return: Cell<Option<bool>>,

    emit_flags: Cell<Option<NodeFlags>>,

    in_strict_mode: Cell<Option<bool>>,

    in_assignment_pattern: Cell<bool>,

    symbol_count: Cell<usize>,

    Symbol: RefCell<Option<fn(SymbolFlags, __String) -> BaseSymbol>>,
    classifiable_names: RefCell<Option<Rc<HashSet<__String>>>>,

    unreachable_flow: RefCell<Rc<FlowNode>>,
    reported_unreachable_flow: RefCell<Rc<FlowNode>>,
    // bind_binary_expression_flow: RefCell<...>,
}

fn create_binder() -> BinderType {
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
        emit_flags: Cell::new(None),
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
    fn call(&self, f: &Node, opts: Rc<CompilerOptions>) {
        self.bind_source_file(f, opts);
    }

    fn file(&self) -> Rc<Node> {
        self.file.borrow().as_ref().unwrap().clone()
    }

    fn set_file(&self, file: Option<Rc<Node>>) {
        *self.file.borrow_mut() = file;
    }

    fn options(&self) -> Rc<CompilerOptions> {
        self.options.borrow().as_ref().unwrap().clone()
    }

    fn set_options(&self, options: Option<Rc<CompilerOptions>>) {
        *self.options.borrow_mut() = options;
    }

    fn language_version(&self) -> ScriptTarget {
        self.language_version.get().unwrap()
    }

    fn set_language_version(&self, language_version: Option<ScriptTarget>) {
        self.language_version.set(language_version);
    }

    fn maybe_parent(&self) -> Option<Rc<Node>> {
        self.parent.borrow().as_ref().map(Clone::clone)
    }

    fn parent(&self) -> Rc<Node> {
        self.parent.borrow().as_ref().unwrap().clone()
    }

    fn set_parent(&self, parent: Option<Rc<Node>>) {
        *self.parent.borrow_mut() = parent;
    }

    fn container(&self) -> Rc<Node> {
        self.container.borrow().as_ref().unwrap().clone()
    }

    fn maybe_container(&self) -> Option<Rc<Node>> {
        self.container.borrow().as_ref().map(Clone::clone)
    }

    fn set_container(&self, container: Option<Rc<Node>>) {
        *self.container.borrow_mut() = container;
    }

    fn this_parent_container(&self) -> Rc<Node> {
        self.this_parent_container
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    fn maybe_this_parent_container(&self) -> Option<Rc<Node>> {
        self.this_parent_container
            .borrow()
            .as_ref()
            .map(Clone::clone)
    }

    fn set_this_parent_container(&self, this_parent_container: Option<Rc<Node>>) {
        *self.this_parent_container.borrow_mut() = this_parent_container;
    }

    fn maybe_block_scope_container(&self) -> Option<Rc<Node>> {
        self.block_scope_container
            .borrow()
            .as_ref()
            .map(Clone::clone)
    }

    fn block_scope_container(&self) -> Rc<Node> {
        self.block_scope_container
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    fn set_block_scope_container(&self, block_scope_container: Option<Rc<Node>>) {
        *self.block_scope_container.borrow_mut() = block_scope_container;
    }

    fn maybe_last_container(&self) -> Option<Rc<Node>> {
        self.last_container.borrow().as_ref().map(Clone::clone)
    }

    fn last_container(&self) -> Rc<Node> {
        self.last_container.borrow().as_ref().unwrap().clone()
    }

    fn set_last_container(&self, last_container: Option<Rc<Node>>) {
        *self.last_container.borrow_mut() = last_container;
    }

    fn maybe_delayed_type_aliases(&self) -> RefMut<Option<Vec<Rc<Node>>>> {
        self.delayed_type_aliases.borrow_mut()
    }

    fn delayed_type_aliases(&self) -> RefMut<Vec<Rc<Node>>> {
        RefMut::map(self.delayed_type_aliases.borrow_mut(), |option| {
            option.as_mut().unwrap()
        })
    }

    fn set_delayed_type_aliases(&self, delayed_type_aliases: Option<Vec<Rc<Node>>>) {
        *self.delayed_type_aliases.borrow_mut() = delayed_type_aliases;
    }

    fn set_seen_this_keyword(&self, seen_this_keyword: Option<bool>) {
        self.seen_this_keyword.set(seen_this_keyword);
    }

    fn set_current_flow(&self, current_flow: Option<Rc<FlowNode>>) {
        *self.current_flow.borrow_mut() = current_flow;
    }

    fn set_current_break_target(&self, current_break_target: Option<Rc<FlowNode>>) {
        *self.current_break_target.borrow_mut() = current_break_target;
    }

    fn set_current_continue_target(&self, current_continue_target: Option<Rc<FlowNode>>) {
        *self.current_continue_target.borrow_mut() = current_continue_target;
    }

    fn set_current_return_target(&self, current_return_target: Option<Rc<FlowNode>>) {
        *self.current_return_target.borrow_mut() = current_return_target;
    }

    fn set_current_true_target(&self, current_true_target: Option<Rc<FlowNode>>) {
        *self.current_true_target.borrow_mut() = current_true_target;
    }

    fn set_current_false_target(&self, current_false_target: Option<Rc<FlowNode>>) {
        *self.current_false_target.borrow_mut() = current_false_target;
    }

    fn set_current_exception_target(&self, current_exception_target: Option<Rc<FlowNode>>) {
        *self.current_exception_target.borrow_mut() = current_exception_target;
    }

    fn set_active_label_list(&self, active_label_list: Option<Rc<ActiveLabel>>) {
        *self.active_label_list.borrow_mut() = active_label_list;
    }

    fn set_has_explicit_return(&self, has_explicit_return: Option<bool>) {
        self.has_explicit_return.set(has_explicit_return);
    }

    fn set_emit_flags(&self, emit_flags: Option<NodeFlags>) {
        self.emit_flags.set(emit_flags);
    }

    fn set_in_strict_mode(&self, in_strict_mode: Option<bool>) {
        self.in_strict_mode.set(in_strict_mode);
    }

    fn in_assignment_pattern(&self) -> bool {
        self.in_assignment_pattern.get()
    }

    fn set_in_assignment_pattern(&self, in_assignment_pattern: bool) {
        self.in_assignment_pattern.set(in_assignment_pattern);
    }

    fn symbol_count(&self) -> usize {
        self.symbol_count.get()
    }

    fn increment_symbol_count(&self) {
        self.symbol_count.set(self.symbol_count() + 1);
    }

    fn set_symbol_count(&self, symbol_count: usize) {
        self.symbol_count.set(symbol_count);
    }

    #[allow(non_snake_case)]
    fn Symbol(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        self.Symbol.borrow().unwrap()
    }

    #[allow(non_snake_case)]
    fn set_Symbol(&self, Symbol: fn(SymbolFlags, __String) -> BaseSymbol) {
        *self.Symbol.borrow_mut() = Some(Symbol);
    }

    fn classifiable_names(&self) -> Rc<HashSet<__String>> {
        self.classifiable_names.borrow().as_ref().unwrap().clone()
    }

    fn set_classifiable_names(&self, classifiable_names: Option<Rc<HashSet<__String>>>) {
        *self.classifiable_names.borrow_mut() = classifiable_names;
    }

    fn bind_source_file(&self, f: &Node /*SourceFile*/, opts: Rc<CompilerOptions>) {
        self.set_file(Some(f.node_wrapper()));
        self.set_options(Some(opts.clone()));
        self.set_language_version(Some(get_emit_script_target(&opts)));
        self.set_in_strict_mode(Some(self.bind_in_strict_mode(f, &opts)));
        self.set_classifiable_names(Some(Rc::new(HashSet::new())));
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

    fn bind_in_strict_mode(&self, file: &Node /*SourceFile*/, opts: &CompilerOptions) -> bool {
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

    fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        self.increment_symbol_count();
        self.Symbol()(flags, name).into()
    }

    fn add_declaration_to_symbol(
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

    fn get_declaration_name(&self, node: &Node) -> Option<__String> {
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

    fn get_display_name(&self, node: &Node /*Declaration*/) -> Cow<'static, str> {
        if is_named_declaration(node) {
            declaration_name_to_string(node.as_named_declaration().maybe_name())
        } else {
            unescape_leading_underscores(
                Debug_.check_defined(self.get_declaration_name(node).as_ref(), None),
            )
            .into()
        }
    }

    fn declare_symbol<TSymbolRef: Borrow<Symbol>>(
        &self,
        symbol_table: &mut SymbolTable,
        parent: Option<TSymbolRef>,
        node: &Node, /*Declaration*/
        includes: SymbolFlags,
        excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        let name = self.get_declaration_name(node);

        let mut symbol = None;
        match name {
            None => unimplemented!(),
            Some(name) => {
                if true {
                    symbol = Some(self.create_symbol(SymbolFlags::None, name.clone()).wrap());
                    symbol_table.insert(name, symbol.as_ref().unwrap().clone());
                }
            }
        }
        let symbol = symbol.unwrap();

        self.add_declaration_to_symbol(&symbol, node, includes);

        symbol
    }

    fn bind_container(&self, node: &Node, container_flags: ContainerFlags) {
        let save_container = self.maybe_container();
        let saved_block_scope_container = self.maybe_block_scope_container();

        if container_flags.intersects(ContainerFlags::IsContainer) {
            self.set_container(Some(node.node_wrapper()));
            self.set_block_scope_container(Some(node.node_wrapper()));
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.container().set_locals(Some(create_symbol_table(None)));
            }
        } else if container_flags.intersects(ContainerFlags::IsBlockScopedContainer) {
            self.set_block_scope_container(Some(node.node_wrapper()));
            self.block_scope_container().set_locals(None);
        }

        if false {
        } else if container_flags.intersects(ContainerFlags::IsInterface) {
            self.bind_children(node);
        } else {
            self.bind_children(node);
        }

        self.set_container(save_container);
        self.set_block_scope_container(saved_block_scope_container);
    }

    fn bind_each_functions_first(&self, nodes: &NodeArray) {
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() == SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() != SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
    }

    fn bind_each(&self, nodes: &NodeArray) {
        for_each(nodes, |node, _| {
            self.bind(Some(node.clone()));
            Option::<()>::None
        });
    }

    fn bind_each_callback<TNodeCallback: FnMut(&Node)>(
        nodes: &NodeArray,
        mut bind_function: TNodeCallback,
    ) {
        for_each(nodes, |node, _| {
            bind_function(&*node);
            Option::<()>::None
        });
    }

    fn bind_each_child(&self, node: &Node) {
        for_each_child(
            node,
            |node| self.bind(Some(node)),
            Some(|nodes: &NodeArray| self.bind_each(nodes)),
        );
    }

    fn bind_children(&self, node: &Node) {
        match node {
            Node::IfStatement(if_statement) => {
                self.bind_if_statement(if_statement);
            }
            Node::ReturnStatement(_) => {
                self.bind_return_or_throw(node);
            }
            Node::ExpressionStatement(expression_statement) => {
                self.bind_expression_statement(expression_statement);
            }
            Node::PrefixUnaryExpression(_) => {
                self.bind_prefix_unary_expression_flow(node);
            }
            Node::BinaryExpression(_) => unimplemented!(),
            Node::VariableDeclaration(_) => {
                self.bind_variable_declaration_flow(node);
            }
            Node::SourceFile(source_file) => {
                self.bind_each_functions_first(&source_file.statements);
            }
            Node::Block(block) => {
                self.bind_each_functions_first(&block.statements);
            }
            Node::ArrayLiteralExpression(_)
            | Node::ObjectLiteralExpression(_)
            | Node::PropertyAssignment(_) => {
                // self.set_in_assignment_pattern(save_in_assignment_pattern);
                self.bind_each_child(node);
            }
            _ => {
                self.bind_each_child(node);
            }
        };
    }

    fn do_with_conditional_branches<TArgument>(
        &self,
        action: fn(&BinderType, TArgument),
        value: TArgument,
    ) {
        action(self, value);
    }

    fn bind_condition<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        self.do_with_conditional_branches(BinderType::bind, node);
    }

    fn bind_if_statement(&self, node: &IfStatement) {
        self.bind_condition(Some(node.expression.clone()));
        self.bind(Some(&*node.then_statement));
        self.bind(node.else_statement.clone());
    }

    fn bind_return_or_throw(&self, node: &Node) {
        self.bind(match node {
            Node::ReturnStatement(return_statement) => return_statement.expression.clone(),
            _ => panic!("Expected return or throw"),
        });
    }

    fn bind_expression_statement(&self, node: &ExpressionStatement) {
        self.bind(Some(node.expression.clone()));
    }

    fn bind_prefix_unary_expression_flow(&self, node: &Node) {
        if false {
        } else {
            self.bind_each_child(node);
        }
    }

    fn bind_variable_declaration_flow(&self, node: &Node /*VariableDeclaration*/) {
        self.bind_each_child(node);
    }

    fn get_container_flags(&self, node: &Node) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::InterfaceDeclaration => {
                return ContainerFlags::IsContainer | ContainerFlags::IsInterface;
            }
            SyntaxKind::TypeAliasDeclaration => {
                return ContainerFlags::IsContainer | ContainerFlags::HasLocals;
            }
            SyntaxKind::SourceFile => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals;
            }
            SyntaxKind::FunctionDeclaration => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals
                    | ContainerFlags::IsFunctionLike;
            }
            SyntaxKind::Block => {
                return if is_function_like(node.maybe_parent())
                    || is_class_static_block_declaration(&*node.parent())
                {
                    ContainerFlags::None
                } else {
                    ContainerFlags::IsBlockScopedContainer
                };
            }
            _ => (),
        }

        ContainerFlags::None
    }

    fn declare_symbol_and_add_to_symbol_table(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::SourceFile => {
                Some(self.declare_source_file_member(node, symbol_flags, symbol_excludes))
            }
            SyntaxKind::InterfaceDeclaration => Some(self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
            )),
            SyntaxKind::FunctionDeclaration | SyntaxKind::TypeAliasDeclaration => {
                Some(self.declare_symbol(
                    &mut *self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                ))
            }
            _ => unimplemented!(),
        }
    }

    fn declare_source_file_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol(
                &mut *self.file().locals(),
                Option::<&Symbol>::None,
                node,
                symbol_flags,
                symbol_excludes,
            )
        }
    }

    fn bind_object_literal_expression(&self, node: &Node /*ObjectLiteralExpression*/) {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    fn bind_anonymous_declaration(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Rc<Symbol> {
        let symbol = self.create_symbol(symbol_flags, name).wrap();
        self.file()
            .as_source_file()
            .keep_strong_reference_to_symbol(symbol.clone());
        self.add_declaration_to_symbol(&symbol, node, symbol_flags);
        symbol
    }

    fn bind_block_scoped_declaration(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        {
            let mut block_scope_container_locals = block_scope_container.maybe_locals();
            if block_scope_container_locals.is_none() {
                *block_scope_container_locals = Some(create_symbol_table(None));
            }
        }
        self.declare_symbol(
            &mut *block_scope_container.locals(),
            Option::<&Symbol>::None,
            node,
            symbol_flags,
            symbol_excludes,
        );
    }

    fn delayed_bind_jsdoc_typedef_tag(&self) {
        // unimplemented!()
    }

    fn bind<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let node = node.borrow();
        set_parent(node, self.maybe_parent());

        self.bind_worker(node);

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node.node_wrapper()));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }

    fn bind_worker(&self, node: &Node) {
        match node {
            Node::TypeParameterDeclaration(_) => self.bind_type_parameter(node),
            Node::ParameterDeclaration(_) => self.bind_parameter(node),
            Node::VariableDeclaration(_) => self.bind_variable_declaration_or_binding_element(node),
            Node::PropertySignature(_) => self.bind_property_worker(node),
            Node::PropertyAssignment(_) => self.bind_property_or_method_or_accessor(
                node,
                SymbolFlags::Property,
                SymbolFlags::PropertyExcludes,
            ),
            Node::FunctionDeclaration(_) => self.bind_function_declaration(node),
            Node::ObjectLiteralExpression(_) => self.bind_object_literal_expression(node),
            Node::InterfaceDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Interface,
                SymbolFlags::InterfaceExcludes,
            ),
            Node::TypeAliasDeclaration(_) => self.bind_block_scoped_declaration(
                node,
                SymbolFlags::TypeAlias,
                SymbolFlags::TypeAliasExcludes,
            ),
            _ => (),
        }
    }

    fn bind_property_worker(&self, node: &Node /*PropertySignature*/) {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if false {
                    unimplemented!()
                } else {
                    SymbolFlags::None
                },
            SymbolFlags::PropertyExcludes,
        )
    }

    fn bind_variable_declaration_or_binding_element(
        &self,
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_variable_declaration = node.as_variable_declaration();
        if !is_binding_pattern(Some(node_as_variable_declaration.name())) {
            if false {
                unimplemented!()
            } else if is_block_or_catch_scoped(node) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        if is_binding_pattern(Some(node.as_parameter_declaration().name())) {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }
    }

    fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    fn bind_property_or_method_or_accessor(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes);
        }
    }

    fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }
}
