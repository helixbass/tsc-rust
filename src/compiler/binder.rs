#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::{
    for_each_child_returns, get_node_id, has_syntactic_modifier, is_enum_const,
    set_parent_recursive, FlowNode, ModifierFlags, NodeId, Symbol, SymbolTable, SyntaxKind,
    __String, append_if_unique, create_symbol_table, for_each, for_each_child,
    get_escaped_text_of_identifier_or_literal, get_name_of_declaration, is_binding_pattern,
    is_block_or_catch_scoped, is_class_static_block_declaration, is_function_like,
    is_property_name_literal, object_allocator, set_parent, set_value_declaration, BaseSymbol,
    ExpressionStatement, IfStatement, InternalSymbolName, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, SymbolFlags, SymbolInterface,
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
    unimplemented!()
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

// lazy_static! {
//     static ref binder: BinderType = create_binder();
// }

pub fn bind_source_file(file: &Node) {
    // binder.call(file);
    create_binder().call(file);
}

#[allow(non_snake_case)]
struct BinderType {
    file: RefCell<Option<Rc</*SourceFile*/ Node>>>,
    parent: RefCell<Option<Rc<Node>>>,
    container: RefCell<Option<Rc<Node>>>,
    block_scope_container: RefCell<Option<Rc<Node>>>,
    Symbol: RefCell<Option<fn(SymbolFlags, __String) -> BaseSymbol>>,
}

fn create_binder() -> BinderType {
    BinderType {
        file: RefCell::new(None),
        parent: RefCell::new(None),
        container: RefCell::new(None),
        block_scope_container: RefCell::new(None),
        Symbol: RefCell::new(None),
    }
}

impl BinderType {
    fn call(&self, f: &Node) {
        self.bind_source_file(f);
    }

    fn file(&self) -> Rc<Node> {
        self.file.borrow().as_ref().unwrap().clone()
    }

    fn set_file(&self, file: Option<Rc<Node>>) {
        *self.file.borrow_mut() = file;
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

    #[allow(non_snake_case)]
    fn Symbol(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        self.Symbol.borrow().unwrap()
    }

    #[allow(non_snake_case)]
    fn set_Symbol(&self, Symbol: fn(SymbolFlags, __String) -> BaseSymbol) {
        *self.Symbol.borrow_mut() = Some(Symbol);
    }

    fn bind_source_file(&self, f: &Node) {
        self.set_file(Some(f.node_wrapper()));

        self.set_Symbol(object_allocator.get_symbol_constructor());

        if true {
            self.bind(Some(&*self.file()));
        }

        self.set_file(None);
        self.set_parent(None);
        self.set_container(None);
    }

    fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
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
            symbol.maybe_declarations().as_ref().map(|vec| vec.clone()),
            node.node_wrapper(),
        );
        symbol.set_declarations(declarations);

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

        if symbol_flags.intersects(SymbolFlags::Value) {
            set_value_declaration(symbol, node);
        }
    }

    fn get_declaration_name(&self, node: &Node) -> Option<__String> {
        let name = get_name_of_declaration(Some(node));
        if let Some(name) = name {
            return if is_property_name_literal(&*name) {
                Some(get_escaped_text_of_identifier_or_literal(&*name))
            } else {
                None
            };
        }
        unimplemented!()
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
