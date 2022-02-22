#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use super::{BinderType, ContainerFlags};
use crate::{
    get_combined_modifier_flags, has_syntactic_modifier, is_ambient_module, is_in_js_file,
    is_jsdoc_type_alias, Debug_, ModifierFlags, NodeFlags, Symbol, SyntaxKind, __String,
    create_symbol_table, for_each, for_each_child, is_binding_pattern, is_block_or_catch_scoped,
    is_class_static_block_declaration, is_function_like, set_parent, ExpressionStatement,
    IfStatement, InternalSymbolName, NamedDeclarationInterface, Node, NodeArray, NodeInterface,
    SymbolFlags, SymbolInterface,
};

impl BinderType {
    pub(super) fn declare_module_member(
        &self,
        node: &Node, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        let has_export_modifier = get_combined_modifier_flags(node)
            .intersects(ModifierFlags::Export)
            || self.jsdoc_treat_as_exported(node);
        if symbol_flags.intersects(SymbolFlags::Alias) {
            if node.kind() == SyntaxKind::ExportSpecifier
                || node.kind() == SyntaxKind::ImportEqualsDeclaration && has_export_modifier
            {
                self.declare_symbol(
                    &mut self.container().symbol().exports().borrow_mut(),
                    Some(self.container().symbol()),
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            } else {
                self.declare_symbol(
                    &mut self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            }
        } else {
            if is_jsdoc_type_alias(node) {
                Debug_.assert(is_in_js_file(Some(node)), None);
            }
            if !is_ambient_module(node)
                && (has_export_modifier
                    || self
                        .container()
                        .flags()
                        .intersects(NodeFlags::ExportContext))
            {
                if self.container().maybe_locals().is_none()
                    || has_syntactic_modifier(node, ModifierFlags::Default)
                        && self.get_declaration_name(node).is_none()
                {
                    return self.declare_symbol(
                        &mut self.container().symbol().exports().borrow_mut(),
                        Some(self.container().symbol()),
                        node,
                        symbol_flags,
                        symbol_excludes,
                        None,
                        None,
                    );
                }
                let export_kind = if symbol_flags.intersects(SymbolFlags::Value) {
                    SymbolFlags::ExportValue
                } else {
                    SymbolFlags::None
                };
                let local = self.declare_symbol(
                    &mut self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    export_kind,
                    symbol_excludes,
                    None,
                    None,
                );
                local.set_export_symbol(Some(self.declare_symbol(
                    &mut self.container().symbol().exports().borrow_mut(),
                    Some(self.container().symbol()),
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )));
                node.set_local_symbol(Some(local.clone()));
                local
            } else {
                self.declare_symbol(
                    &mut self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                )
            }
        }
    }

    pub(super) fn jsdoc_treat_as_exported(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn bind_container(&self, node: &Node, container_flags: ContainerFlags) {
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

    pub(super) fn bind_each_functions_first(&self, nodes: &NodeArray) {
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

    pub(super) fn bind_each(&self, nodes: &NodeArray) {
        for_each(nodes, |node, _| {
            self.bind(Some(node.clone()));
            Option::<()>::None
        });
    }

    pub(super) fn bind_each_callback<TNodeCallback: FnMut(&Node)>(
        nodes: &NodeArray,
        mut bind_function: TNodeCallback,
    ) {
        for_each(nodes, |node, _| {
            bind_function(&*node);
            Option::<()>::None
        });
    }

    pub(super) fn bind_each_child(&self, node: &Node) {
        for_each_child(
            node,
            |node| self.bind(Some(node)),
            Some(|nodes: &NodeArray| self.bind_each(nodes)),
        );
    }

    pub(super) fn bind_children(&self, node: &Node) {
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

    pub(super) fn do_with_conditional_branches<TArgument>(
        &self,
        action: fn(&BinderType, TArgument),
        value: TArgument,
    ) {
        action(self, value);
    }

    pub(super) fn bind_condition<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        self.do_with_conditional_branches(BinderType::bind, node);
    }

    pub(super) fn bind_if_statement(&self, node: &IfStatement) {
        self.bind_condition(Some(node.expression.clone()));
        self.bind(Some(&*node.then_statement));
        self.bind(node.else_statement.clone());
    }

    pub(super) fn bind_return_or_throw(&self, node: &Node) {
        self.bind(match node {
            Node::ReturnStatement(return_statement) => return_statement.expression.clone(),
            _ => panic!("Expected return or throw"),
        });
    }

    pub(super) fn bind_expression_statement(&self, node: &ExpressionStatement) {
        self.bind(Some(node.expression.clone()));
    }

    pub(super) fn bind_prefix_unary_expression_flow(&self, node: &Node) {
        if false {
        } else {
            self.bind_each_child(node);
        }
    }

    pub(super) fn bind_variable_declaration_flow(&self, node: &Node /*VariableDeclaration*/) {
        self.bind_each_child(node);
    }

    pub(super) fn get_container_flags(&self, node: &Node) -> ContainerFlags {
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

    pub(super) fn declare_symbol_and_add_to_symbol_table(
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
                None,
                None,
            )),
            SyntaxKind::FunctionDeclaration | SyntaxKind::TypeAliasDeclaration => {
                Some(self.declare_symbol(
                    &mut *self.container().locals(),
                    Option::<&Symbol>::None,
                    node,
                    symbol_flags,
                    symbol_excludes,
                    None,
                    None,
                ))
            }
            _ => unimplemented!(),
        }
    }

    pub(super) fn declare_source_file_member(
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
                None,
                None,
            )
        }
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    pub(super) fn bind_anonymous_declaration(
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

    pub(super) fn bind_block_scoped_declaration(
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
            None,
            None,
        );
    }

    pub(super) fn delayed_bind_jsdoc_typedef_tag(&self) {
        // unimplemented!()
    }

    pub(super) fn bind<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
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

    pub(super) fn bind_worker(&self, node: &Node) {
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

    pub(super) fn bind_property_worker(&self, node: &Node /*PropertySignature*/) {
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

    pub(super) fn bind_variable_declaration_or_binding_element(
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

    pub(super) fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
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

    pub(super) fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
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

    pub(super) fn bind_property_or_method_or_accessor(
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

    pub(super) fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
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
