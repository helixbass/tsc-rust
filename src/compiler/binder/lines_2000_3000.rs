#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{
    get_module_instance_state, init_flow_node, BinderType, ContainerFlags, ModuleInstanceState,
};
use crate::{
    create_file_diagnostic, create_symbol_table, find_ancestor,
    get_assignment_declaration_property_access_kind, get_enclosing_block_scope_container,
    get_error_span_for_node, get_name_of_declaration, is_assignment_expression,
    is_assignment_target, is_exports_identifier, is_external_or_common_js_module, is_identifier,
    is_jsdoc_enum_tag, is_module_exports_access_expression,
    is_property_access_entity_name_expression, is_property_access_expression, is_source_file,
    is_variable_declaration, AssignmentDeclarationKind, Debug_, DiagnosticMessage, Diagnostics,
    FlowFlags, FlowStart, HasInitializerInterface, Symbol, SymbolInterface, SyntaxKind, __String,
    is_binding_pattern, is_block_or_catch_scoped, set_parent, InternalSymbolName,
    NamedDeclarationInterface, Node, NodeInterface, SymbolFlags,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ElementKind {
    Property = 1,
    Accessor = 2,
}

impl BinderType {
    pub(super) fn declare_module_symbol(
        &self,
        node: &Node, /*ModuleDeclaration*/
    ) -> ModuleInstanceState {
        let state = get_module_instance_state(node, None);
        let instantiated = state != ModuleInstanceState::NonInstantiated;
        self.declare_symbol_and_add_to_symbol_table(
            node,
            if instantiated {
                SymbolFlags::ValueModule
            } else {
                SymbolFlags::NamespaceModule
            },
            if instantiated {
                SymbolFlags::ValueModuleExcludes
            } else {
                SymbolFlags::NamespaceModuleExcludes
            },
        );
        state
    }

    pub(super) fn bind_function_or_constructor_type(
        &self,
        node: &Node, /*SignatureDeclaration | JSDocSignature*/
    ) {
        let symbol = self.create_symbol(
            SymbolFlags::Signature,
            self.get_declaration_name(node).unwrap(),
        );
        self.add_declaration_to_symbol(&symbol, node, SymbolFlags::Signature);

        let type_literal_symbol =
            self.create_symbol(SymbolFlags::TypeLiteral, InternalSymbolName::Type());
        self.add_declaration_to_symbol(&type_literal_symbol, node, SymbolFlags::TypeLiteral);
        let mut type_literal_symbol_members = type_literal_symbol.maybe_members();
        *type_literal_symbol_members = Some(Rc::new(RefCell::new(create_symbol_table(None))));
        type_literal_symbol_members
            .as_ref()
            .unwrap()
            .borrow_mut()
            .insert(symbol.escaped_name().clone(), symbol.wrap());
    }

    pub(super) fn bind_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
    ) {
        if matches!(self.maybe_in_strict_mode(), Some(true)) && !is_assignment_target(node) {
            let mut seen = HashMap::<__String, ElementKind>::new();

            let node_as_object_literal_expression = node.as_object_literal_expression();
            for prop in &*node_as_object_literal_expression.properties {
                if prop.kind() == SyntaxKind::SpreadAssignment
                    || prop.as_named_declaration().name().kind() != SyntaxKind::Identifier
                {
                    continue;
                }

                let identifier = prop.as_named_declaration().name();

                let current_kind = if matches!(
                    prop.kind(),
                    SyntaxKind::PropertyAssignment
                        | SyntaxKind::ShorthandPropertyAssignment
                        | SyntaxKind::MethodDeclaration
                ) {
                    ElementKind::Property
                } else {
                    ElementKind::Accessor
                };

                let identifier_as_identifier = identifier.as_identifier();
                let existing_kind = seen.get(&identifier_as_identifier.escaped_text);
                if existing_kind.is_none() {
                    seen.insert(identifier_as_identifier.escaped_text.clone(), current_kind);
                    continue;
                }
                let existing_kind = *existing_kind.unwrap();

                if current_kind == ElementKind::Property && existing_kind == ElementKind::Property {
                    let file = self.file();
                    let span = get_error_span_for_node(&file, &identifier);
                    file.as_source_file().bind_diagnostics().push(
                        Rc::new(
                            create_file_diagnostic(
                                &file,
                                span.start,
                                span.length,
                                &Diagnostics::An_object_literal_cannot_have_multiple_properties_with_the_same_name_in_strict_mode,
                                None,
                            ).into()
                        )
                    );
                }
            }
        }

        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    pub(super) fn bind_jsx_attributes(&self, node: &Node /*JsxAttributes*/) -> Rc<Symbol> {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::JSXAttributes(),
        )
    }

    pub(super) fn bind_jsx_attribute(
        &self,
        node: &Node, /*JsxAttribute*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes)
    }

    pub(super) fn bind_anonymous_declaration(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Rc<Symbol> {
        let symbol = self.create_symbol(symbol_flags, name).wrap();
        if symbol_flags.intersects(SymbolFlags::EnumMember | SymbolFlags::ClassMember) {
            symbol.set_parent(Some(self.container().symbol()));
        }
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
        match block_scope_container.kind() {
            SyntaxKind::ModuleDeclaration => {
                self.declare_module_member(node, symbol_flags, symbol_excludes);
            }
            SyntaxKind::SourceFile => {
                if is_external_or_common_js_module(&self.container()) {
                    self.declare_module_member(node, symbol_flags, symbol_excludes);
                } else {
                    {
                        let mut block_scope_container_locals = block_scope_container.maybe_locals();
                        if block_scope_container_locals.is_none() {
                            *block_scope_container_locals = Some(create_symbol_table(None));
                            self.add_to_container_chain(&block_scope_container);
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
            }
            _ => {
                {
                    let mut block_scope_container_locals = block_scope_container.maybe_locals();
                    if block_scope_container_locals.is_none() {
                        *block_scope_container_locals = Some(create_symbol_table(None));
                        self.add_to_container_chain(&block_scope_container);
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
        }
    }

    pub(super) fn delayed_bind_jsdoc_typedef_tag(&self) {
        let delayed_type_aliases = self.maybe_delayed_type_aliases();
        if delayed_type_aliases.is_none() {
            return;
        }
        let delayed_type_aliases = delayed_type_aliases.as_deref().unwrap();
        let save_container = self.maybe_container();
        let save_last_container = self.maybe_last_container();
        let save_block_scope_container = self.maybe_block_scope_container();
        let save_parent = self.maybe_parent();
        let save_current_flow = self.maybe_current_flow();
        for type_alias in delayed_type_aliases {
            let host = type_alias.parent().parent();
            self.set_container(Some(
                find_ancestor(host.maybe_parent(), |n| {
                    self.get_container_flags(n)
                        .intersects(ContainerFlags::IsContainer)
                })
                .unwrap_or_else(|| self.file()),
            ));
            self.set_block_scope_container(Some(
                get_enclosing_block_scope_container(&host).unwrap_or_else(|| self.file()),
            ));
            self.set_current_flow(Some(Rc::new(init_flow_node(
                FlowStart::new(FlowFlags::Start, None).into(),
            ))));
            self.set_parent(Some(type_alias.clone()));
            let type_alias_as_jsdoc_type_like_tag = type_alias.as_jsdoc_type_like_tag();
            self.bind(type_alias_as_jsdoc_type_like_tag.maybe_type_expression());
            let decl_name = get_name_of_declaration(Some(&**type_alias));
            if (is_jsdoc_enum_tag(type_alias)
                || type_alias
                    .as_jsdoc_typedef_or_callback_tag()
                    .maybe_full_name()
                    .is_none())
                && matches!(decl_name.as_ref(), Some(decl_name) if is_property_access_entity_name_expression(&decl_name.parent()))
            {
                let decl_name = decl_name.unwrap();
                let is_top_level = self.is_top_level_namespace_assignment(&decl_name.parent());
                if is_top_level {
                    self.bind_potentially_missing_namespaces(
                        Some(self.file().symbol()),
                        &decl_name.parent(),
                        is_top_level,
                        find_ancestor(Some(&*decl_name), |d| {
                            is_property_access_expression(d)
                                && d.as_property_access_expression()
                                    .name
                                    .as_member_name()
                                    .escaped_text()
                                    .eq_str("prototype")
                        })
                        .is_some(),
                        false,
                    );
                    let old_container = self.maybe_container();
                    match get_assignment_declaration_property_access_kind(&decl_name.parent()) {
                        AssignmentDeclarationKind::ExportsProperty
                        | AssignmentDeclarationKind::ModuleExports => {
                            if !is_external_or_common_js_module(&self.file()) {
                                self.set_container(None);
                            } else {
                                self.set_container(Some(self.file()));
                            }
                        }
                        AssignmentDeclarationKind::ThisProperty => {
                            self.set_container(Some(
                                decl_name.parent().as_has_expression().expression(),
                            ));
                        }
                        AssignmentDeclarationKind::PrototypeProperty => {
                            self.set_container(Some(
                                decl_name
                                    .parent()
                                    .as_has_expression()
                                    .expression()
                                    .as_property_access_expression()
                                    .name
                                    .clone(),
                            ));
                        }
                        AssignmentDeclarationKind::Property => {
                            self.set_container(Some(
                                if is_exports_or_module_exports_or_alias(
                                    &self.file(),
                                    &decl_name.parent().as_has_expression().expression(),
                                ) {
                                    self.file()
                                } else if is_property_access_expression(
                                    &decl_name.parent().as_has_expression().expression(),
                                ) {
                                    decl_name
                                        .parent()
                                        .as_has_expression()
                                        .expression()
                                        .as_property_access_expression()
                                        .name
                                        .clone()
                                } else {
                                    decl_name.parent().as_has_expression().expression()
                                },
                            ));
                        }
                        AssignmentDeclarationKind::None => {
                            Debug_.fail(Some("Shouldn't have detected typedef or enum or non-assignment declaration"));
                        }
                        _ => (),
                    }
                    if self.maybe_container().is_some() {
                        self.declare_module_member(
                            type_alias,
                            SymbolFlags::TypeAlias,
                            SymbolFlags::TypeAliasExcludes,
                        );
                    }
                    self.set_container(old_container);
                }
            } else if is_jsdoc_enum_tag(type_alias)
                || match type_alias
                    .as_jsdoc_typedef_or_callback_tag()
                    .maybe_full_name()
                {
                    None => true,
                    Some(full_name) => full_name.kind() == SyntaxKind::Identifier,
                }
            {
                self.set_parent(Some(type_alias.parent()));
                self.bind_block_scoped_declaration(
                    type_alias,
                    SymbolFlags::TypeAlias,
                    SymbolFlags::TypeAliasExcludes,
                );
            } else {
                self.bind(
                    type_alias
                        .as_jsdoc_typedef_or_callback_tag()
                        .maybe_full_name(),
                );
            }
        }
        self.set_container(save_container);
        self.set_last_container(save_last_container);
        self.set_block_scope_container(save_block_scope_container);
        self.set_parent(save_parent);
        self.set_current_flow(save_current_flow);
    }

    pub(super) fn error_on_first_token(
        &self,
        node: &Node,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        unimplemented!()
    }

    pub(super) fn error_or_suggestion_on_node(
        &self,
        is_error: bool,
        node: &Node,
        message: &DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn bind<TNode: Borrow<Node>>(&self, node: Option<TNode>) {
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

    pub(super) fn bind_jsdoc(&self, node: &Node) {
        // unimplemented!()
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

    pub(super) fn bind_potentially_missing_namespaces<TNamespaceSymbol: Borrow<Symbol>>(
        &self,
        namespace_symbol: Option<TNamespaceSymbol>,
        entity_name: &Node, /*BindableStaticNameExpression*/
        is_top_level: bool,
        is_prototype_property: bool,
        container_is_class: bool,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_top_level_namespace_assignment(
        &self,
        property_access: &Node, /*BindableAccessExpression*/
    ) -> bool {
        unimplemented!()
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

    pub(super) fn check_unreachable(&self, node: &Node) -> bool {
        false
        // unimplemented!()
    }
}

pub fn is_exports_or_module_exports_or_alias(
    source_file: &Node, /*SourceFile*/
    node: &Node,        /*Expression*/
) -> bool {
    let mut node = node.node_wrapper();
    let mut i = 0;
    let mut q = vec![node];
    while !q.is_empty() && i < 100 {
        i += 1;
        node = q.remove(0);
        if is_exports_identifier(&node) || is_module_exports_access_expression(&node) {
            return true;
        } else if is_identifier(&node) {
            let symbol = lookup_symbol_for_name(&source_file, &node.as_identifier().escaped_text);
            if let Some(symbol) = symbol {
                if let Some(symbol_value_declaration) =
                    symbol
                        .maybe_value_declaration()
                        .filter(|value_declaration| {
                            is_variable_declaration(&value_declaration)
                                && value_declaration
                                    .as_variable_declaration()
                                    .maybe_initializer()
                                    .is_some()
                        })
                {
                    let init = symbol_value_declaration
                        .as_variable_declaration()
                        .maybe_initializer()
                        .unwrap();
                    q.push(init.clone());
                    if is_assignment_expression(&init, Some(true)) {
                        let init_as_binary_expression = init.as_binary_expression();
                        q.push(init_as_binary_expression.left.clone());
                        q.push(init_as_binary_expression.right.clone());
                    }
                }
            }
        }
    }
    false
}

fn lookup_symbol_for_name(container: &Node, name: &__String) -> Option<Rc<Symbol>> {
    let container_locals = container.maybe_locals();
    let local = container_locals
        .as_ref()
        .and_then(|locals| locals.get(name));
    if let Some(local) = local {
        return Some(local.maybe_export_symbol().unwrap_or(local.clone()));
    }
    if is_source_file(container) {
        let container_as_source_file = container.as_source_file();
        if let Some(container_js_global_augmentations) = container_as_source_file
            .maybe_js_global_augmentations()
            .as_ref()
        {
            let container_js_global_augmentations = container_js_global_augmentations.borrow_mut(); // TODO: doesn't actually need to be mut
            if container_js_global_augmentations.contains_key(name) {
                return container_js_global_augmentations
                    .get(name)
                    .map(Clone::clone);
            }
        }
    }
    container
        .maybe_symbol()
        .and_then(|symbol| symbol.maybe_exports().clone())
        .and_then(|exports| exports.borrow_mut().get(name).map(Clone::clone)) // TODO: same here doesn't need to be mut
}
