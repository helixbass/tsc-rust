use std::{borrow::Borrow, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{get_module_instance_state, Binder, ModuleInstanceState};
use crate::{
    cast, create_symbol_table, find_ancestor, get_assigned_expando_initializer,
    get_combined_node_flags, get_effective_container_for_jsdoc_template_tag,
    get_element_or_property_access_name, get_expando_initializer, get_jsdoc_type_tag,
    get_leftmost_access_expression, get_name_of_declaration, get_name_or_argument,
    get_ranges_where, get_right_most_assigned_expression, has_dynamic_name, has_syntactic_modifier,
    id_text, index_of, is_assignment_expression, is_async_function, is_binary_expression,
    is_bindable_object_define_property_call, is_bindable_static_name_expression,
    is_binding_pattern, is_block, is_block_or_catch_scoped, is_call_expression,
    is_conditional_type_node, is_enum_const, is_enum_declaration, is_exports_identifier,
    is_function_declaration, is_function_symbol, is_identifier, is_in_js_file,
    is_jsdoc_template_tag, is_module_exports_access_expression,
    is_object_literal_or_class_expression_method_or_accessor, is_parameter_declaration,
    is_parameter_property_declaration, is_private_identifier, is_property_access_expression,
    is_prototype_access, is_require_call, is_require_variable_declaration, is_source_file,
    is_statement, is_statement_but_not_declaration, is_variable_declaration, is_variable_statement,
    maybe_is_function_like_declaration, set_parent, should_preserve_const_enums, slice_after, some,
    symbol_name, unreachable_code_is_error, Debug_, Diagnostics, FlowFlags, FlowNodeBase, HasArena,
    HasInitializerInterface, InArena, InternalSymbolName, ModifierFlags, NamedDeclarationInterface,
    Node, NodeFlags, NodeInterface, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    OptionInArena, index_of_eq, slice_after_eq,
};

impl Binder {
    pub(super) fn bind_prototype_assignment(
        &self,
        node: Id<Node>, /*BindableStaticPropertyAssignmentExpression*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        set_parent(&node_as_binary_expression.left.ref_(self), Some(node));
        set_parent(&node_as_binary_expression.right.ref_(self), Some(node));
        self.bind_property_assignment(
            node_as_binary_expression
                .left
                .ref_(self).as_has_expression()
                .expression(),
            node_as_binary_expression.left,
            false,
            true,
        );
    }

    pub(super) fn bind_object_define_prototype_property(
        &self,
        node: Id<Node>, /*BindableObjectDefinePropertyCall*/
    ) {
        let namespace_symbol = self.lookup_symbol_for_property_access(
            node.ref_(self).as_call_expression().arguments.ref_(self)[0]
                .ref_(self).as_property_access_expression()
                .expression,
            None,
        );
        if let Some(namespace_symbol) = namespace_symbol {
            if let Some(namespace_symbol_value_declaration) =
                namespace_symbol.ref_(self).maybe_value_declaration()
            {
                self.add_declaration_to_symbol(
                    namespace_symbol,
                    namespace_symbol_value_declaration,
                    SymbolFlags::Class,
                );
            }
        }
        self.bind_potentially_new_expando_member_to_namespace(node, namespace_symbol, true);
    }

    pub(super) fn bind_prototype_property_assignment(
        &self,
        lhs: Id<Node>, /*BindableStaticAccessExpression*/
        parent: Id<Node>,
    ) {
        let class_prototype = lhs.ref_(self).as_has_expression().expression();
        let constructor_function = class_prototype.ref_(self).as_has_expression().expression();

        set_parent(&constructor_function.ref_(self), Some(class_prototype));
        set_parent(&class_prototype.ref_(self), Some(lhs));
        set_parent(&lhs.ref_(self), Some(parent));

        self.bind_property_assignment(constructor_function, lhs, true, true);
    }

    pub(super) fn bind_object_define_property_assignment(
        &self,
        node: Id<Node>, /*BindableObjectDefinePropertyCall*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_call_expression = node_ref.as_call_expression();
        let mut namespace_symbol = self.lookup_symbol_for_property_access(
            node_as_call_expression.arguments.ref_(self)[0],
            None,
        );
        let is_toplevel = node.ref_(self).parent().ref_(self).parent().ref_(self).kind() == SyntaxKind::SourceFile;
        namespace_symbol = self.bind_potentially_missing_namespaces(
            namespace_symbol,
            node_as_call_expression.arguments.ref_(self)[0],
            is_toplevel,
            false,
            false,
        );
        self.bind_potentially_new_expando_member_to_namespace(node, namespace_symbol, false);
    }

    pub(super) fn bind_special_property_assignment(
        &self,
        node: Id<Node>, /*BindablePropertyAssignmentExpression*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_binary_expression = node_ref.as_binary_expression();
        let parent_symbol = self
            .lookup_symbol_for_property_access(
                node_as_binary_expression
                    .left
                    .ref_(self).as_has_expression()
                    .expression(),
                self.maybe_container(),
            )
            .or_else(|| {
                self.lookup_symbol_for_property_access(
                    node_as_binary_expression
                        .left
                        .ref_(self).as_has_expression()
                        .expression(),
                    self.maybe_block_scope_container(),
                )
            });
        if !is_in_js_file(Some(&node.ref_(self)))
            && !is_function_symbol(
                parent_symbol,
                self,
            )
        {
            return;
        }
        let root_expr = get_leftmost_access_expression(node_as_binary_expression.left, self);
        if is_identifier(&root_expr.ref_(self))
            && matches!(
                lookup_symbol_for_name(
                    self,
                    self.container(),
                    &root_expr.ref_(self).as_identifier().escaped_text
                ),
                Some(symbol) if self.symbol_ref(symbol).flags().intersects(SymbolFlags::Alias)
            )
        {
            return;
        }
        set_parent(&node_as_binary_expression.left.ref_(self), Some(node));
        set_parent(&node_as_binary_expression.right.ref_(self), Some(node));
        if is_identifier(
            &node_as_binary_expression
                .left
                .ref_(self).as_has_expression()
                .expression().ref_(self),
        ) && self.maybe_container() == Some(self.file())
            && is_exports_or_module_exports_or_alias(
                self,
                self.file(),
                node_as_binary_expression
                    .left
                    .ref_(self).as_has_expression()
                    .expression(),
            )
        {
            self.bind_exports_property_assignment(node);
        } else if has_dynamic_name(node, self) {
            self.bind_anonymous_declaration(
                node,
                SymbolFlags::Property | SymbolFlags::Assignment,
                InternalSymbolName::Computed.to_owned(),
            );
            let sym = self.bind_potentially_missing_namespaces(
                parent_symbol,
                node_as_binary_expression
                    .left
                    .ref_(self).as_has_expression()
                    .expression(),
                self.is_top_level_namespace_assignment(node_as_binary_expression.left),
                false,
                false,
            );
            self.add_late_bound_assignment_declaration_to_symbol(node, sym);
        } else {
            self.bind_static_property_assignment(cast(
                Some(node_as_binary_expression.left),
                |&node| is_bindable_static_name_expression(node, None, self),
            ));
        }
    }

    pub(super) fn bind_static_property_assignment(
        &self,
        node: Id<Node>, /*BindableStaticNameExpression*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_has_expression = node_ref.as_has_expression();
        Debug_.assert(!is_identifier(&node_ref), None);
        set_parent(&node_as_has_expression.expression().ref_(self), Some(node));
        self.bind_property_assignment(node_as_has_expression.expression(), node, false, false);
    }

    pub(super) fn bind_potentially_missing_namespaces(
        &self,
        mut namespace_symbol: Option<Id<Symbol>>,
        entity_name: Id<Node>, /*BindableStaticNameExpression*/
        is_toplevel: bool,
        is_prototype_property: bool,
        container_is_class: bool,
    ) -> Option<Id<Symbol>> {
        if matches!(
            namespace_symbol,
            Some(namespace_symbol) if namespace_symbol.ref_(self).flags().intersects(SymbolFlags::Alias)
        ) {
            return Some(namespace_symbol.unwrap());
        }
        if is_toplevel && !is_prototype_property {
            let flags = SymbolFlags::Module | SymbolFlags::Assignment;
            let exclude_flags = SymbolFlags::ValueModuleExcludes & !SymbolFlags::Assignment;
            namespace_symbol = self.for_each_identifier_in_entity_name(
                entity_name,
                namespace_symbol,
                &mut |id, symbol, parent| {
                    if let Some(symbol) = symbol {
                        self.add_declaration_to_symbol(symbol, id, flags);
                        Some(symbol)
                    } else {
                        /*let table =*/
                        if let Some(parent) = parent {
                            Some(self.declare_symbol(
                                &mut parent.ref_(self).exports().ref_mut(self),
                                Some(parent),
                                id,
                                flags,
                                exclude_flags,
                                None,
                                None,
                            ))
                        } else {
                            let file = self.file();
                            let file_ref = file.ref_(self);
                            let mut file_js_global_augmentations =
                                file_ref.as_source_file().maybe_js_global_augmentations();
                            if file_js_global_augmentations.is_none() {
                                *file_js_global_augmentations =
                                    Some(self.alloc_symbol_table(create_symbol_table(
                                        self.arena(),
                                        Option::<&[Id<Symbol>]>::None,
                                    )));
                            }
                            Some(self.declare_symbol(
                                &mut file_js_global_augmentations.clone().unwrap().ref_mut(self),
                                parent,
                                id,
                                flags,
                                exclude_flags,
                                None,
                                None,
                            ))
                        }
                    }
                },
            );
        }
        if container_is_class {
            if let Some(namespace_symbol) = namespace_symbol {
                if let Some(namespace_symbol_value_declaration) =
                    namespace_symbol.ref_(self).maybe_value_declaration()
                {
                    self.add_declaration_to_symbol(
                        namespace_symbol,
                        namespace_symbol_value_declaration,
                        SymbolFlags::Class,
                    );
                }
            }
        }
        namespace_symbol
    }

    pub(super) fn bind_potentially_new_expando_member_to_namespace(
        &self,
        declaration: Id<Node>, /*BindableStaticAccessExpression | CallExpression*/
        namespace_symbol: Option<Id<Symbol>>,
        is_prototype_property: bool,
    ) {
        if match namespace_symbol {
            None => true,
            Some(namespace_symbol) => !self.is_expando_symbol(namespace_symbol),
        } {
            return;
        }
        let namespace_symbol = namespace_symbol.unwrap();

        let namespace_symbol_ref = namespace_symbol.ref_(self);
        let symbol_table = {
            let symbol_table =
                if is_prototype_property {
                    let mut namespace_symbol_members = namespace_symbol_ref.maybe_members_mut();
                    if namespace_symbol_members.is_none() {
                        *namespace_symbol_members = Some(self.alloc_symbol_table(
                            create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                        ));
                    }
                    namespace_symbol_members
                } else {
                    let mut namespace_symbol_exports = namespace_symbol_ref.maybe_exports_mut();
                    if namespace_symbol_exports.is_none() {
                        *namespace_symbol_exports = Some(self.alloc_symbol_table(
                            create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                        ));
                    }
                    namespace_symbol_exports
                };
            let symbol_table = symbol_table.clone().unwrap();
            symbol_table
        };

        let mut includes = SymbolFlags::None;
        let mut excludes = SymbolFlags::None;
        if maybe_is_function_like_declaration(
            get_assigned_expando_initializer(Some(declaration), self).refed(self).as_deref(),
        ) {
            includes = SymbolFlags::Method;
            excludes = SymbolFlags::MethodExcludes;
        } else if is_call_expression(&declaration.ref_(self))
            && is_bindable_object_define_property_call(declaration, self)
        {
            let declaration_ref = declaration.ref_(self);
            let declaration_as_call_expression = declaration_ref.as_call_expression();
            if some(
                Some(
                    &*declaration_as_call_expression.arguments.ref_(self)[2]
                        .ref_(self).as_object_literal_expression()
                        .properties.ref_(self),
                ),
                Some(|&p: &Id<Node>| {
                    let id = get_name_of_declaration(Some(p), self);
                    matches!(
                        id,
                        Some(id) if is_identifier(&id.ref_(self)) && id_text(&id.ref_(self)) == "set"
                    )
                }),
            ) {
                includes |= SymbolFlags::SetAccessor | SymbolFlags::Property;
                excludes |= SymbolFlags::SetAccessorExcludes;
            }
            if some(
                Some(
                    &*declaration_as_call_expression.arguments.ref_(self)[2]
                        .ref_(self).as_object_literal_expression()
                        .properties.ref_(self),
                ),
                Some(|&p: &Id<Node>| {
                    let id = get_name_of_declaration(Some(p), self);
                    matches!(
                        id,
                        Some(id) if is_identifier(&id.ref_(self)) && id_text(&id.ref_(self)) == "get"
                    )
                }),
            ) {
                includes |= SymbolFlags::GetAccessor | SymbolFlags::Property;
                excludes |= SymbolFlags::GetAccessorExcludes;
            }
        }

        if includes == SymbolFlags::None {
            includes = SymbolFlags::Property;
            excludes = SymbolFlags::PropertyExcludes;
        }

        self.declare_symbol(
            &mut symbol_table.ref_mut(self),
            Some(namespace_symbol.clone()),
            declaration,
            includes | SymbolFlags::Assignment,
            excludes & !SymbolFlags::Assignment,
            None,
            None,
        );
    }

    pub(super) fn is_top_level_namespace_assignment(
        &self,
        property_access: Id<Node>, /*BindableAccessExpression*/
    ) -> bool {
        if is_binary_expression(&property_access.ref_(self).parent().ref_(self)) {
            self.get_parent_of_binary_expression(property_access.ref_(self).parent())
                .ref_(self).parent()
                .ref_(self).kind()
                == SyntaxKind::SourceFile
        } else {
            property_access.ref_(self).parent().ref_(self).parent().ref_(self).kind() == SyntaxKind::SourceFile
        }
    }

    pub(super) fn bind_property_assignment(
        &self,
        name: Id<Node>,            /*BindableStaticNameExpression*/
        property_access: Id<Node>, /*BindableStaticNameExpression*/
        is_prototype_property: bool,
        container_is_class: bool,
    ) {
        let mut namespace_symbol = self
            .lookup_symbol_for_property_access(name, self.maybe_container())
            .or_else(|| {
                self.lookup_symbol_for_property_access(name, self.maybe_block_scope_container())
            });
        let is_toplevel = self.is_top_level_namespace_assignment(property_access);
        namespace_symbol = self.bind_potentially_missing_namespaces(
            namespace_symbol,
            property_access.ref_(self).as_has_expression().expression(),
            is_toplevel,
            is_prototype_property,
            container_is_class,
        );
        self.bind_potentially_new_expando_member_to_namespace(
            property_access,
            namespace_symbol,
            is_prototype_property,
        );
    }

    pub(super) fn is_expando_symbol(&self, symbol: Id<Symbol>) -> bool {
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::NamespaceModule)
        {
            return true;
        }
        let node = symbol.ref_(self).maybe_value_declaration();
        if let Some(node) = node {
            if is_call_expression(&node.ref_(self)) {
                return get_assigned_expando_initializer(Some(node), self).is_some();
            }
        }
        let init = node.and_then(|node| {
            if is_variable_declaration(&node.ref_(self)) {
                node.ref_(self).as_variable_declaration().maybe_initializer()
            } else if is_binary_expression(&node.ref_(self)) {
                Some(node.ref_(self).as_binary_expression().right)
            } else if is_property_access_expression(&node.ref_(self)) && is_binary_expression(&node.ref_(self).parent().ref_(self)) {
                Some(node.ref_(self).parent().ref_(self).as_binary_expression().right)
            } else {
                None
            }
        });
        let init = init.map(|init| get_right_most_assigned_expression(init, self));
        if let Some(init) = init {
            let node = node.unwrap();
            let is_prototype_assignment =
                is_prototype_access(if is_variable_declaration(&node.ref_(self)) {
                    node.ref_(self).as_variable_declaration().name()
                } else if is_binary_expression(&node.ref_(self)) {
                    node.ref_(self).as_binary_expression().left
                } else {
                    node
                }, self);
            return get_expando_initializer(
                if is_binary_expression(&init.ref_(self))
                    && matches!(
                        init.ref_(self).as_binary_expression().operator_token.ref_(self).kind(),
                        SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
                    )
                {
                    init.ref_(self).as_binary_expression().right
                } else {
                    init
                },
                is_prototype_assignment,
                self,
            )
            .is_some();
        }
        false
    }

    pub(super) fn get_parent_of_binary_expression(&self, mut expr: Id<Node>) -> Id<Node> {
        while is_binary_expression(&expr.ref_(self).parent().ref_(self)) {
            expr = expr.ref_(self).parent();
        }
        expr.ref_(self).parent()
    }

    pub(super) fn lookup_symbol_for_property_access(
        &self,
        node: Id<Node>, /*BindableStaticNameExpression*/
        lookup_container: Option<Id<Node>>,
    ) -> Option<Id<Symbol>> {
        let lookup_container = lookup_container.unwrap_or_else(|| self.container());
        if is_identifier(&node.ref_(self)) {
            lookup_symbol_for_name(self, lookup_container, &node.ref_(self).as_identifier().escaped_text)
        } else {
            let symbol = self.lookup_symbol_for_property_access(
                node.ref_(self).as_has_expression().expression(),
                None,
            );
            symbol
                .and_then(|symbol| symbol.ref_(self).maybe_exports().clone())
                .and_then(|exports| {
                    get_element_or_property_access_name(node, self)
                        .and_then(|name| exports.ref_(self).get(&*name).cloned())
                })
        }
    }

    pub(super) fn for_each_identifier_in_entity_name(
        &self,
        e: Id<Node>, /*BindableStaticNameExpression*/
        parent: Option<Id<Symbol>>,
        action: &mut impl FnMut(
            Id<Node>, /*Declaration*/
            Option<Id<Symbol>>,
            Option<Id<Symbol>>,
        ) -> Option<Id<Symbol>>,
    ) -> Option<Id<Symbol>> {
        if is_exports_or_module_exports_or_alias(self, self.file(), e) {
            self.file().ref_(self).maybe_symbol()
        } else if is_identifier(&e.ref_(self)) {
            action(
                e,
                self.lookup_symbol_for_property_access(e, Option::<Id<Node>>::None),
                parent,
            )
        } else {
            let s = self.for_each_identifier_in_entity_name(
                e.ref_(self).as_has_expression().expression(),
                parent,
                action,
            );
            let name = get_name_or_argument(&e.ref_(self));
            if is_private_identifier(&name.ref_(self)) {
                Debug_.fail(Some("unexpected PrivateIdentifier"));
            }
            action(
                name,
                s.and_then(|s| s.ref_(self).maybe_exports().clone())
                    .and_then(|exports| {
                        get_element_or_property_access_name(e, self)
                            .and_then(|name| exports.ref_(self).get(&*name).cloned())
                    }),
                s,
            )
        }
    }

    pub(super) fn bind_call_expression(&self, node: Id<Node> /*CallExpression*/) {
        if self
            .file()
            .ref_(self).as_source_file()
            .maybe_common_js_module_indicator()
            .is_none()
            && is_require_call(node, false, self)
        {
            self.set_common_js_module_indicator(node);
        }
    }

    pub(super) fn bind_class_like_declaration(&self, node: Id<Node> /*ClassLikeDeclaration*/) {
        if node.ref_(self).kind() == SyntaxKind::ClassDeclaration {
            self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Class,
                SymbolFlags::ClassExcludes,
            );
        } else {
            let node_ref = node.ref_(self);
            let node_as_class_expression = node_ref.as_class_expression();
            let binding_name = match node_as_class_expression.maybe_name() {
                Some(name) => name.ref_(self).as_identifier().escaped_text.to_owned(),
                None => InternalSymbolName::Class.to_owned(),
            };
            self.bind_anonymous_declaration(node, SymbolFlags::Class, binding_name);
            if let Some(node_name) = node_as_class_expression.maybe_name() {
                self.classifiable_names()
                    .borrow_mut()
                    .insert(node_name.ref_(self).as_identifier().escaped_text.clone());
            }
        }

        let symbol = node.ref_(self).symbol();

        let prototype_symbol = self.alloc_symbol(self.create_symbol(
            SymbolFlags::Property | SymbolFlags::Prototype,
            "prototype".to_owned(),
        ));
        let symbol_exports = symbol.ref_(self).exports();
        let mut symbol_exports = symbol_exports.ref_mut(self);
        let symbol_export = symbol_exports.get(prototype_symbol.ref_(self).escaped_name());
        if let Some(&symbol_export) = symbol_export {
            if let Some(name) = node.ref_(self).as_named_declaration().maybe_name() {
                set_parent(&name.ref_(self), Some(node));
            }
            self.file()
                .ref_(self).as_source_file()
                .bind_diagnostics_mut()
                .push(self.alloc_diagnostic(
                    self.create_diagnostic_for_node(
                        symbol_export
                            .ref_(self)
                            .maybe_declarations()
                            .as_ref()
                            .unwrap()[0],
                        &Diagnostics::Duplicate_identifier_0,
                        Some(vec![symbol_name(prototype_symbol, self)]),
                    )
                    .into(),
                ));
        }
        symbol_exports.insert(
            prototype_symbol.ref_(self).escaped_name().to_owned(),
            prototype_symbol.clone(),
        );
        prototype_symbol.ref_(self).set_parent(Some(symbol));
    }

    pub(super) fn bind_enum_declaration(&self, node: Id<Node> /*EnumDeclaration*/) {
        if is_enum_const(node, self) {
            self.bind_block_scoped_declaration(
                node,
                SymbolFlags::ConstEnum,
                SymbolFlags::ConstEnumExcludes,
            );
        } else {
            self.bind_block_scoped_declaration(
                node,
                SymbolFlags::RegularEnum,
                SymbolFlags::RegularEnumExcludes,
            );
        }
    }

    pub(super) fn bind_variable_declaration_or_binding_element(
        &self,
        node: Id<Node>, /*VariableDeclaration*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_named_declaration = node_ref.as_named_declaration();
        if self.maybe_in_strict_mode() == Some(true) {
            self.check_strict_mode_eval_or_arguments(node, Some(node_as_named_declaration.name()));
        }

        if !is_binding_pattern(Some(&node_as_named_declaration.name().ref_(self))) {
            if is_in_js_file(Some(&node.ref_(self)))
                && is_require_variable_declaration(node, self)
                && get_jsdoc_type_tag(node, self).is_none()
            {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::Alias,
                    SymbolFlags::AliasExcludes,
                );
            } else if is_block_or_catch_scoped(node, self) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else if is_parameter_declaration(node, self) {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::ParameterExcludes,
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

    pub(super) fn bind_parameter(&self, node: Id<Node> /*ParameterDeclaration*/) {
        if node.ref_(self).kind() == SyntaxKind::JSDocParameterTag
            && self.container().ref_(self).kind() != SyntaxKind::JSDocSignature
        {
            return;
        }
        let node_name = match node.ref_(self).kind() {
            SyntaxKind::JSDocParameterTag => Some(node.ref_(self).as_jsdoc_property_like_tag().name),
            _ => node.ref_(self).as_named_declaration().maybe_name(),
        };
        if self.maybe_in_strict_mode() == Some(true)
            && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
        {
            self.check_strict_mode_eval_or_arguments(node, node_name);
        }

        if is_binding_pattern(node_name.refed(self).as_deref()) {
            self.bind_anonymous_declaration(
                node,
                SymbolFlags::FunctionScopedVariable,
                format!(
                    "__{}",
                    index_of_eq(
                        &*node.ref_(self).parent().ref_(self).as_signature_declaration().parameters().ref_(self),
                        &node,
                    )
                ),
            );
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::FunctionScopedVariable,
                SymbolFlags::ParameterExcludes,
            );
        }

        if is_parameter_property_declaration(node, node.ref_(self).parent(), self) {
            let class_declaration = node.ref_(self).parent().ref_(self).parent();
            self.declare_symbol(
                &mut class_declaration.ref_(self).symbol().ref_(self).members().ref_mut(self),
                Some(class_declaration.ref_(self).symbol()),
                node,
                SymbolFlags::Property
                    | if node.ref_(self).as_parameter_declaration().question_token.is_some() {
                        SymbolFlags::Optional
                    } else {
                        SymbolFlags::None
                    },
                SymbolFlags::PropertyExcludes,
                None,
                None,
            );
        }
    }

    pub(super) fn bind_function_declaration(&self, node: Id<Node> /*FunctionDeclaration*/) {
        if !self.file().ref_(self).as_source_file().is_declaration_file()
            && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
        {
            if is_async_function(node, self) {
                self.set_emit_flags(Some(self.emit_flags() | NodeFlags::HasAsyncFunctions));
            }
        }

        self.check_strict_mode_function_name(node);
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            self.check_strict_mode_function_declaration(node);
            self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::Function,
                SymbolFlags::FunctionExcludes,
            );
        }
    }

    pub(super) fn bind_function_expression(
        &self,
        node: Id<Node>, /*FunctionExpression (actually also ArrowFunction)*/
    ) -> Id<Symbol> {
        if !self.file().ref_(self).as_source_file().is_declaration_file()
            && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
        {
            if is_async_function(node, self) {
                self.set_emit_flags(Some(self.emit_flags() | NodeFlags::HasAsyncFunctions));
            }
        }
        if let Some(current_flow) = self.maybe_current_flow() {
            node.ref_(self).set_flow_node(Some(current_flow));
        }
        self.check_strict_mode_function_name(node);
        let binding_name = match node.ref_(self).as_named_declaration().maybe_name() {
            Some(name) => name.ref_(self).as_identifier().escaped_text.clone(),
            None => InternalSymbolName::Function.to_owned(),
        };
        self.bind_anonymous_declaration(node, SymbolFlags::Function, binding_name)
    }

    pub(super) fn bind_property_or_method_or_accessor(
        &self,
        node: Id<Node>,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Id<Symbol>> {
        if !self.file().ref_(self).as_source_file().is_declaration_file()
            && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
            && is_async_function(node, self)
        {
            self.set_emit_flags(Some(self.emit_flags() | NodeFlags::HasAsyncFunctions));
        }

        if let Some(current_flow) = self.maybe_current_flow() {
            if is_object_literal_or_class_expression_method_or_accessor(node, self) {
                node.ref_(self).set_flow_node(Some(current_flow));
            }
        }

        if has_dynamic_name(node, self) {
            Some(self.bind_anonymous_declaration(
                node,
                symbol_flags,
                InternalSymbolName::Computed.to_owned(),
            ))
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes)
        }
    }

    pub(super) fn get_infer_type_container(
        &self,
        node: Id<Node>,
    ) -> Option<Id<Node /*ConditionalTypeNode*/>> {
        let extends_type = find_ancestor(
            Some(node),
            |n| matches!(
                n.ref_(self).maybe_parent(),
                Some(parent) if is_conditional_type_node(&parent.ref_(self))
                    && parent.ref_(self).as_conditional_type_node().extends_type == n
            ),
            self,
        );
        extends_type.map(|extends_type| extends_type.ref_(self).parent())
    }

    pub(super) fn bind_type_parameter(&self, node: Id<Node> /*TypeParameterDeclaration*/) {
        if is_jsdoc_template_tag(&node.ref_(self).parent().ref_(self)) {
            let container = get_effective_container_for_jsdoc_template_tag(node.ref_(self).parent(), self);
            if let Some(container) = container {
                let container_ref = container.ref_(self);
                let mut container_locals = container_ref.maybe_locals_mut();
                if container_locals.is_none() {
                    *container_locals = Some(self.alloc_symbol_table(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    )));
                }
                self.declare_symbol(
                    &mut container_locals.as_ref().unwrap().ref_mut(self),
                    Option::<Id<Symbol>>::None,
                    node,
                    SymbolFlags::TypeParameter,
                    SymbolFlags::TypeParameterExcludes,
                    None,
                    None,
                );
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::TypeParameter,
                    SymbolFlags::TypeParameterExcludes,
                );
            }
        } else if node.ref_(self).parent().ref_(self).kind() == SyntaxKind::InferType {
            let container = self.get_infer_type_container(node.ref_(self).parent());
            if let Some(container) = container {
                let container_ref = container.ref_(self);
                let mut container_locals = container_ref.maybe_locals_mut();
                if container_locals.is_none() {
                    *container_locals = Some(self.alloc_symbol_table(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    )));
                }
                self.declare_symbol(
                    &mut container_locals.as_ref().unwrap().ref_mut(self),
                    Option::<Id<Symbol>>::None,
                    node,
                    SymbolFlags::TypeParameter,
                    SymbolFlags::TypeParameterExcludes,
                    None,
                    None,
                );
            } else {
                self.bind_anonymous_declaration(
                    node,
                    SymbolFlags::TypeParameter,
                    self.get_declaration_name(node).unwrap().into_owned(),
                );
            }
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }

    pub(super) fn should_report_error_on_module_declaration(
        &self,
        node: Id<Node>, /*ModuleDeclaration*/
    ) -> bool {
        let instance_state = get_module_instance_state(node, None, self);
        instance_state == ModuleInstanceState::Instantiated
            || instance_state == ModuleInstanceState::ConstEnumOnly
                && should_preserve_const_enums(&self.options().ref_(self))
    }

    pub(super) fn check_unreachable(&self, node: Id<Node>) -> bool {
        if !self
            .current_flow()
            .ref_(self).flags()
            .intersects(FlowFlags::Unreachable)
        {
            return false;
        }
        if self.current_flow() == self.unreachable_flow() {
            let report_error = is_statement_but_not_declaration(&node.ref_(self))
                && node.ref_(self).kind() != SyntaxKind::EmptyStatement
                || node.ref_(self).kind() == SyntaxKind::ClassDeclaration
                || node.ref_(self).kind() == SyntaxKind::ModuleDeclaration
                    && self.should_report_error_on_module_declaration(node);

            if report_error {
                self.set_current_flow(Some(self.reported_unreachable_flow()));

                if !matches!(self.options().ref_(self).allow_unreachable_code, Some(true)) {
                    let is_error = unreachable_code_is_error(&self.options().ref_(self))
                        && !node.ref_(self).flags().intersects(NodeFlags::Ambient)
                        && (!is_variable_statement(&node.ref_(self))
                            || get_combined_node_flags(
                                node.ref_(self).as_variable_statement().declaration_list,
                                self,
                            )
                            .intersects(NodeFlags::BlockScoped)
                            || node
                                .ref_(self).as_variable_statement()
                                .declaration_list
                                .ref_(self).as_variable_declaration_list()
                                .declarations
                                .ref_(self).iter()
                                .any(|d| {
                                    d.ref_(self).as_variable_declaration().maybe_initializer().is_some()
                                }));

                    each_unreachable_range(node, |start, end| {
                        self.error_or_suggestion_on_range(
                            is_error,
                            start,
                            end,
                            &Diagnostics::Unreachable_code_detected,
                        )
                    }, self);
                }
            }
        }
        true
    }
}

fn each_unreachable_range(node: Id<Node>, mut cb: impl FnMut(Id<Node>, Id<Node>), arena: &impl HasArena) {
    if is_statement(node, arena) && is_executable_statement(node, arena) && is_block(&node.ref_(arena).parent().ref_(arena)) {
        let node_parent = node.ref_(arena).parent();
        let node_parent_ref = node_parent.ref_(arena);
        let statements = &node_parent_ref.as_block().statements;
        let statements_ref = statements.ref_(arena);
        let slice = slice_after_eq(&statements_ref, &node);
        get_ranges_where(
            slice,
            |&node| is_executable_statement(node, arena),
            |start, after_end| cb(slice[start], slice[after_end - 1]),
        );
    } else {
        cb(node, node);
    }
}

fn is_executable_statement(s: Id<Node> /*Statement*/, arena: &impl HasArena) -> bool {
    !is_function_declaration(&s.ref_(arena))
        && !is_purely_type_declaration(s, arena)
        && !is_enum_declaration(&s.ref_(arena))
        && !(is_variable_statement(&s.ref_(arena))
            && !get_combined_node_flags(s, arena).intersects(NodeFlags::Let | NodeFlags::Const)
            && s.ref_(arena).as_variable_statement()
                .declaration_list
                .ref_(arena).as_variable_declaration_list()
                .declarations
                .ref_(arena).iter()
                .any(|d| d.ref_(arena).as_variable_declaration().maybe_initializer().is_none()))
}

fn is_purely_type_declaration(s: Id<Node> /*Statement*/, arena: &impl HasArena) -> bool {
    match s.ref_(arena).kind() {
        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeAliasDeclaration => true,
        SyntaxKind::ModuleDeclaration => {
            get_module_instance_state(s, None, arena) != ModuleInstanceState::Instantiated
        }
        SyntaxKind::EnumDeclaration => has_syntactic_modifier(s, ModifierFlags::Const, arena),
        _ => false,
    }
}

pub fn is_exports_or_module_exports_or_alias(
    binder: &Binder,
    source_file: Id<Node>, /*SourceFile*/
    mut node: Id<Node>,        /*Expression*/
) -> bool {
    let mut i = 0;
    let mut q = vec![node];
    while !q.is_empty() && i < 100 {
        i += 1;
        node = q.remove(0);
        if is_exports_identifier(&node.ref_(binder)) || is_module_exports_access_expression(node, binder) {
            return true;
        } else if is_identifier(&node.ref_(binder)) {
            let symbol =
                lookup_symbol_for_name(binder, source_file, &node.ref_(binder).as_identifier().escaped_text);
            if let Some(symbol) = symbol {
                if let Some(symbol_value_declaration) = binder
                    .symbol_ref(symbol)
                    .maybe_value_declaration()
                    .filter(|value_declaration| {
                        is_variable_declaration(&value_declaration.ref_(binder))
                            && value_declaration
                                .ref_(binder).as_variable_declaration()
                                .maybe_initializer()
                                .is_some()
                    })
                {
                    let init = symbol_value_declaration
                        .ref_(binder).as_variable_declaration()
                        .maybe_initializer()
                        .unwrap();
                    q.push(init);
                    if is_assignment_expression(init, Some(true), binder) {
                        let init_ref = init.ref_(binder);
                        let init_as_binary_expression = init_ref.as_binary_expression();
                        q.push(init_as_binary_expression.left);
                        q.push(init_as_binary_expression.right);
                    }
                }
            }
        }
    }
    false
}

pub(super) fn lookup_symbol_for_name(
    binder: &Binder,
    container: Id<Node>,
    name: &str, /*__String*/
) -> Option<Id<Symbol>> {
    let container_locals = container.ref_(binder).maybe_locals();
    let local = container_locals
        .and_then(|locals| locals.ref_(binder).get(name).cloned());
    if let Some(local) = local {
        return Some(
            binder
                .symbol_ref(local)
                .maybe_export_symbol()
                .unwrap_or_else(|| local.clone()),
        );
    }
    if is_source_file(&container.ref_(binder)) {
        let container_ref = container.ref_(binder);
        let container_as_source_file = container_ref.as_source_file();
        if let Some(container_js_global_augmentations) = container_as_source_file
            .maybe_js_global_augmentations()
            .as_ref()
        {
            let container_js_global_augmentations = container_js_global_augmentations.ref_(binder);
            if container_js_global_augmentations.contains_key(name) {
                return container_js_global_augmentations.get(name).cloned();
            }
        };
    }
    container
        .ref_(binder).maybe_symbol()
        .and_then(|symbol| binder.symbol_ref(symbol).maybe_exports().clone())
        .and_then(|exports| exports.ref_(binder).get(name).cloned())
}
