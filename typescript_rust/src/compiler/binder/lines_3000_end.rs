use std::{borrow::Borrow, ptr};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{get_module_instance_state, BinderType, ModuleInstanceState};
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
};

impl BinderType {
    pub(super) fn bind_prototype_assignment(
        &self,
        node: &Node, /*BindableStaticPropertyAssignmentExpression*/
    ) {
        let node_as_binary_expression = node.as_binary_expression();
        set_parent(&node_as_binary_expression.left, Some(node.node_wrapper()));
        set_parent(&node_as_binary_expression.right, Some(node.node_wrapper()));
        self.bind_property_assignment(
            &node_as_binary_expression
                .left
                .as_has_expression()
                .expression(),
            &node_as_binary_expression.left,
            false,
            true,
        );
    }

    pub(super) fn bind_object_define_prototype_property(
        &self,
        node: &Node, /*BindableObjectDefinePropertyCall*/
    ) {
        let namespace_symbol = self.lookup_symbol_for_property_access(
            &node.as_call_expression().arguments[0]
                .as_property_access_expression()
                .expression,
            Option::<&Node>::None,
        );
        if let Some(namespace_symbol) = namespace_symbol {
            if let Some(namespace_symbol_value_declaration) =
                namespace_symbol.ref_(self).maybe_value_declaration()
            {
                self.add_declaration_to_symbol(
                    namespace_symbol,
                    &namespace_symbol_value_declaration,
                    SymbolFlags::Class,
                );
            }
        }
        self.bind_potentially_new_expando_member_to_namespace(node, namespace_symbol, true);
    }

    pub(super) fn bind_prototype_property_assignment(
        &self,
        lhs: &Node, /*BindableStaticAccessExpression*/
        parent: &Node,
    ) {
        let class_prototype = lhs.as_has_expression().expression();
        let constructor_function = class_prototype.as_has_expression().expression();

        set_parent(&constructor_function, Some(class_prototype.clone()));
        set_parent(&class_prototype, Some(lhs.node_wrapper()));
        set_parent(&lhs, Some(parent.node_wrapper()));

        self.bind_property_assignment(&constructor_function, lhs, true, true);
    }

    pub(super) fn bind_object_define_property_assignment(
        &self,
        node: &Node, /*BindableObjectDefinePropertyCall*/
    ) {
        let node_as_call_expression = node.as_call_expression();
        let mut namespace_symbol = self.lookup_symbol_for_property_access(
            &node_as_call_expression.arguments[0],
            Option::<&Node>::None,
        );
        let is_toplevel = node.parent().parent().kind() == SyntaxKind::SourceFile;
        namespace_symbol = self.bind_potentially_missing_namespaces(
            namespace_symbol,
            &node_as_call_expression.arguments[0],
            is_toplevel,
            false,
            false,
        );
        self.bind_potentially_new_expando_member_to_namespace(node, namespace_symbol, false);
    }

    pub(super) fn bind_special_property_assignment(
        &self,
        node: &Node, /*BindablePropertyAssignmentExpression*/
    ) {
        let node_as_binary_expression = node.as_binary_expression();
        let parent_symbol = self
            .lookup_symbol_for_property_access(
                &node_as_binary_expression
                    .left
                    .as_has_expression()
                    .expression(),
                self.maybe_container(),
            )
            .or_else(|| {
                self.lookup_symbol_for_property_access(
                    &node_as_binary_expression
                        .left
                        .as_has_expression()
                        .expression(),
                    self.maybe_block_scope_container(),
                )
            });
        if !is_in_js_file(Some(node))
            && !is_function_symbol(
                parent_symbol
                    .map(|parent_symbol| parent_symbol.ref_(self))
                    .as_deref(),
            )
        {
            return;
        }
        let root_expr = get_leftmost_access_expression(&node_as_binary_expression.left);
        if is_identifier(&root_expr)
            && matches!(
                lookup_symbol_for_name(
                    self,
                    &self.container(),
                    &root_expr.as_identifier().escaped_text
                ),
                Some(symbol) if self.symbol(symbol).flags().intersects(SymbolFlags::Alias)
            )
        {
            return;
        }
        set_parent(&node_as_binary_expression.left, Some(node));
        set_parent(&node_as_binary_expression.right, Some(node));
        if is_identifier(
            &node_as_binary_expression
                .left
                .as_has_expression()
                .expression(),
        ) && matches!(self.maybe_container(), Some(container) if Gc::ptr_eq(&container, &self.file()))
            && is_exports_or_module_exports_or_alias(
                self,
                &self.file(),
                &node_as_binary_expression
                    .left
                    .as_has_expression()
                    .expression(),
            )
        {
            self.bind_exports_property_assignment(node);
        } else if has_dynamic_name(node) {
            self.bind_anonymous_declaration(
                node,
                SymbolFlags::Property | SymbolFlags::Assignment,
                InternalSymbolName::Computed.to_owned(),
            );
            let sym = self.bind_potentially_missing_namespaces(
                parent_symbol,
                &node_as_binary_expression
                    .left
                    .as_has_expression()
                    .expression(),
                self.is_top_level_namespace_assignment(&node_as_binary_expression.left),
                false,
                false,
            );
            self.add_late_bound_assignment_declaration_to_symbol(node, sym);
        } else {
            self.bind_static_property_assignment(cast(
                Some(&*node_as_binary_expression.left),
                |node| is_bindable_static_name_expression(node, None),
            ));
        }
    }

    pub(super) fn bind_static_property_assignment(
        &self,
        node: &Node, /*BindableStaticNameExpression*/
    ) {
        let node_as_has_expression = node.as_has_expression();
        Debug_.assert(!is_identifier(node), None);
        set_parent(&node_as_has_expression.expression(), Some(node));
        self.bind_property_assignment(&node_as_has_expression.expression(), node, false, false);
    }

    pub(super) fn bind_potentially_missing_namespaces(
        &self,
        mut namespace_symbol: Option<Id<Symbol>>,
        entity_name: &Node, /*BindableStaticNameExpression*/
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
                                &mut parent.ref_(self).exports().borrow_mut(),
                                Some(parent),
                                id,
                                flags,
                                exclude_flags,
                                None,
                                None,
                            ))
                        } else {
                            let file = self.file();
                            let mut file_js_global_augmentations =
                                file.as_source_file().maybe_js_global_augmentations();
                            if file_js_global_augmentations.is_none() {
                                *file_js_global_augmentations =
                                    Some(Gc::new(GcCell::new(create_symbol_table(
                                        self.arena(),
                                        Option::<&[Id<Symbol>]>::None,
                                    ))));
                            }
                            Some(self.declare_symbol(
                                &mut file_js_global_augmentations.clone().unwrap().borrow_mut(),
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
                        &namespace_symbol_value_declaration,
                        SymbolFlags::Class,
                    );
                }
            }
        }
        namespace_symbol
    }

    pub(super) fn bind_potentially_new_expando_member_to_namespace(
        &self,
        declaration: &Node, /*BindableStaticAccessExpression | CallExpression*/
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
                        *namespace_symbol_members = Some(Gc::new(GcCell::new(
                            create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                        )));
                    }
                    namespace_symbol_members
                } else {
                    let mut namespace_symbol_exports = namespace_symbol_ref.maybe_exports_mut();
                    if namespace_symbol_exports.is_none() {
                        *namespace_symbol_exports = Some(Gc::new(GcCell::new(
                            create_symbol_table(self.arena(), Option::<&[Id<Symbol>]>::None),
                        )));
                    }
                    namespace_symbol_exports
                };
            let symbol_table = symbol_table.clone().unwrap();
            symbol_table
        };

        let mut includes = SymbolFlags::None;
        let mut excludes = SymbolFlags::None;
        if maybe_is_function_like_declaration(
            get_assigned_expando_initializer(Some(declaration)).as_deref(),
        ) {
            includes = SymbolFlags::Method;
            excludes = SymbolFlags::MethodExcludes;
        } else if is_call_expression(declaration)
            && is_bindable_object_define_property_call(declaration)
        {
            let declaration_as_call_expression = declaration.as_call_expression();
            if some(
                Some(
                    &declaration_as_call_expression.arguments[2]
                        .as_object_literal_expression()
                        .properties,
                ),
                Some(|p: &Gc<Node>| {
                    let id = get_name_of_declaration(Some(&**p));
                    matches!(id, Some(id) if is_identifier(&id) && id_text(&id) == "set")
                }),
            ) {
                includes |= SymbolFlags::SetAccessor | SymbolFlags::Property;
                excludes |= SymbolFlags::SetAccessorExcludes;
            }
            if some(
                Some(
                    &declaration_as_call_expression.arguments[2]
                        .as_object_literal_expression()
                        .properties,
                ),
                Some(|p: &Gc<Node>| {
                    let id = get_name_of_declaration(Some(&**p));
                    matches!(id, Some(id) if is_identifier(&id) && id_text(&id) == "get")
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
            &mut symbol_table.borrow_mut(),
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
        property_access: &Node, /*BindableAccessExpression*/
    ) -> bool {
        if is_binary_expression(&property_access.parent()) {
            self.get_parent_of_binary_expression(&property_access.parent())
                .parent()
                .kind()
                == SyntaxKind::SourceFile
        } else {
            property_access.parent().parent().kind() == SyntaxKind::SourceFile
        }
    }

    pub(super) fn bind_property_assignment(
        &self,
        name: &Node,            /*BindableStaticNameExpression*/
        property_access: &Node, /*BindableStaticNameExpression*/
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
            &property_access.as_has_expression().expression(),
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
        if let Some(node) = node.as_ref() {
            if is_call_expression(node) {
                return get_assigned_expando_initializer(Some(&**node)).is_some();
            }
        }
        let init = node.as_ref().and_then(|node| {
            if is_variable_declaration(&node) {
                node.as_variable_declaration().maybe_initializer()
            } else if is_binary_expression(&node) {
                Some(node.as_binary_expression().right.clone())
            } else if is_property_access_expression(&node) && is_binary_expression(&node.parent()) {
                Some(node.parent().as_binary_expression().right.clone())
            } else {
                None
            }
        });
        let init = init.map(|init| get_right_most_assigned_expression(&init));
        if let Some(init) = init {
            let node = node.unwrap();
            let is_prototype_assignment =
                is_prototype_access(&*if is_variable_declaration(&node) {
                    node.as_variable_declaration().name()
                } else if is_binary_expression(&node) {
                    node.as_binary_expression().left.clone()
                } else {
                    node.node_wrapper()
                });
            return get_expando_initializer(
                if is_binary_expression(&init)
                    && matches!(
                        init.as_binary_expression().operator_token.kind(),
                        SyntaxKind::BarBarToken | SyntaxKind::QuestionQuestionToken
                    )
                {
                    &init.as_binary_expression().right
                } else {
                    &init
                },
                is_prototype_assignment,
            )
            .is_some();
        }
        false
    }

    pub(super) fn get_parent_of_binary_expression(&self, expr: &Node) -> Gc<Node> {
        let mut expr = expr.node_wrapper();
        while is_binary_expression(&expr.parent()) {
            expr = expr.parent();
        }
        expr.parent()
    }

    pub(super) fn lookup_symbol_for_property_access<TLookupContainer: Borrow<Node>>(
        &self,
        node: &Node, /*BindableStaticNameExpression*/
        lookup_container: Option<TLookupContainer>,
    ) -> Option<Id<Symbol>> {
        let lookup_container =
            lookup_container.map(|lookup_container| lookup_container.borrow().node_wrapper());
        let lookup_container = lookup_container.unwrap_or_else(|| self.container());
        if is_identifier(node) {
            lookup_symbol_for_name(self, &lookup_container, &node.as_identifier().escaped_text)
        } else {
            let symbol = self.lookup_symbol_for_property_access(
                &node.as_has_expression().expression(),
                Option::<&Node>::None,
            );
            symbol
                .and_then(|symbol| symbol.ref_(self).maybe_exports().clone())
                .and_then(|exports| {
                    get_element_or_property_access_name(node)
                        .and_then(|name| (*exports).borrow().get(&*name).cloned())
                })
        }
    }

    pub(super) fn for_each_identifier_in_entity_name<
        TAction: FnMut(
            &Node, /*Declaration*/
            Option<Id<Symbol>>,
            Option<Id<Symbol>>,
        ) -> Option<Id<Symbol>>,
    >(
        &self,
        e: &Node, /*BindableStaticNameExpression*/
        parent: Option<Id<Symbol>>,
        action: &mut TAction,
    ) -> Option<Id<Symbol>> {
        if is_exports_or_module_exports_or_alias(self, &self.file(), e) {
            self.file().maybe_symbol()
        } else if is_identifier(e) {
            action(
                e,
                self.lookup_symbol_for_property_access(e, Option::<&Node>::None),
                parent,
            )
        } else {
            let s = self.for_each_identifier_in_entity_name(
                &e.as_has_expression().expression(),
                parent,
                action,
            );
            let name = get_name_or_argument(e);
            if is_private_identifier(&name) {
                Debug_.fail(Some("unexpected PrivateIdentifier"));
            }
            action(
                &name,
                s.and_then(|s| s.ref_(self).maybe_exports().clone())
                    .and_then(|exports| {
                        get_element_or_property_access_name(e)
                            .and_then(|name| (*exports).borrow().get(&*name).cloned())
                    }),
                s,
            )
        }
    }

    pub(super) fn bind_call_expression(&self, node: &Node /*CallExpression*/) {
        if self
            .file()
            .as_source_file()
            .maybe_common_js_module_indicator()
            .is_none()
            && is_require_call(node, false)
        {
            self.set_common_js_module_indicator(node);
        }
    }

    pub(super) fn bind_class_like_declaration(&self, node: &Node /*ClassLikeDeclaration*/) {
        if node.kind() == SyntaxKind::ClassDeclaration {
            self.bind_block_scoped_declaration(
                node,
                SymbolFlags::Class,
                SymbolFlags::ClassExcludes,
            );
        } else {
            let node_as_class_expression = node.as_class_expression();
            let binding_name = match node_as_class_expression.maybe_name() {
                Some(name) => name.as_identifier().escaped_text.to_owned(),
                None => InternalSymbolName::Class.to_owned(),
            };
            self.bind_anonymous_declaration(node, SymbolFlags::Class, binding_name);
            if let Some(node_name) = node_as_class_expression.maybe_name() {
                self.classifiable_names()
                    .borrow_mut()
                    .insert(node_name.as_identifier().escaped_text.clone());
            }
        }

        let symbol = node.symbol();

        let prototype_symbol = self.alloc_symbol(self.create_symbol(
            SymbolFlags::Property | SymbolFlags::Prototype,
            "prototype".to_owned(),
        ));
        let symbol_exports = symbol.ref_(self).exports();
        let mut symbol_exports = symbol_exports.borrow_mut();
        let symbol_export = symbol_exports.get(prototype_symbol.ref_(self).escaped_name());
        if let Some(&symbol_export) = symbol_export {
            if let Some(name) = node.as_named_declaration().maybe_name() {
                set_parent(&name, Some(node));
            }
            self.file()
                .as_source_file()
                .bind_diagnostics_mut()
                .push(Gc::new(
                    self.create_diagnostic_for_node(
                        &symbol_export
                            .ref_(self)
                            .maybe_declarations()
                            .as_ref()
                            .unwrap()[0],
                        &Diagnostics::Duplicate_identifier_0,
                        Some(vec![symbol_name(&prototype_symbol.ref_(self)).into_owned()]),
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

    pub(super) fn bind_enum_declaration(&self, node: &Node /*EnumDeclaration*/) {
        if is_enum_const(node) {
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
        node: &Node, /*VariableDeclaration*/
    ) {
        let node_as_named_declaration = node.as_named_declaration();
        if matches!(self.maybe_in_strict_mode(), Some(true)) {
            self.check_strict_mode_eval_or_arguments(node, Some(node_as_named_declaration.name()));
        }

        if !is_binding_pattern(Some(node_as_named_declaration.name())) {
            if is_in_js_file(Some(node))
                && is_require_variable_declaration(node)
                && get_jsdoc_type_tag(node).is_none()
            {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::Alias,
                    SymbolFlags::AliasExcludes,
                );
            } else if is_block_or_catch_scoped(node) {
                self.bind_block_scoped_declaration(
                    node,
                    SymbolFlags::BlockScopedVariable,
                    SymbolFlags::BlockScopedVariableExcludes,
                );
            } else if is_parameter_declaration(node) {
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

    pub(super) fn bind_parameter(&self, node: &Node /*ParameterDeclaration*/) {
        if node.kind() == SyntaxKind::JSDocParameterTag
            && self.container().kind() != SyntaxKind::JSDocSignature
        {
            return;
        }
        let node_name = match node.kind() {
            SyntaxKind::JSDocParameterTag => Some(node.as_jsdoc_property_like_tag().name.clone()),
            _ => node.as_named_declaration().maybe_name(),
        };
        if matches!(self.maybe_in_strict_mode(), Some(true))
            && !node.flags().intersects(NodeFlags::Ambient)
        {
            self.check_strict_mode_eval_or_arguments(node, node_name.as_deref());
        }

        if is_binding_pattern(node_name.as_deref()) {
            self.bind_anonymous_declaration(
                node,
                SymbolFlags::FunctionScopedVariable,
                format!(
                    "__{}",
                    index_of(
                        &*node.parent().as_signature_declaration().parameters(),
                        &node.node_wrapper(),
                        |a, b| Gc::ptr_eq(a, b)
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

        if is_parameter_property_declaration(node, &node.parent()) {
            let class_declaration = node.parent().parent();
            self.declare_symbol(
                &mut class_declaration.symbol().ref_(self).members().borrow_mut(),
                Some(class_declaration.symbol()),
                node,
                SymbolFlags::Property
                    | if node.as_parameter_declaration().question_token.is_some() {
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

    pub(super) fn bind_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if !self.file().as_source_file().is_declaration_file()
            && !node.flags().intersects(NodeFlags::Ambient)
        {
            if is_async_function(node) {
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
        node: &Node, /*FunctionExpression (actually also ArrowFunction)*/
    ) -> Id<Symbol> {
        if !self.file().as_source_file().is_declaration_file()
            && !node.flags().intersects(NodeFlags::Ambient)
        {
            if is_async_function(node) {
                self.set_emit_flags(Some(self.emit_flags() | NodeFlags::HasAsyncFunctions));
            }
        }
        if let Some(current_flow) = self.maybe_current_flow() {
            node.set_flow_node(Some(current_flow));
        }
        self.check_strict_mode_function_name(node);
        let binding_name = match node.as_named_declaration().maybe_name() {
            Some(name) => name.as_identifier().escaped_text.clone(),
            None => InternalSymbolName::Function.to_owned(),
        };
        self.bind_anonymous_declaration(node, SymbolFlags::Function, binding_name)
    }

    pub(super) fn bind_property_or_method_or_accessor(
        &self,
        node: &Node,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Id<Symbol>> {
        if !self.file().as_source_file().is_declaration_file()
            && !node.flags().intersects(NodeFlags::Ambient)
            && is_async_function(node)
        {
            self.set_emit_flags(Some(self.emit_flags() | NodeFlags::HasAsyncFunctions));
        }

        if let Some(current_flow) = self.maybe_current_flow() {
            if is_object_literal_or_class_expression_method_or_accessor(node) {
                node.set_flow_node(Some(current_flow));
            }
        }

        if has_dynamic_name(node) {
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
        node: &Node,
    ) -> Option<Gc<Node /*ConditionalTypeNode*/>> {
        let extends_type = find_ancestor(
            Some(node),
            |n| matches!(n.maybe_parent(), Some(parent) if is_conditional_type_node(&parent) && ptr::eq(&*parent.as_conditional_type_node().extends_type, n)),
        );
        extends_type.map(|extends_type| extends_type.parent())
    }

    pub(super) fn bind_type_parameter(&self, node: &Node /*TypeParameterDeclaration*/) {
        if is_jsdoc_template_tag(&node.parent()) {
            let container = get_effective_container_for_jsdoc_template_tag(&node.parent());
            if let Some(container) = container {
                let mut container_locals = container.maybe_locals_mut();
                if container_locals.is_none() {
                    *container_locals = Some(Gc::new(GcCell::new(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    ))));
                }
                self.declare_symbol(
                    &mut container_locals.as_ref().unwrap().borrow_mut(),
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
        } else if node.parent().kind() == SyntaxKind::InferType {
            let container = self.get_infer_type_container(&node.parent());
            if let Some(container) = container {
                let mut container_locals = container.maybe_locals_mut();
                if container_locals.is_none() {
                    *container_locals = Some(Gc::new(GcCell::new(create_symbol_table(
                        self.arena(),
                        Option::<&[Id<Symbol>]>::None,
                    ))));
                }
                self.declare_symbol(
                    &mut container_locals.as_ref().unwrap().borrow_mut(),
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
        node: &Node, /*ModuleDeclaration*/
    ) -> bool {
        let instance_state = get_module_instance_state(node, None);
        instance_state == ModuleInstanceState::Instantiated
            || instance_state == ModuleInstanceState::ConstEnumOnly
                && should_preserve_const_enums(&self.options())
    }

    pub(super) fn check_unreachable(&self, node: &Node) -> bool {
        if !self
            .current_flow()
            .flags()
            .intersects(FlowFlags::Unreachable)
        {
            return false;
        }
        if Gc::ptr_eq(&self.current_flow(), &self.unreachable_flow()) {
            let report_error = is_statement_but_not_declaration(node)
                && node.kind() != SyntaxKind::EmptyStatement
                || node.kind() == SyntaxKind::ClassDeclaration
                || node.kind() == SyntaxKind::ModuleDeclaration
                    && self.should_report_error_on_module_declaration(node);

            if report_error {
                self.set_current_flow(Some(self.reported_unreachable_flow()));

                if !matches!(self.options().allow_unreachable_code, Some(true)) {
                    let is_error = unreachable_code_is_error(&self.options())
                        && !node.flags().intersects(NodeFlags::Ambient)
                        && (!is_variable_statement(node)
                            || get_combined_node_flags(
                                &node.as_variable_statement().declaration_list,
                            )
                            .intersects(NodeFlags::BlockScoped)
                            || node
                                .as_variable_statement()
                                .declaration_list
                                .as_variable_declaration_list()
                                .declarations
                                .iter()
                                .any(|d| {
                                    d.as_variable_declaration().maybe_initializer().is_some()
                                }));

                    each_unreachable_range(node, |start, end| {
                        self.error_or_suggestion_on_range(
                            is_error,
                            start,
                            end,
                            &Diagnostics::Unreachable_code_detected,
                        )
                    });
                }
            }
        }
        true
    }
}

fn each_unreachable_range<TCallback: FnMut(&Node, &Node)>(node: &Node, mut cb: TCallback) {
    if is_statement(node) && is_executable_statement(node) && is_block(&node.parent()) {
        let node_parent = node.parent();
        let statements = &node_parent.as_block().statements;
        let slice = slice_after(statements, &node.node_wrapper(), |a, b| Gc::ptr_eq(a, b));
        get_ranges_where(
            slice,
            |node| is_executable_statement(node),
            |start, after_end| cb(&slice[start], &slice[after_end - 1]),
        );
    } else {
        cb(node, node);
    }
}

fn is_executable_statement(s: &Node /*Statement*/) -> bool {
    !is_function_declaration(s)
        && !is_purely_type_declaration(s)
        && !is_enum_declaration(s)
        && !(is_variable_statement(s)
            && !get_combined_node_flags(s).intersects(NodeFlags::Let | NodeFlags::Const)
            && s.as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations
                .iter()
                .any(|d| d.as_variable_declaration().maybe_initializer().is_none()))
}

fn is_purely_type_declaration(s: &Node /*Statement*/) -> bool {
    match s.kind() {
        SyntaxKind::InterfaceDeclaration | SyntaxKind::TypeAliasDeclaration => true,
        SyntaxKind::ModuleDeclaration => {
            get_module_instance_state(s, None) != ModuleInstanceState::Instantiated
        }
        SyntaxKind::EnumDeclaration => has_syntactic_modifier(s, ModifierFlags::Const),
        _ => false,
    }
}

pub fn is_exports_or_module_exports_or_alias(
    binder: &BinderType,
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
            let symbol =
                lookup_symbol_for_name(binder, source_file, &node.as_identifier().escaped_text);
            if let Some(symbol) = symbol {
                if let Some(symbol_value_declaration) = binder
                    .symbol(symbol)
                    .maybe_value_declaration()
                    .filter(|value_declaration| {
                        is_variable_declaration(value_declaration)
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

pub(super) fn lookup_symbol_for_name(
    binder: &BinderType,
    container: &Node,
    name: &str, /*__String*/
) -> Option<Id<Symbol>> {
    let container_locals = container.maybe_locals();
    let local = container_locals
        .as_ref()
        .and_then(|locals| (**locals).borrow().get(name).cloned());
    if let Some(local) = local {
        return Some(
            binder
                .symbol(local)
                .maybe_export_symbol()
                .unwrap_or_else(|| local.clone()),
        );
    }
    if is_source_file(container) {
        let container_as_source_file = container.as_source_file();
        if let Some(container_js_global_augmentations) = container_as_source_file
            .maybe_js_global_augmentations()
            .as_ref()
        {
            let container_js_global_augmentations = (**container_js_global_augmentations).borrow();
            if container_js_global_augmentations.contains_key(name) {
                return container_js_global_augmentations.get(name).cloned();
            }
        }
    }
    container
        .maybe_symbol()
        .and_then(|symbol| binder.symbol(symbol).maybe_exports().clone())
        .and_then(|exports| (*exports).borrow().get(name).cloned())
}
