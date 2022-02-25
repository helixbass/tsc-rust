#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::BinderType;
use crate::{
    cast, create_symbol_table, get_assigned_expando_initializer,
    get_element_or_property_access_name, get_expando_initializer, get_leftmost_access_expression,
    get_name_of_declaration, get_right_most_assigned_expression, has_dynamic_name, id_text,
    is_binary_expression, is_bindable_object_define_property_call,
    is_bindable_static_name_expression, is_call_expression, is_function_like_declaration,
    is_function_symbol, is_in_js_file, is_property_access_expression, is_prototype_access, some,
    Debug_, InternalSymbolName, SyntaxKind, __String, is_assignment_expression, is_binding_pattern,
    is_block_or_catch_scoped, is_exports_identifier, is_identifier,
    is_module_exports_access_expression, is_source_file, is_variable_declaration, set_parent,
    HasInitializerInterface, NamedDeclarationInterface, Node, NodeInterface, Symbol, SymbolFlags,
    SymbolInterface,
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
        if let Some(namespace_symbol) = namespace_symbol.as_ref() {
            if let Some(namespace_symbol_value_declaration) =
                namespace_symbol.maybe_value_declaration()
            {
                self.add_declaration_to_symbol(
                    &namespace_symbol,
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
        if !is_in_js_file(Some(node)) && !is_function_symbol(parent_symbol.clone()) {
            return;
        }
        let root_expr = get_leftmost_access_expression(&node_as_binary_expression.left);
        if is_identifier(&root_expr)
            && matches!(lookup_symbol_for_name(&self.container(), &root_expr.as_identifier().escaped_text), Some(symbol) if symbol.flags().intersects(SymbolFlags::Alias))
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
        ) && matches!(self.maybe_container(), Some(container) if Rc::ptr_eq(&container, &self.file()))
            && is_exports_or_module_exports_or_alias(
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
                InternalSymbolName::Computed(),
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

    pub(super) fn bind_potentially_missing_namespaces<TNamespaceSymbol: Borrow<Symbol>>(
        &self,
        namespace_symbol: Option<TNamespaceSymbol>,
        entity_name: &Node, /*BindableStaticNameExpression*/
        is_toplevel: bool,
        is_prototype_property: bool,
        container_is_class: bool,
    ) -> Option<Rc<Symbol>> {
        let mut namespace_symbol =
            namespace_symbol.map(|namespace_symbol| namespace_symbol.borrow().symbol_wrapper());
        if matches!(namespace_symbol.as_ref(), Some(namespace_symbol) if namespace_symbol.flags().intersects(SymbolFlags::Alias))
        {
            return Some(namespace_symbol.unwrap());
        }
        if is_toplevel && !is_prototype_property {
            let flags = SymbolFlags::Module | SymbolFlags::Assignment;
            let exclude_flags = SymbolFlags::ValueModuleExcludes & !SymbolFlags::Assignment;
            namespace_symbol = self.for_each_identifier_in_entity_name(
                entity_name,
                namespace_symbol,
                |id, symbol, parent| {
                    if let Some(symbol) = symbol {
                        self.add_declaration_to_symbol(&symbol, id, flags);
                        Some(symbol)
                    } else {
                        /*let table =*/
                        if let Some(parent) = parent {
                            Some(self.declare_symbol(
                                &mut parent.exports().borrow_mut(),
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
                                    Some(Rc::new(RefCell::new(create_symbol_table(None))));
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
            if let Some(namespace_symbol) = namespace_symbol.as_ref() {
                if let Some(namespace_symbol_value_declaration) =
                    namespace_symbol.maybe_value_declaration()
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

    pub(super) fn bind_potentially_new_expando_member_to_namespace<
        TNamespaceSymbol: Borrow<Symbol>,
    >(
        &self,
        declaration: &Node, /*BindableStaticAccessExpression | CallExpression*/
        namespace_symbol: Option<TNamespaceSymbol>,
        is_prototype_property: bool,
    ) {
        let namespace_symbol =
            namespace_symbol.map(|namespace_symbol| namespace_symbol.borrow().symbol_wrapper());
        if match namespace_symbol.as_ref() {
            None => true,
            Some(namespace_symbol) => !self.is_expando_symbol(&namespace_symbol),
        } {
            return;
        }
        let namespace_symbol = namespace_symbol.unwrap();

        let symbol_table = if is_prototype_property {
            let mut namespace_symbol_members = namespace_symbol.maybe_members();
            if namespace_symbol_members.is_none() {
                *namespace_symbol_members = Some(Rc::new(RefCell::new(create_symbol_table(None))));
            }
            namespace_symbol_members
        } else {
            let mut namespace_symbol_exports = namespace_symbol.maybe_exports();
            if namespace_symbol_exports.is_none() {
                *namespace_symbol_exports = Some(Rc::new(RefCell::new(create_symbol_table(None))));
            }
            namespace_symbol_exports
        };
        let symbol_table = symbol_table.as_ref().unwrap();

        let mut includes = SymbolFlags::None;
        let mut excludes = SymbolFlags::None;
        if is_function_like_declaration(
            &get_assigned_expando_initializer(Some(declaration)).unwrap(),
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
                Some(|p: &Rc<Node>| {
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
                Some(|p: &Rc<Node>| {
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

    pub(super) fn is_expando_symbol(&self, symbol: &Symbol) -> bool {
        if symbol
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Class | SymbolFlags::NamespaceModule)
        {
            return true;
        }
        let node = symbol.maybe_value_declaration();
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

    pub(super) fn get_parent_of_binary_expression(&self, expr: &Node) -> Rc<Node> {
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
    ) -> Option<Rc<Symbol>> {
        let lookup_container =
            lookup_container.map(|lookup_container| lookup_container.borrow().node_wrapper());
        let lookup_container = lookup_container.unwrap_or_else(|| self.container());
        if is_identifier(node) {
            lookup_symbol_for_name(&lookup_container, &node.as_identifier().escaped_text)
        } else {
            let symbol = self.lookup_symbol_for_property_access(
                &node.as_has_expression().expression(),
                Option::<&Node>::None,
            );
            symbol
                .and_then(|symbol| symbol.maybe_exports().clone())
                .and_then(|exports| {
                    get_element_or_property_access_name(node)
                        .and_then(|name| RefCell::borrow(&*exports).get(&name).map(Clone::clone))
                })
        }
    }

    pub(super) fn for_each_identifier_in_entity_name<
        TParent: Borrow<Symbol>,
        TAction: FnMut(
            &Node, /*Declaration*/
            Option<Rc<Symbol>>,
            Option<Rc<Symbol>>,
        ) -> Option<Rc<Symbol>>,
    >(
        &self,
        e: &Node, /*BindableStaticNameExpression*/
        parent: Option<TParent>,
        action: TAction,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn bind_call_expression(&self, node: &Node /*CallExpression*/) {
        unimplemented!()
    }

    pub(super) fn bind_class_like_declaration(&self, node: &Node /*ClassLikeDeclaration*/) {
        unimplemented!()
    }

    pub(super) fn bind_enum_declaration(&self, node: &Node /*EnumDeclaration*/) {
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

    pub(super) fn bind_function_expression(&self, node: &Node /*FunctionExpression*/) {
        unimplemented!()
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

pub(super) fn lookup_symbol_for_name(container: &Node, name: &__String) -> Option<Rc<Symbol>> {
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
