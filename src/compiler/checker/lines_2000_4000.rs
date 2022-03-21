#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::ptr;
use std::rc::Rc;

use super::ResolveNameNameArg;
use crate::{
    add_related_info, create_diagnostic_for_node, find, find_ancestor,
    get_immediately_invoked_function_expression, get_jsdoc_host, get_text_of_node,
    get_this_container, has_syntactic_modifier, is_class_like, is_computed_property_name,
    is_entity_name_expression, is_function_like_declaration, is_identifier, is_in_js_file,
    is_jsdoc_template_tag, is_jsdoc_type_alias, is_property_signature, is_qualified_name,
    is_static, is_type_literal_node, is_type_query_node, is_valid_type_only_alias_use_site,
    Diagnostic, DiagnosticMessage, Diagnostics, FindAncestorCallbackReturn, ModifierFlags,
    SyntaxKind, TypeFlags, TypeInterface, __String, declaration_name_to_string,
    get_first_identifier, node_is_missing, unescape_leading_underscores, Debug_, Node,
    NodeInterface, Symbol, SymbolFlags, SymbolInterface, TypeChecker,
};

impl TypeChecker {
    pub(super) fn check_symbol_usage_in_expression_context(
        &self,
        symbol: &Symbol,
        name: &__String,
        use_site: &Node,
    ) {
        if !is_valid_type_only_alias_use_site(use_site) {
            let type_only_declaration = self.get_type_only_alias_declaration(symbol);
            if let Some(type_only_declaration) = type_only_declaration {
                let message = if type_only_declaration.kind() == SyntaxKind::ExportSpecifier {
                    &Diagnostics::_0_cannot_be_used_as_a_value_because_it_was_exported_using_export_type
                } else {
                    &Diagnostics::_0_cannot_be_used_as_a_value_because_it_was_imported_using_import_type
                };
                let unescaped_name = unescape_leading_underscores(name);
                self.add_type_only_declaration_related_info(
                    self.error(Some(use_site), message, Some(vec![unescaped_name.clone()])),
                    Some(type_only_declaration),
                    &unescaped_name,
                );
            }
        }
    }

    pub(super) fn add_type_only_declaration_related_info<
        TTypeOnlyDeclaration: Borrow<Node /*TypeOnlyCompatibleAliasDeclaration*/>,
    >(
        &self,
        diagnostic: Rc<Diagnostic>,
        type_only_declaration: Option<TTypeOnlyDeclaration>,
        unescaped_name: &str,
    ) -> Rc<Diagnostic> {
        if type_only_declaration.is_none() {
            return diagnostic;
        }
        let type_only_declaration = type_only_declaration.unwrap();
        let type_only_declaration = type_only_declaration.borrow();
        add_related_info(
            &diagnostic,
            vec![Rc::new(
                create_diagnostic_for_node(
                    type_only_declaration,
                    if type_only_declaration.kind() == SyntaxKind::ExportSpecifier {
                        &Diagnostics::_0_was_exported_here
                    } else {
                        &Diagnostics::_0_was_imported_here
                    },
                    Some(vec![unescaped_name.to_owned()]),
                )
                .into(),
            )],
        );
        diagnostic
    }

    pub(super) fn get_is_deferred_context<TLastLocation: Borrow<Node>>(
        &self,
        location: &Node,
        last_location: Option<TLastLocation>,
    ) -> bool {
        let last_location =
            last_location.map(|last_location| last_location.borrow().node_wrapper());
        if !matches!(
            location.kind(),
            SyntaxKind::ArrowFunction | SyntaxKind::FunctionExpression
        ) {
            return is_type_query_node(location)
                || ((is_function_like_declaration(location)
                    || location.kind() == SyntaxKind::PropertyDeclaration
                        && !is_static(location))
                    && !matches!(last_location.as_ref(), Some(last_location) if matches!(location.as_named_declaration().maybe_name(), Some(name) if Rc::ptr_eq(last_location, &name))));
        }
        if matches!(last_location.as_ref(), Some(last_location) if matches!(location.as_named_declaration().maybe_name(), Some(name) if Rc::ptr_eq(last_location, &name)))
        {
            return false;
        }
        if location
            .as_function_like_declaration()
            .maybe_asterisk_token()
            .is_some()
            || has_syntactic_modifier(location, ModifierFlags::Async)
        {
            return true;
        }
        get_immediately_invoked_function_expression(location).is_none()
    }

    pub(super) fn is_self_reference_location(&self, node: &Node) -> bool {
        matches!(
            node.kind(),
            SyntaxKind::FunctionDeclaration
                | SyntaxKind::ClassDeclaration
                | SyntaxKind::InterfaceDeclaration
                | SyntaxKind::EnumDeclaration
                | SyntaxKind::TypeAliasDeclaration
                | SyntaxKind::ModuleDeclaration
        )
    }

    pub(super) fn diagnostic_name(&self, name_arg: ResolveNameNameArg) -> Cow<'static, str> {
        match name_arg {
            ResolveNameNameArg::__String(name_arg) => {
                unescape_leading_underscores(&name_arg).into()
            }
            ResolveNameNameArg::Node(name_arg) => declaration_name_to_string(Some(name_arg)),
        }
    }

    pub(super) fn is_type_parameter_symbol_declared_in_container(
        &self,
        symbol: &Symbol,
        container: &Node,
    ) -> bool {
        if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
            for decl in symbol_declarations {
                if decl.kind() == SyntaxKind::TypeParameter {
                    let decl_parent = decl.parent();
                    let parent = if is_jsdoc_template_tag(&decl_parent) {
                        get_jsdoc_host(&decl_parent)
                    } else {
                        decl.maybe_parent()
                    };
                    if matches!(parent, Some(parent) if ptr::eq(&*parent, container)) {
                        return !(is_jsdoc_template_tag(&decl_parent)
                            && find(
                                decl_parent.parent().as_jsdoc().tags.as_deref().unwrap(),
                                |tag: &Rc<Node>, _| is_jsdoc_type_alias(tag),
                            )
                            .is_some());
                    }
                }
            }
        }

        false
    }

    pub(super) fn check_and_report_error_for_missing_prefix(
        &self,
        error_location: &Node,
        name: &__String,
        name_arg: ResolveNameNameArg,
    ) -> bool {
        if !is_identifier(error_location)
            || &error_location.as_identifier().escaped_text != name
            || self.is_type_reference_identifier(error_location)
            || self.is_in_type_query(error_location)
        {
            return false;
        }

        let container = get_this_container(error_location, false);
        let mut location: Option<Rc<Node>> = Some(container.clone());
        while let Some(location_present) = location {
            if is_class_like(&location_present.parent()) {
                let class_symbol = self.get_symbol_of_node(&location_present.parent());
                if class_symbol.is_none() {
                    break;
                }
                let class_symbol = class_symbol.unwrap();

                let constructor_type = self.get_type_of_symbol(&class_symbol);
                if self.get_property_of_type(&constructor_type, name).is_some() {
                    self.error(
                        Some(error_location),
                        &Diagnostics::Cannot_find_name_0_Did_you_mean_the_static_member_1_0,
                        Some(vec![
                            self.diagnostic_name(name_arg).into_owned(),
                            self.symbol_to_string_(
                                &class_symbol,
                                Option::<&Node>::None,
                                None,
                                None,
                                None,
                            ),
                        ]),
                    );
                    return true;
                }

                if Rc::ptr_eq(&location_present, &container) && !is_static(&location_present) {
                    let declared_type = self.get_declared_type_of_symbol(&class_symbol);
                    let instance_type = declared_type.as_base_interface_type().this_type.borrow();
                    let instance_type = instance_type.as_ref().unwrap();
                    if self.get_property_of_type(&instance_type, name).is_some() {
                        self.error(
                            Some(error_location),
                            &Diagnostics::Cannot_find_name_0_Did_you_mean_the_instance_member_this_0,
                            Some(vec![self.diagnostic_name(name_arg).into_owned()]),
                        );
                        return true;
                    }
                }
            }

            location = location_present.maybe_parent();
        }
        false
    }

    pub(super) fn check_and_report_error_for_extending_interface(
        &self,
        error_location: &Node,
    ) -> bool {
        let expression = self.get_entity_name_for_extending_interface(error_location);
        if let Some(expression) = expression {
            if self
                .resolve_entity_name(&expression, SymbolFlags::Interface, Some(true), None)
                .is_some()
            {
                self.error(
                    Some(error_location),
                    &Diagnostics::Cannot_extend_an_interface_0_Did_you_mean_implements,
                    Some(vec![get_text_of_node(&expression, None).into_owned()]),
                );
                return true;
            }
        }
        false
    }

    pub(super) fn get_entity_name_for_extending_interface(
        &self,
        node: &Node,
    ) -> Option<Rc<Node /*EntityNameExpression*/>> {
        match node.kind() {
            SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression => node
                .maybe_parent()
                .and_then(|node_parent| self.get_entity_name_for_extending_interface(&node_parent)),
            SyntaxKind::ExpressionWithTypeArguments => {
                let node_as_expression_with_type_arguments =
                    node.as_expression_with_type_arguments();
                if is_entity_name_expression(&node_as_expression_with_type_arguments.expression) {
                    return Some(node_as_expression_with_type_arguments.expression.clone());
                }
                None
            }
            _ => None,
        }
    }

    pub(super) fn check_and_report_error_for_using_type_as_namespace(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        let namespace_meaning = SymbolFlags::Namespace
            | if is_in_js_file(Some(error_location)) {
                SymbolFlags::Value
            } else {
                SymbolFlags::None
            };
        if meaning == namespace_meaning {
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    SymbolFlags::Type & !namespace_meaning,
                    None,
                    Option::<Rc<Node>>::None,
                    false,
                    None,
                ),
                None,
            );
            let parent = error_location.parent();
            if let Some(symbol) = symbol {
                if is_qualified_name(&parent) {
                    let parent_as_qualified_name = parent.as_qualified_name();
                    Debug_.assert(
                        ptr::eq(&*parent_as_qualified_name.left, error_location),
                        Some("Should only be resolving left side of qualified name as a namespace"),
                    );
                    let prop_name = &parent_as_qualified_name.right.as_identifier().escaped_text;
                    let prop_type = self.get_property_of_type(
                        &self.get_declared_type_of_symbol(&symbol),
                        prop_name,
                    );
                    if prop_type.is_some() {
                        self.error(
                            Some(&*parent),
                            &Diagnostics::Cannot_access_0_1_because_0_is_a_type_but_not_a_namespace_Did_you_mean_to_retrieve_the_type_of_the_property_1_in_0_with_0_1,
                            Some(vec![
                                unescape_leading_underscores(name),
                                unescape_leading_underscores(prop_name),
                            ])
                        );
                        return true;
                    }
                }
                self.error(
                    Some(error_location),
                    &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_namespace_here,
                    Some(vec![unescape_leading_underscores(name)]),
                );
                return true;
            }
        }

        false
    }

    pub(super) fn check_and_report_error_for_using_value_as_type(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        if meaning.intersects(SymbolFlags::Type & !SymbolFlags::Namespace) {
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    !SymbolFlags::Type & SymbolFlags::Value,
                    None,
                    Option::<Rc<Node>>::None,
                    false,
                    None,
                ),
                None,
            );
            if matches!(symbol, Some(symbol) if !symbol.flags().intersects(SymbolFlags::Namespace))
            {
                self.error(
                    Some(error_location),
                    &Diagnostics::_0_refers_to_a_value_but_is_being_used_as_a_type_here_Did_you_mean_typeof_0,
                    Some(vec![unescape_leading_underscores(name)])
                );
                return true;
            }
        }
        false
    }

    pub(super) fn is_primitive_type_name(&self, name: &__String) -> bool {
        matches!(
            &**name,
            "any" | "string" | "number" | "boolean" | "never" | "unknown"
        )
    }

    pub(super) fn check_and_report_error_for_exporting_primitive_type(
        &self,
        error_location: &Node,
        name: &__String,
    ) -> bool {
        if self.is_primitive_type_name(name)
            && error_location.parent().kind() == SyntaxKind::ExportSpecifier
        {
            self.error(
                Some(error_location),
                &Diagnostics::Cannot_export_0_Only_local_declarations_can_be_exported_from_a_module,
                Some(vec![name.clone().into_string()]),
            );
            return true;
        }
        false
    }

    pub(super) fn check_and_report_error_for_using_type_as_value(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        if meaning.intersects(SymbolFlags::Value & !SymbolFlags::NamespaceModule) {
            if self.is_primitive_type_name(name) {
                self.error(
                    Some(error_location),
                    &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here,
                    Some(vec![unescape_leading_underscores(name)]),
                );
                return true;
            }
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    SymbolFlags::Type & !SymbolFlags::Value,
                    None,
                    Option::<Rc<Node>>::None,
                    false,
                    None,
                ),
                None,
            );
            if let Some(symbol) = symbol {
                if !symbol.flags().intersects(SymbolFlags::NamespaceModule) {
                    let raw_name = unescape_leading_underscores(name);
                    if self.is_es2015_or_later_constructor_name(name) {
                        self.error(
                            Some(error_location),
                            &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_es2015_or_later,
                            Some(vec![raw_name])
                        );
                    } else if self.maybe_mapped_type(error_location, &symbol) {
                        let second_arg = if raw_name == "K" { "P" } else { "K" };
                        self.error(
                            Some(error_location),
                            &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here_Did_you_mean_to_use_1_in_0,
                            Some(vec![raw_name, second_arg.to_owned()])
                        );
                    } else {
                        self.error(
                            Some(error_location),
                            &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here,
                            Some(vec![raw_name]),
                        );
                    }
                    return true;
                }
            }
        }
        false
    }

    pub(super) fn maybe_mapped_type(&self, node: &Node, symbol: &Symbol) -> bool {
        let container = find_ancestor(node.maybe_parent(), |n| {
            if is_computed_property_name(n) || is_property_signature(n) {
                false.into()
            } else {
                if is_type_literal_node(n) {
                    true.into()
                } else {
                    FindAncestorCallbackReturn::Quit
                }
            }
        });
        if matches!(container, Some(container) if container.as_type_literal_node().members.len() == 1)
        {
            let type_ = self.get_declared_type_of_symbol(symbol);
            return type_.flags().intersects(TypeFlags::Union)
                && self.all_types_assignable_to_kind(
                    &type_,
                    TypeFlags::StringOrNumberLiteral,
                    Some(true),
                );
        }
        false
    }

    pub(super) fn is_es2015_or_later_constructor_name(&self, n: &__String) -> bool {
        matches!(
            &**n,
            "Promise" | "Symbol" | "Map" | "WeakMap" | "Set" | "WeakSet"
        )
    }

    pub(super) fn check_and_report_error_for_using_namespace_module_as_value(
        &self,
        error_location: &Node,
        name: &__String,
        meaning: SymbolFlags,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_resolved_block_scoped_variable(
        &self,
        result: &Symbol,
        error_location: &Node,
    ) {
        unimplemented!()
    }

    pub(super) fn is_same_scope_descendent_of<TParent: Borrow<Node>>(
        &self,
        initial: &Node,
        parent: Option<TParent>,
        stop_at: &Node,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn resolve_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn resolve_alias(&self, symbol: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn get_type_only_alias_declaration(
        &self,
        symbol: &Symbol,
    ) -> Option<Rc<Node /*TypeOnlyAliasDeclaration*/>> {
        unimplemented!()
    }

    pub(super) fn resolve_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
        meaning: SymbolFlags,
        ignore_errors: Option<bool>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let ignore_errors = ignore_errors.unwrap_or(false);
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        if node_is_missing(Some(name)) {
            return None;
        }

        let symbol: Option<Rc<Symbol>>;
        match name {
            Node::Identifier(name_as_identifier) => {
                let message = if false {
                    unimplemented!()
                } else {
                    self.get_cannot_find_name_diagnostic_for_name(&*get_first_identifier(name))
                };
                let symbol_from_js_prototype: Option<Rc<Symbol>> =
                    if false { unimplemented!() } else { None };
                symbol = self.get_merged_symbol(self.resolve_name_(
                    Some(name),
                    &name_as_identifier.escaped_text,
                    meaning,
                    if ignore_errors || symbol_from_js_prototype.is_some() {
                        None
                    } else {
                        Some(message)
                    },
                    Some(name.node_wrapper()),
                    true,
                    Some(false),
                ));
                if symbol.is_none() {
                    unimplemented!()
                }
            }
            // else if name.kind() == SyntaxKind::QualifiedName
            //     || name.kind() == SyntaxKind::PropertyAccessExpression
            //     unimplemented!()
            _ => Debug_.assert_never(name, Some("Unknown entity name kind.")),
        }
        let symbol = symbol.unwrap();
        if symbol.flags().intersects(meaning) || dont_resolve_alias {
            Some(symbol)
        } else {
            Some(self.resolve_alias(&symbol))
        }
    }

    pub(super) fn resolve_external_module_name_(
        &self,
        location: &Node,
        module_reference_expression: &Node, /*Expression*/
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn resolve_external_module_name_worker(
        &self,
        location: &Node,
        module_reference_expression: &Node, /*Expression*/
        module_not_found_error: Option<&DiagnosticMessage>,
        is_for_augmentation: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let is_for_augmentation = is_for_augmentation.unwrap_or(false);
        unimplemented!()
    }

    pub(super) fn resolve_external_module_symbol<TModuleSymbol: Borrow<Symbol>>(
        &self,
        module_symbol: Option<TModuleSymbol>,
        dont_resolve_alias: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_exports_of_module_as_array(&self, module_symbol: &Symbol) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_get_member_in_module_exports_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn try_get_member_in_module_exports_and_properties_(
        &self,
        member_name: &__String,
        module_symbol: &Symbol,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_merged_symbol<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
    ) -> Option<Rc<Symbol>> {
        symbol.map(|symbol| symbol.borrow().symbol_wrapper())
    }

    pub(super) fn get_symbol_of_node(&self, node: &Node) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(node.maybe_symbol())
    }
}
