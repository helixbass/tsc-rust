use std::{
    borrow::{Borrow, Cow},
    io, ptr,
};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::ResolveNameNameArg;
use crate::{
    add_related_info, break_if_none, concatenate, create_diagnostic_for_node,
    declaration_name_to_string, deduplicate_gc, ends_with, escape_leading_underscores,
    export_assignment_is_alias, find, find_ancestor, get_assignment_declaration_kind,
    get_es_module_interop, get_external_module_import_equals_declaration_expression,
    get_external_module_require_argument, get_immediately_invoked_function_expression,
    get_jsdoc_host, get_leftmost_access_expression, get_mode_for_usage_location,
    get_name_of_declaration, get_root_declaration, get_source_file_of_node, get_text_of_node,
    get_this_container, has_syntactic_modifier, is_access_expression, is_aliasable_expression,
    is_binary_expression, is_binding_element, is_block_or_catch_scoped, is_class_expression,
    is_class_like, is_computed_property_name, is_entity_name, is_entity_name_expression,
    is_export_assignment, is_export_declaration, is_export_specifier, is_function_expression,
    is_function_like, is_function_like_declaration, is_identifier, is_in_js_file,
    is_jsdoc_template_tag, is_jsdoc_type_alias, is_property_access_expression,
    is_property_signature, is_qualified_name, is_require_variable_declaration,
    is_shorthand_ambient_module_symbol, is_source_file, is_source_file_js, is_static,
    is_string_literal_like, is_type_literal_node, is_type_query_node,
    is_valid_type_only_alias_use_site, is_variable_declaration, map, maybe_is_class_like,
    return_ok_default_if_none, should_preserve_const_enums, some, try_find, try_find_last,
    unescape_leading_underscores, AssignmentDeclarationKind, Debug_, Diagnostic, Diagnostics,
    Extension, FindAncestorCallbackReturn, HasArena, HasInitializerInterface, HasTypeInterface,
    InArena, InterfaceTypeInterface, InternalSymbolName, IteratorExt, ModifierFlags, ModuleKind,
    Node, NodeFlags, NodeInterface, OptionTry, Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
    TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn check_symbol_usage_in_expression_context(
        &self,
        symbol: Id<Symbol>,
        name: &str, /*__String*/
        use_site: Id<Node>,
    ) {
        if !is_valid_type_only_alias_use_site(use_site, self) {
            let type_only_declaration = self.get_type_only_alias_declaration(symbol);
            if let Some(type_only_declaration) = type_only_declaration {
                let message = if type_only_declaration.ref_(self).kind()
                    == SyntaxKind::ExportSpecifier
                {
                    &*Diagnostics::_0_cannot_be_used_as_a_value_because_it_was_exported_using_export_type
                } else {
                    &*Diagnostics::_0_cannot_be_used_as_a_value_because_it_was_imported_using_import_type
                };
                let unescaped_name = unescape_leading_underscores(name);
                self.add_type_only_declaration_related_info(
                    self.error(
                        Some(use_site),
                        message,
                        Some(vec![unescaped_name.to_owned()]),
                    ),
                    Some(type_only_declaration),
                    unescaped_name,
                );
            }
        }
    }

    pub(super) fn add_type_only_declaration_related_info(
        &self,
        diagnostic: Gc<Diagnostic>,
        type_only_declaration: Option<Id<Node>>,
        unescaped_name: &str,
    ) -> Gc<Diagnostic> {
        if type_only_declaration.is_none() {
            return diagnostic;
        }
        let type_only_declaration = type_only_declaration.unwrap();
        add_related_info(
            &diagnostic,
            vec![create_diagnostic_for_node(
                type_only_declaration,
                if type_only_declaration.ref_(self).kind() == SyntaxKind::ExportSpecifier {
                    &Diagnostics::_0_was_exported_here
                } else {
                    &Diagnostics::_0_was_imported_here
                },
                Some(vec![unescaped_name.to_owned()]),
            )
            .into()],
        );
        diagnostic
    }

    pub(super) fn get_is_deferred_context(
        &self,
        location: Id<Node>,
        last_location: Option<Id<Node>>,
    ) -> bool {
        if !matches!(
            location.ref_(self).kind(),
            SyntaxKind::ArrowFunction | SyntaxKind::FunctionExpression
        ) {
            return is_type_query_node(&location.ref_(self))
                || ((is_function_like_declaration(location)
                    || location.ref_(self).kind() == SyntaxKind::PropertyDeclaration
                        && !is_static(location, self))
                    && !matches!(
                        last_location,
                        Some(last_location) if matches!(
                            location.ref_(self).as_named_declaration().maybe_name(),
                            Some(name) if last_location == name
                        )
                    ));
        }
        if matches!(
            last_location,
            Some(last_location) if matches!(
                location.ref_(self).as_named_declaration().maybe_name(),
                Some(name) if last_location == name
            )
        ) {
            return false;
        }
        if location
            .ref_(self)
            .as_function_like_declaration()
            .maybe_asterisk_token()
            .is_some()
            || has_syntactic_modifier(location, ModifierFlags::Async, self)
        {
            return true;
        }
        get_immediately_invoked_function_expression(location).is_none()
    }

    pub(super) fn is_self_reference_location(&self, node: Id<Node>) -> bool {
        matches!(
            node.ref_(self).kind(),
            SyntaxKind::FunctionDeclaration
                | SyntaxKind::ClassDeclaration
                | SyntaxKind::InterfaceDeclaration
                | SyntaxKind::EnumDeclaration
                | SyntaxKind::TypeAliasDeclaration
                | SyntaxKind::ModuleDeclaration
        )
    }

    pub(super) fn diagnostic_name<'name_arg>(
        &self,
        name_arg: ResolveNameNameArg<'name_arg>,
    ) -> Cow<'name_arg, str> {
        match name_arg {
            ResolveNameNameArg::Str(name_arg) => unescape_leading_underscores(name_arg).into(),
            ResolveNameNameArg::Node(name_arg) => declaration_name_to_string(Some(name_arg), self),
        }
    }

    pub(super) fn is_type_parameter_symbol_declared_in_container(
        &self,
        symbol: Id<Symbol>,
        container: Id<Node>,
    ) -> bool {
        if let Some(symbol_declarations) = symbol.ref_(self).maybe_declarations().as_deref() {
            for decl in symbol_declarations {
                if decl.ref_(self).kind() == SyntaxKind::TypeParameter {
                    let decl_parent = decl.ref_(self).parent();
                    let parent = if is_jsdoc_template_tag(&decl_parent.ref_(self)) {
                        get_jsdoc_host(decl_parent, self)
                    } else {
                        decl.ref_(self).maybe_parent()
                    };
                    if matches!(parent, Some(parent) if parent == container) {
                        return !(is_jsdoc_template_tag(&decl_parent)
                            && find(
                                decl_parent
                                    .ref_(self)
                                    .parent()
                                    .as_jsdoc()
                                    .tags
                                    .as_deref()
                                    .unwrap(),
                                |tag: &Id<Node>, _| is_jsdoc_type_alias(tag),
                            )
                            .is_some());
                    }
                }
            }
        }

        false
    }

    pub(super) fn check_and_report_error_for_missing_prefix<'name_arg>(
        &self,
        error_location: Id<Node>,
        name: &str, /*__String*/
        name_arg: ResolveNameNameArg<'name_arg>,
    ) -> io::Result<bool> {
        if !is_identifier(&error_location.ref_(self))
            || &error_location.ref_(self).as_identifier().escaped_text != name
            || self.is_type_reference_identifier(error_location)
            || self.is_in_type_query(error_location)
        {
            return Ok(false);
        }

        let container = get_this_container(error_location, false);
        let mut location: Option<Id<Node>> = Some(container.clone());
        while let Some(location_present) = location {
            if maybe_is_class_like(location_present.ref_(self).maybe_parent()) {
                let class_symbol =
                    break_if_none!(self.get_symbol_of_node(location_present.ref_(self).parent())?);

                let constructor_type = self.get_type_of_symbol(class_symbol)?;
                if self.get_property_of_type(constructor_type, name)?.is_some() {
                    self.error(
                        Some(error_location),
                        &Diagnostics::Cannot_find_name_0_Did_you_mean_the_static_member_1_0,
                        Some(vec![
                            self.diagnostic_name(name_arg).into_owned(),
                            self.symbol_to_string_(
                                class_symbol,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                                None,
                            )?,
                        ]),
                    );
                    return Ok(true);
                }

                if location_present == container && !is_static(location_present, self) {
                    let declared_type = self.get_declared_type_of_symbol(class_symbol)?;
                    let instance_type = declared_type
                        .ref_(self)
                        .as_interface_type()
                        .maybe_this_type()
                        .unwrap();
                    if self.get_property_of_type(instance_type, name)?.is_some() {
                        self.error(
                            Some(error_location),
                            &Diagnostics::Cannot_find_name_0_Did_you_mean_the_instance_member_this_0,
                            Some(vec![self.diagnostic_name(name_arg).into_owned()]),
                        );
                        return Ok(true);
                    }
                }
            }

            location = location_present.ref_(self).maybe_parent();
        }
        Ok(false)
    }

    pub(super) fn check_and_report_error_for_extending_interface(
        &self,
        error_location: Id<Node>,
    ) -> io::Result<bool> {
        let expression = self.get_entity_name_for_extending_interface(error_location);
        if let Some(expression) = expression {
            if self
                .resolve_entity_name(
                    expression,
                    SymbolFlags::Interface,
                    Some(true),
                    None,
                    Option::<Id<Node>>::None,
                )?
                .is_some()
            {
                self.error(
                    Some(error_location),
                    &Diagnostics::Cannot_extend_an_interface_0_Did_you_mean_implements,
                    Some(vec![get_text_of_node(expression, None, self).into_owned()]),
                );
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn get_entity_name_for_extending_interface(
        &self,
        node: Id<Node>,
    ) -> Option<Id<Node /*EntityNameExpression*/>> {
        match node.ref_(self).kind() {
            SyntaxKind::Identifier | SyntaxKind::PropertyAccessExpression => node
                .ref_(self)
                .maybe_parent()
                .and_then(|node_parent| self.get_entity_name_for_extending_interface(node_parent)),
            SyntaxKind::ExpressionWithTypeArguments => {
                let node_ref = node.ref_(self);
                let node_as_expression_with_type_arguments =
                    node_ref.as_expression_with_type_arguments();
                if is_entity_name_expression(node_as_expression_with_type_arguments.expression, self) {
                    return Some(node_as_expression_with_type_arguments.expression.clone());
                }
                None
            }
            _ => None,
        }
    }

    pub(super) fn check_and_report_error_for_using_type_as_namespace(
        &self,
        error_location: Id<Node>,
        name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<bool> {
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
                    Option::<Id<Node>>::None,
                    false,
                    None,
                )?,
                None,
            )?;
            let parent = error_location.ref_(self).parent();
            if let Some(symbol) = symbol {
                if is_qualified_name(&parent) {
                    let parent_ref = parent.ref_(self);
                    let parent_as_qualified_name = parent_ref.as_qualified_name();
                    Debug_.assert(
                        parent_as_qualified_name.left == error_location,
                        Some("Should only be resolving left side of qualified name as a namespace"),
                    );
                    let prop_name = &parent_as_qualified_name.right.as_identifier().escaped_text;
                    let prop_type = self.get_property_of_type(
                        self.get_declared_type_of_symbol(symbol)?,
                        prop_name,
                    )?;
                    if prop_type.is_some() {
                        self.error(
                            Some(&*parent),
                            &Diagnostics::Cannot_access_0_1_because_0_is_a_type_but_not_a_namespace_Did_you_mean_to_retrieve_the_type_of_the_property_1_in_0_with_0_1,
                            Some(vec![
                                unescape_leading_underscores(name).to_owned(),
                                unescape_leading_underscores(prop_name).to_owned(),
                            ])
                        );
                        return Ok(true);
                    }
                }
                self.error(
                    Some(error_location),
                    &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_namespace_here,
                    Some(vec![unescape_leading_underscores(name).to_owned()]),
                );
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub(super) fn check_and_report_error_for_using_value_as_type(
        &self,
        error_location: Id<Node>,
        name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<bool> {
        if meaning.intersects(SymbolFlags::Type & !SymbolFlags::Namespace) {
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    !SymbolFlags::Type & SymbolFlags::Value,
                    None,
                    Option::<Id<Node>>::None,
                    false,
                    None,
                )?,
                None,
            )?;
            if matches!(symbol, Some(symbol) if !symbol.ref_(self).flags().intersects(SymbolFlags::Namespace))
            {
                self.error(
                    Some(error_location),
                    &Diagnostics::_0_refers_to_a_value_but_is_being_used_as_a_type_here_Did_you_mean_typeof_0,
                    Some(vec![unescape_leading_underscores(name).to_owned()])
                );
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn is_primitive_type_name(&self, name: &str /*__String*/) -> bool {
        matches!(
            name,
            "any" | "string" | "number" | "boolean" | "never" | "unknown"
        )
    }

    pub(super) fn check_and_report_error_for_exporting_primitive_type(
        &self,
        error_location: Id<Node>,
        name: &str, /*__String*/
    ) -> bool {
        if self.is_primitive_type_name(name)
            && error_location.ref_(self).parent().ref_(self).kind() == SyntaxKind::ExportSpecifier
        {
            self.error(
                Some(error_location),
                &Diagnostics::Cannot_export_0_Only_local_declarations_can_be_exported_from_a_module,
                Some(vec![name.to_owned()]),
            );
            return true;
        }
        false
    }

    pub(super) fn check_and_report_error_for_using_type_as_value(
        &self,
        error_location: Id<Node>,
        name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<bool> {
        if meaning.intersects(SymbolFlags::Value & !SymbolFlags::NamespaceModule) {
            if self.is_primitive_type_name(name) {
                self.error(
                    Some(error_location),
                    &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here,
                    Some(vec![unescape_leading_underscores(name).to_owned()]),
                );
                return Ok(true);
            }
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    SymbolFlags::Type & !SymbolFlags::Value,
                    None,
                    Option::<Id<Node>>::None,
                    false,
                    None,
                )?,
                None,
            )?;
            if let Some(symbol) = symbol {
                if !symbol
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::NamespaceModule)
                {
                    let raw_name = unescape_leading_underscores(name);
                    if self.is_es2015_or_later_constructor_name(name) {
                        self.error(
                            Some(error_location),
                            &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here_Do_you_need_to_change_your_target_library_Try_changing_the_lib_compiler_option_to_es2015_or_later,
                            Some(vec![raw_name.to_owned()])
                        );
                    } else if self.maybe_mapped_type(error_location, symbol)? {
                        let second_arg = if raw_name == "K" { "P" } else { "K" };
                        self.error(
                            Some(error_location),
                            &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here_Did_you_mean_to_use_1_in_0,
                            Some(vec![raw_name.to_owned(), second_arg.to_owned()])
                        );
                    } else {
                        self.error(
                            Some(error_location),
                            &Diagnostics::_0_only_refers_to_a_type_but_is_being_used_as_a_value_here,
                            Some(vec![raw_name.to_owned()]),
                        );
                    }
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    pub(super) fn maybe_mapped_type(&self, node: Id<Node>, symbol: Id<Symbol>) -> io::Result<bool> {
        let container = find_ancestor(
            node.ref_(self).maybe_parent(),
            |n| {
                if is_computed_property_name(&n.ref_(self)) || is_property_signature(&n.ref_(self))
                {
                    false.into()
                } else {
                    if is_type_literal_node(&n.ref_(self)) {
                        true.into()
                    } else {
                        FindAncestorCallbackReturn::Quit
                    }
                }
            },
            self,
        );
        if matches!(container, Some(container) if container.ref_(self).as_type_literal_node().members.len() == 1)
        {
            let type_ = self.get_declared_type_of_symbol(symbol)?;
            return Ok(type_.ref_(self).flags().intersects(TypeFlags::Union)
                && self.all_types_assignable_to_kind(
                    type_,
                    TypeFlags::StringOrNumberLiteral,
                    Some(true),
                )?);
        }
        Ok(false)
    }

    pub(super) fn is_es2015_or_later_constructor_name(&self, n: &str /*__String*/) -> bool {
        matches!(
            n,
            "Promise" | "Symbol" | "Map" | "WeakMap" | "Set" | "WeakSet"
        )
    }

    pub(super) fn check_and_report_error_for_using_namespace_module_as_value(
        &self,
        error_location: Id<Node>,
        name: &str, /*__String*/
        meaning: SymbolFlags,
    ) -> io::Result<bool> {
        if meaning
            .intersects(SymbolFlags::Value & !SymbolFlags::NamespaceModule & !SymbolFlags::Type)
        {
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    SymbolFlags::NamespaceModule & !SymbolFlags::Value,
                    None,
                    Option::<Id<Node>>::None,
                    false,
                    None,
                )?,
                None,
            )?;
            if symbol.is_some() {
                self.error(
                    Some(error_location),
                    &Diagnostics::Cannot_use_namespace_0_as_a_value,
                    Some(vec![unescape_leading_underscores(name).to_owned()]),
                );
                return Ok(true);
            }
        } else if meaning
            .intersects(SymbolFlags::Type & !SymbolFlags::NamespaceModule & !SymbolFlags::Value)
        {
            let symbol = self.resolve_symbol(
                self.resolve_name_(
                    Some(error_location),
                    name,
                    (SymbolFlags::ValueModule | SymbolFlags::NamespaceModule) & !SymbolFlags::Type,
                    None,
                    Option::<Id<Node>>::None,
                    false,
                    None,
                )?,
                None,
            )?;
            if symbol.is_some() {
                self.error(
                    Some(error_location),
                    &Diagnostics::Cannot_use_namespace_0_as_a_type,
                    Some(vec![unescape_leading_underscores(name).to_owned()]),
                );
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn check_resolved_block_scoped_variable(
        &self,
        result: Id<Symbol>,
        error_location: Id<Node>,
    ) -> io::Result<()> {
        Debug_.assert(
            result
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::BlockScopedVariable)
                || result.ref_(self).flags().intersects(SymbolFlags::Class)
                || result.ref_(self).flags().intersects(SymbolFlags::Enum),
            None,
        );
        if result.ref_(self).flags().intersects(
            SymbolFlags::Function | SymbolFlags::FunctionScopedVariable | SymbolFlags::Assignment,
        ) && result.ref_(self).flags().intersects(SymbolFlags::Class)
        {
            return Ok(());
        }
        let result_ref = result.ref_(self);
        let result_declarations = result_ref.maybe_declarations();
        let declaration = result_declarations.as_ref().and_then(|declarations| {
            declarations.iter().find(|d| {
                is_block_or_catch_scoped(d, self)
                    || is_class_like(d)
                    || d.ref_(self).kind() == SyntaxKind::EnumDeclaration
            })
        });

        if declaration.is_none() {
            Debug_.fail(Some(
                "checkResolvedBlockScopedVariable could not find block-scoped declaration",
            ));
        }
        let declaration = declaration.unwrap();

        if !declaration.ref_(self).flags().intersects(NodeFlags::Ambient)
            && !self.is_block_scoped_name_declared_before_use(declaration, error_location)?
        {
            let mut diagnostic_message: Option<Gc<Diagnostic>> = None;
            let declaration_name =
                declaration_name_to_string(get_name_of_declaration(Some(declaration), self), self)
                    .into_owned();
            if result
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::BlockScopedVariable)
            {
                diagnostic_message = Some(self.error(
                    Some(error_location),
                    &Diagnostics::Block_scoped_variable_0_used_before_its_declaration,
                    Some(vec![declaration_name.clone()]),
                ));
            } else if result.ref_(self).flags().intersects(SymbolFlags::Class) {
                diagnostic_message = Some(self.error(
                    Some(error_location),
                    &Diagnostics::Class_0_used_before_its_declaration,
                    Some(vec![declaration_name.clone()]),
                ));
            } else if result
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::RegularEnum)
            {
                diagnostic_message = Some(self.error(
                    Some(error_location),
                    &Diagnostics::Enum_0_used_before_its_declaration,
                    Some(vec![declaration_name.clone()]),
                ));
            } else {
                Debug_.assert(
                    result.ref_(self).flags().intersects(SymbolFlags::ConstEnum),
                    None,
                );
                if should_preserve_const_enums(&self.compiler_options) {
                    diagnostic_message = Some(self.error(
                        Some(error_location),
                        &Diagnostics::Enum_0_used_before_its_declaration,
                        Some(vec![declaration_name.clone()]),
                    ));
                }
            }

            if let Some(diagnostic_message) = diagnostic_message {
                add_related_info(
                    &diagnostic_message,
                    vec![create_diagnostic_for_node(
                        &declaration,
                        &Diagnostics::_0_is_declared_here,
                        Some(vec![declaration_name]),
                    )
                    .into()],
                );
            }
        }

        Ok(())
    }

    pub(super) fn is_same_scope_descendent_of(
        &self,
        initial: Id<Node>,
        parent: Option<Id<Node>>,
        stop_at: Id<Node>,
    ) -> bool {
        if parent.is_none() {
            return false;
        }
        let parent = parent.unwrap();
        find_ancestor(Some(initial), |n| {
            if n == stop_at || is_function_like(Some(n)) {
                FindAncestorCallbackReturn::Quit
            } else {
                (n == parent).into()
            }
        }, self)
        .is_some()
    }

    pub(super) fn get_any_import_syntax(
        &self,
        node: Id<Node>,
    ) -> Option<Id<Node /*AnyImportSyntax*/>> {
        match node.ref_(self).kind() {
            SyntaxKind::ImportEqualsDeclaration => Some(node),
            SyntaxKind::ImportClause => node.ref_(self).maybe_parent(),
            SyntaxKind::NamespaceImport => node.ref_(self).parent().ref_(self).maybe_parent(),
            SyntaxKind::ImportSpecifier => node.ref_(self).parent().ref_(self).parent().ref_(self).maybe_parent(),
            _ => None,
        }
    }

    pub(super) fn get_declaration_of_alias_symbol(
        &self,
        symbol: Id<Symbol>,
    ) -> io::Result<Option<Id<Node /*Declaration*/>>> {
        Ok(symbol
            .ref_(self)
            .maybe_declarations()
            .as_deref()
            .try_and_then(|declarations| {
                try_find_last(declarations, |&declaration, _| {
                    self.is_alias_symbol_declaration(declaration)
                })
            })?
            .cloned())
    }

    pub(super) fn is_alias_symbol_declaration(&self, node: Id<Node>) -> io::Result<bool> {
        Ok(matches!(
            node.ref_(self).kind(),
            SyntaxKind::ImportEqualsDeclaration | SyntaxKind::NamespaceExportDeclaration
        ) || node.ref_(self).kind() == SyntaxKind::ImportClause && node.ref_(self).as_import_clause().name.is_some()
            || matches!(
                node.ref_(self).kind(),
                SyntaxKind::NamespaceImport
                    | SyntaxKind::NamespaceExport
                    | SyntaxKind::ImportSpecifier
                    | SyntaxKind::ExportSpecifier
            )
            || node.ref_(self).kind() == SyntaxKind::ExportAssignment && export_assignment_is_alias(node)
            || is_binary_expression(&node.ref_(self))
                && get_assignment_declaration_kind(node, self)
                    == AssignmentDeclarationKind::ModuleExports
                && export_assignment_is_alias(node)
            || is_access_expression(node) && is_binary_expression(&node.ref_(self).parent().ref_(self)) && {
                let node_parent = node.ref_(self).parent();
                let node_parent_ref = node_parent.ref_(self);
                let node_parent_as_binary_expression = node_parent_ref.as_binary_expression();
                node_parent_as_binary_expression.left == node
                    && node_parent_as_binary_expression.operator_token.ref_(self).kind()
                        == SyntaxKind::EqualsToken
                    && self
                        .is_aliasable_or_js_expression(&node_parent_as_binary_expression.right)?
            }
            || node.ref_(self).kind() == SyntaxKind::ShorthandPropertyAssignment
            || node.ref_(self).kind() == SyntaxKind::PropertyAssignment
                && self
                    .is_aliasable_or_js_expression(node.ref_(self).as_property_assignment().initializer)?
            || is_require_variable_declaration(node, self))
    }

    pub(super) fn is_aliasable_or_js_expression(
        &self,
        e: Id<Node>, /*Expression*/
    ) -> io::Result<bool> {
        Ok(is_aliasable_expression(e)
            || is_function_expression(&e.ref_(self)) && self.is_js_constructor(Some(e))?)
    }

    pub(super) fn get_target_of_import_equals_declaration(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration | VariableDeclaration*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let common_js_property_access = self.get_common_js_property_access(node);
        if let Some(common_js_property_access) = common_js_property_access {
            let common_js_property_access_ref = common_js_property_access.ref_(self);
            let common_js_property_access_as_property_access_expression =
                common_js_property_access_ref.as_property_access_expression();
            let leftmost = get_leftmost_access_expression(
                &common_js_property_access_as_property_access_expression.expression,
            );
            let name = leftmost.ref_(self).as_call_expression().arguments[0];
            return Ok(
                if is_identifier(&common_js_property_access_as_property_access_expression.name) {
                    self.resolve_symbol(
                        self.get_property_of_type(
                            self.resolve_external_module_type_by_literal(name)?,
                            &common_js_property_access_as_property_access_expression
                                .name
                                .as_identifier()
                                .escaped_text,
                        )?,
                        None,
                    )?
                } else {
                    None
                },
            );
        }
        if is_variable_declaration(&node.ref_(self))
            || node.ref_(self).as_import_equals_declaration().module_reference.kind()
                == SyntaxKind::ExternalModuleReference
        {
            let immediate = self.resolve_external_module_name_(
                node,
                &get_external_module_require_argument(node).unwrap_or_else(|| {
                    get_external_module_import_equals_declaration_expression(node)
                }),
                None,
            )?;
            let resolved = self.resolve_external_module_symbol(immediate, None)?;
            self.mark_symbol_of_alias_declaration_if_type_only(
                Some(node),
                immediate,
                resolved,
                false,
            )?;
            return Ok(resolved);
        }
        let resolved = self.get_symbol_of_part_of_right_hand_side_of_import_equals(
            &node.ref_(self).as_import_equals_declaration().module_reference,
            Some(dont_resolve_alias),
        )?;
        self.check_and_report_error_for_resolving_import_alias_to_type_only_symbol(node, resolved)?;
        Ok(resolved)
    }

    pub(super) fn check_and_report_error_for_resolving_import_alias_to_type_only_symbol(
        &self,
        node: Id<Node>, /*ImportEqualsDeclaration*/
        resolved: Option<Id<Symbol>>,
    ) -> io::Result<()> {
        let node_ref = node.ref_(self);
        let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
        if self.mark_symbol_of_alias_declaration_if_type_only(
            Some(node),
            Option::<Id<Symbol>>::None,
            resolved,
            false,
        )? && !node_as_import_equals_declaration.is_type_only
        {
            let type_only_declaration = self
                .get_type_only_alias_declaration(self.get_symbol_of_node(node)?.unwrap())
                .unwrap();
            let is_export = type_only_declaration.kind() == SyntaxKind::ExportSpecifier;
            let message = if is_export {
                &*Diagnostics::An_import_alias_cannot_reference_a_declaration_that_was_exported_using_export_type
            } else {
                &*Diagnostics::An_import_alias_cannot_reference_a_declaration_that_was_imported_using_import_type
            };
            let related_message = if is_export {
                &*Diagnostics::_0_was_exported_here
            } else {
                &*Diagnostics::_0_was_imported_here
            };

            let type_only_declaration_name = &type_only_declaration.as_named_declaration().name();
            let name = unescape_leading_underscores(
                &type_only_declaration_name.as_identifier().escaped_text,
            );
            add_related_info(
                &self.error(
                    Some(&*node_as_import_equals_declaration.module_reference),
                    message,
                    None,
                ),
                vec![create_diagnostic_for_node(
                    &type_only_declaration,
                    related_message,
                    Some(vec![name.to_owned()]),
                )
                .into()],
            );
        }

        Ok(())
    }

    pub(super) fn resolve_export_by_name(
        &self,
        module_symbol: Id<Symbol>,
        name: &str, /*__String*/
        source_node: Option<Id<Node> /*TypeOnlyCompatibleAliasDeclaration*/>,
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let module_symbol_exports = module_symbol.ref_(self).exports();
        let module_symbol_exports = (*module_symbol_exports).borrow();
        let export_value = module_symbol_exports.get(InternalSymbolName::ExportEquals);
        let export_symbol = if let Some(&export_value) = export_value {
            self.get_property_of_type(self.get_type_of_symbol(export_value)?, name)?
        } else {
            module_symbol_exports.get(name).cloned()
        };
        let resolved = self.resolve_symbol(export_symbol, Some(dont_resolve_alias))?;
        self.mark_symbol_of_alias_declaration_if_type_only(
            source_node,
            export_symbol,
            resolved,
            false,
        )?;
        Ok(resolved)
    }

    pub(super) fn is_syntactic_default(&self, node: Id<Node>) -> bool {
        is_export_assignment(node)
            && !matches!(node.as_export_assignment().is_export_equals, Some(true))
            || has_syntactic_modifier(node, ModifierFlags::Default, self)
            || is_export_specifier(node)
    }

    pub(super) fn get_usage_mode_for_expression(
        &self,
        usage: Id<Node>, /*Expression*/
    ) -> Option<ModuleKind> {
        if is_string_literal_like(usage) {
            get_mode_for_usage_location(
                get_source_file_of_node(usage)
                    .as_source_file()
                    .maybe_implied_node_format(),
                usage,
            )
        } else {
            None
        }
    }

    pub(super) fn is_esm_format_import_importing_commonjs_format_file(
        &self,
        usage_mode: Option<ModuleKind>,
        target_mode: Option<ModuleKind>,
    ) -> bool {
        matches!(usage_mode, Some(ModuleKind::ESNext))
            && matches!(target_mode, Some(ModuleKind::CommonJS))
    }

    pub(super) fn is_only_imported_as_default(&self, usage: Id<Node> /*Expression*/) -> bool {
        let usage_mode = self.get_usage_mode_for_expression(usage);
        matches!(usage_mode, Some(ModuleKind::ESNext))
            && ends_with(
                &usage.as_literal_like_node().text(),
                Extension::Json.to_str(),
            )
    }

    pub(super) fn can_have_synthetic_default(
        &self,
        file: Option<Id<Node> /*SourceFile*/>,
        module_symbol: Id<Symbol>,
        dont_resolve_alias: bool,
        usage: Id<Node>, /*Expression*/
    ) -> io::Result<bool> {
        let file = file.map(|file| file.borrow().node_wrapper());
        let usage_mode = file
            .as_ref()
            .and_then(|_| self.get_usage_mode_for_expression(usage));
        if let Some(file) = file.as_ref() {
            if let Some(usage_mode) = usage_mode {
                let result = self.is_esm_format_import_importing_commonjs_format_file(
                    Some(usage_mode),
                    file.as_source_file().maybe_implied_node_format(),
                );
                if usage_mode == ModuleKind::ESNext || result {
                    return Ok(result);
                }
            }
        }
        if !self.allow_synthetic_default_imports {
            return Ok(false);
        }
        if match file.as_ref() {
            None => true,
            Some(file) => file.as_source_file().is_declaration_file(),
        } {
            let default_export_symbol = self.resolve_export_by_name(
                module_symbol,
                InternalSymbolName::Default,
                Option::<Id<Node>>::None,
                true,
            )?;
            if matches!(
                default_export_symbol,
                Some(default_export_symbol) if some(
                    default_export_symbol.ref_(self).maybe_declarations().as_deref(),
                    Some(|declaration: &Id<Node>| self.is_syntactic_default(declaration))
                )
            ) {
                return Ok(false);
            }
            if self
                .resolve_export_by_name(
                    module_symbol,
                    &escape_leading_underscores("__esModule"),
                    Option::<Id<Node>>::None,
                    dont_resolve_alias,
                )?
                .is_some()
            {
                return Ok(false);
            }
            return Ok(true);
        }
        let file = file.unwrap();
        if !is_source_file_js(&file) {
            return Ok(self.has_export_assignment_symbol(module_symbol));
        }
        Ok(file
            .as_source_file()
            .maybe_external_module_indicator()
            .is_none()
            && self
                .resolve_export_by_name(
                    module_symbol,
                    &escape_leading_underscores("__esModule"),
                    Option::<Id<Node>>::None,
                    dont_resolve_alias,
                )?
                .is_none())
    }

    pub(super) fn get_target_of_import_clause(
        &self,
        node: Id<Node>, /*ImportClause*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let node_parent = node.parent();
        let node_parent_as_import_declaration = node_parent.as_import_declaration();
        let module_symbol = return_ok_default_if_none!(self.resolve_external_module_name_(
            node,
            &node_parent_as_import_declaration.module_specifier,
            None,
        )?);
        let export_default_symbol: Option<Id<Symbol>>;
        if is_shorthand_ambient_module_symbol(&module_symbol.ref_(self)) {
            export_default_symbol = Some(module_symbol.clone());
        } else {
            export_default_symbol = self.resolve_export_by_name(
                module_symbol,
                InternalSymbolName::Default,
                Some(node),
                dont_resolve_alias,
            )?;
        }

        let file = module_symbol
            .ref_(self)
            .maybe_declarations()
            .as_ref()
            .and_then(|declarations| {
                declarations
                    .iter()
                    .find(|node| is_source_file(node))
                    .map(Clone::clone)
            });
        let has_default_only =
            self.is_only_imported_as_default(&node_parent_as_import_declaration.module_specifier);
        let has_synthetic_default = self.can_have_synthetic_default(
            file.as_deref(),
            module_symbol,
            dont_resolve_alias,
            &node_parent_as_import_declaration.module_specifier,
        )?;
        if export_default_symbol.is_none() && !has_synthetic_default && !has_default_only {
            if self.has_export_assignment_symbol(module_symbol) {
                let compiler_option_name = if self.module_kind >= ModuleKind::ES2015 {
                    "allowSyntheticDefaultImports"
                } else {
                    "esModuleInterop"
                };
                let module_symbol_exports = module_symbol.ref_(self).exports();
                let module_symbol_exports = (*module_symbol_exports).borrow();
                let export_equals_symbol = *module_symbol_exports
                    .get(InternalSymbolName::ExportEquals)
                    .unwrap();
                let export_assignment = export_equals_symbol.ref_(self).maybe_value_declaration();
                let err = self.error(
                    node.as_import_clause().name.as_deref(),
                    &Diagnostics::Module_0_can_only_be_default_imported_using_the_1_flag,
                    Some(vec![
                        self.symbol_to_string_(
                            module_symbol,
                            Option::<Id<Node>>::None,
                            None,
                            None,
                            None,
                        )?,
                        compiler_option_name.to_owned(),
                    ]),
                );

                if let Some(export_assignment) = export_assignment {
                    add_related_info(
                        &err,
                        vec![
                            create_diagnostic_for_node(
                                &export_assignment,
                                &Diagnostics::This_module_is_declared_with_using_export_and_can_only_be_used_with_a_default_import_when_using_the_0_flag,
                                Some(
                                    vec![
                                        compiler_option_name.to_owned(),
                                    ]
                                )
                            ).into()
                        ]
                    );
                }
            } else {
                self.report_non_default_export(module_symbol, node)?;
            }
        } else if has_synthetic_default || has_default_only {
            let resolved = self
                .resolve_external_module_symbol(Some(module_symbol), Some(dont_resolve_alias))?
                .try_or_else(|| {
                    self.resolve_symbol(Some(module_symbol), Some(dont_resolve_alias))
                })?;
            self.mark_symbol_of_alias_declaration_if_type_only(
                Some(node),
                Some(module_symbol),
                resolved,
                false,
            )?;
            return Ok(resolved);
        }
        self.mark_symbol_of_alias_declaration_if_type_only(
            Some(node),
            export_default_symbol,
            Option::<Id<Symbol>>::None,
            false,
        )?;
        Ok(export_default_symbol)
    }

    pub(super) fn report_non_default_export(
        &self,
        module_symbol: Id<Symbol>,
        node: Id<Node>, /*ImportClause*/
    ) -> io::Result<()> {
        let node_as_import_clause = node.as_import_clause();
        if matches!(
            module_symbol.ref_(self).maybe_exports().as_ref(),
            Some(exports) if (**exports).borrow().contains_key(node.symbol().ref_(self).escaped_name())
        ) {
            self.error(
                node_as_import_clause.name.as_deref(),
                &Diagnostics::Module_0_has_no_default_export_Did_you_mean_to_use_import_1_from_0_instead,
                Some(vec![
                    self.symbol_to_string_(module_symbol, Option::<Id<Node>>::None, None, None, None)?,
                    self.symbol_to_string_(node.symbol(), Option::<Id<Node>>::None, None, None, None)?,
                ])
            );
        } else {
            let diagnostic = self.error(
                node_as_import_clause.name.as_deref(),
                &Diagnostics::Module_0_has_no_default_export,
                Some(vec![self.symbol_to_string_(
                    module_symbol,
                    Option::<Id<Node>>::None,
                    None,
                    None,
                    None,
                )?]),
            );
            let export_star =
                module_symbol
                    .ref_(self)
                    .maybe_exports()
                    .as_ref()
                    .and_then(|exports| {
                        (**exports)
                            .borrow()
                            .get(InternalSymbolName::ExportStar)
                            .cloned()
                    });
            if let Some(export_star) = export_star {
                let default_export = export_star.ref_(self).maybe_declarations().as_deref().try_and_then(|declarations| -> io::Result<_> {
                    Ok(declarations.iter().try_find_(|decl| -> io::Result<_> {
                        Ok(is_export_declaration(decl) && matches!(
                            decl.as_export_declaration().module_specifier.as_ref(),
                            Some(decl_module_specifier) if matches!(
                                self.resolve_external_module_name_(
                                    decl,
                                    decl_module_specifier,
                                    None
                                )?.and_then(|resolved| {
                                    resolved.ref_(self).maybe_exports().as_ref().map(|exports| exports.clone())
                                }),
                                Some(exports) if (*exports).borrow().contains_key(InternalSymbolName::Default)
                            )
                        ))
                    })?.cloned())
                })?;
                if let Some(default_export) = default_export {
                    add_related_info(
                        &diagnostic,
                        vec![create_diagnostic_for_node(
                            &default_export,
                            &Diagnostics::export_Asterisk_does_not_re_export_a_default,
                            None,
                        )
                        .into()],
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_target_of_namespace_import(
        &self,
        node: Id<Node>, /*NamespaceImport*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let module_specifier = node
            .parent()
            .parent()
            .as_import_declaration()
            .module_specifier
            .clone();
        let immediate = self.resolve_external_module_name_(node, &module_specifier, None)?;
        let resolved =
            self.resolve_es_module_symbol(immediate, &module_specifier, dont_resolve_alias, false)?;
        self.mark_symbol_of_alias_declaration_if_type_only(Some(node), immediate, resolved, false)?;
        Ok(resolved)
    }

    pub(super) fn get_target_of_namespace_export(
        &self,
        node: Id<Node>, /*NamespaceExport*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let module_specifier = node
            .parent()
            .as_export_declaration()
            .module_specifier
            .clone();
        let immediate = module_specifier.as_ref().try_and_then(|module_specifier| {
            self.resolve_external_module_name_(node, module_specifier, None)
        })?;
        let resolved = module_specifier.as_ref().try_and_then(|module_specifier| {
            self.resolve_es_module_symbol(immediate, &module_specifier, dont_resolve_alias, false)
        })?;
        self.mark_symbol_of_alias_declaration_if_type_only(Some(node), immediate, resolved, false)?;
        Ok(resolved)
    }

    pub(super) fn combine_value_and_type_symbols(
        &self,
        value_symbol: Id<Symbol>,
        type_symbol: Id<Symbol>,
    ) -> Id<Symbol> {
        if value_symbol == self.unknown_symbol() && type_symbol == self.unknown_symbol() {
            return self.unknown_symbol();
        }
        if value_symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Type | SymbolFlags::Namespace)
        {
            return value_symbol;
        }
        let result = self.alloc_symbol(
            self.create_symbol(
                value_symbol.ref_(self).flags() | type_symbol.ref_(self).flags(),
                value_symbol.ref_(self).escaped_name().to_owned(),
                None,
            )
            .into(),
        );
        result
            .ref_(self)
            .set_declarations(deduplicate_gc(&concatenate(
                value_symbol
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .map_or_else(|| vec![], Clone::clone),
                type_symbol
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .map_or_else(|| vec![], Clone::clone),
            )));
        result.ref_(self).set_parent(
            value_symbol
                .ref_(self)
                .maybe_parent()
                .or_else(|| type_symbol.ref_(self).maybe_parent()),
        );
        if let Some(value_symbol_value_declaration) =
            value_symbol.ref_(self).maybe_value_declaration()
        {
            result
                .ref_(self)
                .set_value_declaration(value_symbol_value_declaration);
        }
        if let Some(type_symbol_members) = type_symbol.ref_(self).maybe_members().as_ref() {
            *result.ref_(self).maybe_members_mut() = Some(Gc::new(GcCell::new(
                (**type_symbol_members).borrow().clone(),
            )));
        }
        if let Some(value_symbol_exports) = value_symbol.ref_(self).maybe_exports().as_ref() {
            *result.ref_(self).maybe_exports_mut() = Some(Gc::new(GcCell::new(
                (**value_symbol_exports).borrow().clone(),
            )));
        }
        result
    }

    pub(super) fn get_export_of_module(
        &self,
        symbol: Id<Symbol>,
        name: Id<Node>,      /*Identifier*/
        specifier: Id<Node>, /*Declaration*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if symbol.ref_(self).flags().intersects(SymbolFlags::Module) {
            let export_symbol = (*self.get_exports_of_symbol(symbol)?)
                .borrow()
                .get(&name.as_identifier().escaped_text)
                .cloned();
            let resolved = self.resolve_symbol(export_symbol, Some(dont_resolve_alias))?;
            self.mark_symbol_of_alias_declaration_if_type_only(
                Some(specifier),
                export_symbol,
                resolved,
                false,
            )?;
            return Ok(resolved);
        }
        Ok(None)
    }

    pub(super) fn get_property_of_variable(
        &self,
        symbol: Id<Symbol>,
        name: &str, /*__String*/
    ) -> io::Result<Option<Id<Symbol>>> {
        if symbol.ref_(self).flags().intersects(SymbolFlags::Variable) {
            let type_annotation = symbol
                .ref_(self)
                .maybe_value_declaration()
                .unwrap()
                .as_variable_declaration()
                .maybe_type()
                .clone();
            if let Some(type_annotation) = type_annotation {
                return self.resolve_symbol(
                    self.get_property_of_type(
                        self.get_type_from_type_node_(&type_annotation)?,
                        name,
                    )?,
                    None,
                );
            }
        }
        Ok(None)
    }

    pub(super) fn get_external_module_member(
        &self,
        node: Id<Node>, /*ImportDeclaration | ExportDeclaration | VariableDeclaration*/
        specifier: Id<Node>, /*ImportOrExportSpecifier | BindingElement | PropertyAccessExpression*/
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let dont_resolve_alias = dont_resolve_alias.unwrap_or(false);
        let module_specifier =
            get_external_module_require_argument(node).unwrap_or_else(|| match node {
                Node::ImportDeclaration(node) => node.module_specifier.clone(),
                Node::ExportDeclaration(node) => node.module_specifier.clone().unwrap(),
                _ => panic!("Expected ImportDeclaration or ExportDeclaration"),
            });
        let module_symbol = self.resolve_external_module_name_(node, &module_specifier, None)?;
        let name: Id<Node> = if !is_property_access_expression(specifier) {
            specifier.as_has_property_name().maybe_property_name()
        } else {
            None
        }
        .unwrap_or_else(|| specifier.as_named_declaration().name());
        if !is_identifier(&name) {
            return Ok(None);
        }
        let name_as_identifier = name.as_identifier();
        let suppress_interop_error = name_as_identifier.escaped_text == InternalSymbolName::Default
            && (matches!(
                self.compiler_options.allow_synthetic_default_imports,
                Some(true)
            ) || matches!(get_es_module_interop(&self.compiler_options), Some(true)));
        let target_symbol = self.resolve_es_module_symbol(
            module_symbol,
            &module_specifier,
            false,
            suppress_interop_error,
        )?;
        if target_symbol.is_none() {
            return Ok(None);
        }
        let target_symbol = target_symbol.unwrap();
        let module_symbol = module_symbol.unwrap();
        if !(&*name_as_identifier.escaped_text).is_empty() {
            if is_shorthand_ambient_module_symbol(&module_symbol.ref_(self)) {
                return Ok(Some(module_symbol));
            }

            let mut symbol_from_variable: Option<Id<Symbol>>;
            if
            /*moduleSymbol &&*/
            module_symbol
                .ref_(self)
                .maybe_exports()
                .as_ref()
                .and_then(|exports| {
                    (**exports)
                        .borrow()
                        .get(InternalSymbolName::ExportEquals)
                        .cloned()
                })
                .is_some()
            {
                symbol_from_variable = self.get_property_of_type_(
                    self.get_type_of_symbol(target_symbol)?,
                    &name_as_identifier.escaped_text,
                    Some(true),
                )?;
            } else {
                symbol_from_variable =
                    self.get_property_of_variable(target_symbol, &name_as_identifier.escaped_text)?;
            }
            symbol_from_variable =
                self.resolve_symbol(symbol_from_variable, Some(dont_resolve_alias))?;

            let mut symbol_from_module =
                self.get_export_of_module(target_symbol, &name, specifier, dont_resolve_alias)?;
            if symbol_from_module.is_none()
                && name_as_identifier.escaped_text == InternalSymbolName::Default
            {
                let file = module_symbol
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|declarations| {
                        declarations
                            .iter()
                            .find(|declaration: &&Id<Node>| is_source_file(declaration))
                            .map(Clone::clone)
                    });
                if self.is_only_imported_as_default(&module_specifier)
                    || self.can_have_synthetic_default(
                        file,
                        module_symbol,
                        dont_resolve_alias,
                        &module_specifier,
                    )?
                {
                    symbol_from_module = self
                        .resolve_external_module_symbol(
                            Some(module_symbol),
                            Some(dont_resolve_alias),
                        )?
                        .try_or_else(|| {
                            self.resolve_symbol(Some(module_symbol), Some(dont_resolve_alias))
                        })?;
                }
            }
            let symbol =
                match (symbol_from_module, symbol_from_variable) {
                    (Some(symbol_from_module), Some(symbol_from_variable))
                        if symbol_from_module != symbol_from_variable =>
                    {
                        Some(self.combine_value_and_type_symbols(
                            symbol_from_variable,
                            symbol_from_module,
                        ))
                    }
                    _ => symbol_from_module.or(symbol_from_variable),
                };
            if symbol.is_none() {
                let module_name = self.get_fully_qualified_name(module_symbol, Some(node))?;
                let declaration_name = declaration_name_to_string(Some(name), self);
                let suggestion =
                    self.get_suggested_symbol_for_nonexistent_module(&name, target_symbol)?;
                if let Some(suggestion) = suggestion {
                    let suggestion_name = self.symbol_to_string_(
                        suggestion,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                        None,
                    )?;
                    let diagnostic = self.error(
                        Some(&*name),
                        &Diagnostics::_0_has_no_exported_member_named_1_Did_you_mean_2,
                        Some(vec![
                            module_name,
                            declaration_name.into_owned(),
                            suggestion_name.clone(),
                        ]),
                    );
                    if let Some(suggestion_value_declaration) =
                        suggestion.ref_(self).maybe_value_declaration()
                    {
                        add_related_info(
                            &diagnostic,
                            vec![create_diagnostic_for_node(
                                &suggestion_value_declaration,
                                &Diagnostics::_0_is_declared_here,
                                Some(vec![suggestion_name]),
                            )
                            .into()],
                        );
                    }
                } else {
                    if matches!(
                        module_symbol.ref_(self).maybe_exports().as_ref(),
                        Some(exports) if (**exports).borrow().contains_key(InternalSymbolName::Default)
                    ) {
                        self.error(
                            Some(&*name),
                            &Diagnostics::Module_0_has_no_exported_member_1_Did_you_mean_to_use_import_1_from_0_instead,
                            Some(vec![
                                module_name,
                                declaration_name.into_owned(),
                            ])
                        );
                    } else {
                        self.report_non_exported_member(
                            node,
                            &name,
                            declaration_name.into_owned(),
                            module_symbol,
                            module_name,
                        )?;
                    }
                }
            }
            return Ok(symbol);
        }
        Ok(None)
    }

    pub(super) fn report_non_exported_member(
        &self,
        node: Id<Node>, /*ImportDeclaration | ExportDeclaration | VariableDeclaration*/
        name: Id<Node>, /*Identifier*/
        declaration_name: String,
        module_symbol: Id<Symbol>,
        module_name: String,
    ) -> io::Result<()> {
        let local_symbol =
            module_symbol
                .ref_(self)
                .maybe_value_declaration()
                .and_then(|value_declaration| {
                    value_declaration
                        .maybe_locals()
                        .as_ref()
                        .and_then(|locals| {
                            (**locals)
                                .borrow()
                                .get(&name.as_identifier().escaped_text)
                                .cloned()
                        })
                });
        let module_symbol_ref = module_symbol.ref_(self);
        let exports = module_symbol_ref.maybe_exports();
        if let Some(local_symbol) = local_symbol {
            let exported_equals_symbol = exports.as_ref().and_then(|exports| {
                (**exports)
                    .borrow()
                    .get(InternalSymbolName::ExportEquals)
                    .cloned()
            });
            if let Some(exported_equals_symbol) = exported_equals_symbol {
                if self
                    .get_symbol_if_same_reference(exported_equals_symbol, local_symbol)?
                    .is_some()
                {
                    self.report_invalid_import_equals_export_member(
                        node,
                        name,
                        declaration_name,
                        module_name,
                    );
                } else {
                    self.error(
                        Some(name),
                        &Diagnostics::Module_0_has_no_exported_member_1,
                        Some(vec![module_name, declaration_name]),
                    );
                }
            } else {
                let exported_symbol =
                    exports.as_ref().try_and_then(|exports| -> io::Result<_> {
                        Ok(try_find(
                            &self.symbols_to_array(&(**exports).borrow()),
                            |&symbol: &Id<Symbol>, _| -> io::Result<_> {
                                Ok(self
                                    .get_symbol_if_same_reference(symbol, local_symbol)?
                                    .is_some())
                            },
                        )?
                        .cloned())
                    })?;
                let diagnostic = if let Some(exported_symbol) = exported_symbol {
                    self.error(
                        Some(name),
                        &Diagnostics::Module_0_declares_1_locally_but_it_is_exported_as_2,
                        Some(vec![
                            module_name,
                            declaration_name.clone(),
                            self.symbol_to_string_(
                                exported_symbol,
                                Option::<Id<Node>>::None,
                                None,
                                None,
                                None,
                            )?,
                        ]),
                    )
                } else {
                    self.error(
                        Some(name),
                        &Diagnostics::Module_0_declares_1_locally_but_it_is_not_exported,
                        Some(vec![module_name, declaration_name.clone()]),
                    )
                };
                if let Some(local_symbol_declarations) =
                    local_symbol.ref_(self).maybe_declarations().as_deref()
                {
                    add_related_info(
                        &diagnostic,
                        map(local_symbol_declarations, |decl: &Id<Node>, index| {
                            create_diagnostic_for_node(
                                decl,
                                if index == 0 {
                                    &Diagnostics::_0_is_declared_here
                                } else {
                                    &Diagnostics::and_here
                                },
                                Some(vec![declaration_name.clone()]),
                            )
                            .into()
                        }),
                    );
                }
            }
        } else {
            self.error(
                Some(name),
                &Diagnostics::Module_0_has_no_exported_member_1,
                Some(vec![module_name, declaration_name]),
            );
        }

        Ok(())
    }

    pub(super) fn report_invalid_import_equals_export_member(
        &self,
        node: Id<Node>, /*ImportDeclaration | ExportDeclaration | VariableDeclaration*/
        name: Id<Node>, /*Identifier*/
        declaration_name: String,
        module_name: String,
    ) {
        if self.module_kind >= ModuleKind::ES2015 {
            let message = if matches!(get_es_module_interop(&self.compiler_options), Some(true)) {
                &*Diagnostics::_0_can_only_be_imported_by_using_a_default_import
            } else {
                &*Diagnostics::_0_can_only_be_imported_by_turning_on_the_esModuleInterop_flag_and_using_a_default_import
            };
            self.error(Some(name), message, Some(vec![declaration_name]));
        } else {
            if is_in_js_file(Some(node)) {
                let message = if matches!(get_es_module_interop(&self.compiler_options), Some(true))
                {
                    &*Diagnostics::_0_can_only_be_imported_by_using_a_require_call_or_by_using_a_default_import
                } else {
                    &*Diagnostics::_0_can_only_be_imported_by_using_a_require_call_or_by_turning_on_the_esModuleInterop_flag_and_using_a_default_import
                };
                self.error(Some(name), message, Some(vec![declaration_name]));
            } else {
                let message = if matches!(get_es_module_interop(&self.compiler_options), Some(true))
                {
                    &*Diagnostics::_0_can_only_be_imported_by_using_import_1_require_2_or_a_default_import
                } else {
                    &*Diagnostics::_0_can_only_be_imported_by_using_import_1_require_2_or_by_turning_on_the_esModuleInterop_flag_and_using_a_default_import
                };
                self.error(
                    Some(name),
                    message,
                    Some(vec![
                        declaration_name.clone(),
                        declaration_name,
                        module_name,
                    ]),
                );
            }
        }
    }

    pub(super) fn get_target_of_import_specifier(
        &self,
        node: Id<Node>, /*ImportSpecifier | BindingElement*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let root = if is_binding_element(node) {
            get_root_declaration(node, self)
        } else {
            node.parent().parent().parent()
        };
        let common_js_property_access = self.get_common_js_property_access(&root);
        let common_js_property_access_is_some = common_js_property_access.is_some();
        let resolved = self.get_external_module_member(
            &root,
            &common_js_property_access.unwrap_or_else(|| node.node_wrapper()),
            Some(dont_resolve_alias),
        )?;
        let name = node
            .as_has_property_name()
            .maybe_property_name()
            .unwrap_or_else(|| node.as_named_declaration().name());
        if common_js_property_access_is_some {
            if let Some(resolved) = resolved {
                if is_identifier(&name) {
                    return self.resolve_symbol(
                        self.get_property_of_type_(
                            self.get_type_of_symbol(resolved)?,
                            &name.as_identifier().escaped_text,
                            None,
                        )?,
                        Some(dont_resolve_alias),
                    );
                }
            }
        }
        self.mark_symbol_of_alias_declaration_if_type_only(
            Some(node),
            Option::<Id<Symbol>>::None,
            resolved,
            false,
        )?;
        Ok(resolved)
    }

    pub(super) fn get_common_js_property_access(
        &self,
        node: Id<Node>,
    ) -> Option<Id<Node /*PropertyAccessExpression*/>> {
        if is_variable_declaration(node) {
            if let Some(node_initializer) = node.as_variable_declaration().maybe_initializer() {
                if is_property_access_expression(&node_initializer) {
                    return Some(node_initializer);
                }
            }
        }
        None
    }

    pub(super) fn get_target_of_namespace_export_declaration(
        &self,
        node: Id<Node>, /*NamespaceExportDeclaration*/
        dont_resolve_alias: bool,
    ) -> io::Result<Id<Symbol>> {
        let resolved = self
            .resolve_external_module_symbol(Some(node.parent().symbol()), Some(dont_resolve_alias))?
            .unwrap();
        self.mark_symbol_of_alias_declaration_if_type_only(
            Some(node),
            Option::<Id<Symbol>>::None,
            Some(resolved),
            false,
        )?;
        Ok(resolved)
    }

    pub(super) fn get_target_of_export_specifier(
        &self,
        node: Id<Node>, /*ExportSpecifier*/
        meaning: SymbolFlags,
        dont_resolve_alias: Option<bool>,
    ) -> io::Result<Option<Id<Symbol>>> {
        let resolved = if node
            .parent()
            .parent()
            .as_export_declaration()
            .module_specifier
            .is_some()
        {
            self.get_external_module_member(&node.parent().parent(), node, dont_resolve_alias)?
        } else {
            let node_as_export_specifier = node.as_export_specifier();
            self.resolve_entity_name(
                &node_as_export_specifier
                    .property_name
                    .clone()
                    .unwrap_or_else(|| node_as_export_specifier.name.clone()),
                meaning,
                Some(false),
                dont_resolve_alias,
                Option::<Id<Node>>::None,
            )?
        };
        self.mark_symbol_of_alias_declaration_if_type_only(
            Some(node),
            Option::<Id<Symbol>>::None,
            resolved,
            false,
        )?;
        Ok(resolved)
    }

    pub(super) fn get_target_of_export_assignment(
        &self,
        node: Id<Node>, /*ExportAssignment | BinaryExpression*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        let expression = if is_export_assignment(node) {
            node.as_export_assignment().expression.clone()
        } else {
            node.as_binary_expression().right.clone()
        };
        let resolved = self.get_target_of_alias_like_expression(&expression, dont_resolve_alias)?;
        self.mark_symbol_of_alias_declaration_if_type_only(
            Some(node),
            Option::<Id<Symbol>>::None,
            resolved,
            false,
        )?;
        Ok(resolved)
    }

    pub(super) fn get_target_of_alias_like_expression(
        &self,
        expression: Id<Node>, /*Expression*/
        dont_resolve_alias: bool,
    ) -> io::Result<Option<Id<Symbol>>> {
        if is_class_expression(expression) {
            return Ok(self
                .check_expression_cached(expression, None)?
                .ref_(self)
                .maybe_symbol());
        }
        if !is_entity_name(expression) && !is_entity_name_expression(expression, self) {
            return Ok(None);
        }
        let alias_like = self.resolve_entity_name(
            expression,
            SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
            Some(true),
            Some(dont_resolve_alias),
            Option::<Id<Node>>::None,
        )?;
        if alias_like.is_some() {
            return Ok(alias_like);
        }
        self.check_expression_cached(expression, None)?;
        Ok((*self.get_node_links(expression))
            .borrow()
            .resolved_symbol
            .clone())
    }
}
