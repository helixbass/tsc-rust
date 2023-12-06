use std::{borrow::Borrow, collections::HashMap, io, iter, ptr};

use gc::Gc;
use id_arena::Id;
use itertools::Either;

use super::EmitResolverCreateResolver;
use crate::{
    add_related_info, are_option_gcs_equal, bind_source_file, concatenate,
    create_diagnostic_for_node, escape_leading_underscores, external_helpers_module_name_text,
    for_each_entry_bool, get_all_accessor_declarations, get_declaration_of_kind,
    get_effective_modifier_flags, get_external_module_name, get_factory, get_first_identifier,
    get_parse_tree_node, get_source_file_of_node, has_syntactic_modifier, is_ambient_module,
    is_binding_pattern, is_declaration, is_declaration_readonly, is_effective_external_module,
    is_entity_name, is_enum_const, is_expression, is_external_or_common_js_module,
    is_function_declaration, is_function_like, is_generated_identifier, is_get_accessor,
    is_global_scope_augmentation, is_identifier, is_jsdoc_parameter_tag, is_named_declaration,
    is_private_identifier_class_element_declaration, is_property_access_expression,
    is_property_declaration, is_qualified_name, is_set_accessor, is_string_literal,
    is_type_only_import_or_export_declaration, is_var_const, is_variable_declaration,
    is_variable_like_or_accessor, maybe_get_source_file_of_node, maybe_is_class_like,
    modifier_to_flag, node_can_be_decorated, node_is_present, parse_isolated_entity_name,
    return_ok_default_if_none, should_preserve_const_enums, token_to_string, try_cast,
    try_for_each_child_bool, try_some, Debug_, Diagnostic, Diagnostics, EmitResolver,
    ExternalEmitHelpers, FunctionLikeDeclarationInterface, HasInitializerInterface, LiteralType,
    ModifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeBuilderFlags, NodeCheckFlags,
    NodeFlags, NodeInterface, ObjectFlags, PragmaArgumentName, PragmaName, Signature,
    SignatureKind, StringOrNumber, Symbol, SymbolFlags, SymbolInterface, SymbolTracker, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeInterface, TypeReferenceSerializationKind, UnwrapOrEmpty,
};

impl TypeChecker {
    pub(super) fn is_alias_resolved_to_value(
        &self,
        symbol: Option<impl Borrow<Symbol>>,
    ) -> io::Result<bool> {
        if symbol.is_none() {
            return Ok(false);
        }
        let symbol = symbol.unwrap();
        let symbol = symbol.borrow();
        let ref target = self.resolve_alias(symbol)?;
        if Gc::ptr_eq(target, &self.unknown_symbol()) {
            return Ok(true);
        }
        Ok(target.flags().intersects(SymbolFlags::Value)
            && (should_preserve_const_enums(&self.compiler_options)
                || !self.is_const_enum_or_const_enum_only_module(target)))
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        self.is_const_enum_symbol(s) || s.maybe_const_enum_only_module() == Some(true)
    }

    pub(super) fn is_referenced_alias_declaration(
        &self,
        node: &Node,
        check_children: Option<bool>,
    ) -> io::Result<bool> {
        if self.is_alias_symbol_declaration(node)? {
            let symbol = self.get_symbol_of_node(node)?;
            let links = symbol.as_ref().map(|symbol| self.get_symbol_links(symbol));
            if matches!(
                links.as_ref(),
                Some(links) if (**links).borrow().referenced == Some(true)
            ) {
                return Ok(true);
            }
            let target = (*self.get_symbol_links(symbol.as_ref().unwrap()))
                .borrow()
                .target
                .clone();
            if matches!(
                target.as_ref(),
                Some(target) if get_effective_modifier_flags(node).intersects(ModifierFlags::Export) &&
                    target.flags().intersects(SymbolFlags::Value) && (
                        should_preserve_const_enums(&self.compiler_options) ||
                        !self.is_const_enum_or_const_enum_only_module(target)
                    )
            ) {
                return Ok(true);
            }
        }

        if check_children == Some(true) {
            return try_for_each_child_bool(
                node,
                |node| self.is_referenced_alias_declaration(node, check_children),
                Option::<fn(&NodeArray) -> io::Result<bool>>::None,
            );
        }
        Ok(false)
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> io::Result<bool> {
        if node_is_present(
            node.maybe_as_function_like_declaration()
                .and_then(|node| node.maybe_body()),
        ) {
            if is_get_accessor(node) || is_set_accessor(node) {
                return Ok(false);
            }
            let symbol = self.get_symbol_of_node(node)?;
            let signatures_of_symbol = self.get_signatures_of_symbol(symbol.as_deref())?;
            return Ok(signatures_of_symbol.len() > 1
                || signatures_of_symbol.len() == 1
                    && !matches!(
                        signatures_of_symbol[0].declaration.as_ref(),
                        Some(declaration) if ptr::eq(
                            &**declaration,
                            node,
                        )
                    ));
        }
        Ok(false)
    }

    pub(super) fn is_required_initialized_parameter(
        &self,
        parameter: &Node, /*ParameterDeclaration | JSDocParameterTag*/
    ) -> io::Result<bool> {
        Ok(self.strict_null_checks
            && !self.is_optional_parameter_(parameter)?
            && !is_jsdoc_parameter_tag(parameter)
            && parameter
                .as_parameter_declaration()
                .maybe_initializer()
                .is_some()
            && !has_syntactic_modifier(parameter, ModifierFlags::ParameterPropertyModifier))
    }

    pub(super) fn is_optional_uninitialized_parameter_property(
        &self,
        parameter: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        Ok(self.strict_null_checks
            && self.is_optional_parameter_(parameter)?
            && parameter
                .as_parameter_declaration()
                .maybe_initializer()
                .is_none()
            && has_syntactic_modifier(parameter, ModifierFlags::ParameterPropertyModifier))
    }

    pub(super) fn is_optional_uninitialized_parameter_(
        &self,
        parameter: &Node, /*ParameterDeclaration*/
    ) -> io::Result<bool> {
        Ok(self.strict_null_checks
            && self.is_optional_parameter_(parameter)?
            && parameter
                .as_parameter_declaration()
                .maybe_initializer()
                .is_none())
    }

    pub(super) fn is_expando_function_declaration(
        &self,
        node: &Node, /*Declaration*/
    ) -> io::Result<bool> {
        let ref declaration = return_ok_default_if_none!(get_parse_tree_node(
            Some(node),
            Some(is_function_declaration)
        ));
        let ref symbol = return_ok_default_if_none!(self.get_symbol_of_node(declaration)?);
        if !symbol.flags().intersects(SymbolFlags::Function) {
            return Ok(false);
        }
        Ok(for_each_entry_bool(
            &*(*self.get_exports_of_symbol(symbol)?).borrow(),
            |p: &Gc<Symbol>, _| {
                p.flags().intersects(SymbolFlags::Value)
                    && matches!(
                        p.maybe_value_declaration().as_ref(),
                        Some(p_value_declaration) if is_property_access_expression(p_value_declaration)
                    )
            },
        ))
    }

    pub(super) fn get_properties_of_container_function(
        &self,
        node: &Node, /*Declaration*/
    ) -> io::Result<impl Iterator<Item = Gc<Symbol>>> {
        let declaration = get_parse_tree_node(Some(node), Some(is_function_declaration));
        if declaration.is_none() {
            return Ok(Either::Right(iter::empty()));
        }
        let declaration = declaration.as_ref().unwrap();
        let symbol = self.get_symbol_of_node(declaration)?;
        Ok(if let Some(symbol) = symbol.as_ref() {
            Some(self.get_properties_of_type(self.get_type_of_symbol(symbol)?)?)
        } else {
            None
        }
        .unwrap_or_empty())
    }

    pub(super) fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags {
        let node_id = node.maybe_id().unwrap_or(0);
        /*if (nodeId < 0 || nodeId >= nodeLinks.length) return 0;*/
        self.node_links()
            .get(&node_id)
            .cloned()
            .map_or(NodeCheckFlags::None, |node_links| {
                (*node_links).borrow().flags
            })
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> io::Result<Option<StringOrNumber>> {
        self.compute_enum_member_values(&node.parent())?;
        let ret = (*self.get_node_links(node))
            .borrow()
            .enum_member_value
            .clone();
        Ok(ret)
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        matches!(
            node.kind(),
            SyntaxKind::EnumMember
                | SyntaxKind::PropertyAccessExpression
                | SyntaxKind::ElementAccessExpression
        )
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> io::Result<Option<StringOrNumber>> {
        if node.kind() == SyntaxKind::EnumMember {
            return self.get_enum_member_value(node);
        }

        let symbol = (*self.get_node_links(node))
            .borrow()
            .resolved_symbol
            .clone();
        if let Some(symbol) = symbol
            .as_ref()
            .filter(|symbol| symbol.flags().intersects(SymbolFlags::EnumMember))
        {
            let ref member = symbol.maybe_value_declaration().unwrap();
            if is_enum_const(&member.parent()) {
                return self.get_enum_member_value(member);
            }
        }

        Ok(None)
    }

    pub(super) fn is_function_type(&self, type_: Id<Type>) -> io::Result<bool> {
        Ok(self.type_(type_).flags().intersects(TypeFlags::Object)
            && !self
                .get_signatures_of_type(type_, SignatureKind::Call)?
                .is_empty())
    }

    pub(super) fn get_type_reference_serialization_kind(
        &self,
        type_name_in: &Node, /*EntityName*/
        location: Option<impl Borrow<Node>>,
    ) -> io::Result<TypeReferenceSerializationKind> {
        let type_name = get_parse_tree_node(Some(type_name_in), Some(is_entity_name));
        if type_name.is_none() {
            return Ok(TypeReferenceSerializationKind::Unknown);
        }
        let type_name = type_name.as_ref().unwrap();

        let mut location = location.map(|location| location.borrow().node_wrapper());
        if location.is_some() {
            location = get_parse_tree_node(location.as_deref(), Option::<fn(&Node) -> bool>::None);
            if location.is_none() {
                return Ok(TypeReferenceSerializationKind::Unknown);
            }
        }

        let mut is_type_only = false;
        if is_qualified_name(type_name) {
            let root_value_symbol = self.resolve_entity_name(
                &get_first_identifier(type_name),
                SymbolFlags::Value,
                Some(true),
                Some(true),
                location.as_deref(),
            )?;
            is_type_only = matches!(
                root_value_symbol.as_ref(),
                Some(root_value_symbol) if matches!(
                    root_value_symbol.maybe_declarations().as_ref(),
                    Some(root_value_symbol_declarations) if root_value_symbol_declarations.into_iter().all(
                        |declaration| is_type_only_import_or_export_declaration(declaration)
                    )
                )
            );
        }
        let value_symbol = self.resolve_entity_name(
            type_name,
            SymbolFlags::Value,
            Some(true),
            Some(true),
            location.as_deref(),
        )?;
        let resolved_symbol = if let Some(value_symbol) = value_symbol
            .as_ref()
            .filter(|value_symbol| value_symbol.flags().intersects(SymbolFlags::Alias))
        {
            Some(self.resolve_alias(value_symbol)?)
        } else {
            value_symbol.clone()
        };
        is_type_only = is_type_only
            || matches!(
                value_symbol.as_ref(),
                Some(value_symbol) if matches!(
                    value_symbol.maybe_declarations().as_ref(),
                    Some(value_symbol_declarations) if value_symbol_declarations.into_iter().all(
                        |declaration| is_type_only_import_or_export_declaration(declaration)
                    )
                )
            );

        let type_symbol = self.resolve_entity_name(
            type_name,
            SymbolFlags::Type,
            Some(true),
            Some(false),
            location.as_deref(),
        )?;
        if let Some(resolved_symbol) = resolved_symbol.as_ref().filter(|resolved_symbol| {
            matches!(
                type_symbol.as_ref(),
                Some(type_symbol) if Gc::ptr_eq(
                    *resolved_symbol,
                    type_symbol
                )
            )
        }) {
            let global_promise_symbol = self.get_global_promise_constructor_symbol(false)?;
            if matches!(
                global_promise_symbol.as_ref(),
                Some(global_promise_symbol) if Gc::ptr_eq(
                    resolved_symbol,
                    global_promise_symbol
                )
            ) {
                return Ok(TypeReferenceSerializationKind::Promise);
            }

            let constructor_type = self.get_type_of_symbol(resolved_symbol)?;
            if
            /*constructorType &&*/
            self.is_constructor_type(constructor_type)? {
                return Ok(if is_type_only {
                    TypeReferenceSerializationKind::TypeWithCallSignature
                } else {
                    TypeReferenceSerializationKind::TypeWithConstructSignatureAndValue
                });
            }
        }

        if type_symbol.is_none() {
            return Ok(if is_type_only {
                TypeReferenceSerializationKind::ObjectType
            } else {
                TypeReferenceSerializationKind::Unknown
            });
        }
        let type_symbol = type_symbol.as_ref().unwrap();
        let type_ = self.get_declared_type_of_symbol(type_symbol)?;
        Ok(if self.is_error_type(type_) {
            if is_type_only {
                TypeReferenceSerializationKind::ObjectType
            } else {
                TypeReferenceSerializationKind::Unknown
            }
        } else if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::AnyOrUnknown)
        {
            TypeReferenceSerializationKind::ObjectType
        } else if self.is_type_assignable_to_kind(
            type_,
            TypeFlags::Void | TypeFlags::Nullable | TypeFlags::Never,
            None,
        )? {
            TypeReferenceSerializationKind::VoidNullableOrNeverType
        } else if self.is_type_assignable_to_kind(type_, TypeFlags::BooleanLike, None)? {
            TypeReferenceSerializationKind::BooleanType
        } else if self.is_type_assignable_to_kind(type_, TypeFlags::NumberLike, None)? {
            TypeReferenceSerializationKind::NumberLikeType
        } else if self.is_type_assignable_to_kind(type_, TypeFlags::BigIntLike, None)? {
            TypeReferenceSerializationKind::BigIntLikeType
        } else if self.is_type_assignable_to_kind(type_, TypeFlags::StringLike, None)? {
            TypeReferenceSerializationKind::StringLikeType
        } else if self.is_tuple_type(type_) {
            TypeReferenceSerializationKind::ArrayLikeType
        } else if self.is_type_assignable_to_kind(type_, TypeFlags::ESSymbolLike, None)? {
            TypeReferenceSerializationKind::ESSymbolType
        } else if self.is_function_type(type_)? {
            TypeReferenceSerializationKind::TypeWithCallSignature
        } else if self.is_array_type(type_) {
            TypeReferenceSerializationKind::ArrayLikeType
        } else {
            TypeReferenceSerializationKind::ObjectType
        })
    }

    pub(super) fn create_type_of_declaration(
        &self,
        declaration_in: &Node, /*AccessorDeclaration | VariableLikeDeclaration | PropertyAccessExpression*/
        enclosing_declaration: &Node,
        mut flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
        add_undefined: Option<bool>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        let declaration = get_parse_tree_node(
            Some(declaration_in),
            Some(|node: &Node| is_variable_like_or_accessor(node)),
        );
        if declaration.is_none() {
            return Ok(Some(get_factory().create_token(SyntaxKind::AnyKeyword)));
        }
        let declaration = declaration.as_ref().unwrap();
        let symbol = self.get_symbol_of_node(declaration)?;
        let mut type_ = if let Some(symbol) = symbol.as_ref().filter(|symbol| {
            !symbol
                .flags()
                .intersects(SymbolFlags::TypeLiteral | SymbolFlags::Signature)
        }) {
            self.get_widened_literal_type(self.get_type_of_symbol(symbol)?)?
        } else {
            self.error_type()
        };
        if self
            .type_(type_)
            .flags()
            .intersects(TypeFlags::UniqueESSymbol)
            && are_option_gcs_equal(self.type_(type_).maybe_symbol().as_ref(), symbol.as_ref())
        {
            flags |= NodeBuilderFlags::AllowUniqueESSymbolType;
        }
        if add_undefined == Some(true) {
            type_ = self.get_optional_type_(type_, None)?;
        }
        self.node_builder().type_to_type_node(
            type_,
            Some(enclosing_declaration),
            Some(flags | NodeBuilderFlags::MultilineObjectLiterals),
            Some(tracker),
        )
    }

    pub(super) fn create_return_type_of_signature_declaration(
        &self,
        signature_declaration_in: &Node, /*SignatureDeclaration*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        let signature_declaration = get_parse_tree_node(
            Some(signature_declaration_in),
            Some(|node: &Node| is_function_like(Some(node))),
        );
        if signature_declaration.is_none() {
            return Ok(Some(get_factory().create_token(SyntaxKind::AnyKeyword)));
        }
        let signature_declaration = signature_declaration.as_ref().unwrap();
        let signature = self.get_signature_from_declaration_(signature_declaration)?;
        self.node_builder().type_to_type_node(
            self.get_return_type_of_signature(signature)?,
            Some(enclosing_declaration),
            Some(flags | NodeBuilderFlags::MultilineObjectLiterals),
            Some(tracker),
        )
    }

    pub(super) fn create_type_of_expression(
        &self,
        expr_in: &Node, /*Expression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Option<Gc<Node /*TypeNode*/>>> {
        let expr = get_parse_tree_node(Some(expr_in), Some(|node: &Node| is_expression(node)));
        if expr.is_none() {
            return Ok(Some(get_factory().create_token(SyntaxKind::AnyKeyword)));
        }
        let expr = expr.as_ref().unwrap();
        let type_ = self.get_widened_type(self.get_regular_type_of_expression(expr)?)?;
        self.node_builder().type_to_type_node(
            type_,
            Some(enclosing_declaration),
            Some(flags | NodeBuilderFlags::MultilineObjectLiterals),
            Some(tracker),
        )
    }

    pub(super) fn has_global_name(&self, name: &str) -> bool {
        self.globals()
            .contains_key(&*escape_leading_underscores(name))
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> io::Result<Option<Gc<Symbol>>> {
        let resolved_symbol = (*self.get_node_links(reference))
            .borrow()
            .resolved_symbol
            .clone();
        if resolved_symbol.is_some() {
            return Ok(resolved_symbol);
        }

        let mut location = reference.node_wrapper();
        if start_in_declaration_container == Some(true) {
            let ref parent = reference.parent();
            if is_declaration(parent)
                && matches!(
                    parent.as_named_declaration().maybe_name().as_ref(),
                    Some(parent_name) if ptr::eq(
                        reference,
                        &**parent_name
                    )
                )
            {
                location = self.get_declaration_container(parent);
            }
        }

        self.resolve_name_(
            Some(location),
            &reference.as_identifier().escaped_text,
            SymbolFlags::Value | SymbolFlags::ExportValue | SymbolFlags::Alias,
            None,
            Option::<Gc<Node>>::None,
            true,
            None,
        )
    }

    pub(super) fn get_referenced_value_declaration(
        &self,
        reference_in: &Node, /*Identifier*/
    ) -> io::Result<Option<Gc<Node /*Declaration*/>>> {
        if !is_generated_identifier(reference_in) {
            let reference =
                get_parse_tree_node(Some(reference_in), Some(|node: &Node| is_identifier(node)));
            if let Some(reference) = reference.as_ref() {
                let symbol = self.get_referenced_value_symbol(reference, None)?;
                if let Some(symbol) = symbol.as_ref() {
                    return Ok(self
                        .get_export_symbol_of_value_symbol_if_exported(Some(&**symbol))
                        .as_ref()
                        .and_then(|export_symbol| export_symbol.maybe_value_declaration()));
                }
            }
        }

        Ok(None)
    }

    pub(super) fn is_literal_const_declaration(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
    ) -> io::Result<bool> {
        if is_declaration_readonly(node) || is_variable_declaration(node) && is_var_const(node) {
            return Ok(self.is_fresh_literal_type(
                self.get_type_of_symbol(&self.get_symbol_of_node(node)?.unwrap())?,
            ));
        }
        Ok(false)
    }

    pub(super) fn literal_type_to_node(
        &self,
        type_: Id<Type>, /*FreshableType*/
        enclosing: &Node,
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let enum_result = if self.type_(type_).flags().intersects(TypeFlags::EnumLiteral) {
            self.node_builder().symbol_to_expression(
                &self.type_(type_).symbol(),
                Some(SymbolFlags::Value),
                Some(enclosing),
                None,
                Some(tracker),
            )?
        } else if type_ == self.true_type() {
            Some(get_factory().create_true())
        } else if type_ == self.false_type() {
            Some(get_factory().create_false())
        } else {
            None
        };
        if let Some(enum_result) = enum_result {
            return Ok(enum_result);
        }
        Ok(match self.type_(type_ ){
            Type::LiteralType(LiteralType::BigIntLiteralType(type_)) => {
                get_factory().create_big_int_literal(type_.value.clone())
            }
            Type::LiteralType(LiteralType::NumberLiteralType(type_)) => {
                get_factory().create_numeric_literal(type_.value.clone(), None)
            }
            Type::LiteralType(LiteralType::StringLiteralType(type_)) => {
                get_factory().create_string_literal(type_.value.clone(), None, None)
            }
            _ => unreachable!(),
        })
    }

    pub(super) fn create_literal_const_value(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> io::Result<Gc<Node>> {
        let type_ = self.get_type_of_symbol(&self.get_symbol_of_node(node)?.unwrap())?;
        self.literal_type_to_node(type_, node, tracker)
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Gc<Node /*EntityName*/>> {
        /*location ?*/
        self.get_jsx_namespace_(Some(location));
        get_source_file_of_node(location)
            .as_source_file()
            .maybe_local_jsx_factory()
            .clone()
            .or_else(|| self._jsx_factory_entity.borrow().clone())
        /*: _jsxFactoryEntity*/
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Gc<Node /*EntityName*/>> {
        // if (location) {
        let file = maybe_get_source_file_of_node(Some(location));
        if let Some(file) = file.as_ref() {
            let file_as_source_file = file.as_source_file();
            if let Some(file_local_jsx_fragment_factory) = file_as_source_file
                .maybe_local_jsx_fragment_factory()
                .as_ref()
            {
                return Some(file_local_jsx_fragment_factory.clone());
            }
            let file_pragmas = file_as_source_file.pragmas();
            let jsx_frag_pragmas = file_pragmas.get(&PragmaName::Jsxfrag);
            let jsx_frag_pragma =
                jsx_frag_pragmas.and_then(|jsx_frag_pragmas| jsx_frag_pragmas.get(0));
            if let Some(jsx_frag_pragma) = jsx_frag_pragma {
                let ret = parse_isolated_entity_name(
                    jsx_frag_pragma
                        .arguments
                        .get(&PragmaArgumentName::Factory)
                        .unwrap()
                        .as_without_captured_span()
                        .clone(),
                    self.language_version,
                );
                *file_as_source_file.maybe_local_jsx_fragment_factory() = ret.clone();
                return ret;
            }
        }
        // }

        if let Some(compiler_options_jsx_fragment_factory) =
            self.compiler_options.jsx_fragment_factory.as_ref()
        {
            return parse_isolated_entity_name(
                compiler_options_jsx_fragment_factory.clone(),
                self.language_version,
            );
        }
        None
    }

    pub(super) fn create_resolver(&self) -> Gc<Box<dyn EmitResolver>> {
        Gc::new(Box::new(EmitResolverCreateResolver::new(self.rc_wrapper())))
    }

    pub(super) fn get_external_module_file_from_declaration(
        &self,
        declaration: &Node, /*AnyImportOrReExport | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> io::Result<Option<Gc<Node /*SourceFile*/>>> {
        let specifier = if declaration.kind() == SyntaxKind::ModuleDeclaration {
            try_cast(
                declaration.as_module_declaration().name(),
                |node: &Gc<Node>| is_string_literal(node),
            )
        } else {
            get_external_module_name(declaration)
        };
        let module_symbol = return_ok_default_if_none!(self.resolve_external_module_name_worker(
            specifier.as_ref().unwrap(),
            specifier.as_ref().unwrap(),
            None,
            None,
        )?);
        Ok(get_declaration_of_kind(
            &module_symbol,
            SyntaxKind::SourceFile,
        ))
    }

    pub(super) fn initialize_type_checker(&self) -> io::Result<()> {
        for file in &*self.host.get_source_files() {
            bind_source_file(file, self.compiler_options.clone());
            // println!("post-binding: {:#?}", file);
        }

        *self.maybe_amalgamated_duplicates() = Some(HashMap::new());

        let mut augmentations: Option<Vec<Vec<Gc<Node /*StringLiteral | Identifier*/>>>> = None;
        for file in &*self.host.get_source_files() {
            let file_as_source_file = file.as_source_file();
            if file_as_source_file.maybe_redirect_info().is_some() {
                continue;
            }
            if !is_external_or_common_js_module(file) {
                let file_global_this_symbol = (*file.locals()).borrow().get("globalThis").cloned();
                if let Some(ref file_global_this_symbol_declarations) = file_global_this_symbol
                    .as_ref()
                    .and_then(|file_global_this_symbol| {
                        file_global_this_symbol.maybe_declarations().clone()
                    })
                {
                    for declaration in file_global_this_symbol_declarations {
                        self.diagnostics().add(
                            Gc::new(
                                create_diagnostic_for_node(
                                    declaration,
                                    &Diagnostics::Declaration_name_conflicts_with_built_in_global_identifier_0,
                                    Some(vec![
                                        "globalThis".to_owned()
                                    ])
                                ).into()
                            )
                        );
                    }
                }
                self.merge_symbol_table(self.globals_rc(), &(*file.locals()).borrow(), None)?;
            }
            if let Some(file_js_global_augmentations) =
                file_as_source_file.maybe_js_global_augmentations().clone()
            {
                self.merge_symbol_table(
                    self.globals_rc(),
                    &(*file_js_global_augmentations).borrow(),
                    None,
                )?;
            }
            if let Some(file_pattern_ambient_modules) = file_as_source_file
                .maybe_pattern_ambient_modules()
                .as_ref()
                .filter(|file_pattern_ambient_modules| !file_pattern_ambient_modules.is_empty())
            {
                let mut pattern_ambient_modules = self.maybe_pattern_ambient_modules();
                *pattern_ambient_modules = Some(concatenate(
                    pattern_ambient_modules.clone().unwrap_or_else(|| vec![]),
                    file_pattern_ambient_modules.clone(),
                ));
            }
            let file_module_augmentations = file_as_source_file.maybe_module_augmentations();
            // TODO: this should end up being .unwrap()'able
            // let file_module_augmentations = file_module_augmentations.as_ref().unwrap();
            let file_module_augmentations =
                file_module_augmentations.clone().unwrap_or_else(|| vec![]);
            let file_module_augmentations = &file_module_augmentations;
            if !file_module_augmentations.is_empty() {
                if augmentations.is_none() {
                    augmentations = Some(vec![]);
                }
                augmentations
                    .as_mut()
                    .unwrap()
                    .push(file_module_augmentations.clone());
            }
            if let Some(file_symbol_global_exports) = file
                .maybe_symbol()
                .as_ref()
                .and_then(|file_symbol| file_symbol.maybe_global_exports().clone())
            {
                let source = file_symbol_global_exports;
                let mut globals = self.globals_mut();
                for (id, source_symbol) in &*(*source).borrow() {
                    if !globals.contains_key(id) {
                        globals.insert(id.clone(), source_symbol.clone());
                    }
                }
            }
        }

        if let Some(augmentations) = augmentations.as_ref() {
            for list in augmentations {
                for augmentation in list {
                    if !is_global_scope_augmentation(&augmentation.parent()) {
                        continue;
                    }
                    self.merge_module_augmentation(augmentation)?;
                }
            }
        }

        self.add_to_symbol_table(
            &mut *self.globals_mut(),
            &self.builtin_globals(),
            &Diagnostics::Declaration_name_conflicts_with_built_in_global_identifier_0,
        );

        self.get_symbol_links(&self.undefined_symbol())
            .borrow_mut()
            .type_ = Some(self.undefined_widening_type());
        self.get_symbol_links(&self.arguments_symbol())
            .borrow_mut()
            .type_ = self.get_global_type("IArguments", 0, true)?;
        self.get_symbol_links(&self.unknown_symbol())
            .borrow_mut()
            .type_ = Some(self.error_type());
        self.get_symbol_links(&self.global_this_symbol())
            .borrow_mut()
            .type_ = Some(
            self.alloc_type(
                self.create_object_type(ObjectFlags::Anonymous, Some(self.global_this_symbol()))
                    .into(),
            ),
        );

        *self.global_array_type.borrow_mut() = self.get_global_type("Array", 1, true)?;
        *self.global_object_type.borrow_mut() = self.get_global_type("Object", 0, true)?;
        *self.global_function_type.borrow_mut() = self.get_global_type("Function", 0, true)?;
        *self.global_callable_function_type.borrow_mut() = Some(
            if self.strict_bind_call_apply {
                self.get_global_type("CallableFunction", 0, true)?
            } else {
                None
            }
            .unwrap_or_else(|| self.global_function_type()),
        );
        *self.global_newable_function_type.borrow_mut() = Some(
            if self.strict_bind_call_apply {
                self.get_global_type("NewableFunction", 0, true)?
            } else {
                None
            }
            .unwrap_or_else(|| self.global_function_type()),
        );
        *self.global_string_type.borrow_mut() = self.get_global_type("String", 0, true)?;
        *self.global_number_type.borrow_mut() = self.get_global_type("Number", 0, true)?;
        *self.global_boolean_type.borrow_mut() = self.get_global_type("Boolean", 0, true)?;
        *self.global_reg_exp_type.borrow_mut() = self.get_global_type("RegExp", 0, true)?;
        *self.any_array_type.borrow_mut() = Some(self.create_array_type(self.any_type(), None));

        *self.auto_array_type.borrow_mut() = Some(self.create_array_type(self.auto_type(), None));
        if self.auto_array_type() == self.empty_object_type() {
            *self.auto_array_type.borrow_mut() = Some(self.create_anonymous_type(
                Option::<&Symbol>::None,
                self.empty_symbols(),
                vec![],
                vec![],
                vec![],
            )?);
        }

        *self.global_readonly_array_type.borrow_mut() = self
            .get_global_type_or_undefined("ReadonlyArray", Some(1))?
            .or_else(|| self.global_array_type.borrow().clone());
        *self.any_readonly_array_type.borrow_mut() = Some(
            if let Some(global_readonly_array_type) = *self.global_readonly_array_type.borrow() {
                self.create_type_from_generic_global_type(
                    global_readonly_array_type,
                    vec![self.any_type()],
                )
            } else {
                self.any_array_type()
            },
        );
        *self.global_this_type.borrow_mut() =
            self.get_global_type_or_undefined("ThisType", Some(1))?;

        if let Some(augmentations) = augmentations.as_ref() {
            for list in augmentations {
                for augmentation in list {
                    if is_global_scope_augmentation(&augmentation.parent()) {
                        continue;
                    }
                    self.merge_module_augmentation(augmentation)?;
                }
            }
        }

        for duplicate_info_for_files in self
            .maybe_amalgamated_duplicates()
            .as_ref()
            .unwrap()
            .values()
        {
            let first_file = &duplicate_info_for_files.first_file;
            let second_file = &duplicate_info_for_files.second_file;
            let conflicting_symbols = &duplicate_info_for_files.conflicting_symbols;
            if conflicting_symbols.len() < 8 {
                for (symbol_name, duplicate_info_for_symbol) in conflicting_symbols {
                    let is_block_scoped = duplicate_info_for_symbol.is_block_scoped;
                    let first_file_locations = &duplicate_info_for_symbol.first_file_locations;
                    let second_file_locations = &duplicate_info_for_symbol.second_file_locations;
                    let message = if is_block_scoped {
                        &*Diagnostics::Cannot_redeclare_block_scoped_variable_0
                    } else {
                        &*Diagnostics::Duplicate_identifier_0
                    };
                    for node in first_file_locations {
                        self.add_duplicate_declaration_error(
                            node,
                            message,
                            symbol_name,
                            Some(second_file_locations),
                        );
                    }
                    for node in second_file_locations {
                        self.add_duplicate_declaration_error(
                            node,
                            message,
                            symbol_name,
                            Some(first_file_locations),
                        );
                    }
                }
            } else {
                let list: String = conflicting_symbols
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ");
                self.diagnostics().add(
                    {
                        let diagnostic: Gc<Diagnostic> = Gc::new(
                            create_diagnostic_for_node(
                                first_file,
                                &Diagnostics::Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0,
                                Some(vec![
                                    list.clone()
                                ])
                            ).into()
                        );
                        add_related_info(
                            &diagnostic,
                            vec![
                                Gc::new(
                                    create_diagnostic_for_node(
                                        second_file,
                                        &Diagnostics::Conflicts_are_in_this_file,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                        diagnostic
                    }
                );
                self.diagnostics().add(
                    {
                        let diagnostic: Gc<Diagnostic> = Gc::new(
                            create_diagnostic_for_node(
                                second_file,
                                &Diagnostics::Definitions_of_the_following_identifiers_conflict_with_those_in_another_file_Colon_0,
                                Some(vec![
                                    list.clone()
                                ])
                            ).into()
                        );
                        add_related_info(
                            &diagnostic,
                            vec![
                                Gc::new(
                                    create_diagnostic_for_node(
                                        first_file,
                                        &Diagnostics::Conflicts_are_in_this_file,
                                        None,
                                    ).into()
                                )
                            ]
                        );
                        diagnostic
                    }
                );
            }
        }
        *self.maybe_amalgamated_duplicates() = None;

        Ok(())
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) -> io::Result<()> {
        if self.requested_external_emit_helpers() & helpers != helpers
            && self.compiler_options.import_helpers == Some(true)
        {
            let source_file = get_source_file_of_node(location);
            if is_effective_external_module(&source_file, &self.compiler_options)
                && !location.flags().intersects(NodeFlags::Ambient)
            {
                let helpers_module = self.resolve_helpers_module(&source_file, location)?;
                if !Gc::ptr_eq(&helpers_module, &self.unknown_symbol()) {
                    let unchecked_helpers = helpers & !self.requested_external_emit_helpers();
                    let mut helper = ExternalEmitHelpers::FirstEmitHelper;
                    while helper <= ExternalEmitHelpers::LastEmitHelper {
                        if unchecked_helpers.intersects(helper) {
                            let name = self.get_helper_name(helper);
                            let symbol = self.get_symbol(
                                &(*helpers_module.maybe_exports().clone().unwrap()).borrow(),
                                &escape_leading_underscores(name),
                                SymbolFlags::Value,
                            )?;
                            match symbol.as_ref() {
                                None => {
                                    self.error(
                                        Some(location),
                                        &Diagnostics::This_syntax_requires_an_imported_helper_named_1_which_does_not_exist_in_0_Consider_upgrading_your_version_of_0,
                                        Some(vec![
                                            external_helpers_module_name_text.to_owned(),
                                            name.to_owned(),
                                        ])
                                    );
                                }
                                Some(symbol) => {
                                    if helper.intersects(ExternalEmitHelpers::ClassPrivateFieldGet)
                                    {
                                        if !try_some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))?),
                                            Some(|signature: &Gc<Signature>| -> io::Result<_> {
                                                Ok(self.get_parameter_count(signature)? > 3)
                                            }),
                                        )? {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    4_usize.to_string(),
                                                ])
                                            );
                                        }
                                    } else if helper
                                        .intersects(ExternalEmitHelpers::ClassPrivateFieldSet)
                                    {
                                        if !try_some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))?),
                                            Some(|signature: &Gc<Signature>| -> io::Result<_> {
                                                Ok(self.get_parameter_count(signature)? > 4)
                                            }),
                                        )? {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    5_usize.to_string(),
                                                ])
                                            );
                                        }
                                    } else if helper.intersects(ExternalEmitHelpers::SpreadArray) {
                                        if !try_some(
                                            Some(&*self.get_signatures_of_symbol(Some(&**symbol))?),
                                            Some(|signature: &Gc<Signature>| -> io::Result<_> {
                                                Ok(self.get_parameter_count(signature)? > 2)
                                            }),
                                        )? {
                                            self.error(
                                                Some(location),
                                                &Diagnostics::This_syntax_requires_an_imported_helper_named_1_with_2_parameters_which_is_not_compatible_with_the_one_in_0_Consider_upgrading_your_version_of_0,
                                                Some(vec![
                                                    external_helpers_module_name_text.to_owned(),
                                                    name.to_owned(),
                                                    3_usize.to_string(),
                                                ])
                                            );
                                        }
                                    }
                                }
                            }
                        }
                        // helper <<= 1;
                        let helper_bits = helper.bits() << 1;
                        if !(helper_bits <= ExternalEmitHelpers::LastEmitHelper.bits()) {
                            break;
                        }
                        helper = ExternalEmitHelpers::from_bits(helper_bits).unwrap();
                    }
                }
                self.set_requested_external_emit_helpers(
                    self.requested_external_emit_helpers() | helpers,
                );
            }
        }

        Ok(())
    }

    pub(super) fn get_helper_name(&self, helper: ExternalEmitHelpers) -> &'static str {
        match helper {
            ExternalEmitHelpers::Extends => "__extends",
            ExternalEmitHelpers::Assign => "__assign",
            ExternalEmitHelpers::Rest => "__rest",
            ExternalEmitHelpers::Decorate => "__decorate",
            ExternalEmitHelpers::Metadata => "__metadata",
            ExternalEmitHelpers::Param => "__param",
            ExternalEmitHelpers::Awaiter => "__awaiter",
            ExternalEmitHelpers::Generator => "__generator",
            ExternalEmitHelpers::Values => "__values",
            ExternalEmitHelpers::Read => "__read",
            ExternalEmitHelpers::SpreadArray => "__spreadArray",
            ExternalEmitHelpers::Await => "__await",
            ExternalEmitHelpers::AsyncGenerator => "__asyncGenerator",
            ExternalEmitHelpers::AsyncDelegator => "__asyncDelegator",
            ExternalEmitHelpers::AsyncValues => "__asyncValues",
            ExternalEmitHelpers::ExportStar => "__exportStar",
            ExternalEmitHelpers::ImportStar => "__importStar",
            ExternalEmitHelpers::ImportDefault => "__importDefault",
            ExternalEmitHelpers::MakeTemplateObject => "__makeTemplateObject",
            ExternalEmitHelpers::ClassPrivateFieldGet => "__classPrivateFieldGet",
            ExternalEmitHelpers::ClassPrivateFieldSet => "__classPrivateFieldSet",
            ExternalEmitHelpers::ClassPrivateFieldIn => "__classPrivateFieldIn",
            ExternalEmitHelpers::CreateBinding => "__createBinding",
            _ => Debug_.fail(Some("Unrecognized helper")),
        }
    }

    pub(super) fn resolve_helpers_module(
        &self,
        node: &Node, /*SourceFile*/
        error_node: &Node,
    ) -> io::Result<Gc<Symbol>> {
        let mut external_helpers_module = self.maybe_external_helpers_module();
        if external_helpers_module.is_none() {
            *external_helpers_module = Some(
                self.resolve_external_module(
                    node,
                    external_helpers_module_name_text,
                    Some(&Diagnostics::This_syntax_requires_an_imported_helper_but_module_0_cannot_be_found),
                    error_node,
                    None,
                )?.unwrap_or_else(|| self.unknown_symbol())
            );
        }
        Ok(external_helpers_module.clone().unwrap())
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        self.check_grammar_decorators(node) || self.check_grammar_modifiers(node)
    }

    pub(super) fn check_grammar_decorators(&self, node: &Node) -> bool {
        if node.maybe_decorators().is_none() {
            return false;
        }
        if !node_can_be_decorated(node, Some(node.parent()), node.parent().maybe_parent()) {
            if node.kind() == SyntaxKind::MethodDeclaration
                && !node_is_present(node.as_method_declaration().maybe_body())
            {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::A_decorator_can_only_decorate_a_method_implementation_not_an_overload,
                    None,
                );
            } else {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Decorators_are_not_valid_here,
                    None,
                );
            }
        } else if matches!(
            node.kind(),
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor
        ) {
            let accessors = get_all_accessor_declarations(
                &node.parent().as_class_like_declaration().members(),
                node,
            );
            if accessors.first_accessor.maybe_decorators().is_some()
                && matches!(
                    accessors.second_accessor.as_ref(),
                    Some(accessors_second_accessor) if ptr::eq(
                        node,
                        &**accessors_second_accessor
                    )
                )
            {
                return self.grammar_error_on_first_token(
                    node,
                    &Diagnostics::Decorators_cannot_be_applied_to_multiple_get_Slashset_accessors_of_the_same_name,
                    None,
                );
            }
        }
        false
    }

    pub(super) fn check_grammar_modifiers(&self, node: &Node) -> bool {
        let quick_result = self.report_obvious_modifier_errors(node);
        if let Some(quick_result) = quick_result {
            return quick_result;
        }

        let mut last_static: Option<Gc<Node>> = None;
        let mut last_declare: Option<Gc<Node>> = None;
        let mut last_async: Option<Gc<Node>> = None;
        let mut last_readonly: Option<Gc<Node>> = None;
        let mut last_override: Option<Gc<Node>> = None;
        let mut flags = ModifierFlags::None;
        for modifier in node.maybe_modifiers().as_ref().unwrap() {
            if modifier.kind() != SyntaxKind::ReadonlyKeyword {
                if matches!(
                    node.kind(),
                    SyntaxKind::PropertySignature | SyntaxKind::MethodSignature
                ) {
                    return self.grammar_error_on_node(
                        modifier,
                        &Diagnostics::_0_modifier_cannot_appear_on_a_type_member,
                        Some(vec![token_to_string(modifier.kind()).unwrap().to_owned()]),
                    );
                }
                if node.kind() == SyntaxKind::IndexSignature
                    && (modifier.kind() != SyntaxKind::StaticKeyword
                        || !maybe_is_class_like(node.maybe_parent()))
                {
                    return self.grammar_error_on_node(
                        modifier,
                        &Diagnostics::_0_modifier_cannot_appear_on_an_index_signature,
                        Some(vec![token_to_string(modifier.kind()).unwrap().to_owned()]),
                    );
                }
            }
            match modifier.kind() {
                SyntaxKind::ConstKeyword => {
                    if node.kind() != SyntaxKind::EnumDeclaration {
                        return self.grammar_error_on_node(
                            node,
                            &Diagnostics::A_class_member_cannot_have_the_0_keyword,
                            Some(vec![token_to_string(SyntaxKind::ConstKeyword)
                                .unwrap()
                                .to_owned()]),
                        );
                    }
                }
                SyntaxKind::OverrideKeyword => {
                    if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["override".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["override".to_owned(), "declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["override".to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["override".to_owned(), "async".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Override;
                    last_override = Some(modifier.clone());
                }

                SyntaxKind::PublicKeyword
                | SyntaxKind::ProtectedKeyword
                | SyntaxKind::PrivateKeyword => {
                    let text = self.visibility_to_string(modifier_to_flag(modifier.kind()));

                    if flags.intersects(ModifierFlags::AccessibilityModifier) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::Accessibility_modifier_already_seen,
                            None,
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "override".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Static) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec![text.to_owned(), "async".to_owned()]),
                        );
                    } else if matches!(
                        node.parent().kind(),
                        SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_module_or_namespace_element,
                            Some(vec![
                                text.to_owned(),
                            ])
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        if modifier.kind() == SyntaxKind::PrivateKeyword {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec![text.to_owned(), "abstract".to_owned()]),
                            );
                        } else {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_must_precede_1_modifier,
                                Some(vec![text.to_owned(), "abstract".to_owned()]),
                            );
                        }
                    } else if is_private_identifier_class_element_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::An_accessibility_modifier_cannot_be_used_with_a_private_identifier,
                            None
                        );
                    }
                    flags |= modifier_to_flag(modifier.kind());
                }

                SyntaxKind::StaticKeyword => {
                    if flags.intersects(ModifierFlags::Static) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "readonly".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "async".to_owned()]),
                        );
                    } else if matches!(
                        node.parent().kind(),
                        SyntaxKind::ModuleBlock | SyntaxKind::SourceFile
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_module_or_namespace_element,
                            Some(vec![
                                "static".to_owned(),
                            ])
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["static".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["static".to_owned(), "abstract".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["static".to_owned(), "override".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Static;
                    last_static = Some(modifier.clone());
                }

                SyntaxKind::ReadonlyKeyword => {
                    if flags.intersects(ModifierFlags::Readonly) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["readonly".to_owned()]),
                        );
                    } else if !matches!(
                        node.kind(),
                        SyntaxKind::PropertyDeclaration
                            | SyntaxKind::PropertySignature
                            | SyntaxKind::IndexSignature
                            | SyntaxKind::Parameter
                    ) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::readonly_modifier_can_only_appear_on_a_property_declaration_or_index_signature,
                            None,
                        );
                    }
                    flags |= ModifierFlags::Readonly;
                    last_readonly = Some(modifier.clone());
                }

                SyntaxKind::ExportKeyword => {
                    if flags.intersects(ModifierFlags::Export) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["export".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "abstract".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "async".to_owned()]),
                        );
                    } else if maybe_is_class_like(node.maybe_parent()) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_class_elements_of_this_kind,
                            Some(vec!["export".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["export".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Export;
                }
                SyntaxKind::DefaultKeyword => {
                    let container = if node.parent().kind() == SyntaxKind::SourceFile {
                        node.parent()
                    } else {
                        node.parent().parent()
                    };
                    if container.kind() == SyntaxKind::ModuleDeclaration
                        && !is_ambient_module(&container)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::A_default_export_can_only_be_used_in_an_ECMAScript_style_module,
                            None,
                        );
                    } else if !flags.intersects(ModifierFlags::Export) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_must_precede_1_modifier,
                            Some(vec!["export".to_owned(), "default".to_owned()]),
                        );
                    }

                    flags |= ModifierFlags::Default;
                }
                SyntaxKind::DeclareKeyword => {
                    if flags.intersects(ModifierFlags::Ambient) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Override) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["override".to_owned()]),
                        );
                    } else if maybe_is_class_like(node.maybe_parent())
                        && !is_property_declaration(node)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_class_elements_of_this_kind,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["declare".to_owned()]),
                        );
                    } else if node.parent().flags().intersects(NodeFlags::Ambient)
                        && node.parent().kind() == SyntaxKind::ModuleBlock
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::A_declare_modifier_cannot_be_used_in_an_already_ambient_context,
                            None,
                        );
                    } else if is_private_identifier_class_element_declaration(node) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_a_private_identifier,
                            Some(vec!["declare".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Ambient;
                    last_declare = Some(modifier.clone());
                }

                SyntaxKind::AbstractKeyword => {
                    if flags.intersects(ModifierFlags::Abstract) {
                        self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["abstract".to_owned()]),
                        );
                    }
                    if !matches!(
                        node.kind(),
                        SyntaxKind::ClassDeclaration | SyntaxKind::ConstructorType
                    ) {
                        if !matches!(
                            node.kind(),
                            SyntaxKind::MethodDeclaration
                                | SyntaxKind::PropertyDeclaration
                                | SyntaxKind::GetAccessor
                                | SyntaxKind::SetAccessor
                        ) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::abstract_modifier_can_only_appear_on_a_class_method_or_property_declaration,
                                None,
                            );
                        }
                        if !(node.parent().kind() == SyntaxKind::ClassDeclaration
                            && has_syntactic_modifier(&node.parent(), ModifierFlags::Abstract))
                        {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::Abstract_methods_can_only_appear_within_an_abstract_class,
                                None,
                            );
                        }
                        if flags.intersects(ModifierFlags::Static) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec!["static".to_owned(), "abstract".to_owned()]),
                            );
                        }
                        if flags.intersects(ModifierFlags::Private) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                Some(vec!["private".to_owned(), "abstract".to_owned()]),
                            );
                        }
                        if flags.intersects(ModifierFlags::Async) {
                            if let Some(last_async) = last_async.as_ref() {
                                return self.grammar_error_on_node(
                                    last_async,
                                    &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                                    Some(vec!["async".to_owned(), "abstract".to_owned()]),
                                );
                            }
                        }
                        if flags.intersects(ModifierFlags::Override) {
                            return self.grammar_error_on_node(
                                modifier,
                                &Diagnostics::_0_modifier_must_precede_1_modifier,
                                Some(vec!["abstract".to_owned(), "override".to_owned()]),
                            );
                        }
                    }
                    if is_named_declaration(node)
                        && node.as_named_declaration().name().kind()
                            == SyntaxKind::PrivateIdentifier
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_a_private_identifier,
                            Some(vec!["abstract".to_owned()]),
                        );
                    }

                    flags |= ModifierFlags::Abstract;
                }

                SyntaxKind::AsyncKeyword => {
                    if flags.intersects(ModifierFlags::Async) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_already_seen,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if flags.intersects(ModifierFlags::Ambient)
                        || node.parent().flags().intersects(NodeFlags::Ambient)
                    {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_in_an_ambient_context,
                            Some(vec!["async".to_owned()]),
                        );
                    } else if node.kind() == SyntaxKind::Parameter {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_appear_on_a_parameter,
                            Some(vec!["async".to_owned()]),
                        );
                    }
                    if flags.intersects(ModifierFlags::Abstract) {
                        return self.grammar_error_on_node(
                            modifier,
                            &Diagnostics::_0_modifier_cannot_be_used_with_1_modifier,
                            Some(vec!["async".to_owned(), "abstract".to_owned()]),
                        );
                    }
                    flags |= ModifierFlags::Async;
                    last_async = Some(modifier.clone());
                }
                _ => (),
            }
        }

        if node.kind() == SyntaxKind::Constructor {
            if flags.intersects(ModifierFlags::Static) {
                return self.grammar_error_on_node(
                    last_static.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["static".to_owned()]),
                );
            }
            if flags.intersects(ModifierFlags::Abstract) {
                return self.grammar_error_on_node(
                    // TODO: this is what's in the Typescript version but seems like it should be lastDeclare instead?
                    last_static.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["abstract".to_owned()]),
                );
            }
            if flags.intersects(ModifierFlags::Override) {
                return self.grammar_error_on_node(
                    last_override.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["override".to_owned()]),
                );
            } else if flags.intersects(ModifierFlags::Async) {
                return self.grammar_error_on_node(
                    last_async.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["async".to_owned()]),
                );
            } else if flags.intersects(ModifierFlags::Readonly) {
                return self.grammar_error_on_node(
                    last_readonly.as_ref().unwrap(),
                    &Diagnostics::_0_modifier_cannot_appear_on_a_constructor_declaration,
                    Some(vec!["readonly".to_owned()]),
                );
            }
            return false;
        } else if matches!(
            node.kind(),
            SyntaxKind::ImportDeclaration | SyntaxKind::ImportEqualsDeclaration
        ) && flags.intersects(ModifierFlags::Ambient)
        {
            return self.grammar_error_on_node(
                last_declare.as_ref().unwrap(),
                &Diagnostics::A_0_modifier_cannot_be_used_with_an_import_declaration,
                Some(vec!["declare".to_owned()]),
            );
        } else if node.kind() == SyntaxKind::Parameter
            && flags.intersects(ModifierFlags::ParameterPropertyModifier)
            && is_binding_pattern(node.as_parameter_declaration().maybe_name())
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_parameter_property_may_not_be_declared_using_a_binding_pattern,
                None,
            );
        } else if node.kind() == SyntaxKind::Parameter
            && flags.intersects(ModifierFlags::ParameterPropertyModifier)
            && node.as_parameter_declaration().dot_dot_dot_token.is_some()
        {
            return self.grammar_error_on_node(
                node,
                &Diagnostics::A_parameter_property_cannot_be_declared_using_a_rest_parameter,
                None,
            );
        }
        if flags.intersects(ModifierFlags::Async) {
            return self.check_grammar_async_modifier(node, last_async.as_ref().unwrap());
        }
        false
    }
}
