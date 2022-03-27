#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::HashSet;
use std::ptr;
use std::rc::Rc;

use super::{
    get_symbol_id, MappedTypeModifiers, MembersOrExportsResolutionKind, NodeBuilderContext,
};
use crate::{
    are_option_rcs_equal, concatenate, create_printer, create_symbol_table,
    declaration_name_to_string, escape_leading_underscores, escape_string, factory, find_ancestor,
    first_defined, get_check_flags, get_combined_modifier_flags, get_declaration_of_kind,
    get_effective_type_annotation_node, get_effective_type_parameter_declarations,
    get_emit_script_target, get_first_identifier, get_name_of_declaration, get_source_file_of_node,
    has_dynamic_name, has_effective_modifier, has_only_expression_initializer, is_ambient_module,
    is_bindable_object_define_property_call, is_binding_pattern, is_call_expression,
    is_computed_property_name, is_external_module_augmentation, is_identifier_text,
    is_internal_module_import_equals_declaration, is_property_assignment, is_property_declaration,
    is_property_signature, is_source_file, is_type_alias, is_variable_declaration, maybe_for_each,
    push_if_unique_rc, range_equals_rc, starts_with, symbol_name, synthetic_factory,
    try_to_add_to_set, walk_up_parenthesized_types, BaseInterfaceType, CharacterCodes, CheckFlags,
    Debug_, EmitHint, EmitTextWriter, InterfaceType, InterfaceTypeInterface,
    InterfaceTypeWithDeclaredMembersInterface, InternalSymbolName, LiteralType, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeBuilderFlags, NodeFlags, NodeInterface,
    ObjectFlags, ObjectFlagsTypeInterface, PrinterOptionsBuilder, Signature, SignatureFlags,
    Symbol, SymbolFlags, SymbolId, SymbolInterface, SymbolTable, SyntaxKind, Type, TypeChecker,
    TypeFlags, TypeFormatFlags, TypeInterface, TypeMapper, TypePredicate, TypePredicateKind,
    UnderscoreEscapedMap, UnionOrIntersectionTypeInterface, __String, maybe_append_if_unique_rc,
};

impl TypeChecker {
    pub(super) fn type_predicate_to_string_worker<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: TypeFormatFlags,
        writer: Rc<RefCell<dyn EmitTextWriter>>,
    ) {
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let predicate: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_
                    .create_type_predicate_node(
                        synthetic_factory_,
                        if matches!(
                            type_predicate.kind,
                            TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
                        ) {
                            Some(
                                factory_
                                    .create_token(synthetic_factory_, SyntaxKind::AssertsKeyword)
                                    .into(),
                            )
                        } else {
                            None
                        },
                        if matches!(
                            type_predicate.kind,
                            TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
                        ) {
                            Into::<Rc<Node>>::into(factory_.create_identifier(
                                synthetic_factory_,
                                type_predicate.parameter_name.as_ref().unwrap(),
                                Option::<NodeArray>::None,
                                None,
                            ))
                        } else {
                            factory_.create_this_type_node(synthetic_factory_).into()
                        },
                        type_predicate.type_.as_ref().and_then(|type_| {
                            self.node_builder.type_to_type_node(
                                self,
                                type_,
                                enclosing_declaration.as_deref(),
                                Some(
                                    self.to_node_builder_flags(Some(flags))
                                        | NodeBuilderFlags::IgnoreErrors
                                        | NodeBuilderFlags::WriteTypeParametersInQualifiedName,
                                ),
                                None,
                            )
                        }),
                    )
                    .into()
            })
        });
        let mut printer = create_printer(
            PrinterOptionsBuilder::default()
                .remove_comments(Some(true))
                .build()
                .unwrap(),
        );
        let source_file = enclosing_declaration
            .as_deref()
            .and_then(|enclosing_declaration| get_source_file_of_node(Some(enclosing_declaration)));
        printer.write_node(EmitHint::Unspecified, &predicate, source_file, writer);
        // return writer;
    }

    pub(super) fn format_union_types(&self, types: &[Rc<Type>]) -> Vec<Rc<Type>> {
        let mut result: Vec<Rc<Type>> = vec![];
        let mut flags: TypeFlags = TypeFlags::None;
        let mut i = 0;
        while i < types.len() {
            let t = &types[i];
            flags |= t.flags();
            if !t.flags().intersects(TypeFlags::Nullable) {
                if t.flags()
                    .intersects(TypeFlags::BooleanLiteral | TypeFlags::EnumLiteral)
                {
                    let base_type = if t.flags().intersects(TypeFlags::BooleanLiteral) {
                        self.boolean_type()
                    } else {
                        self.get_base_type_of_enum_literal_type(t)
                    };
                    if base_type.flags().intersects(TypeFlags::Union) {
                        let base_type_as_union_type = base_type.as_union_type();
                        let count = base_type_as_union_type.types().len();
                        if i + count <= types.len()
                            && Rc::ptr_eq(
                                &self.get_regular_type_of_literal_type(&*types[i + count - 1]),
                                &self.get_regular_type_of_literal_type(
                                    &base_type_as_union_type.types()[count - 1],
                                ),
                            )
                        {
                            result.push(base_type);
                            i += count - 1;
                            continue;
                        }
                    }
                }
                result.push(t.clone());
            }
            i += 1;
        }
        if flags.intersects(TypeFlags::Null) {
            result.push(self.null_type());
        }
        if flags.intersects(TypeFlags::Undefined) {
            result.push(self.undefined_type());
        }
        result /*|| types*/
    }

    pub(super) fn visibility_to_string(&self, flags: ModifierFlags) -> Option<&'static str> {
        if flags == ModifierFlags::Private {
            return Some("private");
        }
        if flags == ModifierFlags::Protected {
            return Some("protected");
        }
        Some("public")
    }

    pub(super) fn get_type_alias_for_type_literal(&self, type_: &Type) -> Option<Rc<Symbol>> {
        if let Some(type_symbol) = type_.maybe_symbol() {
            if type_symbol.flags().intersects(SymbolFlags::TypeLiteral) {
                if let Some(type_symbol_declarations) = type_symbol.maybe_declarations().as_deref()
                {
                    let node =
                        walk_up_parenthesized_types(&type_symbol_declarations[0].parent()).unwrap();
                    if node.kind() == SyntaxKind::TypeAliasDeclaration {
                        return self.get_symbol_of_node(&node);
                    }
                }
            }
        }
        None
    }

    pub(super) fn is_top_level_in_external_module_augmentation(&self, node: &Node) -> bool {
        /*node &&*/
        matches!(
            node.maybe_parent(),
            Some(node_parent) if node_parent.kind() == SyntaxKind::ModuleBlock && is_external_module_augmentation(&node_parent.parent())
        )
    }

    pub(super) fn is_default_binding_context(&self, location: &Node) -> bool {
        location.kind() == SyntaxKind::SourceFile || is_ambient_module(location)
    }

    pub(super) fn get_name_of_symbol_from_name_type(
        &self,
        symbol: &Symbol,
        context: Option<&NodeBuilderContext>,
    ) -> Option<String> {
        let name_type = RefCell::borrow(&self.get_symbol_links(symbol))
            .name_type
            .clone()?;
        if name_type
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteral)
        {
            let name: String = match &*name_type {
                Type::LiteralType(LiteralType::StringLiteralType(name_type)) => {
                    name_type.value.clone()
                }
                Type::LiteralType(LiteralType::NumberLiteralType(name_type)) => {
                    name_type.value.value().to_string()
                }
                _ => panic!("Expected string or number literal type"),
            };
            if !is_identifier_text(
                &name,
                Some(get_emit_script_target(&self.compiler_options)),
                None,
            ) && !self.is_numeric_literal_name(&name)
            {
                return Some(format!(
                    "\"{}\"",
                    escape_string(&name, Some(CharacterCodes::double_quote))
                ));
            }
            if self.is_numeric_literal_name(&name) && starts_with(&name, "-") {
                return Some(format!("[{}]", name));
            }
            return Some(name);
        }
        if name_type.flags().intersects(TypeFlags::UniqueESSymbol) {
            return Some(format!(
                "[{}]",
                self.get_name_of_symbol_as_written(
                    &name_type.as_unique_es_symbol_type().symbol,
                    context
                )
            ));
        }
        None
    }

    pub(super) fn get_name_of_symbol_as_written(
        &self,
        symbol: &Symbol,
        context: Option<&NodeBuilderContext>,
    ) -> Cow<'static, str> {
        if let Some(context) = context {
            if symbol.escaped_name() == &InternalSymbolName::Default()
                && !context
                    .flags()
                    .intersects(NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope)
                && (!context
                    .flags()
                    .intersects(NodeBuilderFlags::InInitialEntityName)
                    || match symbol.maybe_declarations().as_deref() {
                        None => true,
                        Some(symbol_declarations) => matches!(
                            context.maybe_enclosing_declaration().as_deref(),
                            Some(context_enclosing_declaration) if are_option_rcs_equal(
                                find_ancestor(symbol_declarations.get(0).map(Clone::clone), |declaration| self.is_default_binding_context(declaration)).as_ref(),
                                find_ancestor(Some(context_enclosing_declaration), |declaration| self.is_default_binding_context(declaration)).as_ref(),
                            )
                        ),
                    })
            {
                return "default".into();
            }
        }
        if let Some(symbol_declarations) = symbol.maybe_declarations().as_deref() {
            if !symbol_declarations.is_empty() {
                let mut declaration = first_defined(symbol_declarations, |d: &Rc<Node>, _| {
                    if get_name_of_declaration(Some(&**d)).is_some() {
                        Some(d)
                    } else {
                        None
                    }
                });
                let name = declaration
                    .and_then(|declaration| get_name_of_declaration(Some(&**declaration)));
                if let Some(declaration) = declaration {
                    if let Some(name) = name {
                        if is_call_expression(declaration)
                            && is_bindable_object_define_property_call(declaration)
                        {
                            return symbol_name(symbol).into();
                        }
                        if is_computed_property_name(&name)
                            && !get_check_flags(symbol).intersects(CheckFlags::Late)
                        {
                            let name_type = RefCell::borrow(&self.get_symbol_links(symbol))
                                .name_type
                                .clone();
                            if matches!(name_type, Some(name_type) if name_type.flags().intersects(TypeFlags::StringOrNumberLiteral))
                            {
                                let result =
                                    self.get_name_of_symbol_from_name_type(symbol, context);
                                if let Some(result) = result {
                                    return result.into();
                                }
                            }
                        }
                        return declaration_name_to_string(Some(name));
                    }
                }
                if declaration.is_none() {
                    declaration = Some(&symbol_declarations[0]);
                }
                let declaration = declaration.unwrap();
                if let Some(declaration_parent) = declaration.maybe_parent() {
                    if declaration_parent.kind() == SyntaxKind::VariableDeclaration {
                        return declaration_name_to_string(Some(
                            declaration_parent.as_variable_declaration().name(),
                        ));
                    }
                }
                match declaration.kind() {
                    SyntaxKind::ClassExpression
                    | SyntaxKind::FunctionExpression
                    | SyntaxKind::ArrowFunction => {
                        if let Some(context) = context {
                            if !context.encountered_error.get()
                                && !context
                                    .flags()
                                    .intersects(NodeBuilderFlags::AllowAnonymousIdentifier)
                            {
                                context.encountered_error.set(true);
                            }
                        }
                        return if declaration.kind() == SyntaxKind::ClassExpression {
                            "(Anonymous class)"
                        } else {
                            "(Anonymous function)"
                        }
                        .into();
                    }
                    _ => (),
                }
            }
        }
        let name = self.get_name_of_symbol_from_name_type(symbol, context);
        if let Some(name) = name {
            name
        } else {
            symbol_name(symbol)
        }
        .into()
    }

    pub(super) fn is_declaration_visible(&self, node: &Node) -> bool {
        // if (node) {
        let links = self.get_node_links(node);
        if RefCell::borrow(&links).is_visible.is_none() {
            links.borrow_mut().is_visible = Some(self.determine_if_declaration_is_visible(node));
        }
        let ret = RefCell::borrow(&links).is_visible.unwrap();
        ret
        // }

        // return false;
    }

    pub(super) fn determine_if_declaration_is_visible(&self, node: &Node) -> bool {
        match node.kind() {
            SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocEnumTag => {
                matches!(
                    node.maybe_parent()
                        .and_then(|node_parent| node_parent.maybe_parent())
                        .and_then(|node_parent_parent| node_parent_parent.maybe_parent()),
                    Some(node_parent_parent_parent) if is_source_file(&node_parent_parent_parent)
                )
            }
            SyntaxKind::BindingElement => self.is_declaration_visible(&node.parent().parent()),
            SyntaxKind::VariableDeclaration
            | SyntaxKind::ModuleDeclaration
            | SyntaxKind::ClassDeclaration
            | SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::EnumDeclaration
            | SyntaxKind::ImportEqualsDeclaration => {
                if node.kind() == SyntaxKind::VariableDeclaration
                    && is_binding_pattern(Some(node.as_variable_declaration().name()))
                    && node
                        .as_variable_declaration()
                        .name()
                        .as_has_elements()
                        .elements()
                        .is_empty()
                {
                    return false;
                }
                if is_external_module_augmentation(node) {
                    return true;
                }
                let parent = self.get_declaration_container(node);
                if !get_combined_modifier_flags(node).intersects(ModifierFlags::Export)
                    && !(node.kind() != SyntaxKind::ImportEqualsDeclaration
                        && parent.kind() != SyntaxKind::SourceFile
                        && parent.flags().intersects(NodeFlags::Ambient))
                {
                    return self.is_global_source_file(&parent);
                }
                self.is_declaration_visible(&parent)
            }

            SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertySignature
            | SyntaxKind::GetAccessor
            | SyntaxKind::SetAccessor
            | SyntaxKind::MethodDeclaration
            | SyntaxKind::MethodSignature => {
                if has_effective_modifier(node, ModifierFlags::Private | ModifierFlags::Protected) {
                    return false;
                }
                self.is_declaration_visible(&node.parent())
            }

            SyntaxKind::Constructor
            | SyntaxKind::ConstructSignature
            | SyntaxKind::CallSignature
            | SyntaxKind::IndexSignature
            | SyntaxKind::Parameter
            | SyntaxKind::ModuleBlock
            | SyntaxKind::FunctionType
            | SyntaxKind::ConstructorType
            | SyntaxKind::TypeLiteral
            | SyntaxKind::TypeReference
            | SyntaxKind::ArrayType
            | SyntaxKind::TupleType
            | SyntaxKind::UnionType
            | SyntaxKind::IntersectionType
            | SyntaxKind::ParenthesizedType
            | SyntaxKind::NamedTupleMember => self.is_declaration_visible(&node.parent()),

            SyntaxKind::ImportClause
            | SyntaxKind::NamespaceImport
            | SyntaxKind::ImportSpecifier => false,

            SyntaxKind::TypeParameter
            | SyntaxKind::SourceFile
            | SyntaxKind::NamespaceExportDeclaration => true,

            SyntaxKind::ExportAssignment => false,

            _ => false,
        }
    }

    pub(super) fn collect_linked_aliases(
        &self,
        node: &Node, /*Identifier*/
        set_visibility: Option<bool>,
    ) -> Option<Vec<Rc<Node>>> {
        let mut export_symbol: Option<Rc<Symbol>> = None;
        if
        /*node.parent &&*/
        node.parent().kind() == SyntaxKind::ExportAssignment {
            export_symbol = self.resolve_name_(
                Some(node),
                &node.as_identifier().escaped_text,
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                None,
                Some(node.node_wrapper()),
                false,
                None,
            );
        } else if node.parent().kind() == SyntaxKind::ExportSpecifier {
            export_symbol = self.get_target_of_export_specifier(
                &node.parent(),
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                None,
            );
        }
        let result: RefCell<Option<Vec<Rc<Node>>>> = RefCell::new(None);
        if let Some(export_symbol) = export_symbol {
            let mut visited: HashSet<SymbolId> = HashSet::new();
            visited.insert(get_symbol_id(&export_symbol));
            self.build_visible_node_list(
                set_visibility.unwrap_or(false),
                &result,
                &mut visited,
                export_symbol.maybe_declarations().as_deref(),
            );
        }
        result.into_inner()
    }

    pub(super) fn build_visible_node_list(
        &self,
        set_visibility: bool,
        result: &RefCell<Option<Vec<Rc<Node>>>>,
        visited: &mut HashSet<SymbolId>,
        declarations: Option<&[Rc<Node /*Declaration*/>]>,
    ) {
        maybe_for_each(declarations, |declaration: &Rc<Node>, _| {
            let result_node = self
                .get_any_import_syntax(declaration)
                .unwrap_or_else(|| declaration.node_wrapper());
            if set_visibility {
                self.get_node_links(declaration).borrow_mut().is_visible = Some(true);
            } else {
                let mut result = result.borrow_mut();
                if result.is_none() {
                    *result = Some(vec![]);
                }
                push_if_unique_rc(result.as_mut().unwrap(), &result_node);
            }

            if is_internal_module_import_equals_declaration(declaration) {
                let internal_module_reference =
                    &declaration.as_import_equals_declaration().module_reference;
                let first_identifier = get_first_identifier(internal_module_reference);
                let import_symbol = self.resolve_name_(
                    Some(&**declaration),
                    &first_identifier.as_identifier().escaped_text,
                    SymbolFlags::Value | SymbolFlags::Type | SymbolFlags::Namespace,
                    None,
                    Option::<Rc<Node>>::None,
                    false,
                    None,
                );
                if let Some(import_symbol) = import_symbol
                /*&& visited*/
                {
                    if try_to_add_to_set(visited, get_symbol_id(&import_symbol)) {
                        self.build_visible_node_list(
                            set_visibility,
                            result,
                            visited,
                            import_symbol.maybe_declarations().as_deref(),
                        );
                    }
                }
            }
            Option::<()>::None
        });
    }

    pub(super) fn get_declaration_container(&self, node: &Node) -> Rc<Node> {
        unimplemented!()
    }

    pub(super) fn get_type_of_property_of_type_(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn is_type_any<TTypeRef: Borrow<Type>>(&self, type_: Option<TTypeRef>) -> bool {
        match type_ {
            Some(type_) => {
                let type_ = type_.borrow();
                type_.flags().intersects(TypeFlags::Any)
            }
            None => false,
        }
    }

    pub(super) fn is_error_type(&self, type_: &Type) -> bool {
        ptr::eq(type_, &*self.error_type())
            || type_.flags().intersects(TypeFlags::Any) && type_.maybe_alias_symbol().is_some()
    }

    pub(super) fn add_optionality(
        &self,
        type_: &Type,
        is_property: Option<bool>,
        is_optional: Option<bool>,
    ) -> Rc<Type> {
        let is_property = is_property.unwrap_or(false);
        let is_optional = is_optional.unwrap_or(true);
        if self.strict_null_checks && is_optional {
            self.get_optional_type_(type_, Some(is_property))
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Option<Rc<Type>> {
        let is_property =
            is_property_declaration(declaration) || is_property_signature(declaration);
        let is_optional = false;

        let declared_type = self.try_get_type_from_effective_type_node(declaration);
        if let Some(declared_type) = declared_type {
            return Some(self.add_optionality(
                &declared_type,
                Some(is_property),
                Some(is_optional),
            ));
        }

        if has_only_expression_initializer(declaration)
            && declaration
                .as_has_initializer()
                .maybe_initializer()
                .is_some()
        {
            let type_ = self.check_declaration_initializer(declaration, Option::<&Type>::None);
            let type_ = self.widen_type_inferred_from_initializer(declaration, &type_);
            return Some(self.add_optionality(&type_, Some(is_property), Some(is_optional)));
        }

        None
    }

    pub(super) fn get_widened_type_for_variable_like_declaration(
        &self,
        declaration: &Node,
    ) -> Rc<Type> {
        self.widen_type_for_variable_like_declaration(
            self.get_type_for_variable_like_declaration(declaration),
            declaration,
        )
    }

    pub(super) fn widen_type_for_variable_like_declaration<TTypeRef: Borrow<Type>>(
        &self,
        type_: Option<TTypeRef>,
        declaration: &Node,
    ) -> Rc<Type> {
        if let Some(type_) = type_ {
            return self.get_widened_type(type_.borrow());
        }
        unimplemented!()
    }

    pub(super) fn try_get_type_from_effective_type_node(
        &self,
        declaration: &Node, /*Declaration*/
    ) -> Option<Rc<Type>> {
        let type_node = get_effective_type_annotation_node(declaration);
        type_node.map(|type_node| self.get_type_from_type_node_(&*type_node))
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let links_type_is_none = { (*links).borrow().type_.is_none() };
        if links_type_is_none {
            let type_ = self.get_type_of_variable_or_parameter_or_property_worker(symbol);
            let mut links_ref = links.borrow_mut();
            if links_ref.type_.is_none() {
                links_ref.type_ = Some(type_);
            }
        }
        let links_ref = (*links).borrow();
        links_ref.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_variable_or_parameter_or_property_worker(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type> {
        Debug_.assert_is_defined(&symbol.maybe_value_declaration(), None);
        let declaration = symbol.maybe_value_declaration().unwrap();

        let type_: Rc<Type>;
        if false {
            unimplemented!()
        } else if is_property_assignment(&declaration) {
            type_ = self
                .try_get_type_from_effective_type_node(&declaration)
                .unwrap_or_else(|| self.check_property_assignment(&declaration, None));
        } else if is_property_signature(&declaration) || is_variable_declaration(&declaration) {
            type_ = self.get_widened_type_for_variable_like_declaration(&declaration);
        } else {
            unimplemented!()
        }

        type_
    }

    pub(super) fn get_base_type_variable_of_class(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_instantiated_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.type_.is_none() {
            let type_ = self.instantiate_type(
                Some(self.get_type_of_symbol(links.target.as_ref().unwrap())),
                links.mapper.as_ref(),
            );
            links.type_ = type_;
        }
        links.type_.clone().unwrap()
    }

    pub(super) fn get_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        let check_flags = get_check_flags(symbol);
        if check_flags.intersects(CheckFlags::Instantiated) {
            return self.get_type_of_instantiated_symbol(symbol);
        }
        if symbol
            .flags()
            .intersects(SymbolFlags::Variable | SymbolFlags::Property)
        {
            return self.get_type_of_variable_or_parameter_or_property(symbol);
        }
        unimplemented!()
    }

    pub(super) fn get_non_missing_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.remove_missing_type(
            &self.get_type_of_symbol(symbol),
            symbol.flags().intersects(SymbolFlags::Optional),
        )
    }

    pub(super) fn append_type_parameters(
        &self,
        mut type_parameters: Option<Vec<Rc<Type>>>,
        declarations: &[Rc<Node>],
    ) -> Option<Vec<Rc<Type>>> {
        for declaration in declarations {
            type_parameters = Some(maybe_append_if_unique_rc(
                type_parameters,
                &self.get_declared_type_of_type_parameter(
                    &self.get_symbol_of_node(&**declaration).unwrap(),
                ),
            ));
        }
        type_parameters
    }

    pub(super) fn get_outer_type_parameters(
        &self,
        node: &Node,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        None
    }

    pub(super) fn get_outer_type_parameters_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declaration = if symbol.flags().intersects(SymbolFlags::Class) {
            symbol.maybe_value_declaration()
        } else {
            get_declaration_of_kind(symbol, SyntaxKind::InterfaceDeclaration)
        };
        Debug_.assert(
            declaration.is_some(),
            Some("Class was missing valueDeclaration -OR- non-class had no interface declarations"),
        );
        let declaration = declaration.unwrap();
        self.get_outer_type_parameters(&*declaration)
    }

    pub(super) fn get_local_type_parameters_of_class_or_interface_or_type_alias(
        &self,
        symbol: &Symbol,
    ) -> Option<Vec<Rc<Type /*TypeParameter*/>>> {
        let declarations = symbol.maybe_declarations();
        if declarations.is_none() {
            return None;
        }
        let declarations = declarations.as_ref().unwrap();
        let mut result: Option<Vec<Rc<Type /*TypeParameter*/>>> = None;
        for node in declarations {
            if node.kind() == SyntaxKind::InterfaceDeclaration
                || node.kind() == SyntaxKind::ClassDeclaration
                || node.kind() == SyntaxKind::ClassExpression
                || false
                || is_type_alias(&**node)
            {
                let declaration = node;
                result = self.append_type_parameters(
                    result,
                    &get_effective_type_parameter_declarations(&*declaration),
                );
            }
        }
        result
    }

    pub(super) fn get_declared_type_of_class_or_interface(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*InterfaceType*/> {
        let links = self.get_symbol_links(symbol);
        let original_links = links.clone();
        let mut original_links_ref = original_links.borrow_mut();
        if original_links_ref.declared_type.is_none() {
            let kind = if symbol.flags().intersects(SymbolFlags::Class) {
                ObjectFlags::Class
            } else {
                ObjectFlags::Interface
            };

            let type_ = self.create_object_type(kind, Some(symbol));
            let outer_type_parameters =
                self.get_outer_type_parameters_of_class_or_interface(symbol);
            let local_type_parameters =
                self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
            let mut need_to_set_constraint = false;
            let type_: InterfaceType = if outer_type_parameters.is_some()
                || local_type_parameters.is_some()
                || kind == ObjectFlags::Class
                || false
            {
                need_to_set_constraint = true;
                type_.set_object_flags(type_.object_flags() | ObjectFlags::Reference);
                let mut this_type = self.create_type_parameter(Some(symbol.symbol_wrapper()));
                this_type.is_this_type = Some(true);
                BaseInterfaceType::new(
                    type_,
                    Some(concatenate(
                        outer_type_parameters.clone().unwrap_or_else(|| vec![]),
                        local_type_parameters.clone().unwrap_or_else(|| vec![]),
                    )),
                    outer_type_parameters,
                    local_type_parameters,
                    Some(this_type.into()),
                )
            } else {
                BaseInterfaceType::new(type_, None, None, None, None)
            }
            .into();
            let type_rc: Rc<Type> = type_.into();
            if need_to_set_constraint {
                *type_rc
                    .as_interface_type()
                    .maybe_this_type_mut()
                    .as_ref()
                    .unwrap()
                    .as_type_parameter()
                    .constraint
                    .borrow_mut() = Some(Rc::downgrade(&type_rc));
            }
            original_links_ref.declared_type = Some(type_rc.clone());
            if !Rc::ptr_eq(&links, &original_links) {
                let mut links_ref = links.borrow_mut();
                links_ref.declared_type = Some(type_rc);
            }
        }
        original_links_ref.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_type_alias(&self, symbol: &Symbol) -> Rc<Type> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            let declaration = Debug_.check_defined(
                symbol
                    .maybe_declarations()
                    .as_ref()
                    .and_then(|declarations| {
                        declarations
                            .iter()
                            .find(|declaration| is_type_alias(&***declaration))
                            .map(|rc| rc.clone())
                    }),
                None,
            );
            let type_node = if false {
                unimplemented!()
            } else {
                Some(declaration.as_type_alias_declaration().type_.clone())
            };
            let type_ = type_node.map_or_else(
                || self.error_type(),
                |type_node| self.get_type_from_type_node_(&type_node),
            );
            if true {
                let type_parameters =
                    self.get_local_type_parameters_of_class_or_interface_or_type_alias(symbol);
                if let Some(type_parameters) = type_parameters {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
            links.declared_type = Some(type_);
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_base_type_of_enum_literal_type(&self, type_: &Type) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_declared_type_of_type_parameter(
        &self,
        symbol: &Symbol,
    ) -> Rc<Type /*TypeParameter*/> {
        let links = self.get_symbol_links(symbol);
        let mut links = links.borrow_mut();
        if links.declared_type.is_none() {
            links.declared_type = Some(
                self.create_type_parameter(Some(symbol.symbol_wrapper()))
                    .into(),
            );
        }
        links.declared_type.clone().unwrap()
    }

    pub(super) fn get_declared_type_of_symbol(&self, symbol: &Symbol) -> Rc<Type> {
        self.try_get_declared_type_of_symbol(symbol)
            .unwrap_or_else(|| unimplemented!())
    }

    pub(super) fn try_get_declared_type_of_symbol(&self, symbol: &Symbol) -> Option<Rc<Type>> {
        if symbol
            .flags()
            .intersects(SymbolFlags::Class | SymbolFlags::Interface)
        {
            return Some(self.get_declared_type_of_class_or_interface(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeAlias) {
            return Some(self.get_declared_type_of_type_alias(symbol));
        }
        if symbol.flags().intersects(SymbolFlags::TypeParameter) {
            return Some(self.get_declared_type_of_type_parameter(symbol));
        }
        unimplemented!()
    }

    pub(super) fn create_instantiated_symbol_table(
        &self,
        symbols: &[Rc<Symbol>],
        mapper: &TypeMapper,
        mapping_this_only: bool,
    ) -> SymbolTable {
        let mut result = create_symbol_table(None);
        for symbol in symbols {
            result.insert(
                symbol.escaped_name().clone(),
                if mapping_this_only && true {
                    symbol.clone()
                } else {
                    self.instantiate_symbol(&symbol, mapper)
                },
            );
        }
        result
    }

    pub(super) fn resolve_declared_members(&self, type_: &Type /*InterfaceType*/) -> Rc<Type> {
        let type_as_interface_type = type_.as_interface_type();
        if type_as_interface_type.maybe_declared_properties().is_none() {
            let symbol = type_.symbol();
            let members = self.get_members_of_symbol(&symbol);
            type_as_interface_type
                .set_declared_properties(self.get_named_members(&*(*members).borrow()));
        }
        type_.type_wrapper()
    }

    pub(super) fn is_type_usable_as_property_name(&self, type_: &Type) -> bool {
        type_
            .flags()
            .intersects(TypeFlags::StringOrNumberLiteralOrUnique)
    }

    pub(super) fn has_bindable_name(&self, node: &Node /*Declaration*/) -> bool {
        !has_dynamic_name(node) || unimplemented!()
    }

    pub(super) fn get_property_name_from_type(
        &self,
        type_: &Type, /*StringLiteralType | NumberLiteralType | UniqueESSymbolType*/
    ) -> __String {
        if type_
            .flags()
            .intersects(TypeFlags::StringLiteral | TypeFlags::NumberLiteral)
        {
            return match type_ {
                Type::LiteralType(LiteralType::NumberLiteralType(number_literal_type)) => {
                    escape_leading_underscores(&number_literal_type.value.to_string())
                }
                Type::LiteralType(LiteralType::StringLiteralType(string_literal_type)) => {
                    escape_leading_underscores(&string_literal_type.value)
                }
                _ => panic!("Expected NumberLiteralType or StringLiteralType"),
            };
        }
        Debug_.fail(None)
    }

    pub(super) fn get_resolved_members_or_exports_of_symbol(
        &self,
        symbol: &Symbol,
        resolution_kind: MembersOrExportsResolutionKind,
    ) -> Rc<RefCell<UnderscoreEscapedMap<Rc<Symbol>>>> {
        unimplemented!()
    }

    pub(super) fn get_members_of_symbol(&self, symbol: &Symbol) -> Rc<RefCell<SymbolTable>> {
        if false {
            unimplemented!()
        } else {
            symbol
                .maybe_members()
                .clone()
                .unwrap_or_else(|| unimplemented!())
        }
    }

    pub(super) fn get_late_bound_symbol(&self, symbol: &Symbol) -> Rc<Symbol> {
        symbol.symbol_wrapper()
    }

    pub(super) fn resolve_object_type_members(
        &self,
        type_: &Type,  /*ObjectType*/
        source: &Type, /*InterfaceTypeWithDeclaredMembers*/
        type_parameters: Vec<Rc<Type /*TypeParameter*/>>,
        type_arguments: Vec<Rc<Type>>,
    ) {
        let mut mapper: Option<TypeMapper> = None;
        let members: Rc<RefCell<SymbolTable>>;
        let call_signatures: Vec<Rc<Signature>>;
        let construct_signatures: Vec<Rc<Signature>>;
        let source_as_interface_type_with_declared_members =
            source.as_interface_type_with_declared_members();
        if range_equals_rc(&type_parameters, &type_arguments, 0, type_parameters.len()) {
            members = if let Some(source_symbol) = source.maybe_symbol() {
                self.get_members_of_symbol(&source_symbol)
            } else {
                unimplemented!()
            };
            call_signatures = source_as_interface_type_with_declared_members
                .declared_call_signatures()
                .clone();
            construct_signatures = source_as_interface_type_with_declared_members
                .declared_construct_signatures()
                .clone();
        } else {
            let type_parameters_len_is_1 = type_parameters.len() == 1;
            mapper = Some(self.create_type_mapper(type_parameters, Some(type_arguments)));
            members = Rc::new(RefCell::new(
                self.create_instantiated_symbol_table(
                    source
                        .as_base_interface_type()
                        .maybe_declared_properties()
                        .as_ref()
                        .unwrap(),
                    mapper.as_ref().unwrap(),
                    type_parameters_len_is_1,
                ),
            ));
            call_signatures = self.instantiate_signatures(
                &*source_as_interface_type_with_declared_members.declared_call_signatures(),
                mapper.as_ref().unwrap(),
            );
            construct_signatures = self.instantiate_signatures(
                &*source_as_interface_type_with_declared_members.declared_construct_signatures(),
                mapper.as_ref().unwrap(),
            );
        }
        self.set_structured_type_members(
            type_.as_object_type(),
            members,
            call_signatures,
            construct_signatures,
            vec![], // TODO: this is wrong
        );
    }

    pub(super) fn resolve_class_or_interface_members(&self, type_: &Type /*InterfaceType*/) {
        self.resolve_object_type_members(
            type_,
            &self.resolve_declared_members(type_),
            vec![],
            vec![],
        );
    }

    pub(super) fn resolve_type_reference_members(&self, type_: &Type /*TypeReference*/) {
        let type_as_type_reference = type_.as_type_reference();
        let source = self.resolve_declared_members(&type_as_type_reference.target);
        let source_as_interface_type = source.as_interface_type();
        let type_parameters = concatenate(
            source_as_interface_type
                .maybe_type_parameters()
                .map(|type_parameters| type_parameters.to_owned())
                .unwrap(),
            vec![source_as_interface_type.maybe_this_type().unwrap()],
        );
        let type_arguments = self.get_type_arguments(type_);
        let padded_type_arguments = if type_arguments.len() == type_parameters.len() {
            type_arguments
        } else {
            concatenate(type_arguments, vec![type_.type_wrapper()])
        };
        self.resolve_object_type_members(type_, &source, type_parameters, padded_type_arguments);
    }

    pub(super) fn create_signature(
        &self,
        declaration: Option<Rc<Node>>,
        type_parameters: Option<Vec<Rc<Type>>>,
        this_parameter: Option<Rc<Symbol>>,
        parameters: Vec<Rc<Symbol>>,
        resolved_return_type: Option<Rc<Type>>,
        resolved_type_predicate: Option<TypePredicate>,
        min_argument_count: usize,
        flags: SignatureFlags,
    ) -> Signature {
        let mut sig = (self.Signature)(flags);
        sig.declaration = declaration;
        sig.type_parameters = type_parameters;
        sig.set_parameters(parameters);
        sig.this_parameter = this_parameter;
        *sig.resolved_return_type.borrow_mut() = resolved_return_type;
        sig.resolved_type_predicate = resolved_type_predicate;
        sig.set_min_argument_count(min_argument_count);
        sig
    }

    pub(super) fn create_union_signature(
        &self,
        signature: Rc<Signature>,
        union_signatures: &[Rc<Signature>],
    ) -> Signature {
        unimplemented!()
    }

    pub(super) fn get_type_parameter_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_constraint_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_name_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_template_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_mapped_type_with_keyof_constraint_declaration(
        &self,
        type_: &Type, /*MappedType*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_modifiers_type_from_mapped_type(
        &self,
        type_: &Type, /*MappedType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_mapped_type_modifiers(
        &self,
        type_: &Type, /*MappedType*/
    ) -> MappedTypeModifiers {
        unimplemented!()
    }

    pub(super) fn is_generic_mapped_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn resolve_structured_type_members(
        &self,
        type_: &Type, /*StructuredType*/
    ) -> Rc<Type /*ResolvedType*/> {
        if !type_.as_resolvable_type().is_resolved() {
            if let Type::ObjectType(object_type) = &*type_
            /*type_.flags().intersects(TypeFlags::Object)*/
            {
                if object_type
                    .object_flags()
                    .intersects(ObjectFlags::Reference)
                {
                    self.resolve_type_reference_members(type_);
                } else if object_type
                    .object_flags()
                    .intersects(ObjectFlags::ClassOrInterface)
                {
                    self.resolve_class_or_interface_members(type_);
                } else {
                    unimplemented!()
                }
            } else {
                unimplemented!()
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn get_properties_of_object_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            return self
                .resolve_structured_type_members(type_)
                .as_resolved_type()
                .properties()
                .iter()
                .map(Clone::clone)
                .collect();
        }
        unimplemented!()
    }

    pub(super) fn get_property_of_object_type(
        &self,
        type_: &Type,
        name: &__String,
    ) -> Option<Rc<Symbol>> {
        if type_.flags().intersects(TypeFlags::Object) {
            let resolved = self.resolve_structured_type_members(type_);
            let symbol = (*resolved.as_resolved_type().members())
                .borrow()
                .get(name)
                .map(Clone::clone);
            if let Some(symbol) = symbol {
                if self.symbol_is_value(&symbol) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub(super) fn get_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        let type_ = self.get_reduced_apparent_type(type_);
        if type_.flags().intersects(TypeFlags::UnionOrIntersection) {
            unimplemented!()
        } else {
            self.get_properties_of_object_type(&type_)
        }
    }

    pub(super) fn for_each_property_of_type<TAction: FnMut(&Symbol, &__String)>(
        &self,
        type_: &Type,
        action: TAction,
    ) {
        unimplemented!()
    }

    pub(super) fn get_constraint_of_type_parameter(
        &self,
        type_parameter: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        if self.has_non_circular_base_constraint(type_parameter) {
            self.get_constraint_from_type_parameter(type_parameter)
        } else {
            None
        }
    }

    pub(super) fn has_non_circular_base_constraint(
        &self,
        type_: &Type, /*InstantiableType*/
    ) -> bool {
        !Rc::ptr_eq(
            &self.get_resolved_base_constraint(type_),
            &self.circular_constraint_type(),
        )
    }

    pub(super) fn get_resolved_base_constraint(
        &self,
        type_: &Type, /*InstantiableType | UnionOrIntersectionType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_default_from_type_parameter_(
        &self,
        type_: &Type, /*TypeParameter*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }
}
