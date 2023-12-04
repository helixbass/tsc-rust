use std::{
    borrow::{Borrow, Cow},
    cell::RefCell,
    collections::HashSet,
    convert::{TryFrom, TryInto},
    io, ptr,
};

use gc::{Gc, GcCell};
use id_arena::Id;

use super::{get_symbol_id, NodeBuilderContext, TypeFacts};
use crate::{
    are_option_gcs_equal, create_printer, create_symbol_table, declaration_name_to_string,
    escape_string, find_ancestor, first_defined, get_check_flags, get_combined_modifier_flags,
    get_declaration_modifier_flags_from_symbol, get_emit_script_target, get_factory,
    get_first_identifier, get_name_of_declaration, get_root_declaration, has_effective_modifier,
    is_ambient_module, is_bindable_object_define_property_call, is_binding_pattern,
    is_call_expression, is_computed_property_name, is_external_module_augmentation,
    is_identifier_text, is_internal_module_import_equals_declaration, is_left_hand_side_expression,
    is_source_file, map, maybe_get_source_file_of_node, parse_node_factory, push_if_unique_gc,
    return_ok_default_if_none, return_ok_none_if_none, set_parent, set_text_range, starts_with,
    symbol_name, try_add_to_set, try_map, try_maybe_for_each, try_using_single_line_string_writer,
    walk_up_parenthesized_types, CharacterCodes, CheckFlags, EmitHint, EmitTextWriter,
    InterfaceTypeInterface, InternalSymbolName, LiteralType, ModifierFlags,
    NamedDeclarationInterface, Node, NodeBuilderFlags, NodeFlags, NodeInterface, ObjectFlags,
    ObjectFlagsTypeInterface, OptionTry, PrinterOptionsBuilder, Symbol, SymbolFlags, SymbolId,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeFormatFlags, TypeInterface,
    TypePredicate, TypePredicateKind, TypeReferenceInterface, TypeSystemEntity,
    TypeSystemPropertyName, UnionOrIntersectionTypeInterface,
};

impl TypeChecker {
    pub fn type_predicate_to_string_(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        Ok(if let Some(writer) = writer {
            self.type_predicate_to_string_worker(
                type_predicate,
                enclosing_declaration,
                flags,
                writer.clone(),
            )?;
            writer.get_text()
        } else {
            try_using_single_line_string_writer(|writer| {
                self.type_predicate_to_string_worker(
                    type_predicate,
                    enclosing_declaration,
                    flags,
                    writer,
                )
            })?
        })
    }

    pub(super) fn type_predicate_to_string_worker(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: TypeFormatFlags,
        writer: Gc<Box<dyn EmitTextWriter>>,
    ) -> io::Result<()> {
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let predicate = get_factory().create_type_predicate_node(
            if matches!(
                type_predicate.kind,
                TypePredicateKind::AssertsThis | TypePredicateKind::AssertsIdentifier
            ) {
                Some(get_factory().create_token(SyntaxKind::AssertsKeyword))
            } else {
                None
            },
            if matches!(
                type_predicate.kind,
                TypePredicateKind::Identifier | TypePredicateKind::AssertsIdentifier
            ) {
                get_factory().create_identifier(type_predicate.parameter_name.as_ref().unwrap())
            } else {
                get_factory().create_this_type_node()
            },
            type_predicate.type_.as_ref().try_and_then(|type_| {
                self.node_builder().type_to_type_node(
                    type_,
                    enclosing_declaration.as_deref(),
                    Some(
                        self.to_node_builder_flags(Some(flags))
                            | NodeBuilderFlags::IgnoreErrors
                            | NodeBuilderFlags::WriteTypeParametersInQualifiedName,
                    ),
                    None,
                )
            })?,
        );
        let printer = create_printer(
            PrinterOptionsBuilder::default()
                .remove_comments(Some(true))
                .build()
                .unwrap(),
            None,
        );
        let source_file = enclosing_declaration
            .as_deref()
            .and_then(|enclosing_declaration| {
                maybe_get_source_file_of_node(Some(enclosing_declaration))
            });
        printer.write_node(
            EmitHint::Unspecified,
            &predicate,
            source_file.as_deref(),
            writer,
        )?;
        // return writer;

        Ok(())
    }

    pub(super) fn format_union_types(&self, types: &[Id<Type>]) -> io::Result<Vec<Id<Type>>> {
        let mut result: Vec<Id<Type>> = vec![];
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
                        self.get_base_type_of_enum_literal_type(t)?
                    };
                    if base_type.flags().intersects(TypeFlags::Union) {
                        let base_type_as_union_type = base_type.as_union_type();
                        let count = base_type_as_union_type.types().len();
                        if i + count <= types.len()
                            && Gc::ptr_eq(
                                &self.get_regular_type_of_literal_type(&*types[i + count - 1]),
                                &self.get_regular_type_of_literal_type(
                                    &base_type_as_union_type.types()[count - 1],
                                ),
                            )
                        {
                            result.push(base_type);
                            i += count - 1 + 1;
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
        Ok(result) /*|| types*/
    }

    pub(super) fn visibility_to_string(&self, flags: ModifierFlags) -> &'static str /*>*/ {
        if flags == ModifierFlags::Private {
            return "private";
        }
        if flags == ModifierFlags::Protected {
            return "protected";
        }
        "public"
    }

    pub(super) fn get_type_alias_for_type_literal(
        &self,
        type_: Id<Type>,
    ) -> io::Result<Option<Gc<Symbol>>> {
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
        Ok(None)
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
        let name_type = (*self.get_symbol_links(symbol))
            .borrow()
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
                self.get_name_of_symbol_as_written(&name_type.symbol(), context)
            ));
        }
        None
    }

    pub(super) fn get_name_of_symbol_as_written<'symbol>(
        &self,
        symbol: &'symbol Symbol,
        context: Option<&NodeBuilderContext>,
    ) -> Cow<'symbol, str> {
        if let Some(context) = context {
            if symbol.escaped_name() == InternalSymbolName::Default
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
                            Some(context_enclosing_declaration) if are_option_gcs_equal(
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
                let mut declaration = first_defined(symbol_declarations, |d: &Gc<Node>, _| {
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
                            return symbol_name(symbol);
                        }
                        if is_computed_property_name(&name)
                            && !get_check_flags(symbol).intersects(CheckFlags::Late)
                        {
                            let name_type =
                                (*self.get_symbol_links(symbol)).borrow().name_type.clone();
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
                            if !context.encountered_error()
                                && !context
                                    .flags()
                                    .intersects(NodeBuilderFlags::AllowAnonymousIdentifier)
                            {
                                context.set_encountered_error(true);
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
            name.into()
        } else {
            symbol_name(symbol)
        }
    }

    pub(super) fn is_declaration_visible(&self, node: &Node) -> bool {
        // if (node) {
        let links = self.get_node_links(node);
        if (*links).borrow().is_visible.is_none() {
            links.borrow_mut().is_visible = Some(self.determine_if_declaration_is_visible(node));
        }
        let ret = (*links).borrow().is_visible.unwrap();
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
    ) -> io::Result<Option<Vec<Gc<Node>>>> {
        let mut export_symbol: Option<Gc<Symbol>> = None;
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
            )?;
        } else if node.parent().kind() == SyntaxKind::ExportSpecifier {
            export_symbol = self.get_target_of_export_specifier(
                &node.parent(),
                SymbolFlags::Value
                    | SymbolFlags::Type
                    | SymbolFlags::Namespace
                    | SymbolFlags::Alias,
                None,
            )?;
        }
        let result: RefCell<Option<Vec<Gc<Node>>>> = RefCell::new(None);
        if let Some(export_symbol) = export_symbol {
            let mut visited: HashSet<SymbolId> = HashSet::new();
            visited.insert(get_symbol_id(&export_symbol));
            self.build_visible_node_list(
                set_visibility.unwrap_or(false),
                &result,
                &mut visited,
                export_symbol.maybe_declarations().as_deref(),
            )?;
        }
        Ok(result.into_inner())
    }

    pub(super) fn build_visible_node_list(
        &self,
        set_visibility: bool,
        result: &RefCell<Option<Vec<Gc<Node>>>>,
        visited: &mut HashSet<SymbolId>,
        declarations: Option<&[Gc<Node /*Declaration*/>]>,
    ) -> io::Result<()> {
        try_maybe_for_each(declarations, |declaration: &Gc<Node>, _| -> io::Result<_> {
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
                push_if_unique_gc(result.as_mut().unwrap(), &result_node);
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
                    Option::<Gc<Node>>::None,
                    false,
                    None,
                )?;
                if let Some(import_symbol) = import_symbol
                /*&& visited*/
                {
                    if try_add_to_set(visited, get_symbol_id(&import_symbol)) {
                        self.build_visible_node_list(
                            set_visibility,
                            result,
                            visited,
                            import_symbol.maybe_declarations().as_deref(),
                        )?;
                    }
                }
            }
            Ok(Option::<()>::None)
        })?;

        Ok(())
    }

    pub(super) fn push_type_resolution(
        &self,
        target: &TypeSystemEntity,
        property_name: TypeSystemPropertyName,
    ) -> bool {
        let resolution_cycle_start_index =
            self.find_resolution_cycle_start_index(target, property_name);
        if resolution_cycle_start_index >= 0 {
            let length = self.resolution_targets().len();
            let mut resolution_results = self.resolution_results();
            for i in resolution_cycle_start_index.try_into().unwrap()..length {
                resolution_results[i] = false;
            }
            return false;
        }
        self.resolution_targets().push(target.clone());
        self.resolution_results().push(true);
        self.resolution_property_names().push(property_name);
        true
    }

    pub(super) fn find_resolution_cycle_start_index(
        &self,
        target: &TypeSystemEntity,
        property_name: TypeSystemPropertyName,
    ) -> isize {
        let resolution_targets = self.resolution_targets();
        let mut i: isize = isize::try_from(resolution_targets.len()).unwrap() - 1;
        let resolution_property_names = self.resolution_property_names();
        while i >= 0 {
            let i_as_usize: usize = i.try_into().unwrap();
            if self.has_type(
                &resolution_targets[i_as_usize],
                resolution_property_names[i_as_usize],
            ) {
                return -1;
            }
            if &resolution_targets[i_as_usize] == target
                && resolution_property_names[i_as_usize] == property_name
            {
                return i;
            }
            i -= 1;
        }
        -1
    }

    pub(super) fn has_type(
        &self,
        target: &TypeSystemEntity,
        property_name: TypeSystemPropertyName,
    ) -> bool {
        match property_name {
            TypeSystemPropertyName::Type => (*self.get_symbol_links(target.as_symbol()))
                .borrow()
                .type_
                .is_some(),
            TypeSystemPropertyName::EnumTagType => (*self.get_node_links(target.as_node()))
                .borrow()
                .resolved_enum_type
                .is_some(),
            TypeSystemPropertyName::DeclaredType => (*self.get_symbol_links(target.as_symbol()))
                .borrow()
                .declared_type
                .is_some(),
            TypeSystemPropertyName::ResolvedBaseConstructorType => target
                .as_type()
                .as_not_actually_interface_type()
                .maybe_resolved_base_constructor_type()
                .is_some(),
            TypeSystemPropertyName::ResolvedReturnType => {
                target.as_signature().maybe_resolved_return_type().is_some()
            }
            TypeSystemPropertyName::ImmediateBaseConstraint => {
                target.as_type().maybe_immediate_base_constraint().is_some()
            }
            TypeSystemPropertyName::ResolvedTypeArguments => target
                .as_type()
                .as_type_reference()
                .maybe_resolved_type_arguments()
                .is_some(),
            TypeSystemPropertyName::ResolvedBaseTypes => matches!(
                target
                    .as_type()
                    .as_not_actually_interface_type()
                    .maybe_base_types_resolved(),
                Some(true)
            ),
        }
        // Debug.assertNever(propertyName);
    }

    pub(super) fn pop_type_resolution(&self) -> bool {
        self.resolution_targets().pop();
        self.resolution_property_names().pop();
        self.resolution_results().pop().unwrap()
    }

    pub(super) fn get_declaration_container(&self, node: &Node) -> Gc<Node> {
        find_ancestor(Some(get_root_declaration(node)), |node| {
            !matches!(
                node.kind(),
                SyntaxKind::VariableDeclaration
                    | SyntaxKind::VariableDeclarationList
                    | SyntaxKind::ImportSpecifier
                    | SyntaxKind::NamedImports
                    | SyntaxKind::NamespaceImport
                    | SyntaxKind::ImportClause
            )
        })
        .unwrap()
        .parent()
    }

    pub(super) fn get_type_of_prototype_property(
        &self,
        prototype: &Symbol,
    ) -> io::Result<Id<Type>> {
        let class_type =
            self.get_declared_type_of_symbol(&self.get_parent_of_symbol(prototype)?.unwrap())?;
        Ok(
            if let Some(class_type_type_parameters) = class_type
                .as_interface_type()
                .maybe_type_parameters()
                .as_deref()
            {
                self.create_type_reference(
                    &class_type,
                    Some(map(class_type_type_parameters, |_, _| self.any_type())),
                )
            } else {
                class_type
            },
        )
    }

    pub(super) fn get_type_of_property_of_type_(
        &self,
        type_: Id<Type>,
        name: &str, /*__String*/
    ) -> io::Result<Option<Id<Type>>> {
        let prop = return_ok_none_if_none!(self.get_property_of_type_(type_, name, None)?);
        Ok(Some(self.get_type_of_symbol(&prop)?))
    }

    pub(super) fn get_type_of_property_or_index_signature(
        &self,
        type_: Id<Type>,
        name: &str, /*__String*/
    ) -> io::Result<Id<Type>> {
        Ok(self
            .get_type_of_property_of_type_(type_, name)?
            .try_or_else(|| -> io::Result<_> {
                Ok(self
                    .get_applicable_index_info_for_name(type_, name)?
                    .map(|index_info| index_info.type_.clone()))
            })?
            .unwrap_or_else(|| self.unknown_type()))
    }

    pub(super) fn is_type_any(&self, type_: Option<Id<Type>>) -> bool {
        match type_ {
            Some(type_) => {
                let type_ = type_.borrow();
                type_.flags().intersects(TypeFlags::Any)
            }
            None => false,
        }
    }

    pub(super) fn is_error_type(&self, type_: Id<Type>) -> bool {
        ptr::eq(type_, &*self.error_type())
            || type_.flags().intersects(TypeFlags::Any) && type_.maybe_alias_symbol().is_some()
    }

    pub(super) fn get_type_for_binding_element_parent(
        &self,
        node: &Node, /*BindingElementGrandparent*/
    ) -> io::Result<Option<Id<Type>>> {
        let symbol = self.get_symbol_of_node(node)?;
        symbol
            .as_ref()
            .and_then(|symbol| (*self.get_symbol_links(symbol)).borrow().type_.clone())
            .try_or_else(|| self.get_type_for_variable_like_declaration(node, false))
    }

    pub(super) fn get_rest_type(
        &self,
        source: Id<Type>,
        properties: &[Gc<Node /*PropertyName*/>],
        symbol: Option<impl Borrow<Symbol>>,
    ) -> io::Result<Id<Type>> {
        let source = self.filter_type(source, |t| !t.flags().intersects(TypeFlags::Nullable));
        if source.flags().intersects(TypeFlags::Never) {
            return Ok(self.empty_object_type());
        }
        let symbol = symbol.map(|symbol| symbol.borrow().symbol_wrapper());
        if source.flags().intersects(TypeFlags::Union) {
            return Ok(self
                .try_map_type(
                    &source,
                    &mut |t| {
                        Ok(Some(self.get_rest_type(
                            t,
                            properties,
                            symbol.as_deref(),
                        )?))
                    },
                    None,
                )?
                .unwrap());
        }
        let omit_key_type = self.get_union_type(
            &try_map(properties, |property: &Gc<Node>, _| {
                self.get_literal_type_from_property_name(property)
            })?,
            None,
            Option::<&Symbol>::None,
            None,
            None,
        )?;
        if self.is_generic_object_type(&source)? || self.is_generic_index_type(&omit_key_type)? {
            if omit_key_type.flags().intersects(TypeFlags::Never) {
                return Ok(source);
            }

            let omit_type_alias = self.get_global_omit_symbol()?;
            if omit_type_alias.is_none() {
                return Ok(self.error_type());
            }
            let omit_type_alias = omit_type_alias.unwrap();
            return self.get_type_alias_instantiation(
                &omit_type_alias,
                Some(&vec![source, omit_key_type]),
                Option::<&Symbol>::None,
                None,
            );
        }
        let mut members = create_symbol_table(Option::<&[Gc<Symbol>]>::None);
        for prop in self.get_properties_of_type(&source)? {
            if !self.is_type_assignable_to(
                &*self.get_literal_type_from_property(
                    &prop,
                    TypeFlags::StringOrNumberLiteralOrUnique,
                    None,
                )?,
                &omit_key_type,
            )? && !get_declaration_modifier_flags_from_symbol(&prop, None)
                .intersects(ModifierFlags::Private | ModifierFlags::Protected)
                && self.is_spreadable_property(&prop)
            {
                members.insert(
                    prop.escaped_name().to_owned(),
                    self.get_spread_symbol(&prop, false)?,
                );
            }
        }
        let result = self.create_anonymous_type(
            symbol,
            Gc::new(GcCell::new(members)),
            vec![],
            vec![],
            self.get_index_infos_of_type(&source)?,
        )?;
        let result_as_interface_type = result.as_interface_type();
        result_as_interface_type.set_object_flags(
            result_as_interface_type.object_flags() | ObjectFlags::ObjectRestType,
        );
        Ok(result)
    }

    pub(super) fn is_generic_type_with_undefined_constraint(
        &self,
        type_: Id<Type>,
    ) -> io::Result<bool> {
        Ok(type_.flags().intersects(TypeFlags::Instantiable)
            && self.maybe_type_of_kind(
                &self
                    .get_base_constraint_of_type(type_)?
                    .unwrap_or_else(|| self.unknown_type()),
                TypeFlags::Undefined,
            ))
    }

    pub(super) fn get_non_undefined_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        let type_or_constraint = if self.try_some_type(type_, |type_| {
            self.is_generic_type_with_undefined_constraint(type_)
        })? {
            self.try_map_type(
                type_,
                &mut |t| {
                    Ok(Some(if t.flags().intersects(TypeFlags::Instantiable) {
                        self.get_base_constraint_or_type(t)?
                    } else {
                        t.type_wrapper()
                    }))
                },
                None,
            )?
            .unwrap()
        } else {
            type_.type_wrapper()
        };
        self.get_type_with_facts(&type_or_constraint, TypeFacts::NEUndefined)
    }

    pub(super) fn get_flow_type_of_destructuring(
        &self,
        node: &Node, /*BindingElement | PropertyAssignment | ShorthandPropertyAssignment | Expression*/
        declared_type: Id<Type>,
    ) -> io::Result<Id<Type>> {
        let reference = self.get_synthetic_element_access(node)?;
        Ok(if let Some(reference) = reference {
            self.get_flow_type_of_reference(&reference, declared_type, None, Option::<&Node>::None)?
        } else {
            declared_type.type_wrapper()
        })
    }

    pub(super) fn get_synthetic_element_access(
        &self,
        node: &Node, /*BindingElement | PropertyAssignment | ShorthandPropertyAssignment | Expression*/
    ) -> io::Result<Option<Gc<Node /*ElementAccessExpression*/>>> {
        let parent_access = return_ok_default_if_none!(self.get_parent_element_access(node)?);
        let ret = parent_access.maybe_flow_node().clone().try_and_then(
            |parent_access_flow_node| -> io::Result<_> {
                let prop_name =
                    return_ok_default_if_none!(self.get_destructuring_property_name(node)?);
                let literal = parse_node_factory.with(|parse_node_factory_| {
                    parse_node_factory_.create_string_literal(prop_name, None, None)
                });
                set_text_range(&*literal, Some(node));
                let lhs_expr = if is_left_hand_side_expression(&parent_access) {
                    parent_access.clone()
                } else {
                    parse_node_factory.with(|parse_node_factory_| {
                        parse_node_factory_.create_parenthesized_expression(parent_access.clone())
                    })
                };
                let result = parse_node_factory.with(|parse_node_factory_| {
                    parse_node_factory_
                        .create_element_access_expression(lhs_expr.clone(), literal.clone())
                });
                set_text_range(&*result, Some(node));
                set_parent(&literal, Some(&*result));
                set_parent(&result, Some(node));
                if !Gc::ptr_eq(&lhs_expr, &parent_access) {
                    set_parent(&lhs_expr, Some(&*result));
                }
                result.set_flow_node(Some(parent_access_flow_node));
                Ok(Some(result))
            },
        )?;
        Ok(ret)
    }
}
