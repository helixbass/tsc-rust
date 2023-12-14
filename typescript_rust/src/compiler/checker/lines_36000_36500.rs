use std::{
    borrow::{Borrow, Cow},
    collections::{HashMap, HashSet},
    io, ptr,
};

use gc::Gc;
use id_arena::Id;

use super::{get_node_id, UnusedKind};
use crate::{
    cast_present, create_diagnostic_for_node, create_file_diagnostic, first, for_each,
    get_class_extends_heritage_element, get_effective_jsdoc_host, get_effective_return_type_node,
    get_effective_type_parameter_declarations, get_function_flags, get_jsdoc_host, get_jsdoc_tags,
    get_jsdoc_type_tag, get_name_of_declaration, get_root_declaration, get_source_file_of_node,
    has_effective_modifier, has_rest_parameter, has_syntactic_modifier, id_text, is_ambient_module,
    is_array_binding_pattern, is_binding_element, is_binding_pattern, is_class_declaration,
    is_class_expression, is_for_in_or_of_statement, is_function_or_module_block, is_identifier,
    is_import_clause, is_import_equals_declaration, is_import_specifier, is_in_js_file,
    is_jsdoc_augments_tag, is_jsdoc_template_tag, is_named_declaration, is_object_binding_pattern,
    is_parameter, is_parameter_property_declaration, is_private_identifier,
    is_private_identifier_class_element_declaration, is_type_only_import_or_export_declaration,
    is_variable_declaration, last, node_is_missing, node_is_present, parameter_is_this_keyword,
    range_of_node, range_of_type_parameters, symbol_name, try_add_to_set, try_cast, try_for_each,
    CharacterCodes, Debug_, Diagnostic, Diagnostics, FunctionFlags, HasArena, InArena,
    JSDocTagInterface, ModifierFlags, NamedDeclarationInterface, Node, NodeFlags, NodeInterface,
    OptionTry, ScriptTarget, SignatureDeclarationInterface, SymbolFlags, SymbolInterface,
    SyntaxKind, TextRange, TypeChecker,
};

impl TypeChecker {
    pub(super) fn check_jsdoc_implements_tag(&self, node: Id<Node> /*JSDocImplementsTag*/) {
        let node_as_jsdoc_implements_tag = node.as_jsdoc_implements_tag();
        let class_like = get_effective_jsdoc_host(node);
        if match class_like.as_ref() {
            None => true,
            Some(class_like) => {
                !is_class_declaration(class_like) && !is_class_expression(class_like)
            }
        } {
            self.error(
                class_like,
                &Diagnostics::JSDoc_0_is_not_attached_to_a_class,
                Some(vec![
                    id_text(&node_as_jsdoc_implements_tag.tag_name()).to_owned()
                ]),
            );
        }
    }

    pub(super) fn check_jsdoc_augments_tag(&self, node: Id<Node> /*JSDocAugmentsTag*/) {
        let node_as_jsdoc_augments_tag = node.as_jsdoc_augments_tag();
        let class_like = get_effective_jsdoc_host(node);
        if match class_like.as_ref() {
            None => true,
            Some(class_like) => {
                !is_class_declaration(class_like) && !is_class_expression(class_like)
            }
        } {
            self.error(
                class_like,
                &Diagnostics::JSDoc_0_is_not_attached_to_a_class,
                Some(vec![
                    id_text(&node_as_jsdoc_augments_tag.tag_name()).to_owned()
                ]),
            );
            return;
        }
        let class_like = class_like.unwrap();

        let augments_tags = get_jsdoc_tags(&class_like)
            .into_iter()
            .filter(|jsdoc_tag| is_jsdoc_augments_tag(jsdoc_tag))
            .collect::<Vec<_>>();
        Debug_.assert(!augments_tags.is_empty(), None);
        if augments_tags.len() > 1 {
            self.error(
                Some(&*augments_tags[1]),
                &Diagnostics::Class_declarations_cannot_have_more_than_one_augments_or_extends_tag,
                None,
            );
        }

        let name = self
            .get_identifier_from_entity_name_expression(
                &node_as_jsdoc_augments_tag
                    .class
                    .as_expression_with_type_arguments()
                    .expression,
            )
            .unwrap();
        let extend = get_class_extends_heritage_element(&class_like);
        if let Some(extend) = extend.as_ref() {
            let class_name = self.get_identifier_from_entity_name_expression(
                &extend.as_expression_with_type_arguments().expression,
            );
            if let Some(class_name) = class_name.as_ref().filter(|class_name| {
                name.as_member_name().escaped_text() != class_name.as_member_name().escaped_text()
            }) {
                self.error(
                    Some(&*name),
                    &Diagnostics::JSDoc_0_1_does_not_match_the_extends_2_clause,
                    Some(vec![
                        id_text(&node_as_jsdoc_augments_tag.tag_name()).to_owned(),
                        id_text(&name).to_owned(),
                        id_text(class_name).to_owned(),
                    ]),
                );
            }
        }
    }

    pub(super) fn check_jsdoc_accessibility_modifiers(
        &self,
        node: Id<Node>, /*JSDocPublicTag | JSDocProtectedTag | JSDocPrivateTag*/
    ) {
        let host = get_jsdoc_host(node);
        if matches!(
            host.as_ref(),
            Some(host) if is_private_identifier_class_element_declaration(host)
        ) {
            self.error(
                Some(node),
                &Diagnostics::An_accessibility_modifier_cannot_be_used_with_a_private_identifier,
                None,
            );
        }
    }

    pub(super) fn get_identifier_from_entity_name_expression(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> Option<Id<Node /*Identifier | PrivateIdentifier*/>> {
        match node.kind() {
            SyntaxKind::Identifier => Some(node.node_wrapper()),
            SyntaxKind::PropertyAccessExpression => {
                Some(node.as_property_access_expression().name.clone())
            }
            _ => None,
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: Id<Node>, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) -> io::Result<()> {
        self.check_decorators(node)?;
        self.check_signature_declaration(node)?;
        let function_flags = get_function_flags(Some(node));

        let node_as_signature_declaration = node.as_signature_declaration();
        if let Some(node_name) = node_as_signature_declaration
            .maybe_name()
            .as_ref()
            .filter(|node_name| node_name.kind() == SyntaxKind::ComputedPropertyName)
        {
            self.check_computed_property_name(node_name)?;
        }

        if self.has_bindable_name(node)? {
            let symbol = self.get_symbol_of_node(node)?.unwrap();
            let local_symbol = node.maybe_local_symbol().unwrap_or_else(|| symbol.clone());

            let first_declaration = local_symbol
                .ref_(self)
                .maybe_declarations()
                .as_ref()
                .and_then(|local_symbol_declarations| {
                    local_symbol_declarations
                        .iter()
                        .find(|declaration| {
                            declaration.kind() == node.kind()
                                && !declaration.flags().intersects(NodeFlags::JavaScriptFile)
                        })
                        .cloned()
                });

            if matches!(
                first_declaration.as_ref(),
                Some(first_declaration) if ptr::eq(node, &**first_declaration)
            ) {
                self.check_function_or_constructor_symbol(local_symbol)?;
            }

            if symbol.ref_(self).maybe_parent().is_some() {
                self.check_function_or_constructor_symbol(symbol)?;
            }
        }

        let body = if node.kind() == SyntaxKind::MethodSignature {
            None
        } else {
            node.as_function_like_declaration().maybe_body()
        };
        self.check_source_element(body.as_deref())?;
        self.check_all_code_paths_in_non_void_function_return_or_throw(
            node,
            self.get_return_type_from_annotation(node)?,
        )?;

        if self.produce_diagnostics && get_effective_return_type_node(node).is_none() {
            if node_is_missing(body.as_deref()) && !self.is_private_within_ambient(node) {
                self.report_implicit_any(node, self.any_type(), None)?;
            }

            if function_flags.intersects(FunctionFlags::Generator)
                && node_is_present(body.as_deref())
            {
                self.get_return_type_of_signature(self.get_signature_from_declaration_(node)?)?;
            }
        }

        if is_in_js_file(Some(node)) {
            let type_tag = get_jsdoc_type_tag(node);
            if let Some(type_tag_type_expression) = type_tag
                .as_ref()
                .and_then(|type_tag| type_tag.as_jsdoc_type_like_tag().maybe_type_expression())
                .as_ref()
                .try_filter(|type_tag_type_expression| -> io::Result<_> {
                    Ok(self
                        .get_contextual_call_signature(
                            self.get_type_from_type_node_(type_tag_type_expression)?,
                            node,
                        )?
                        .is_none())
                })?
            {
                self.error(
                    Some(&*type_tag_type_expression.as_jsdoc_type_expression().type_),
                    &Diagnostics::The_type_of_a_function_declaration_must_match_the_function_s_signature,
                    None,
                );
            }
        }

        Ok(())
    }

    pub(super) fn register_for_unused_identifiers_check(
        &self,
        node: Id<Node>, /*PotentiallyUnusedIdentifier*/
    ) {
        if self.produce_diagnostics {
            let source_file = get_source_file_of_node(node);
            let mut all_potentially_unused_identifiers = self.all_potentially_unused_identifiers();
            let potentially_unused_identifiers = all_potentially_unused_identifiers
                .entry(source_file.as_source_file().path().clone())
                .or_insert_with(|| vec![]);
            potentially_unused_identifiers.push(node.node_wrapper());
        }
    }

    pub(super) fn check_unused_identifiers(
        &self,
        potentially_unused_identifiers: &[Id<Node /*PotentiallyUnusedIdentifier*/>],
        mut add_diagnostic: impl FnMut(Id<Node>, UnusedKind, Gc<Diagnostic>), /*AddUnusedDiagnostic*/
    ) -> io::Result<()> {
        for node in potentially_unused_identifiers {
            match node.kind() {
                SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => {
                    self.check_unused_class_members(node, &mut add_diagnostic)?;
                    self.check_unused_type_parameters(node, &mut add_diagnostic)?;
                }
                SyntaxKind::SourceFile
                | SyntaxKind::ModuleDeclaration
                | SyntaxKind::Block
                | SyntaxKind::CaseBlock
                | SyntaxKind::ForStatement
                | SyntaxKind::ForInStatement
                | SyntaxKind::ForOfStatement => {
                    self.check_unused_locals_and_parameters(node, &mut add_diagnostic);
                }
                SyntaxKind::Constructor
                | SyntaxKind::FunctionExpression
                | SyntaxKind::FunctionDeclaration
                | SyntaxKind::ArrowFunction
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    if node.as_function_like_declaration().maybe_body().is_some() {
                        self.check_unused_locals_and_parameters(node, &mut add_diagnostic);
                    }
                    self.check_unused_type_parameters(node, &mut add_diagnostic)?;
                }
                SyntaxKind::MethodSignature
                | SyntaxKind::CallSignature
                | SyntaxKind::ConstructSignature
                | SyntaxKind::FunctionType
                | SyntaxKind::ConstructorType
                | SyntaxKind::TypeAliasDeclaration
                | SyntaxKind::InterfaceDeclaration => {
                    self.check_unused_type_parameters(node, &mut add_diagnostic)?;
                }
                SyntaxKind::InferType => {
                    self.check_unused_infer_type_parameter(node, &mut add_diagnostic);
                }
                _ => Debug_.assert_never(
                    node,
                    Some("Node should not have been registered for unused identifiers check"),
                ),
            }
        }

        Ok(())
    }

    pub(super) fn error_unused_local<
        TAddDiagnostic: FnMut(Id<Node>, UnusedKind, Gc<Diagnostic>),
    >(
        &self,
        declaration: Id<Node>, /*Declaration*/
        name: &str,
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        let node = get_name_of_declaration(Some(declaration))
            .unwrap_or_else(|| declaration.node_wrapper());
        let message = if self.is_type_declaration(declaration) {
            &*Diagnostics::_0_is_declared_but_never_used
        } else {
            &*Diagnostics::_0_is_declared_but_its_value_is_never_read
        };
        add_diagnostic(
            declaration,
            UnusedKind::Local,
            Gc::new(create_diagnostic_for_node(&node, message, Some(vec![name.to_owned()])).into()),
        );
    }

    pub(super) fn is_identifier_that_starts_with_underscore(&self, node: Id<Node>) -> bool {
        is_identifier(node) && id_text(node).chars().next() == Some(CharacterCodes::underscore)
    }

    pub(super) fn check_unused_class_members(
        &self,
        node: Id<Node>, /*ClassDeclaration | ClassExpression*/
        add_diagnostic: &mut impl FnMut(Id<Node>, UnusedKind, Gc<Diagnostic>),
    ) -> io::Result<()> {
        for member in &node.as_class_like_declaration().members() {
            match member.kind() {
                SyntaxKind::MethodDeclaration
                | SyntaxKind::PropertyDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    if !(member.kind() == SyntaxKind::SetAccessor
                        && member
                            .symbol()
                            .ref_(self)
                            .flags()
                            .intersects(SymbolFlags::GetAccessor))
                    {
                        let symbol = self.get_symbol_of_node(member)?.unwrap();
                        if match symbol.ref_(self).maybe_is_referenced() {
                            None => true,
                            Some(symbol_is_referenced) => symbol_is_referenced == SymbolFlags::None,
                        } && (has_effective_modifier(member, ModifierFlags::Private)
                            || is_named_declaration(member)
                                && is_private_identifier(&member.as_named_declaration().name()))
                            && !member.flags().intersects(NodeFlags::Ambient)
                        {
                            add_diagnostic(
                                member,
                                UnusedKind::Local,
                                Gc::new(
                                    create_diagnostic_for_node(
                                        &member.as_named_declaration().name(),
                                        &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                                        Some(vec![self.symbol_to_string_(
                                            symbol,
                                            Option::<Id<Node>>::None,
                                            None,
                                            None,
                                            None,
                                        )?]),
                                    )
                                    .into(),
                                ),
                            );
                        }
                    }
                }
                SyntaxKind::Constructor => {
                    for parameter in &member.as_constructor_declaration().parameters() {
                        if match parameter.symbol().ref_(self).maybe_is_referenced() {
                            None => true,
                            Some(parameter_symbol_is_referenced) => {
                                parameter_symbol_is_referenced == SymbolFlags::None
                            }
                        } && has_syntactic_modifier(parameter, ModifierFlags::Private)
                        {
                            add_diagnostic(
                                parameter,
                                UnusedKind::Local,
                                Gc::new(
                                    create_diagnostic_for_node(
                                        &parameter.as_parameter_declaration().name(),
                                        &Diagnostics::Property_0_is_declared_but_its_value_is_never_read,
                                        Some(vec![
                                            symbol_name(&parameter.symbol().ref_(self)).into_owned()
                                        ])
                                    ).into()
                                )
                            );
                        }
                    }
                }
                SyntaxKind::IndexSignature
                | SyntaxKind::SemicolonClassElement
                | SyntaxKind::ClassStaticBlockDeclaration => (),
                _ => Debug_.fail(Some("Unexpected class member")),
            }
        }

        Ok(())
    }

    pub(super) fn check_unused_infer_type_parameter<
        TAddDiagnostic: FnMut(Id<Node>, UnusedKind, Gc<Diagnostic>),
    >(
        &self,
        node: Id<Node>, /*InferTypeNode*/
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        let type_parameter = &node.as_infer_type_node().type_parameter;
        if self.is_type_parameter_unused(type_parameter) {
            add_diagnostic(
                node,
                UnusedKind::Parameter,
                Gc::new(
                    create_diagnostic_for_node(
                        node,
                        &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                        Some(vec![id_text(
                            &type_parameter.as_type_parameter_declaration().name(),
                        )
                        .to_owned()]),
                    )
                    .into(),
                ),
            );
        }
    }

    pub(super) fn check_unused_type_parameters(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration | SignatureDeclaration | InterfaceDeclaration | TypeAliasDeclaration*/
        add_diagnostic: &mut impl FnMut(Id<Node>, UnusedKind, Gc<Diagnostic>),
    ) -> io::Result<()> {
        let symbol = self.get_symbol_of_node(node)?.unwrap();
        let symbol_ref = symbol.ref_(self);
        let declarations = symbol_ref.maybe_declarations();
        if match declarations.as_ref() {
            None => true,
            Some(declarations) => !ptr::eq(&**last(declarations), node),
        } {
            return Ok(());
        }

        let type_parameters = get_effective_type_parameter_declarations(node);
        let mut seen_parents_with_every_unused: HashSet<*const Node> = HashSet::new();

        for type_parameter in &type_parameters {
            if !self.is_type_parameter_unused(type_parameter) {
                continue;
            }

            let type_parameter_name = type_parameter.as_type_parameter_declaration().name();
            let name = id_text(&type_parameter_name);
            let parent = type_parameter.parent();
            if parent.kind() != SyntaxKind::InferType
                && parent
                    .as_has_type_parameters()
                    .maybe_type_parameters()
                    .as_ref()
                    .unwrap()
                    .into_iter()
                    .all(|type_parameter| self.is_type_parameter_unused(type_parameter))
            {
                if try_add_to_set(&mut seen_parents_with_every_unused, &*parent) {
                    let source_file = get_source_file_of_node(&parent);
                    let range = if is_jsdoc_template_tag(&parent) {
                        range_of_node(&parent)
                    } else {
                        range_of_type_parameters(
                            &source_file,
                            parent
                                .as_has_type_parameters()
                                .maybe_type_parameters()
                                .as_ref()
                                .unwrap(),
                        )
                    };
                    let only = parent
                        .as_has_type_parameters()
                        .maybe_type_parameters()
                        .as_ref()
                        .unwrap()
                        .len()
                        == 1;
                    let message = if only {
                        &*Diagnostics::_0_is_declared_but_its_value_is_never_read
                    } else {
                        &*Diagnostics::All_type_parameters_are_unused
                    };
                    let args = if only {
                        Some(vec![name.to_owned()])
                    } else {
                        None
                    };
                    add_diagnostic(
                        type_parameter,
                        UnusedKind::Parameter,
                        Gc::new(
                            create_file_diagnostic(
                                &source_file,
                                range.pos(),
                                range.end() - range.pos(),
                                message,
                                args,
                            )
                            .into(),
                        ),
                    );
                }
            } else {
                add_diagnostic(
                    type_parameter,
                    UnusedKind::Parameter,
                    Gc::new(
                        create_diagnostic_for_node(
                            type_parameter,
                            &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                            Some(vec![name.to_owned()]),
                        )
                        .into(),
                    ),
                );
            }
        }

        Ok(())
    }

    pub(super) fn is_type_parameter_unused(
        &self,
        type_parameter: Id<Node>, /*TypeParameterDeclaration*/
    ) -> bool {
        !matches!(
            self.get_merged_symbol(type_parameter.maybe_symbol()).unwrap().ref_(self).maybe_is_referenced(),
            Some(type_parameter_symbol_is_referenced) if type_parameter_symbol_is_referenced.intersects(SymbolFlags::TypeParameter)
        ) && !self.is_identifier_that_starts_with_underscore(
            &type_parameter.as_type_parameter_declaration().name(),
        )
    }

    pub(super) fn add_to_group<TKey, TValue, TGetKey: FnMut(&TKey) -> String>(
        &self,
        map: &mut HashMap<String, (TKey, Vec<TValue>)>,
        key: TKey,
        value: TValue,
        mut get_key: TGetKey,
    ) {
        let key_string = get_key(&key);
        let group = map.entry(key_string).or_insert_with(|| (key, vec![]));
        group.1.push(value);
    }

    pub(super) fn try_get_root_parameter_declaration(
        &self,
        node: Id<Node>,
    ) -> Option<Id<Node /*ParameterDeclaration*/>> {
        try_cast(get_root_declaration(node), |root_declaration: &Id<Node>| {
            is_parameter(root_declaration)
        })
    }

    pub(super) fn is_valid_unused_local_declaration(
        &self,
        declaration: Id<Node>, /*Declaration*/
    ) -> bool {
        if is_binding_element(declaration) {
            let declaration_as_binding_element = declaration.as_binding_element();
            if is_object_binding_pattern(&declaration.parent()) {
                return declaration_as_binding_element.property_name.is_some()
                    && self.is_identifier_that_starts_with_underscore(
                        &declaration_as_binding_element.name(),
                    );
            }
            return self
                .is_identifier_that_starts_with_underscore(&declaration_as_binding_element.name());
        }
        is_ambient_module(declaration)
            || (is_variable_declaration(declaration)
                && is_for_in_or_of_statement(&declaration.parent().parent())
                || self.is_imported_declaration(declaration))
                && self.is_identifier_that_starts_with_underscore(
                    &declaration.as_named_declaration().name(),
                )
    }

    pub(super) fn check_unused_locals_and_parameters<
        TAddDiagnostic: FnMut(Id<Node>, UnusedKind, Gc<Diagnostic>),
    >(
        &self,
        node_with_locals: Id<Node>,
        add_diagnostic: &mut TAddDiagnostic,
    ) {
        let mut unused_imports: HashMap<
            String,
            (
                Id<Node /*ImportClause*/>,
                Vec<Id<Node /*ImportedDeclaration*/>>,
            ),
        > = HashMap::new();
        let mut unused_destructures: HashMap<
            String,
            (
                Id<Node /*BindingPattern*/>,
                Vec<Id<Node /*BindingElement*/>>,
            ),
        > = HashMap::new();
        let mut unused_variables: HashMap<
            String,
            (
                Id<Node /*VariableDeclarationList*/>,
                Vec<Id<Node /*VariableDeclaration*/>>,
            ),
        > = HashMap::new();
        for &local in (*node_with_locals.locals()).borrow().values() {
            if if local
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::TypeParameter)
            {
                !(local.ref_(self).flags().intersects(SymbolFlags::Variable)
                    && !matches!(
                        local.ref_(self).maybe_is_referenced(),
                        Some(local_is_referenced) if local_is_referenced.intersects(SymbolFlags::Variable)
                    ))
            } else {
                (match local.ref_(self).maybe_is_referenced() {
                    None => false,
                    Some(local_is_referenced) => local_is_referenced != SymbolFlags::None,
                }) || local.ref_(self).maybe_export_symbol().is_some()
            } {
                continue;
            }

            if let Some(local_declarations) = local.ref_(self).maybe_declarations().as_ref() {
                for declaration in local_declarations {
                    if self.is_valid_unused_local_declaration(declaration) {
                        continue;
                    }

                    if self.is_imported_declaration(declaration) {
                        self.add_to_group(
                            &mut unused_imports,
                            self.import_clause_from_imported(declaration),
                            declaration.clone(),
                            |key: &Id<Node>| get_node_id(key).to_string(),
                        );
                    } else if is_binding_element(declaration)
                        && is_object_binding_pattern(&declaration.parent())
                    {
                        let declaration_parent = declaration.parent();
                        let last_element =
                            last(&declaration_parent.as_object_binding_pattern().elements);
                        if Gc::ptr_eq(declaration, last_element)
                            || last(&declaration_parent.as_object_binding_pattern().elements)
                                .as_binding_element()
                                .dot_dot_dot_token
                                .is_none()
                        {
                            self.add_to_group(
                                &mut unused_destructures,
                                declaration_parent.clone(),
                                declaration.clone(),
                                |key: &Id<Node>| get_node_id(key).to_string(),
                            );
                        }
                    } else if is_variable_declaration(declaration) {
                        self.add_to_group(
                            &mut unused_variables,
                            declaration.parent(),
                            declaration.clone(),
                            |key: &Id<Node>| get_node_id(key).to_string(),
                        );
                    } else {
                        let parameter = local
                            .ref_(self)
                            .maybe_value_declaration()
                            .as_ref()
                            .and_then(|local_value_declaration| {
                                self.try_get_root_parameter_declaration(local_value_declaration)
                            });
                        let name = local
                            .ref_(self)
                            .maybe_value_declaration()
                            .as_ref()
                            .and_then(|local_value_declaration| {
                                get_name_of_declaration(Some(&**local_value_declaration))
                            });
                        if let (Some(parameter), Some(name)) = (parameter.as_ref(), name.as_ref()) {
                            if !is_parameter_property_declaration(parameter, &parameter.parent())
                                && !parameter_is_this_keyword(parameter)
                                && !self.is_identifier_that_starts_with_underscore(name)
                            {
                                if is_binding_element(declaration)
                                    && is_array_binding_pattern(&declaration.parent())
                                {
                                    self.add_to_group(
                                        &mut unused_destructures,
                                        declaration.parent(),
                                        declaration.clone(),
                                        |key: &Id<Node>| get_node_id(key).to_string(),
                                    );
                                } else {
                                    add_diagnostic(
                                        parameter,
                                        UnusedKind::Parameter,
                                        Gc::new(
                                            create_diagnostic_for_node(
                                                name,
                                                &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                                                Some(vec![
                                                    symbol_name(&local.ref_(self)).into_owned()
                                                ])
                                            ).into()
                                        )
                                    );
                                }
                            }
                        } else {
                            self.error_unused_local(
                                declaration,
                                &symbol_name(&local.ref_(self)),
                                add_diagnostic,
                            );
                        }
                    }
                }
            }
        }
        for (import_clause, unuseds) in unused_imports.values() {
            let import_decl = import_clause.parent();
            let import_clause_as_import_clause = import_clause.as_import_clause();
            let n_declarations = if import_clause_as_import_clause.name.is_some() {
                1
            } else {
                0
            } + if let Some(import_clause_named_bindings) =
                import_clause_as_import_clause.named_bindings.as_ref()
            {
                if import_clause_named_bindings.kind() == SyntaxKind::NamespaceImport {
                    1
                } else {
                    import_clause_named_bindings
                        .as_named_imports()
                        .elements
                        .len()
                }
            } else {
                0
            };
            if n_declarations == unuseds.len() {
                add_diagnostic(
                    &import_decl,
                    UnusedKind::Local,
                    Gc::new(if unuseds.len() == 1 {
                        create_diagnostic_for_node(
                            &import_decl,
                            &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                            Some(vec![
                                id_text(&first(unuseds).as_named_declaration().name()).to_owned()
                            ]),
                        )
                        .into()
                    } else {
                        create_diagnostic_for_node(
                            &import_decl,
                            &Diagnostics::All_imports_in_import_declaration_are_unused,
                            None,
                        )
                        .into()
                    }),
                );
            } else {
                for unused in unuseds {
                    self.error_unused_local(
                        unused,
                        &id_text(&unused.as_named_declaration().name()),
                        add_diagnostic,
                    );
                }
            }
        }
        for (binding_pattern, binding_elements) in unused_destructures.values() {
            let kind = if self
                .try_get_root_parameter_declaration(&binding_pattern.parent())
                .is_some()
            {
                UnusedKind::Parameter
            } else {
                UnusedKind::Local
            };
            if binding_pattern.as_has_elements().elements().len() == binding_elements.len() {
                if binding_elements.len() == 1
                    && binding_pattern.parent().kind() == SyntaxKind::VariableDeclaration
                    && binding_pattern.parent().parent().kind()
                        == SyntaxKind::VariableDeclarationList
                {
                    self.add_to_group(
                        &mut unused_variables,
                        binding_pattern.parent().parent(),
                        binding_pattern.parent(),
                        |key: &Id<Node>| get_node_id(key).to_string(),
                    );
                } else {
                    add_diagnostic(
                        binding_pattern,
                        kind,
                        Gc::new(if binding_elements.len() == 1 {
                            create_diagnostic_for_node(
                                binding_pattern,
                                &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                                Some(vec![self
                                    .binding_name_text(
                                        &first(binding_elements).as_binding_element().name(),
                                    )
                                    .into_owned()]),
                            )
                            .into()
                        } else {
                            create_diagnostic_for_node(
                                binding_pattern,
                                &Diagnostics::All_destructured_elements_are_unused,
                                None,
                            )
                            .into()
                        }),
                    );
                }
            } else {
                for e in binding_elements {
                    add_diagnostic(
                        e,
                        kind,
                        Gc::new(
                            create_diagnostic_for_node(
                                e,
                                &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                                Some(vec![self
                                    .binding_name_text(&e.as_binding_element().name())
                                    .into_owned()]),
                            )
                            .into(),
                        ),
                    );
                }
            }
        }
        for (declaration_list, declarations) in unused_variables.values() {
            if declaration_list
                .as_variable_declaration_list()
                .declarations
                .len()
                == declarations.len()
            {
                add_diagnostic(
                    declaration_list,
                    UnusedKind::Local,
                    Gc::new(if declarations.len() == 1 {
                        create_diagnostic_for_node(
                            &first(declarations).as_variable_declaration().name(),
                            &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                            Some(vec![self
                                .binding_name_text(
                                    &first(declarations).as_variable_declaration().name(),
                                )
                                .into_owned()]),
                        )
                        .into()
                    } else {
                        create_diagnostic_for_node(
                            &*if declaration_list.parent().kind() == SyntaxKind::VariableStatement {
                                declaration_list.parent()
                            } else {
                                declaration_list.clone()
                            },
                            &Diagnostics::All_variables_are_unused,
                            None,
                        )
                        .into()
                    }),
                );
            } else {
                for decl in declarations {
                    add_diagnostic(
                        decl,
                        UnusedKind::Local,
                        Gc::new(
                            create_diagnostic_for_node(
                                decl,
                                &Diagnostics::_0_is_declared_but_its_value_is_never_read,
                                Some(vec![self
                                    .binding_name_text(&decl.as_variable_declaration().name())
                                    .into_owned()]),
                            )
                            .into(),
                        ),
                    );
                }
            }
        }
    }

    pub(super) fn binding_name_text<'name>(
        &self,
        name: Id<Node>, /*BindingName*/
    ) -> Cow<'name, str> {
        match name.kind() {
            SyntaxKind::Identifier => id_text(name).into(),
            SyntaxKind::ArrayBindingPattern | SyntaxKind::ObjectBindingPattern => self
                .binding_name_text(
                    &*cast_present(
                        first(&**name.as_has_elements().elements()),
                        |element: &&Id<Node>| is_binding_element(element),
                    )
                    .as_binding_element()
                    .name(),
                )
                .into_owned()
                .into(),
            _ => Debug_.assert_never(name, None),
        }
    }

    pub(super) fn is_imported_declaration(&self, node: Id<Node>) -> bool {
        matches!(
            node.kind(),
            SyntaxKind::ImportClause | SyntaxKind::ImportSpecifier | SyntaxKind::NamespaceImport
        )
    }

    pub(super) fn import_clause_from_imported(
        &self,
        decl: Id<Node>, /*ImportedDeclaration*/
    ) -> Id<Node /*ImportClause*/> {
        if decl.kind() == SyntaxKind::ImportClause {
            decl.node_wrapper()
        } else if decl.kind() == SyntaxKind::NamespaceImport {
            decl.parent()
        } else {
            decl.parent().parent()
        }
    }

    pub(super) fn check_block(
        &self,
        node: Id<Node>, /*Block (actually | ModuleBlock)*/
    ) -> io::Result<()> {
        if node.kind() == SyntaxKind::Block {
            self.check_grammar_statement_in_ambient_context(node);
        }
        let node_as_has_statements = node.as_has_statements();
        if is_function_or_module_block(node) {
            let save_flow_analysis_disabled = self.flow_analysis_disabled();
            try_for_each(
                &node_as_has_statements.statements(),
                |statement, _| -> io::Result<_> {
                    self.check_source_element(Some(&**statement))?;
                    Ok(Option::<()>::None)
                },
            )?;
            self.set_flow_analysis_disabled(save_flow_analysis_disabled);
        } else {
            try_for_each(
                &node_as_has_statements.statements(),
                |statement, _| -> io::Result<_> {
                    self.check_source_element(Some(&**statement))?;
                    Ok(Option::<()>::None)
                },
            )?;
        }
        if node.maybe_locals().is_some() {
            self.register_for_unused_identifiers_check(node);
        }

        Ok(())
    }

    pub(super) fn check_collision_with_arguments_in_generated_code(
        &self,
        node: Id<Node>, /*SignatureDeclaration*/
    ) {
        if self.language_version >= ScriptTarget::ES2015
            || !has_rest_parameter(node)
            || node.flags().intersects(NodeFlags::Ambient)
            || node_is_missing(
                node.maybe_as_function_like_declaration()
                    .and_then(|node| node.maybe_body()),
            )
        {
            return;
        }

        for_each(
            &node.as_signature_declaration().parameters(),
            |p: &Id<Node>, _| -> Option<()> {
                if matches!(
                    p.as_parameter_declaration().maybe_name().as_ref(),
                    Some(p_name) if !is_binding_pattern(Some(&**p_name)) &&
                        &p_name.as_identifier().escaped_text == self.arguments_symbol().ref_(self).escaped_name()
                ) {
                    self.error_skipped_on(
                        "noEmit".to_owned(),
                        Some(&**p),
                        &Diagnostics::Duplicate_identifier_arguments_Compiler_uses_arguments_to_initialize_rest_parameters,
                        None,
                    );
                }
                None
            },
        );
    }

    pub(super) fn need_collision_check_for_identifier(
        &self,
        node: Id<Node>,
        identifier: Option<Id<Node> /*Identifier*/>,
        name: &str,
    ) -> bool {
        if identifier.is_none() {
            return false;
        }
        let identifier = identifier.unwrap();
        let identifier: Id<Node> = identifier.borrow();
        if identifier.as_identifier().escaped_text != name {
            return false;
        }

        if matches!(
            node.kind(),
            SyntaxKind::PropertyDeclaration
                | SyntaxKind::PropertySignature
                | SyntaxKind::MethodDeclaration
                | SyntaxKind::MethodSignature
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor
                | SyntaxKind::PropertyAssignment
        ) {
            return false;
        }

        if node.flags().intersects(NodeFlags::Ambient) {
            return false;
        }

        if is_import_clause(node) || is_import_equals_declaration(node) || is_import_specifier(node)
        {
            if is_type_only_import_or_export_declaration(node) {
                return false;
            }
        }

        let root = get_root_declaration(node);
        if is_parameter(&root)
            && node_is_missing(root.parent().as_function_like_declaration().maybe_body())
        {
            return false;
        }

        true
    }
}
