use std::{borrow::Borrow, io};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use super::{ClassFacts, TransformTypeScript};
use crate::{
    add_range, child_is_decorated, class_or_constructor_parameter_is_decorated, create_token_range,
    get_effective_base_type_node, get_emit_flags, get_first_constructor_with_body, get_properties,
    get_strict_option_value, has_syntactic_modifier, insert_statements_after_standard_prologue,
    is_class_element, is_external_module, is_heritage_clause, is_identifier, is_json_source_file,
    is_modifier, is_parameter_property_declaration, is_static, maybe_visit_nodes,
    move_range_past_decorators, node_or_child_is_decorated, parameter_is_this_keyword,
    set_emit_flags, skip_outer_expressions, skip_trivia, some, try_maybe_visit_each_child,
    try_maybe_visit_nodes, try_visit_each_child, try_visit_lexical_environment_full,
    try_visit_nodes, AsDoubleDeref, BoolExt, ClassLikeDeclarationInterface, EmitFlags,
    HasStatementsInterface, InterfaceOrClassLikeDeclarationInterface, Matches, ModifierFlags,
    ModuleKind, NamedDeclarationInterface, Node, NodeArray, NodeArrayExt, NodeArrayOrVec, NodeExt,
    NodeFlags, NodeInterface, ReadonlyTextRange, ScriptTarget,
    SignatureDeclarationInterface, SourceFileLike, SyntaxKind, TextRange, TransformFlags,
    VisitResult, HasArena, InArena,
    CoreTransformationContext,
};

impl TransformTypeScript {
    pub(super) fn visit_source_file(
        &self,
        node: Id<Node>, /*SourceFile*/
    ) -> io::Result<Id<Node>> {
        let always_strict = get_strict_option_value(&self.compiler_options.ref_(self), "alwaysStrict")
            && !(is_external_module(&node.ref_(self)) && self.module_kind >= ModuleKind::ES2015)
            && !is_json_source_file(&node.ref_(self));

        Ok(self.factory.ref_(self).update_source_file(
            node,
            try_visit_lexical_environment_full(
                &node.ref_(self).as_source_file().statements(),
                |node: Id<Node>| self.source_element_visitor(node),
                &*self.context.ref_(self),
                Some(0),
                Some(always_strict),
                Option::<
                    fn(
                        Option<Id<NodeArray>>,
                        Option<&mut dyn FnMut(Id<Node>) -> io::Result<VisitResult>>,
                        Option<&dyn Fn(Id<Node>) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> io::Result<Option<Id<NodeArray>>>,
                >::None,
                self,
            )?,
            None,
            None,
            None,
            None,
            None,
        ))
    }

    pub(super) fn get_class_facts(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
        static_properties: &[Id<Node /*PropertyDeclaration*/>],
    ) -> ClassFacts {
        let mut facts = ClassFacts::None;
        if !static_properties.is_empty() {
            facts |= ClassFacts::HasStaticInitializedProperties;
        }
        let extends_clause_element = get_effective_base_type_node(node, self);
        if extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                extends_clause_element
                    .ref_(self).as_expression_with_type_arguments()
                    .expression,
                None,
                self,
            )
            .ref_(self).kind()
                != SyntaxKind::NullKeyword
        }) {
            facts |= ClassFacts::IsDerivedClass;
        }
        if class_or_constructor_parameter_is_decorated(node, self) {
            facts |= ClassFacts::HasConstructorDecorators;
        }
        if child_is_decorated(node, None, self) {
            facts |= ClassFacts::HasMemberDecorators;
        }
        if self.is_export_of_namespace(node) {
            facts |= ClassFacts::IsExportOfNamespace;
        } else if self.is_default_external_module_export(node) {
            facts |= ClassFacts::IsDefaultExternalExport;
        } else if self.is_named_external_module_export(node) {
            facts |= ClassFacts::IsNamedExternalExport;
        }
        if self.language_version <= ScriptTarget::ES5
            && facts.intersects(ClassFacts::MayNeedImmediatelyInvokedFunctionExpression)
        {
            facts |= ClassFacts::UseImmediatelyInvokedFunctionExpression;
        }
        facts
    }

    pub(super) fn has_type_script_class_syntax(&self, node: Id<Node>) -> bool {
        node.ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsTypeScriptClassSyntax)
    }

    pub(super) fn is_class_like_declaration_with_type_script_syntax(
        &self,
        node: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        some(
            node.ref_(self).maybe_decorators().as_double_deref(),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) || some(
            node_as_class_like_declaration
                .maybe_type_parameters()
                .as_double_deref(),
            Option::<fn(&Id<Node>) -> bool>::None,
        ) || some(
            node_as_class_like_declaration
                .maybe_heritage_clauses()
                .as_double_deref(),
            Some(|&heritage_clause: &Id<Node>| self.has_type_script_class_syntax(heritage_clause)),
        ) || some(
            Some(&**node_as_class_like_declaration.members()),
            Some(|&member: &Id<Node>| self.has_type_script_class_syntax(member)),
        )
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_ref = node.ref_(self);
        let node_as_class_declaration = node_ref.as_class_declaration();
        #[allow(clippy::nonminimal_bool)]
        if !self.is_class_like_declaration_with_type_script_syntax(node)
            && !(self.maybe_current_namespace().is_some()
                && has_syntactic_modifier(node, ModifierFlags::Export, self))
        {
            return Ok(try_maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )?
            .map(Into::into));
        }

        let static_properties = get_properties(node, true, true, self);
        let facts = self.get_class_facts(node, &static_properties);

        if facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression) {
            self.context.ref_(self).start_lexical_environment();
        }

        let name = node_as_class_declaration.maybe_name().or_else(|| {
            if facts.intersects(ClassFacts::NeedsName) {
                Some(self.factory.ref_(self).get_generated_name_for_node(Some(node), None))
            } else {
                None
            }
        });
        let class_statement = if facts.intersects(ClassFacts::HasConstructorDecorators) {
            self.create_class_declaration_head_with_decorators(node, name)?
        } else {
            self.create_class_declaration_head_without_decorators(node, name, facts)?
        };

        let mut statements: Vec<Id<Node /*Stateent*/>> = vec![class_statement.clone()];

        self.add_class_element_decoration_statements(&mut statements, node, false)?;
        self.add_class_element_decoration_statements(&mut statements, node, true)?;
        self.add_constructor_decoration_statement(&mut statements, node)?;

        if facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression) {
            let closing_brace_location = create_token_range(
                skip_trivia(
                    &self.current_source_file().ref_(self).as_source_file().text_as_chars(),
                    node_as_class_declaration.members().end(),
                    None,
                    None,
                    None,
                ),
                SyntaxKind::CloseBraceToken,
            );
            let local_name = self.factory.ref_(self).get_internal_name(node, None, None);

            let outer = self
                .factory
                .ref_(self).create_partially_emitted_expression(local_name, None)
                .set_text_range_end(closing_brace_location.end(), self)
                .set_emit_flags(EmitFlags::NoComments, self);

            let statement = self
                .factory
                .ref_(self).create_return_statement(Some(outer))
                .set_text_range_pos(closing_brace_location.pos(), self)
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoTokenSourceMaps, self);
            statements.push(statement);

            insert_statements_after_standard_prologue(
                &mut statements,
                self.context.ref_(self).end_lexical_environment().as_deref(),
                self,
            );

            let iife = self
                .factory
                .ref_(self).create_immediately_invoked_arrow_function(statements, None, None)
                .set_emit_flags(EmitFlags::TypeScriptClassWrapper, self);

            let var_statement = self
                .factory
                .ref_(self).create_variable_statement(
                    Option::<Id<NodeArray>>::None,
                    self.factory.ref_(self).create_variable_declaration_list(
                        vec![self.factory.ref_(self).create_variable_declaration(
                            Some(self.factory.ref_(self).get_local_name(node, Some(false), Some(false))),
                            None,
                            None,
                            Some(iife),
                        )],
                        None,
                    ),
                )
                .set_original_node(Some(node), self)
                .set_comment_range(&*node.ref_(self), self)
                .set_source_map_range(Some(self.alloc_source_map_range((&move_range_past_decorators(&node.ref_(self))).into())), self)
                .start_on_new_line(self);
            statements = vec![var_statement];
        }

        if facts.intersects(ClassFacts::IsExportOfNamespace) {
            self.add_export_member_assignment(&mut statements, node);
        } else if facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression)
            || facts.intersects(ClassFacts::HasConstructorDecorators)
        {
            if facts.intersects(ClassFacts::IsDefaultExternalExport) {
                statements.push(
                    self.factory
                        .ref_(self).create_export_default(self.factory.ref_(self).get_local_name(
                            node,
                            Some(false),
                            Some(true),
                        )),
                );
            } else if facts.intersects(ClassFacts::IsNamedExternalExport) {
                statements.push(self.factory.ref_(self).create_external_module_export(
                    self.factory.ref_(self).get_local_name(node, Some(false), Some(true)),
                ));
            }
        }

        if statements.len() > 1 {
            statements.push(
                self.factory
                    .ref_(self).create_end_of_declaration_marker(node),
            );
            set_emit_flags(
                class_statement,
                get_emit_flags(&class_statement.ref_(self)) | EmitFlags::HasEndOfDeclarationMarker,
                self,
            );
        }

        Ok(Some(if statements.len() == 1 {
            statements[0].clone().into()
        } else {
            statements.into()
        }))
    }

    pub(super) fn create_class_declaration_head_without_decorators(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
        name: Option<Id<Node /*Identifier*/>>,
        facts: ClassFacts,
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_class_declaration = node_ref.as_class_declaration();
        let modifiers = (!(facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression)))
            .then_and(|| {
                maybe_visit_nodes(
                    node.ref_(self).maybe_modifiers().as_deref(),
                    Some(|node: Id<Node>| self.modifier_visitor(node)),
                    Some(|node: Id<Node>| is_modifier(&node.ref_(self))),
                    None,
                    None,
                    self,
                )
            });

        let class_declaration = self.factory.ref_(self).create_class_declaration(
            Option::<Id<NodeArray>>::None,
            modifiers,
            name,
            Option::<Id<NodeArray>>::None,
            try_maybe_visit_nodes(
                node_as_class_declaration
                    .maybe_heritage_clauses()
                    .as_deref(),
                Some(|node: Id<Node>| self.visitor(node)),
                Some(|node: Id<Node>| is_heritage_clause(&node.ref_(self))),
                None,
                None,
                self,
            )?,
            self.transform_class_members(node)?,
        );

        let mut emit_flags = get_emit_flags(&node.ref_(self));
        if facts.intersects(ClassFacts::HasStaticInitializedProperties) {
            emit_flags |= EmitFlags::NoTrailingSourceMap;
        }

        Ok(class_declaration
            .set_text_range(Some(&*node.ref_(self)), self)
            .set_original_node(Some(node), self)
            .set_emit_flags(emit_flags, self))
    }

    pub(super) fn create_class_declaration_head_with_decorators(
        &self,
        node: Id<Node>, /*ClassDeclaration*/
        name: Option<Id<Node /*Identifier*/>>,
    ) -> io::Result<Id<Node>> {
        let node_ref = node.ref_(self);
        let node_as_class_declaration = node_ref.as_class_declaration();
        let location = move_range_past_decorators(&node.ref_(self));
        let class_alias = self.get_class_alias_if_needed(node);

        let decl_name = if self.language_version <= ScriptTarget::ES2015 {
            self.factory
                .ref_(self).get_internal_name(node, Some(false), Some(true))
        } else {
            self.factory.ref_(self).get_local_name(node, Some(false), Some(true))
        };

        let heritage_clauses = try_maybe_visit_nodes(
            node_as_class_declaration
                .maybe_heritage_clauses()
                .as_deref(),
            Some(|node: Id<Node>| self.visitor(node)),
            Some(|node: Id<Node>| is_heritage_clause(&node.ref_(self))),
            None,
            None,
            self,
        )?;
        let members = self.transform_class_members(node)?;
        let class_expression = self
            .factory
            .ref_(self).create_class_expression(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                name,
                Option::<Id<NodeArray>>::None,
                heritage_clauses,
                members,
            )
            .set_original_node(Some(node), self)
            .set_text_range(Some(&location.to_readonly_text_range()), self);

        Ok(self
            .factory
            .ref_(self).create_variable_statement(
                Option::<Id<NodeArray>>::None,
                self.factory.ref_(self).create_variable_declaration_list(
                    vec![self.factory.ref_(self).create_variable_declaration(
                        Some(decl_name),
                        None,
                        None,
                        Some(if let Some(class_alias) = class_alias {
                            self.factory
                                .ref_(self).create_assignment(class_alias, class_expression)
                        } else {
                            class_expression
                        }),
                    )],
                    Some(NodeFlags::Let),
                ),
            )
            .set_original_node(Some(node), self)
            .set_text_range(Some(&location.to_readonly_text_range()), self)
            .set_comment_range(&*node.ref_(self), self))
    }

    pub(super) fn visit_class_expression(
        &self,
        node: Id<Node>, /*ClassExpression*/
    ) -> io::Result<Id<Node /*<Expression>*/>> {
        let node_ref = node.ref_(self);
        let node_as_class_expression = node_ref.as_class_expression();
        if !self.is_class_like_declaration_with_type_script_syntax(node) {
            return try_visit_each_child(
                node,
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            );
        }

        Ok(self
            .factory
            .ref_(self).create_class_expression(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                node_as_class_expression.maybe_name(),
                Option::<Id<NodeArray>>::None,
                try_maybe_visit_nodes(
                    node_as_class_expression.maybe_heritage_clauses().as_deref(),
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node: Id<Node>| is_heritage_clause(&node.ref_(self))),
                    None,
                    None,
                    self,
                )?,
                self.transform_class_members(node)?,
            )
            .set_original_node(Some(node), self)
            .set_text_range(Some(&*node.ref_(self)), self))
    }

    pub(super) fn transform_class_members(
        &self,
        node: Id<Node>, /*ClassDeclaration | ClassExpression*/
    ) -> io::Result<Id<NodeArray>> {
        let mut members: Vec<Id<Node /*ClassElement*/>> = Default::default();
        let constructor = get_first_constructor_with_body(node, self);
        let parameters_with_property_assignments = constructor.map(|constructor| {
            constructor
                .ref_(self).as_constructor_declaration()
                .parameters()
                .owned_iter()
                .filter(move |&p| is_parameter_property_declaration(p, constructor, self))
        });
        if let Some(parameters_with_property_assignments) = parameters_with_property_assignments {
            for parameter in parameters_with_property_assignments {
                let parameter_ref = parameter.ref_(self);
                let parameter_as_parameter_declaration = parameter_ref.as_parameter_declaration();
                if is_identifier(&parameter_as_parameter_declaration.name().ref_(self)) {
                    members.push(
                        self.factory
                            .ref_(self).create_property_declaration(
                                Option::<Id<NodeArray>>::None,
                                Option::<Id<NodeArray>>::None,
                                parameter_as_parameter_declaration.name(),
                                None,
                                None,
                                None,
                            )
                            .set_original_node(Some(parameter), self),
                    );
                }
            }
        }

        let node_ref = node.ref_(self);
        let node_as_class_like_declaration = node_ref.as_class_like_declaration();
        add_range(
            &mut members,
            Some(&try_visit_nodes(
                &node_as_class_like_declaration.members(),
                Some(|node: Id<Node>| self.class_element_visitor(node)),
                Some(|node: Id<Node>| is_class_element(&node.ref_(self))),
                None,
                None,
                self,
            )?),
            None,
            None,
        );
        Ok(self
            .factory
            .ref_(self).create_node_array(Some(members), None)
            .set_text_range(Some(&*node_as_class_like_declaration.members()), self))
    }

    pub(super) fn get_decorated_class_elements<'a>(
        &'a self,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
        is_static: bool,
    ) -> impl Iterator<Item = Id<Node /*ClassElement*/>> + 'a {
        node.ref_(self).as_class_like_declaration()
            .members()
            .owned_iter()
            .filter(move |&m| {
                if is_static {
                    self.is_static_decorated_class_element(m, node)
                } else {
                    self.is_instance_decorated_class_element(m, node)
                }
            })
    }

    pub(super) fn is_static_decorated_class_element(
        &self,
        member: Id<Node>, /*ClassElement*/
        parent: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        self.is_decorated_class_element(member, true, parent)
    }

    pub(super) fn is_instance_decorated_class_element(
        &self,
        member: Id<Node>, /*ClassElement*/
        parent: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        self.is_decorated_class_element(member, false, parent)
    }

    pub(super) fn is_decorated_class_element(
        &self,
        member: Id<Node>, /*ClassElement*/
        is_static_element: bool,
        parent: Id<Node>, /*ClassLikeDeclaration*/
    ) -> bool {
        node_or_child_is_decorated(member, Some(parent), None, self)
            && is_static_element == is_static(member, self)
    }

    pub(super) fn get_decorators_of_parameters(
        &self,
        node: Option<Id<Node /*FunctionLikeDeclaration*/>>,
    ) -> Option<Vec<Option<NodeArrayOrVec>>> {
        let mut decorators: Option<Vec<Option<NodeArrayOrVec /*Decorator*/>>> = Default::default();
        if let Some(node) = node {
            let node_ref = node.ref_(self);
            let node_as_function_like_declaration = node_ref.as_function_like_declaration();
            let parameters = node_as_function_like_declaration.parameters();
            let first_parameter_is_this =
                !parameters.is_empty() && parameter_is_this_keyword(parameters[0], self);
            let first_parameter_offset = if first_parameter_is_this { 1 } else { 0 };
            let num_parameters = if first_parameter_is_this {
                parameters.len() - 1
            } else {
                parameters.len()
            };
            for i in 0..num_parameters {
                let parameter = &parameters[i + first_parameter_offset];
                if decorators.is_some() || parameter.ref_(self).maybe_decorators().is_some() {
                    decorators.get_or_insert_with(|| vec![None; num_parameters])[i] =
                        parameter.ref_(self).maybe_decorators().map(Into::into);
                }
            }
        }

        decorators
    }

    pub(super) fn get_all_decorators_of_constructor(
        &self,
        node: Id<Node>, /*ClassExpression | ClassDeclaration*/
    ) -> Option<AllDecorators> {
        let decorators = node.ref_(self).maybe_decorators();
        let parameters = self.get_decorators_of_parameters(get_first_constructor_with_body(node, self));
        if decorators.is_none() && parameters.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters,
        })
    }
}

#[derive(Trace, Finalize)]
pub(super) struct AllDecorators {
    pub decorators: Option<NodeArrayOrVec /*Decorator*/>,
    pub parameters: Option<Vec<Option<NodeArrayOrVec /*Decorator*/>>>,
}
