use std::borrow::Borrow;

use gc::{Finalize, Gc, Trace};

use crate::{
    add_range, child_is_decorated, class_or_constructor_parameter_is_decorated, create_token_range,
    get_effective_base_type_node, get_emit_flags, get_first_constructor_with_body, get_properties,
    get_strict_option_value, has_syntactic_modifier, insert_statements_after_standard_prologue,
    is_class_element, is_external_module, is_heritage_clause, is_identifier, is_json_source_file,
    is_modifier, is_parameter_property_declaration, is_static, move_range_past_decorators,
    node_or_child_is_decorated, parameter_is_this_keyword, set_emit_flags, skip_outer_expressions,
    skip_trivia, some, visit_each_child, visit_lexical_environment, visit_nodes, AsDoubleDeref,
    BoolExt, ClassLikeDeclarationInterface, EmitFlags, HasStatementsInterface,
    InterfaceOrClassLikeDeclarationInterface, Matches, ModifierFlags, ModuleKind,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayExt, NodeArrayOrVec, NodeExt, NodeFlags,
    NodeInterface, NodeWrappered, ReadonlyTextRange, ScriptTarget, SignatureDeclarationInterface,
    SourceFileLike, SyntaxKind, TextRange, TransformFlags, VisitResult,
};

use super::{ClassFacts, TransformTypeScript};
use crate::try_visit_each_child;
use crate::try_visit_lexical_environment;
use crate::try_visit_nodes;
use std::io;

impl TransformTypeScript {
    pub(super) fn visit_source_file(
        &self,
        node: &Node, /*SourceFile*/
    ) -> io::Result<Gc<Node>> {
        let always_strict = get_strict_option_value(&self.compiler_options, "alwaysStrict")
            && !(is_external_module(node) && self.module_kind >= ModuleKind::ES2015)
            && !is_json_source_file(node);

        Ok(self.factory.update_source_file(
            node,
            try_visit_lexical_environment(
                &node.as_source_file().statements(),
                |node: &Node| self.source_element_visitor(node),
                &**self.context,
                Some(0),
                Some(always_strict),
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> io::Result<Option<Gc<NodeArray>>>,
                >::None,
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
        node: &Node, /*ClassDeclaration*/
        static_properties: &[Gc<Node /*PropertyDeclaration*/>],
    ) -> ClassFacts {
        let mut facts = ClassFacts::None;
        if !static_properties.is_empty() {
            facts |= ClassFacts::HasStaticInitializedProperties;
        }
        let extends_clause_element = get_effective_base_type_node(node);
        if extends_clause_element.matches(|extends_clause_element| {
            skip_outer_expressions(
                &extends_clause_element
                    .as_expression_with_type_arguments()
                    .expression,
                None,
            )
            .kind()
                != SyntaxKind::NullKeyword
        }) {
            facts |= ClassFacts::IsDerivedClass;
        }
        if class_or_constructor_parameter_is_decorated(node) {
            facts |= ClassFacts::HasConstructorDecorators;
        }
        if child_is_decorated(node, Option::<&Node>::None) {
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

    pub(super) fn has_type_script_class_syntax(&self, node: &Node) -> bool {
        node.transform_flags()
            .intersects(TransformFlags::ContainsTypeScriptClassSyntax)
    }

    pub(super) fn is_class_like_declaration_with_type_script_syntax(
        &self,
        node: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        some(
            node.maybe_decorators().as_double_deref(),
            Option::<fn(&Gc<Node>) -> bool>::None,
        ) || some(
            node_as_class_like_declaration
                .maybe_type_parameters()
                .as_double_deref(),
            Option::<fn(&Gc<Node>) -> bool>::None,
        ) || some(
            node_as_class_like_declaration
                .maybe_heritage_clauses()
                .as_double_deref(),
            Some(|heritage_clause: &Gc<Node>| self.has_type_script_class_syntax(heritage_clause)),
        ) || some(
            Some(&**node_as_class_like_declaration.members()),
            Some(|member: &Gc<Node>| self.has_type_script_class_syntax(member)),
        )
    }

    pub(super) fn visit_class_declaration(
        &self,
        node: &Node, /*ClassDeclaration*/
    ) -> io::Result<VisitResult> /*<Statement>*/ {
        let node_as_class_declaration = node.as_class_declaration();
        if !self.is_class_like_declaration_with_type_script_syntax(node)
            && !(self.maybe_current_namespace().is_some()
                && has_syntactic_modifier(node, ModifierFlags::Export))
        {
            return Ok(try_visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .map(Into::into));
        }

        let static_properties = get_properties(node, true, true);
        let facts = self.get_class_facts(node, &static_properties);

        if facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression) {
            self.context.start_lexical_environment();
        }

        let name = node_as_class_declaration.maybe_name().or_else(|| {
            if facts.intersects(ClassFacts::NeedsName) {
                Some(self.factory.get_generated_name_for_node(Some(node), None))
            } else {
                None
            }
        });
        let class_statement = if facts.intersects(ClassFacts::HasConstructorDecorators) {
            self.create_class_declaration_head_with_decorators(node, name.as_deref())?
        } else {
            self.create_class_declaration_head_without_decorators(node, name.as_deref(), facts)?
        };

        let mut statements: Vec<Gc<Node /*Stateent*/>> = vec![class_statement.clone()];

        self.add_class_element_decoration_statements(&mut statements, node, false)?;
        self.add_class_element_decoration_statements(&mut statements, node, true)?;
        self.add_constructor_decoration_statement(&mut statements, node)?;

        if facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression) {
            let closing_brace_location = create_token_range(
                skip_trivia(
                    &self.current_source_file().as_source_file().text_as_chars(),
                    node_as_class_declaration.members().end(),
                    None,
                    None,
                    None,
                ),
                SyntaxKind::CloseBraceToken,
            );
            let local_name = self.factory.get_internal_name(node, None, None);

            let outer = self
                .factory
                .create_partially_emitted_expression(local_name, None)
                .wrap()
                .set_text_range_end(closing_brace_location.end())
                .set_emit_flags(EmitFlags::NoComments);

            let statement = self
                .factory
                .create_return_statement(Some(outer))
                .wrap()
                .set_text_range_pos(closing_brace_location.pos())
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoTokenSourceMaps);
            statements.push(statement);

            insert_statements_after_standard_prologue(
                &mut statements,
                self.context.end_lexical_environment().as_deref(),
            );

            let iife = self
                .factory
                .create_immediately_invoked_arrow_function(statements, None, None)
                .set_emit_flags(EmitFlags::TypeScriptClassWrapper);

            let var_statement = self
                .factory
                .create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory
                        .create_variable_declaration_list(
                            vec![self
                                .factory
                                .create_variable_declaration(
                                    Some(self.factory.get_local_name(
                                        node,
                                        Some(false),
                                        Some(false),
                                    )),
                                    None,
                                    None,
                                    Some(iife),
                                )
                                .wrap()],
                            None,
                        )
                        .wrap(),
                )
                .wrap()
                .set_original_node(Some(node.node_wrapper()))
                .set_comment_range(node)
                .set_source_map_range(Some((&move_range_past_decorators(node)).into()))
                .start_on_new_line();
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
                        .create_export_default(self.factory.get_local_name(
                            node,
                            Some(false),
                            Some(true),
                        )),
                );
            } else if facts.intersects(ClassFacts::IsNamedExternalExport) {
                statements.push(self.factory.create_external_module_export(
                    self.factory.get_local_name(node, Some(false), Some(true)),
                ));
            }
        }

        if statements.len() > 1 {
            statements.push(
                self.factory
                    .create_end_of_declaration_marker(node.node_wrapper()),
            );
            set_emit_flags(
                &*class_statement,
                get_emit_flags(&class_statement) | EmitFlags::HasEndOfDeclarationMarker,
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
        node: &Node, /*ClassDeclaration*/
        name: Option<impl Borrow<Node /*Identifier*/>>,
        facts: ClassFacts,
    ) -> io::Result<Gc<Node>> {
        let node_as_class_declaration = node.as_class_declaration();
        let modifiers = (!(facts.intersects(ClassFacts::UseImmediatelyInvokedFunctionExpression)))
            .then_and(|| {
                visit_nodes(
                    node.maybe_modifiers().as_deref(),
                    Some(|node: &Node| self.modifier_visitor(node)),
                    Some(is_modifier),
                    None,
                    None,
                )
            });

        let class_declaration = self
            .factory
            .create_class_declaration(
                Option::<Gc<NodeArray>>::None,
                modifiers,
                name.node_wrappered(),
                Option::<Gc<NodeArray>>::None,
                try_visit_nodes(
                    node_as_class_declaration
                        .maybe_heritage_clauses()
                        .as_deref(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_heritage_clause),
                    None,
                    None,
                )?,
                self.transform_class_members(node)?,
            )
            .wrap();

        let mut emit_flags = get_emit_flags(node);
        if facts.intersects(ClassFacts::HasStaticInitializedProperties) {
            emit_flags |= EmitFlags::NoTrailingSourceMap;
        }

        Ok(class_declaration
            .set_text_range(Some(node))
            .set_original_node(Some(node.node_wrapper()))
            .set_emit_flags(emit_flags))
    }

    pub(super) fn create_class_declaration_head_with_decorators(
        &self,
        node: &Node, /*ClassDeclaration*/
        name: Option<impl Borrow<Node /*Identifier*/>>,
    ) -> io::Result<Gc<Node>> {
        let node_as_class_declaration = node.as_class_declaration();
        let location = move_range_past_decorators(node);
        let class_alias = self.get_class_alias_if_needed(node);

        let decl_name = if self.language_version <= ScriptTarget::ES2015 {
            self.factory
                .get_internal_name(node, Some(false), Some(true))
        } else {
            self.factory.get_local_name(node, Some(false), Some(true))
        };

        let heritage_clauses = try_visit_nodes(
            node_as_class_declaration
                .maybe_heritage_clauses()
                .as_deref(),
            Some(|node: &Node| self.visitor(node)),
            Some(is_heritage_clause),
            None,
            None,
        )?;
        let members = self.transform_class_members(node)?;
        let class_expression = self
            .factory
            .create_class_expression(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                name.node_wrappered(),
                Option::<Gc<NodeArray>>::None,
                heritage_clauses,
                members,
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()))
            .set_text_range(Some(&location.to_readonly_text_range()));

        Ok(self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(decl_name),
                                None,
                                None,
                                Some(if let Some(class_alias) = class_alias {
                                    self.factory
                                        .create_assignment(class_alias, class_expression)
                                        .wrap()
                                } else {
                                    class_expression
                                }),
                            )
                            .wrap()],
                        Some(NodeFlags::Let),
                    )
                    .wrap(),
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()))
            .set_text_range(Some(&location.to_readonly_text_range()))
            .set_comment_range(node))
    }

    pub(super) fn visit_class_expression(
        &self,
        node: &Node, /*ClassExpression*/
    ) -> io::Result<Gc<Node /*<Expression>*/>> {
        let node_as_class_expression = node.as_class_expression();
        if !self.is_class_like_declaration_with_type_script_syntax(node) {
            return Ok(try_visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
            )?
            .unwrap());
        }

        Ok(self
            .factory
            .create_class_expression(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                node_as_class_expression.maybe_name(),
                Option::<Gc<NodeArray>>::None,
                try_visit_nodes(
                    node_as_class_expression.maybe_heritage_clauses().as_deref(),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_heritage_clause),
                    None,
                    None,
                )?,
                self.transform_class_members(node)?,
            )
            .wrap()
            .set_original_node(Some(node.node_wrapper()))
            .set_text_range(Some(node)))
    }

    pub(super) fn transform_class_members(
        &self,
        node: &Node, /*ClassDeclaration | ClassExpression*/
    ) -> io::Result<Gc<NodeArray>> {
        let mut members: Vec<Gc<Node /*ClassElement*/>> = Default::default();
        let constructor = get_first_constructor_with_body(node);
        let parameters_with_property_assignments = constructor.as_ref().map(|constructor| {
            constructor
                .as_constructor_declaration()
                .parameters()
                .owned_iter()
                .filter(|p| is_parameter_property_declaration(p, constructor))
        });
        if let Some(parameters_with_property_assignments) = parameters_with_property_assignments {
            for parameter in parameters_with_property_assignments {
                let parameter_as_parameter_declaration = parameter.as_parameter_declaration();
                if is_identifier(&parameter_as_parameter_declaration.name()) {
                    members.push(
                        self.factory
                            .create_property_declaration(
                                Option::<Gc<NodeArray>>::None,
                                Option::<Gc<NodeArray>>::None,
                                parameter_as_parameter_declaration.name(),
                                None,
                                None,
                                None,
                            )
                            .set_original_node(Some(parameter)),
                    );
                }
            }
        }

        let node_as_class_like_declaration = node.as_class_like_declaration();
        add_range(
            &mut members,
            try_visit_nodes(
                Some(&node_as_class_like_declaration.members()),
                Some(|node: &Node| self.class_element_visitor(node)),
                Some(is_class_element),
                None,
                None,
            )?
            .as_double_deref(),
            None,
            None,
        );
        Ok(self
            .factory
            .create_node_array(Some(members), None)
            .set_text_range(Some(&*node_as_class_like_declaration.members())))
    }

    pub(super) fn get_decorated_class_elements<'self_and_node>(
        &'self_and_node self,
        node: &'self_and_node Node, /*ClassExpression | ClassDeclaration*/
        is_static: bool,
    ) -> impl Iterator<Item = Gc<Node /*ClassElement*/>> + 'self_and_node {
        node.as_class_like_declaration()
            .members()
            .owned_iter()
            .filter(move |m| {
                if is_static {
                    self.is_static_decorated_class_element(m, node)
                } else {
                    self.is_instance_decorated_class_element(m, node)
                }
            })
    }

    pub(super) fn is_static_decorated_class_element(
        &self,
        member: &Node, /*ClassElement*/
        parent: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        self.is_decorated_class_element(member, true, parent)
    }

    pub(super) fn is_instance_decorated_class_element(
        &self,
        member: &Node, /*ClassElement*/
        parent: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        self.is_decorated_class_element(member, false, parent)
    }

    pub(super) fn is_decorated_class_element(
        &self,
        member: &Node, /*ClassElement*/
        is_static_element: bool,
        parent: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        node_or_child_is_decorated(member, Some(parent), Option::<&Node>::None)
            && is_static_element == is_static(member)
    }

    pub(super) fn get_decorators_of_parameters(
        &self,
        node: Option<impl Borrow<Node /*FunctionLikeDeclaration*/>>,
    ) -> Option<Vec<Option<NodeArrayOrVec>>> {
        let mut decorators: Option<Vec<Option<NodeArrayOrVec /*Decorator*/>>> = Default::default();
        if let Some(node) = node {
            let node: &Node = node.borrow();
            let node_as_function_like_declaration = node.as_function_like_declaration();
            let parameters = node_as_function_like_declaration.parameters();
            let first_parameter_is_this =
                !parameters.is_empty() && parameter_is_this_keyword(&parameters[0]);
            let first_parameter_offset = if first_parameter_is_this { 1 } else { 0 };
            let num_parameters = if first_parameter_is_this {
                parameters.len() - 1
            } else {
                parameters.len()
            };
            for i in 0..num_parameters {
                let parameter = &parameters[i + first_parameter_offset];
                if decorators.is_some() || parameter.maybe_decorators().is_some() {
                    decorators.get_or_insert_with(|| vec![None; num_parameters])[i] =
                        parameter.maybe_decorators().map(Into::into);
                }
            }
        }

        decorators
    }

    pub(super) fn get_all_decorators_of_constructor(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
    ) -> Option<AllDecorators> {
        let decorators = node.maybe_decorators();
        let parameters = self.get_decorators_of_parameters(get_first_constructor_with_body(node));
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
