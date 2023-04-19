use std::borrow::Borrow;

use gc::Gc;

use crate::{
    child_is_decorated, class_or_constructor_parameter_is_decorated, create_token_range,
    get_effective_base_type_node, get_emit_flags, get_properties, get_strict_option_value,
    has_syntactic_modifier, insert_statements_after_standard_prologue, is_external_module,
    is_json_source_file, move_range_past_decorators, set_emit_flags, skip_outer_expressions,
    skip_trivia, some, visit_each_child, visit_lexical_environment, AsDoubleDeref,
    ClassLikeDeclarationInterface, EmitFlags, HasStatementsInterface, Matches, ModifierFlags,
    ModuleKind, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeInterface,
    ReadonlyTextRange, ScriptTarget, SourceFileLike, SyntaxKind, TextRange, TransformFlags,
    VisitResult,
};

use super::{ClassFacts, TransformTypeScript};

impl TransformTypeScript {
    pub(super) fn visit_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        let always_strict = get_strict_option_value(&self.compiler_options, "alwaysStrict")
            && !(is_external_module(node) && self.module_kind >= ModuleKind::ES2015)
            && !is_json_source_file(node);

        self.factory.update_source_file(
            node,
            visit_lexical_environment(
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
                    ) -> Option<Gc<NodeArray>>,
                >::None,
            ),
            None,
            None,
            None,
            None,
            None,
        )
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
    ) -> VisitResult /*<Statement>*/ {
        let node_as_class_declaration = node.as_class_declaration();
        if !self.is_class_like_declaration_with_type_script_syntax(node)
            && !(self.maybe_current_namespace().is_some()
                && has_syntactic_modifier(node, ModifierFlags::Export))
        {
            return visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .map(Into::into);
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
            self.create_class_declaration_head_with_decorators(node, name.as_deref())
        } else {
            self.create_class_declaration_head_without_decorators(node, name.as_deref(), facts)
        };

        let mut statements: Vec<Gc<Node /*Stateent*/>> = vec![class_statement.clone()];

        self.add_class_element_decoration_statements(&mut statements, node, false);
        self.add_class_element_decoration_statements(&mut statements, node, true);
        self.add_constructor_decoration_statement(&mut statements, node);

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

        Some(if statements.len() == 1 {
            statements[0].clone().into()
        } else {
            statements.into()
        })
    }

    pub(super) fn create_class_declaration_head_without_decorators(
        &self,
        node: &Node, /*ClassExpression*/
        name: Option<impl Borrow<Node /*Identifier*/>>,
        facts: ClassFacts,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn create_class_declaration_head_with_decorators(
        &self,
        node: &Node, /*ClassExpression*/
        name: Option<impl Borrow<Node /*Identifier*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn visit_class_expression(
        &self,
        node: &Node, /*ClassExpression*/
    ) -> Gc<Node /*<Expression>*/> {
        unimplemented!()
    }
}
