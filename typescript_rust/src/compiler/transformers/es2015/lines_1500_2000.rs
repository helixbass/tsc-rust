use std::{borrow::Borrow, io};

use gc::Gc;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    create_expression_for_property_name, create_member_access_for_property_name,
    get_all_accessor_declarations, get_comment_range, get_source_map_range,
    get_use_define_for_class_fields, insert_statement_after_custom_prologue,
    is_computed_property_name, is_identifier, is_private_identifier, is_property_name,
    set_emit_flags, set_source_map_range, start_on_new_line, try_visit_node,
    unescape_leading_underscores, AllAccessorDeclarations, Debug_, EmitFlags,
    GeneratedIdentifierFlags, NamedDeclarationInterface, Node, NodeArray, NodeExt, NodeInterface,
    NodeWrappered, PropertyDescriptorAttributesBuilder, ReadonlyTextRange,
    ReadonlyTextRangeConcrete, SyntaxKind, VisitResult,
};

impl TransformES2015 {
    pub(super) fn insert_capture_this_for_node_if_needed(
        &self,
        statements: &mut Vec<Gc<Node>>, /*Statement*/
        node: &Node,
    ) -> bool {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::CapturedLexicalThis)
            && node.kind() != SyntaxKind::ArrowFunction
        {
            self.insert_capture_this_for_node(
                statements,
                node,
                Some(self.factory.create_this().wrap()),
            );
            return true;
        }
        false
    }

    pub(super) fn insert_capture_this_for_node(
        &self,
        statements: &mut Vec<Gc<Node>>, /*Statement*/
        node: &Node,
        initializer: Option<impl Borrow<Node /*Expression*/>>,
    ) {
        let initializer = initializer.node_wrappered();
        self.enable_substitutions_for_captured_this();
        let capture_this_statement = self
            .factory
            .create_variable_statement(
                Option::<Gc<NodeArray>>::None,
                self.factory
                    .create_variable_declaration_list(
                        vec![self
                            .factory
                            .create_variable_declaration(
                                Some(self.factory.create_unique_name(
                                    "this",
                                    Some(
                                        GeneratedIdentifierFlags::Optimistic
                                            | GeneratedIdentifierFlags::FileLevel,
                                    ),
                                )),
                                None,
                                None,
                                initializer,
                            )
                            .wrap()],
                        None,
                    )
                    .wrap(),
            )
            .wrap()
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::CustomPrologue)
            .set_source_map_range(Some(node.into()));
        insert_statement_after_custom_prologue(statements, Some(capture_this_statement));
    }

    pub(super) fn insert_capture_new_target_if_needed(
        &self,
        mut statements: Vec<Gc<Node>>, /*Statement*/
        node: &Node,                   /*FunctionLikeDeclaration*/
        copy_on_write: bool,
    ) -> Vec<Gc<Node>> /*Statement*/ {
        if self
            .maybe_hierarchy_facts()
            .unwrap_or_default()
            .intersects(HierarchyFacts::NewTarget)
        {
            let new_target: Gc<Node /*Expression*/>;
            match node.kind() {
                SyntaxKind::ArrowFunction => {
                    return statements;
                }
                SyntaxKind::MethodDeclaration
                | SyntaxKind::GetAccessor
                | SyntaxKind::SetAccessor => {
                    new_target = self.factory.create_void_zero();
                }
                SyntaxKind::Constructor => {
                    new_target = self
                        .factory
                        .create_property_access_expression(
                            self.factory
                                .create_this()
                                .wrap()
                                .set_emit_flags(EmitFlags::NoSubstitution),
                            "constructor",
                        )
                        .wrap();
                }
                SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionExpression => {
                    new_target = self
                        .factory
                        .create_conditional_expression(
                            self.factory
                                .create_logical_and(
                                    self.factory
                                        .create_this()
                                        .wrap()
                                        .set_emit_flags(EmitFlags::NoSubstitution),
                                    self.factory
                                        .create_binary_expression(
                                            self.factory
                                                .create_this()
                                                .wrap()
                                                .set_emit_flags(EmitFlags::NoSubstitution),
                                            SyntaxKind::InstanceOfKeyword,
                                            self.factory.get_local_name(node, None, None),
                                        )
                                        .wrap(),
                                )
                                .wrap(),
                            None,
                            self.factory
                                .create_property_access_expression(
                                    self.factory
                                        .create_this()
                                        .wrap()
                                        .set_emit_flags(EmitFlags::NoSubstitution),
                                    "constructor",
                                )
                                .wrap(),
                            None,
                            self.factory.create_void_zero(),
                        )
                        .wrap();
                }
                _ => Debug_.fail_bad_syntax_kind(node, None),
            }

            let capture_new_target_statement = self
                .factory
                .create_variable_statement(
                    Option::<Gc<NodeArray>>::None,
                    self.factory
                        .create_variable_declaration_list(
                            vec![self
                                .factory
                                .create_variable_declaration(
                                    Some(self.factory.create_unique_name(
                                        "_newTarget",
                                        Some(
                                            GeneratedIdentifierFlags::Optimistic
                                                | GeneratedIdentifierFlags::FileLevel,
                                        ),
                                    )),
                                    None,
                                    None,
                                    Some(new_target),
                                )
                                .wrap()],
                            None,
                        )
                        .wrap(),
                )
                .wrap()
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::CustomPrologue);

            if copy_on_write {
                statements = statements.clone();
            }

            insert_statement_after_custom_prologue(
                &mut statements,
                Some(capture_new_target_statement),
            );
        }

        statements
    }

    pub(super) fn add_class_members(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassExpression | ClassDeclaration*/
    ) -> io::Result<()> {
        let node_as_class_like_declaration = node.as_class_like_declaration();
        for member in &node_as_class_like_declaration.members() {
            match member.kind() {
                SyntaxKind::SemicolonClassElement => {
                    statements.push(self.transform_semicolon_class_element_to_statement(member));
                }
                SyntaxKind::MethodDeclaration => {
                    statements.push(self.transform_class_method_declaration_to_statement(
                        &self.get_class_member_prefix(node, member),
                        member,
                        node,
                    )?);
                }
                SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                    let accessors = get_all_accessor_declarations(
                        &node_as_class_like_declaration.members(),
                        member,
                    );
                    if Gc::ptr_eq(member, &accessors.first_accessor) {
                        statements.push(self.transform_accessors_to_statement(
                            &self.get_class_member_prefix(node, member),
                            &accessors,
                            node,
                        )?);
                    }
                }
                SyntaxKind::Constructor | SyntaxKind::ClassStaticBlockDeclaration => (),
                _ => Debug_.fail_bad_syntax_kind(
                    member,
                    self.maybe_current_source_file()
                        .as_ref()
                        .map(|current_source_file| {
                            current_source_file.as_source_file().file_name().clone()
                        })
                        .as_deref(),
                ),
            }
        }

        Ok(())
    }

    pub(super) fn transform_semicolon_class_element_to_statement(
        &self,
        member: &Node, /*SemicolonClassElement*/
    ) -> Gc<Node> {
        self.factory
            .create_empty_statement()
            .wrap()
            .set_text_range(Some(member))
    }

    pub(super) fn transform_class_method_declaration_to_statement(
        &self,
        receiver: &Node, /*LeftHandSideExpression*/
        member: &Node,   /*MethodDeclaration*/
        container: &Node,
    ) -> io::Result<Gc<Node>> {
        let member_as_method_declaration = member.as_method_declaration();
        let comment_range: ReadonlyTextRangeConcrete = get_comment_range(member).into();
        let source_map_range = get_source_map_range(member);
        let member_function = self.transform_function_like_to_expression(
            member,
            Some(member),
            Option::<&Node>::None,
            Some(container),
        );
        let ref property_name = try_visit_node(
            Some(member_as_method_declaration.name()),
            Some(|node: &Node| self.visitor(node)),
            Some(is_property_name),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?
        .unwrap();
        let e: Gc<Node /*Expression*/>;
        if !is_private_identifier(property_name)
            && get_use_define_for_class_fields(&self.context.get_compiler_options())
        {
            let name = if is_computed_property_name(property_name) {
                property_name.as_computed_property_name().expression.clone()
            } else if is_identifier(property_name) {
                self.factory
                    .create_string_literal(
                        unescape_leading_underscores(&property_name.as_identifier().escaped_text)
                            .to_owned(),
                        None,
                        None,
                    )
                    .wrap()
            } else {
                property_name.clone()
            };
            e = self.factory.create_object_define_property_call(
                receiver.node_wrapper(),
                name,
                self.factory.create_property_descriptor(
                    PropertyDescriptorAttributesBuilder::default()
                        .value(member_function.clone())
                        .enumerable(false)
                        .writable(true)
                        .configurable(true)
                        .build()
                        .unwrap(),
                    None,
                ),
            );
        } else {
            let member_name = create_member_access_for_property_name(
                &self.factory,
                receiver,
                property_name,
                member_as_method_declaration.maybe_name().as_deref(),
            );
            e = self
                .factory
                .create_assignment(member_name, member_function.clone())
                .wrap();
        }
        set_emit_flags(&*member_function, EmitFlags::NoComments);
        set_source_map_range(&*member_function, Some(source_map_range));
        Ok(self
            .factory
            .create_expression_statement(e)
            .wrap()
            .set_text_range(Some(member))
            .set_original_node(Some(member.node_wrapper()))
            .set_comment_range(&comment_range)
            .set_emit_flags(EmitFlags::NoSourceMap))
    }

    pub(super) fn transform_accessors_to_statement(
        &self,
        receiver: &Node, /*LeftHandSideExpression*/
        accessors: &AllAccessorDeclarations,
        container: &Node,
    ) -> io::Result<Gc<Node /*Statement*/>> {
        Ok(self
            .factory
            .create_expression_statement(
                self.transform_accessors_to_expression(receiver, accessors, container, false)?,
            )
            .wrap()
            .set_emit_flags(EmitFlags::NoComments)
            .set_source_map_range(Some(get_source_map_range(&accessors.first_accessor))))
    }

    pub(super) fn transform_accessors_to_expression(
        &self,
        receiver: &Node, /*LeftHandSideExpression*/
        accessors: &AllAccessorDeclarations,
        container: &Node,
        starts_on_new_line: bool,
    ) -> io::Result<Gc<Node /*Expression*/>> {
        let first_accessor = &accessors.first_accessor;
        let get_accessor = accessors.get_accessor.as_ref();
        let set_accessor = accessors.set_accessor.as_ref();
        let target = self
            .factory
            .clone_node(receiver)
            .set_text_range(Some(receiver))
            .and_set_parent(receiver.maybe_parent())
            .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoTrailingSourceMap)
            .set_source_map_range(
                first_accessor
                    .as_named_declaration()
                    .maybe_name()
                    .as_deref()
                    .map(Into::into),
            );

        let ref visited_accessor_name = try_visit_node(
            first_accessor.as_named_declaration().maybe_name(),
            Some(|node: &Node| self.visitor(node)),
            Some(is_property_name),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?
        .unwrap();
        if is_private_identifier(visited_accessor_name) {
            Debug_.fail_bad_syntax_kind(
                visited_accessor_name,
                Some("Encountered unhandled private identifier while transforming ES2015."),
            );
        }
        let property_name =
            create_expression_for_property_name(&self.factory, visited_accessor_name)
                .set_emit_flags(EmitFlags::NoComments | EmitFlags::NoLeadingSourceMap)
                .set_source_map_range(
                    first_accessor
                        .as_named_declaration()
                        .maybe_name()
                        .as_deref()
                        .map(Into::into),
                );

        let mut properties: Vec<Gc<Node /*ObjectLiteralElementLike*/>> = Default::default();
        if let Some(get_accessor) = get_accessor {
            let getter_function = self
                .transform_function_like_to_expression(
                    get_accessor,
                    Option::<&Node>::None,
                    Option::<&Node>::None,
                    Some(container),
                )
                .set_source_map_range(Some(get_source_map_range(get_accessor)))
                .set_emit_flags(EmitFlags::NoLeadingComments);
            let getter = self
                .factory
                .create_property_assignment("get", getter_function)
                .wrap()
                .set_comment_range(&ReadonlyTextRangeConcrete::from(get_comment_range(
                    get_accessor,
                )));
            properties.push(getter);
        }

        if let Some(set_accessor) = set_accessor {
            let setter_function = self
                .transform_function_like_to_expression(
                    set_accessor,
                    Option::<&Node>::None,
                    Option::<&Node>::None,
                    Some(container),
                )
                .set_source_map_range(Some(get_source_map_range(set_accessor)))
                .set_emit_flags(EmitFlags::NoLeadingComments);
            let setter = self
                .factory
                .create_property_assignment("set", setter_function)
                .wrap()
                .set_comment_range(&ReadonlyTextRangeConcrete::from(get_comment_range(
                    set_accessor,
                )));
            properties.push(setter);
        }

        properties.push(
            self.factory
                .create_property_assignment(
                    "enumerable",
                    if get_accessor.is_some() || set_accessor.is_some() {
                        self.factory.create_false().wrap()
                    } else {
                        self.factory.create_true().wrap()
                    },
                )
                .wrap(),
        );
        properties.push(
            self.factory
                .create_property_assignment("configurable", self.factory.create_true().wrap())
                .wrap(),
        );

        let call = self
            .factory
            .create_call_expression(
                self.factory
                    .create_property_access_expression(
                        self.factory
                            .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                            .wrap(),
                        "defineProperty",
                    )
                    .wrap(),
                Option::<Gc<NodeArray>>::None,
                Some(vec![
                    target,
                    property_name,
                    self.factory
                        .create_object_literal_expression(Some(properties), Some(true))
                        .wrap(),
                ]),
            )
            .wrap();
        if starts_on_new_line {
            start_on_new_line(&*call);
        }

        Ok(call)
    }

    pub(super) fn visit_arrow_function(&self, _node: &Node /*ArrowFunction*/) -> VisitResult {
        unimplemented!()
    }

    pub(super) fn visit_function_expression(
        &self,
        _node: &Node, /*FunctionExpression*/
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_function_declaration(
        &self,
        _node: &Node, /*FunctionDeclaration*/
    ) -> Gc<Node /*FunctionDeclaration*/> {
        unimplemented!()
    }

    pub(super) fn transform_function_like_to_expression(
        &self,
        _node: &Node, /*FunctionLikeDeclaration*/
        _location: Option<&impl ReadonlyTextRange>,
        _name: Option<impl Borrow<Node /*Identifier*/>>,
        _container: Option<impl Borrow<Node>>,
    ) -> Gc<Node /*FunctionExpression*/> {
        unimplemented!()
    }
}
