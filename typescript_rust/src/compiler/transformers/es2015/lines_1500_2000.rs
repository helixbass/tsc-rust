use std::borrow::Borrow;

use gc::Gc;

use super::{HierarchyFacts, TransformES2015};
use crate::{
    get_all_accessor_declarations, insert_statement_after_custom_prologue, AllAccessorDeclarations,
    Debug_, EmitFlags, GeneratedIdentifierFlags, Node, NodeArray, NodeExt, NodeInterface,
    NodeWrappered, SyntaxKind, VisitResult,
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
    ) {
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
                    ));
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
                        ));
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
        _receiver: &Node, /*LeftHandSideExpression*/
        _member: &Node,   /*MethodDeclaration*/
        _container: &Node,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn transform_accessors_to_statement(
        &self,
        _receiver: &Node, /*LeftHandSideExpression*/
        _accessor: &AllAccessorDeclarations,
        _container: &Node,
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
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
}
