use std::ptr;

use gc::Gc;
use itertools::Itertools;

use crate::{
    add_range, get_all_accessor_declarations, maybe_map, move_range_past_decorators,
    AllAccessorDeclarations, EmitFlags, FunctionLikeDeclarationInterface, Matches, Node, NodeExt,
    NodeInterface, ScriptTarget, SyntaxKind, UnwrapOrEmpty,
};

use super::{AllDecorators, TransformTypeScript};

impl TransformTypeScript {
    pub(super) fn get_all_decorators_of_class_element(
        &self,
        node: &Node,   /*ClassExpression | ClassDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> Option<AllDecorators> {
        match member.kind() {
            SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
                self.get_all_decorators_of_accessors(node, member)
            }
            SyntaxKind::MethodDeclaration => self.get_all_decorators_of_method(member),
            SyntaxKind::PropertyDeclaration => self.get_all_decorators_of_property(member),
            _ => None,
        }
    }

    pub(super) fn get_all_decorators_of_accessors(
        &self,
        node: &Node,     /*ClassExpression | ClassDeclaration*/
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<AllDecorators> {
        if accessor
            .as_function_like_declaration()
            .maybe_body()
            .is_none()
        {
            return None;
        }

        let AllAccessorDeclarations {
            first_accessor,
            second_accessor,
            set_accessor,
            ..
        } = get_all_accessor_declarations(&node.as_class_like_declaration().members(), accessor);
        let first_accessor_with_decorators = if first_accessor.maybe_decorators().is_some() {
            Some(first_accessor.clone())
        } else if second_accessor
            .as_ref()
            .matches(|second_accessor| second_accessor.maybe_decorators().is_some())
        {
            second_accessor.clone()
        } else {
            None
        };
        let first_accessor_with_decorators = first_accessor_with_decorators?;
        if !ptr::eq(accessor, &*first_accessor_with_decorators) {
            return None;
        }

        let decorators = first_accessor_with_decorators.maybe_decorators();
        let parameters = self.get_decorators_of_parameters(set_accessor.as_deref());
        if decorators.is_none() && parameters.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters,
        })
    }

    pub(super) fn get_all_decorators_of_method(
        &self,
        method: &Node, /*MethodDeclaration*/
    ) -> Option<AllDecorators> {
        let method_as_method_declaration = method.as_method_declaration();
        if method_as_method_declaration.maybe_body().is_none() {
            return None;
        }

        let decorators = method.maybe_decorators();
        let parameters = self.get_decorators_of_parameters(Some(method));
        if decorators.is_none() && parameters.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters,
        })
    }

    pub(super) fn get_all_decorators_of_property(
        &self,
        property: &Node, /*PropertyDeclaration*/
    ) -> Option<AllDecorators> {
        let decorators = property.maybe_decorators();
        if decorators.is_none() {
            return None;
        }

        Some(AllDecorators {
            decorators: decorators.map(Into::into),
            parameters: None,
        })
    }

    pub(super) fn transform_all_decorators_of_declaration(
        &self,
        node: &Node,      /*Declaration*/
        container: &Node, /*ClassLikeDeclaration*/
        all_decorators: Option<&AllDecorators>,
    ) -> Option<Vec<Gc<Node>>> {
        let all_decorators = all_decorators?;

        let mut decorator_expressions: Vec<Gc<Node /*Expression*/>> = Default::default();
        add_range(
            &mut decorator_expressions,
            maybe_map(
                all_decorators.decorators.as_deref(),
                |decorator: &Gc<Node>, _| self.transform_decorator(decorator),
            )
            .as_deref(),
            None,
            None,
        );
        add_range(
            &mut decorator_expressions,
            all_decorators
                .parameters
                .as_ref()
                .map(|all_decorators_parameters| {
                    all_decorators_parameters
                        .iter()
                        .enumerate()
                        .flat_map(|(index, parameter)| {
                            self.transform_decorators_of_parameter(parameter.as_deref(), index)
                                .map(IntoIterator::into_iter)
                                .unwrap_or_empty()
                        })
                        .collect_vec()
                })
                .as_deref(),
            None,
            None,
        );
        self.add_type_metadata(node, container, &mut decorator_expressions);
        Some(decorator_expressions)
    }

    pub(super) fn add_class_element_decoration_statements(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration*/
        is_static: bool,
    ) {
        add_range(
            statements,
            maybe_map(
                self.generate_class_element_decoration_expressions(node, is_static),
                |expression: Gc<Node>, _| self.expression_to_statement(expression),
            )
            .as_deref(),
            None,
            None,
        );
    }

    pub(super) fn generate_class_element_decoration_expressions(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
        is_static: bool,
    ) -> Option<Vec<Gc<Node>>> {
        let members = self.get_decorated_class_elements(node, is_static);
        let mut expressions: Option<Vec<Gc<Node>>> = Default::default();
        for ref member in members {
            let expression = self.generate_class_element_decoration_expression(node, member);
            if let Some(expression) = expression {
                expressions
                    .get_or_insert_with(|| Default::default())
                    .push(expression);
            }
        }
        expressions
    }

    pub(super) fn generate_class_element_decoration_expression(
        &self,
        node: &Node,   /*ClassExpression | ClassDeclaration*/
        member: &Node, /*ClassElement*/
    ) -> Option<Gc<Node>> {
        let all_decorators = self.get_all_decorators_of_class_element(node, member);
        let decorator_expressions =
            self.transform_all_decorators_of_declaration(member, node, all_decorators.as_ref())?;

        let prefix = self.get_class_member_prefix(node, member);
        let member_name = self.get_expression_for_property_name(member, true);
        let descriptor = if self.language_version > ScriptTarget::ES3 {
            if member.kind() == SyntaxKind::PropertyDeclaration {
                Some(self.factory.create_void_zero())
            } else {
                Some(self.factory.create_null().wrap())
            }
        } else {
            None
        };

        Some(
            self.emit_helpers()
                .create_decorate_helper(
                    &decorator_expressions,
                    &prefix,
                    Some(&*member_name),
                    descriptor,
                )
                .set_text_range(Some(
                    &move_range_past_decorators(member).into_readonly_text_range(),
                ))
                .set_emit_flags(EmitFlags::NoComments),
        )
    }

    pub(super) fn add_constructor_decoration_statement(
        &self,
        statements: &mut Vec<Gc<Node /*Statement*/>>,
        node: &Node, /*ClassDeclaration*/
    ) {
        let expression = self.generate_constructor_decoration_expression(node);
        if let Some(expression) = expression {
            statements.push(
                self.factory
                    .create_expression_statement(expression)
                    .wrap()
                    .set_original_node(Some(node.node_wrapper())),
            );
        }
    }

    pub(super) fn generate_constructor_decoration_expression(
        &self,
        node: &Node, /*ClassExpression | ClassDeclaration*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn transform_decorator(&self, decorator: &Node /*Decorator*/) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn transform_decorators_of_parameter(
        &self,
        decorators: Option<&[Gc<Node /*Decorator*/>]>,
        parameter_offset: usize,
    ) -> Option<Vec<Gc<Node>>> {
        unimplemented!()
    }

    pub(super) fn add_type_metadata(
        &self,
        node: &Node,      /*Declaration*/
        container: &Node, /*ClassLikeDeclaration*/
        decorator_expressions: &mut Vec<Gc<Node /*Expression*/>>,
    ) {
        unimplemented!()
    }
}
