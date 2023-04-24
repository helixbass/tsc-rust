use std::ptr;

use gc::Gc;

use crate::{
    get_all_accessor_declarations, AllAccessorDeclarations, Matches, Node, NodeInterface,
    SyntaxKind,
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
        _method: &Node, /*MethodDeclaration*/
    ) -> Option<AllDecorators> {
        unimplemented!()
    }

    pub(super) fn get_all_decorators_of_property(
        &self,
        _property: &Node, /*PropertyDeclaration*/
    ) -> Option<AllDecorators> {
        unimplemented!()
    }

    pub(super) fn add_class_element_decoration_statements(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _node: &Node, /*ClassDeclaration*/
        _is_static: bool,
    ) {
        unimplemented!()
    }

    pub(super) fn add_constructor_decoration_statement(
        &self,
        _statements: &mut Vec<Gc<Node /*Statement*/>>,
        _node: &Node, /*ClassDeclaration*/
    ) {
        unimplemented!()
    }
}
