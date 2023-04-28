use std::ptr;

use gc::Gc;

use crate::{
    find_ancestor, is_conditional_type_node, is_identifier, Matches, Node, NodeArray,
    NodeInterface, ScriptTarget, SyntaxKind, TypeReferenceSerializationKind, VisitResult,
};

use super::TransformTypeScript;

impl TransformTypeScript {
    pub(super) fn serialize_type_list(
        &self,
        types: &[Gc<Node /*TypeNode*/>],
    ) -> Gc<Node /*SerializedTypeNode*/> {
        let mut serialized_union: Option<Gc<Node /*SerializedTypeNode*/>> = Default::default();
        for type_node in types {
            let mut type_node = type_node.clone();
            while type_node.kind() == SyntaxKind::ParenthesizedType {
                type_node = type_node.as_parenthesized_type_node().type_.clone();
            }
            if type_node.kind() == SyntaxKind::NeverKeyword {
                continue;
            }
            if !self.strict_null_checks
                && (type_node.kind() == SyntaxKind::LiteralType
                    && type_node.as_literal_type_node().literal.kind() == SyntaxKind::NullKeyword
                    || type_node.kind() == SyntaxKind::UndefinedKeyword)
            {
                continue;
            }
            let serialized_individual = self.serialize_type_node(Some(&*type_node));

            if is_identifier(&serialized_individual)
                && serialized_individual.as_identifier().escaped_text == "Object"
            {
                return serialized_individual;
            } else if let Some(serialized_union) = serialized_union.as_ref() {
                if !is_identifier(serialized_union)
                    || !is_identifier(&serialized_individual)
                    || serialized_union.as_identifier().escaped_text
                        != serialized_individual.as_identifier().escaped_text
                {
                    return self
                        .factory
                        .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                        .wrap();
                }
            } else {
                serialized_union = Some(serialized_individual);
            }
        }

        serialized_union.unwrap_or_else(|| self.factory.create_void_zero())
    }

    pub(super) fn serialize_type_reference_node(
        &self,
        node: &Node, /*TypeReferenceNode*/
    ) -> Gc<Node /*SerializedTypeNode*/> {
        let node_as_type_reference_node = node.as_type_reference_node();
        let kind = self.resolver.get_type_reference_serialization_kind(
            &node_as_type_reference_node.type_name,
            self.maybe_current_name_scope()
                .or_else(|| self.maybe_current_lexical_scope())
                .as_deref(),
        );
        match kind {
            TypeReferenceSerializationKind::Unknown => {
                if find_ancestor(Some(node), |n: &Node| {
                    n.maybe_parent().matches(|ref n_parent| {
                        is_conditional_type_node(n_parent) && {
                            let n_parent_as_conditional_type_node =
                                n_parent.as_conditional_type_node();
                            ptr::eq(&*n_parent_as_conditional_type_node.true_type, n)
                                || ptr::eq(&*n_parent_as_conditional_type_node.false_type, n)
                        }
                    })
                })
                .is_some()
                {
                    return self
                        .factory
                        .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                        .wrap();
                }

                let serialized = self.serialize_entity_name_as_expression_fallback(
                    &node_as_type_reference_node.type_name,
                );
                let temp = self.factory.create_temp_variable(
                    Some(|node: &Node| self.context.hoist_variable_declaration(node)),
                    None,
                );
                self.factory
                    .create_conditional_expression(
                        self.factory.create_type_check(
                            self.factory
                                .create_assignment(temp.clone(), serialized)
                                .wrap(),
                            "function",
                        ),
                        None,
                        temp,
                        None,
                        self.factory
                            .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                            .wrap(),
                    )
                    .wrap()
            }
            TypeReferenceSerializationKind::TypeWithConstructSignatureAndValue => {
                self.serialize_entity_name_as_expression(&node_as_type_reference_node.type_name)
            }
            TypeReferenceSerializationKind::VoidNullableOrNeverType => {
                self.factory.create_void_zero()
            }
            TypeReferenceSerializationKind::BigIntLikeType => {
                self.get_global_big_int_name_with_fallback()
            }
            TypeReferenceSerializationKind::BooleanType => self
                .factory
                .create_identifier("Boolean", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::NumberLikeType => self
                .factory
                .create_identifier("Number", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::StringLikeType => self
                .factory
                .create_identifier("String", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::ArrayLikeType => self
                .factory
                .create_identifier("Array", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::ESSymbolType => {
                if self.language_version < ScriptTarget::ES2015 {
                    self.get_global_symbol_name_with_fallback()
                } else {
                    self.factory
                        .create_identifier("Symbol", Option::<Gc<NodeArray>>::None, None)
                        .wrap()
                }
            }
            TypeReferenceSerializationKind::TypeWithCallSignature => self
                .factory
                .create_identifier("Function", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::Promise => self
                .factory
                .create_identifier("Promise", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            TypeReferenceSerializationKind::ObjectType => self
                .factory
                .create_identifier("Object", Option::<Gc<NodeArray>>::None, None)
                .wrap(),
            // default:
            //     return Debug.assertNever(kind);
        }
    }

    pub(super) fn create_checked_value(
        &self,
        left: Gc<Node /*Expression*/>,
        right: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        self.factory
            .create_logical_and(
                self.factory
                    .create_strict_inequality(
                        self.factory.create_type_of_expression(left).wrap(),
                        self.factory
                            .create_string_literal("undefined".to_owned(), None, None)
                            .wrap(),
                    )
                    .wrap(),
                right,
            )
            .wrap()
    }

    pub(super) fn serialize_entity_name_as_expression_fallback(
        &self,
        node: &Node, /*EntityName*/
    ) -> Gc<Node /*BinaryExpression*/> {
        unimplemented!()
    }

    pub(super) fn serialize_entity_name_as_expression(
        &self,
        node: &Node, /*EntityName*/
    ) -> Gc<Node /*SerializedEntityNameAsExpression*/> {
        unimplemented!()
    }

    pub(super) fn get_global_symbol_name_with_fallback(
        &self,
    ) -> Gc<Node /*ConditionalExpression*/> {
        unimplemented!()
    }

    pub(super) fn get_global_big_int_name_with_fallback(&self) -> Gc<Node /*SerializedTypeNode*/> {
        unimplemented!()
    }

    pub(super) fn get_expression_for_property_name(
        &self,
        _member: &Node, /*ClassElement | EnumMember*/
        _generate_name_for_computed_property_name: bool,
    ) -> Gc<Node /*Expression*/> {
        unimplemented!()
    }

    pub(super) fn visit_heritage_clause(
        &self,
        _node: &Node, /*HeritageClause*/
    ) -> Option<Gc<Node /*HeritageClause*/>> {
        unimplemented!()
    }

    pub(super) fn visit_expression_with_type_arguments(
        &self,
        _node: &Node, /*ExpressionWithTypeArguments*/
    ) -> Gc<Node /*ExpressionWithTypeArguments*/> {
        unimplemented!()
    }

    pub(super) fn visit_property_declaration(
        &self,
        _node: &Node, /*PropertyDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn visit_constructor(
        &self,
        _node: &Node, /*ConstructorDeclaration*/
    ) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }
}
