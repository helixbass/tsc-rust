#[cfg(test)]
mod tests {
    use gc::Gc;
    use typescript_rust::{get_factory, Node, NodeArray, NodeInterface, SyntaxKind};

    fn assert_syntax_kind(node: &Node, expected: SyntaxKind) {
        assert_eq!(
            node.kind(),
            expected,
            "Actual: {:?} Expected: {:?}",
            node.kind(),
            expected,
        );
    }

    mod factory_create_export_assignment {
        use super::*;

        fn check_expression(expression: &Node /*Expression*/) {
            let node = get_factory().create_export_assignment(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                Some(false),
                expression.node_wrapper(),
            );
            assert_syntax_kind(&node.expression, SyntaxKind::ParenthesizedExpression);
        }

        #[test]
        fn test_parenthesizes_default_export_if_necessary() {
            let factory = get_factory();
            let clazz = factory
                .create_class_expression(
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    Some("C"),
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    vec![factory.create_property_declaration(
                        Option::<Gc<NodeArray>>::None,
                        Some(vec![factory.create_token(SyntaxKind::StaticKeyword).wrap()]),
                        "prop",
                        None,
                        None,
                        Some(
                            factory
                                .create_string_literal("1".to_owned(), None, None)
                                .wrap(),
                        ),
                    )],
                )
                .wrap();
            check_expression(&clazz);
            check_expression(
                &factory
                    .create_property_access_expression(clazz, "prop")
                    .wrap(),
            );

            let func = factory
                .create_function_expression(
                    Option::<Gc<NodeArray>>::None,
                    None,
                    Some("fn"),
                    Option::<Gc<NodeArray>>::None,
                    Option::<Gc<NodeArray>>::None,
                    None,
                    factory.create_block(vec![], None).wrap(),
                )
                .wrap();
            check_expression(&func);
            check_expression(
                &factory
                    .create_call_expression(
                        func.clone(),
                        Option::<Gc<NodeArray>>::None,
                        Option::<Gc<NodeArray>>::None,
                    )
                    .wrap(),
            );
            check_expression(
                &factory
                    .create_tagged_template_expression(
                        func,
                        Option::<Gc<NodeArray>>::None,
                        factory
                            .create_no_substitution_template_literal(
                                Some("".to_owned()),
                                None,
                                None,
                            )
                            .wrap(),
                    )
                    .wrap(),
            );

            check_expression(
                &factory
                    .create_binary_expression(
                        factory
                            .create_string_literal("a".to_owned(), None, None)
                            .wrap(),
                        SyntaxKind::CommaToken,
                        factory
                            .create_string_literal("b".to_owned(), None, None)
                            .wrap(),
                    )
                    .wrap(),
            );
            check_expression(
                &factory
                    .create_comma_list_expression(vec![
                        factory
                            .create_string_literal("a".to_owned(), None, None)
                            .wrap(),
                        factory
                            .create_string_literal("b".to_owned(), None, None)
                            .wrap(),
                    ])
                    .wrap(),
            );
        }
    }

    mod factory_create_arrow_function {
        use typescript_rust::FunctionLikeDeclarationInterface;

        use super::*;

        fn check_body(body: &Node /*ConciseBody*/) {
            let node = get_factory().create_arrow_function(
                Option::<Gc<NodeArray>>::None,
                Option::<Gc<NodeArray>>::None,
                vec![],
                None,
                None,
                body.node_wrapper(),
            );
            assert_syntax_kind(
                &node.maybe_body().unwrap(),
                SyntaxKind::ParenthesizedExpression,
            );
        }

        #[test]
        fn test_parenthesizes_concise_body_if_necessary() {
            let factory = get_factory();
            check_body(
                &factory
                    .create_object_literal_expression(Option::<Gc<NodeArray>>::None, None)
                    .wrap(),
            );
            check_body(
                &factory
                    .create_property_access_expression(
                        factory
                            .create_object_literal_expression(Option::<Gc<NodeArray>>::None, None)
                            .wrap(),
                        "prop",
                    )
                    .wrap(),
            );
            check_body(
                &factory
                    .create_as_expression(
                        factory
                            .create_property_access_expression(
                                factory
                                    .create_object_literal_expression(
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    )
                                    .wrap(),
                                "prop",
                            )
                            .wrap(),
                        factory
                            .create_type_reference_node("T", Option::<Gc<NodeArray>>::None)
                            .wrap(),
                    )
                    .wrap(),
            );
            check_body(
                &factory
                    .create_non_null_expression(
                        factory
                            .create_property_access_expression(
                                factory
                                    .create_object_literal_expression(
                                        Option::<Gc<NodeArray>>::None,
                                        None,
                                    )
                                    .wrap(),
                                "prop",
                            )
                            .wrap(),
                    )
                    .wrap(),
            );
            check_body(
                &factory
                    .create_comma_list_expression(vec![
                        factory
                            .create_string_literal("a".to_owned(), None, None)
                            .wrap(),
                        factory
                            .create_string_literal("b".to_owned(), None, None)
                            .wrap(),
                    ])
                    .wrap(),
            );
            check_body(
                &factory
                    .create_binary_expression(
                        factory
                            .create_string_literal("a".to_owned(), None, None)
                            .wrap(),
                        SyntaxKind::CommaToken,
                        factory
                            .create_string_literal("b".to_owned(), None, None)
                            .wrap(),
                    )
                    .wrap(),
            );
        }
    }
}
