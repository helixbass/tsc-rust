#[cfg(test)]
mod tests {
    use typescript_rust::{Node, NodeInterface, SyntaxKind};

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
        use gc::Gc;
        use typescript_rust::{get_factory, NodeArray};

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
}
