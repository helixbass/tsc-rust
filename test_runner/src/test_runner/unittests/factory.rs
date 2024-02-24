#[cfg(test)]
mod tests {
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
        use typescript_rust::{id_arena::Id, AllArenas, HasArena, InArena};

        use super::*;

        fn check_expression(expression: Id<Node /*Expression*/>, arena: &impl HasArena) {
            let node = get_factory(arena).create_export_assignment(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                Some(false),
                expression,
            );
            assert_syntax_kind(
                &node
                    .ref_(arena)
                    .as_export_assignment()
                    .expression
                    .ref_(arena),
                SyntaxKind::ParenthesizedExpression,
            );
        }

        #[test]
        fn test_parenthesizes_default_export_if_necessary() {
            let ref arena = AllArenas::default();
            let factory = get_factory(arena);
            let clazz = factory.create_class_expression(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                Some("C"),
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                vec![factory.create_property_declaration(
                    Option::<Id<NodeArray>>::None,
                    Some(vec![factory.create_token(SyntaxKind::StaticKeyword)]),
                    "prop",
                    None,
                    None,
                    Some(factory.create_string_literal("1".to_owned(), None, None)),
                )],
            );
            check_expression(clazz, arena);
            check_expression(
                factory.create_property_access_expression(clazz, "prop"),
                arena,
            );

            let func = factory.create_function_expression(
                Option::<Id<NodeArray>>::None,
                None,
                Some("fn"),
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                None,
                factory.create_block(vec![], None),
            );
            check_expression(func, arena);
            check_expression(
                factory.create_call_expression(
                    func,
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                ),
                arena,
            );
            check_expression(
                factory.create_tagged_template_expression(
                    func,
                    Option::<Id<NodeArray>>::None,
                    factory.create_no_substitution_template_literal(
                        Some("".to_owned()),
                        None,
                        None,
                    ),
                ),
                arena,
            );

            check_expression(
                factory.create_binary_expression(
                    factory.create_string_literal("a".to_owned(), None, None),
                    SyntaxKind::CommaToken,
                    factory.create_string_literal("b".to_owned(), None, None),
                ),
                arena,
            );
            check_expression(
                factory.create_comma_list_expression(vec![
                    factory.create_string_literal("a".to_owned(), None, None),
                    factory.create_string_literal("b".to_owned(), None, None),
                ]),
                arena,
            );
        }
    }

    mod factory_create_arrow_function {
        use typescript_rust::{
            id_arena::Id, AllArenas, FunctionLikeDeclarationInterface, HasArena, InArena,
        };

        use super::*;

        fn check_body(body: Id<Node> /*ConciseBody*/, arena: &impl HasArena) {
            let node = get_factory(arena).create_arrow_function(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                vec![],
                None,
                None,
                body,
            );
            assert_syntax_kind(
                &node
                    .ref_(arena)
                    .as_arrow_function()
                    .maybe_body()
                    .unwrap()
                    .ref_(arena),
                SyntaxKind::ParenthesizedExpression,
            );
        }

        #[test]
        fn test_parenthesizes_concise_body_if_necessary() {
            let ref arena = AllArenas::default();
            let factory = get_factory(arena);
            check_body(
                factory.create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
                arena,
            );
            check_body(
                factory.create_property_access_expression(
                    factory.create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
                    "prop",
                ),
                arena,
            );
            check_body(
                factory.create_as_expression(
                    factory.create_property_access_expression(
                        factory
                            .create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
                        "prop",
                    ),
                    factory.create_type_reference_node("T", Option::<Id<NodeArray>>::None),
                ),
                arena,
            );
            check_body(
                factory.create_non_null_expression(factory.create_property_access_expression(
                    factory.create_object_literal_expression(Option::<Id<NodeArray>>::None, None),
                    "prop",
                )),
                arena,
            );
            check_body(
                factory.create_comma_list_expression(vec![
                    factory.create_string_literal("a".to_owned(), None, None),
                    factory.create_string_literal("b".to_owned(), None, None),
                ]),
                arena,
            );
            check_body(
                factory.create_binary_expression(
                    factory.create_string_literal("a".to_owned(), None, None),
                    SyntaxKind::CommaToken,
                    factory.create_string_literal("b".to_owned(), None, None),
                ),
                arena,
            );
        }
    }

    mod create_binary_expression {
        use typescript_rust::{id_arena::Id, AllArenas, InArena};

        use super::*;

        #[test]
        fn test_parenthesizes_arrow_function_in_rhs_if_necessary() {
            let ref arena = AllArenas::default();
            let factory = get_factory(arena);
            let lhs = factory.create_identifier("foo");
            let rhs = factory.create_arrow_function(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                vec![],
                None,
                None,
                factory.create_block(vec![], None),
            );
            let check_rhs = |operator: SyntaxKind /*BinaryOperator*/, expect_parens: bool| {
                let node = factory.create_binary_expression(lhs, operator, rhs);
                assert_syntax_kind(
                    &node.ref_(arena).as_binary_expression().right.ref_(arena),
                    if expect_parens {
                        SyntaxKind::ParenthesizedExpression
                    } else {
                        SyntaxKind::ArrowFunction
                    },
                );
            };

            check_rhs(SyntaxKind::CommaToken, false);
            check_rhs(SyntaxKind::EqualsToken, false);
            check_rhs(SyntaxKind::PlusEqualsToken, false);
            check_rhs(SyntaxKind::BarBarToken, true);
            check_rhs(SyntaxKind::AmpersandAmpersandToken, true);
            check_rhs(SyntaxKind::QuestionQuestionToken, true);
            check_rhs(SyntaxKind::EqualsEqualsToken, true);
            check_rhs(SyntaxKind::BarBarEqualsToken, false);
            check_rhs(SyntaxKind::AmpersandAmpersandEqualsToken, false);
            check_rhs(SyntaxKind::QuestionQuestionEqualsToken, false);
        }
    }
}
