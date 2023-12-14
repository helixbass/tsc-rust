use gc::Gc;
use id_arena::Id;

use super::{
    try_visit_node_returns, try_visit_nodes_returns, visit_node_returns, visit_nodes_returns,
};
use crate::{
    for_each, maybe_for_each, try_for_each, try_maybe_for_each, ClassLikeDeclarationInterface,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasQuestionTokenInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InterfaceOrClassLikeDeclarationInterface, JSDocTagInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, OptionTry,
    SignatureDeclarationInterface, StringOrNodeArray, SyntaxKind,
};

pub fn for_each_child_returns<TReturn>(
    node: Id<Node>,
    mut cb_node: impl FnMut(Id<Node>) -> Option<TReturn>,
    mut cb_nodes: Option<impl FnMut(&NodeArray) -> Option<TReturn>>,
) -> Option<TReturn> {
    if
    /* !node ||*/
    node.kind() <= SyntaxKind::LastToken {
        return None;
    }
    match node {
        Node::QualifiedName(node) => visit_node_returns(&mut cb_node, Some(&*node.left))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.right))),
        Node::TypeParameterDeclaration(node) => visit_node_returns(&mut cb_node, Some(node.name()))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.constraint.clone()).or_else(|| {
                    visit_node_returns(&mut cb_node, node.default.clone())
                        .or_else(|| visit_node_returns(&mut cb_node, node.expression.clone()))
                })
            }),
        Node::ShorthandPropertyAssignment(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                            visit_node_returns(&mut cb_node, node.question_token.clone()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.exclamation_token.clone())
                                        .or_else(|| {
                                            visit_node_returns(
                                                &mut cb_node,
                                                node.equals_token.clone(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_node_returns(
                                                        &mut cb_node,
                                                        node.object_assignment_initializer.clone(),
                                                    )
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::SpreadAssignment(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::ParameterDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                            visit_node_returns(&mut cb_node, node.question_token.clone()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_type()).or_else(
                                        || {
                                            visit_node_returns(
                                                &mut cb_node,
                                                node.maybe_initializer(),
                                            )
                                        },
                                    )
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::PropertyDeclaration(node) => {
            visit_nodes_returns(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_deref(),
            )
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())
                    .or_else(|| {
                        visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                            visit_node_returns(&mut cb_node, node.question_token.clone()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.exclamation_token.clone())
                                        .or_else(|| {
                                            visit_node_returns(&mut cb_node, node.maybe_type())
                                                .or_else(|| {
                                                    visit_node_returns(
                                                        &mut cb_node,
                                                        node.maybe_initializer(),
                                                    )
                                                })
                                        })
                                },
                            )
                        })
                    })
            })
        }
        Node::PropertySignature(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_node_returns(&mut cb_node, node.question_token.clone()).or_else(
                            || {
                                visit_node_returns(&mut cb_node, node.maybe_type()).or_else(|| {
                                    visit_node_returns(&mut cb_node, node.maybe_initializer())
                                })
                            },
                        )
                    })
                },
            )
        }),
        Node::PropertyAssignment(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_node_returns(&mut cb_node, node.question_token.clone())
                            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.initializer)))
                    })
                },
            )
        }),
        Node::VariableDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_node_returns(&mut cb_node, node.exclamation_token.clone()).or_else(
                            || {
                                visit_node_returns(&mut cb_node, node.maybe_type()).or_else(|| {
                                    visit_node_returns(&mut cb_node, node.maybe_initializer())
                                })
                            },
                        )
                    })
                },
            )
        }),
        Node::BindingElement(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.property_name.clone()).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                                visit_node_returns(&mut cb_node, node.maybe_initializer())
                            })
                        })
                    })
                },
            )
        }),
        Node::FunctionTypeNode(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::ConstructorTypeNode(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::CallSignatureDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::ConstructSignatureDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::IndexSignatureDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::MethodDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::MethodSignature(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                Some(&node.parameters()),
                            )
                            .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                        })
                    })
                },
            )
        }),
        Node::ConstructorDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::GetAccessorDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::SetAccessorDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::FunctionExpression(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::FunctionDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::ArrowFunction(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_asterisk_token()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                            visit_node_returns(&mut cb_node, node.maybe_question_token()).or_else(
                                || {
                                    visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())
                                        .or_else(|| {
                                            visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )
                                                    .or_else(|| {
                                                        visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )
                                                        .or_else(|| {
                                                            visit_node_returns(
                                                                &mut cb_node,
                                                                Some(
                                                                    &*node
                                                                        .equals_greater_than_token,
                                                                ),
                                                            )
                                                            .or_else(|| {
                                                                visit_node_returns(
                                                                    &mut cb_node,
                                                                    node.maybe_body(),
                                                                )
                                                            })
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        }),
        Node::ClassStaticBlockDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.body)))
        }),
        Node::TypeReferenceNode(node) => {
            visit_node_returns(&mut cb_node, Some(node.type_name.clone())).or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
            })
        }
        Node::TypePredicateNode(node) => {
            visit_node_returns(&mut cb_node, node.asserts_modifier.clone()).or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.parameter_name))
                    .or_else(|| visit_node_returns(&mut cb_node, node.type_.clone()))
            })
        }
        Node::TypeQueryNode(node) => visit_node_returns(&mut cb_node, Some(&*node.expr_name)),
        Node::TypeLiteralNode(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))
        }
        Node::ArrayTypeNode(node) => {
            visit_node_returns(&mut cb_node, Some(node.element_type.clone()))
        }
        Node::TupleTypeNode(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::UnionTypeNode(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))
        }
        Node::IntersectionTypeNode(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))
        }
        Node::ConditionalTypeNode(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.check_type)).or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.extends_type)).or_else(|| {
                    visit_node_returns(&mut cb_node, Some(&*node.true_type))
                        .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.false_type)))
                })
            })
        }
        Node::InferTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.type_parameter)),
        Node::ImportTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.argument))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.qualifier.clone()).or_else(|| {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_deref(),
                    )
                })
            }),
        Node::ParenthesizedTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.type_)),
        Node::TypeOperatorNode(node) => visit_node_returns(&mut cb_node, Some(&*node.type_)),
        Node::IndexedAccessTypeNode(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.object_type))
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.index_type)))
        }
        Node::MappedTypeNode(node) => visit_node_returns(&mut cb_node, node.readonly_token.clone())
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.type_parameter)).or_else(|| {
                    visit_node_returns(&mut cb_node, node.name_type.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.question_token.clone()).or_else(
                            || {
                                visit_node_returns(&mut cb_node, node.type_.clone()).or_else(|| {
                                    visit_nodes_returns(
                                        &mut cb_node,
                                        cb_nodes.as_mut(),
                                        node.members.as_deref(),
                                    )
                                })
                            },
                        )
                    })
                })
            }),
        Node::LiteralTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.literal)),
        Node::NamedTupleMember(node) => {
            visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone()).or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.name)).or_else(|| {
                    visit_node_returns(&mut cb_node, node.question_token.clone())
                        .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.type_)))
                })
            })
        }
        Node::ObjectBindingPattern(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ArrayBindingPattern(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ArrayLiteralExpression(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ObjectLiteralExpression(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.properties))
        }
        Node::PropertyAccessExpression(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression)).or_else(|| {
                visit_node_returns(&mut cb_node, node.question_dot_token.clone())
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.name)))
            })
        }
        Node::ElementAccessExpression(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression)).or_else(|| {
                visit_node_returns(&mut cb_node, node.question_dot_token.clone())
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.argument_expression)))
            })
        }
        Node::CallExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.question_dot_token.clone()).or_else(|| {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_deref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.arguments))
                    })
                })
            }),
        Node::NewExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
                .or_else(|| {
                    visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.arguments.as_deref())
                })
            }),
        Node::TaggedTemplateExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.tag))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.question_dot_token.clone()).or_else(|| {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_deref(),
                    )
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.template)))
                })
            }),
        Node::TypeAssertion(node) => visit_node_returns(&mut cb_node, Some(&*node.type_))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.expression))),
        Node::ParenthesizedExpression(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression))
        }
        Node::DeleteExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::TypeOfExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::VoidExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::PrefixUnaryExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.operand)),
        Node::YieldExpression(node) => {
            visit_node_returns(&mut cb_node, node.asterisk_token.clone())
                .or_else(|| visit_node_returns(&mut cb_node, node.expression.clone()))
        }
        Node::AwaitExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::PostfixUnaryExpression(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.operand))
        }
        Node::BinaryExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.left))
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.operator_token))
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.right)))
            }),
        Node::AsExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.type_))),
        Node::NonNullExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::MetaProperty(node) => visit_node_returns(&mut cb_node, Some(&*node.name)),
        Node::ConditionalExpression(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.condition)).or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.question_token)).or_else(|| {
                    visit_node_returns(&mut cb_node, Some(&*node.when_true)).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(&*node.colon_token))
                            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.when_false)))
                    })
                })
            })
        }
        Node::SpreadElement(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::Block(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
        }
        Node::ModuleBlock(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
        }
        Node::SourceFile(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements()))
                .or_else(|| visit_node_returns(&mut cb_node, Some(node.end_of_file_token())))
        }
        Node::VariableStatement(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())
                .or_else(|| visit_node_returns(&mut cb_node, Some(node.declaration_list.clone())))
        }),
        Node::VariableDeclarationList(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.declarations))
        }
        Node::ExpressionStatement(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression))
        }
        Node::IfStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.then_statement))
                    .or_else(|| visit_node_returns(&mut cb_node, node.else_statement.clone()))
            }),
        Node::DoStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.statement))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.expression))),
        Node::WhileStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.statement))),
        Node::ForStatement(node) => visit_node_returns(&mut cb_node, node.initializer.clone())
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.condition.clone()).or_else(|| {
                    visit_node_returns(&mut cb_node, node.incrementor.clone())
                        .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.statement)))
                })
            }),
        Node::ForInStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.initializer))
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.expression))
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.statement)))
            }),
        Node::ForOfStatement(node) => visit_node_returns(&mut cb_node, node.await_modifier.clone())
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.initializer)).or_else(|| {
                    visit_node_returns(&mut cb_node, Some(&*node.expression))
                        .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.statement)))
                })
            }),
        Node::BreakStatement(node) => visit_node_returns(&mut cb_node, node.label.clone()),
        Node::ContinueStatement(node) => visit_node_returns(&mut cb_node, node.label.clone()),
        Node::ReturnStatement(node) => visit_node_returns(&mut cb_node, node.expression.clone()),
        Node::WithStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.statement))),
        Node::SwitchStatement(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression)).or_else(|| {
            visit_node_returns(&mut cb_node, Some(&*node.case_block))
            })
        }
        Node::CaseBlock(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.clauses))
        }
        Node::CaseClause(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
            }),
        Node::DefaultClause(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
        }
        Node::LabeledStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.label))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.statement))),
        Node::ThrowStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::TryStatement(node) => visit_node_returns(&mut cb_node, Some(&*node.try_block))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.catch_clause.clone())
                    .or_else(|| visit_node_returns(&mut cb_node, node.finally_block.clone()))
            }),
        Node::CatchClause(node) => {
            visit_node_returns(&mut cb_node, node.variable_declaration.clone())
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.block)))
        }
        Node::Decorator(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::ClassDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses().as_deref(),
                            )
                            .or_else(|| {
                                visit_nodes_returns(
                                    &mut cb_node,
                                    cb_nodes.as_mut(),
                                    Some(&node.members()),
                                )
                            })
                        })
                    })
                },
            )
        }),
        Node::ClassExpression(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses().as_deref(),
                            )
                            .or_else(|| {
                                visit_nodes_returns(
                                    &mut cb_node,
                                    cb_nodes.as_mut(),
                                    Some(&node.members()),
                                )
                            })
                        })
                    })
                },
            )
        }),
        Node::InterfaceDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses().as_deref(),
                            )
                            .or_else(|| {
                                visit_nodes_returns(
                                    &mut cb_node,
                                    cb_nodes.as_mut(),
                                    Some(&node.members),
                                )
                            })
                        })
                    })
                },
            )
        }),
        Node::TypeAliasDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, Some(node.type_.clone())))
                    })
                },
            )
        }),
        Node::EnumDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))
                    })
                },
            )
        }),
        Node::EnumMember(node) => visit_node_returns(&mut cb_node, Some(&*node.name))
            .or_else(|| visit_node_returns(&mut cb_node, node.initializer.clone())),
        Node::ModuleDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name())
                        .or_else(|| visit_node_returns(&mut cb_node, node.body.clone()))
                },
            )
        }),
        Node::ImportEqualsDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name())
                        .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.module_reference)))
                },
            )
        }),
        Node::ImportDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.import_clause.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(&*node.module_specifier)).or_else(
                            || visit_node_returns(&mut cb_node, node.assert_clause.clone()),
                        )
                    })
                },
            )
        }),
        Node::ImportClause(node) => visit_node_returns(&mut cb_node, node.name.clone())
            .or_else(|| visit_node_returns(&mut cb_node, node.named_bindings.clone())),
        Node::AssertClause(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::AssertEntry(node) => visit_node_returns(&mut cb_node, Some(&*node.name))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.value))),
        Node::NamespaceExportDeclaration(node) => {
            visit_node_returns(&mut cb_node, node.maybe_name())
        }
        Node::NamespaceImport(node) => visit_node_returns(&mut cb_node, Some(&*node.name)),
        Node::NamespaceExport(node) => visit_node_returns(&mut cb_node, Some(&*node.name)),
        Node::NamedImports(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::NamedExports(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ExportDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.export_clause.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.module_specifier.clone()).or_else(
                            || visit_node_returns(&mut cb_node, node.assert_clause.clone()),
                        )
                    })
                },
            )
        }),
        Node::ImportSpecifier(node) => visit_node_returns(&mut cb_node, node.property_name.clone())
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.name))),
        Node::ExportSpecifier(node) => visit_node_returns(&mut cb_node, node.property_name.clone())
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.name))),
        Node::ExportAssignment(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.expression)))
        }),
        Node::TemplateExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.head))
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.template_spans))
            }),
        Node::TemplateSpan(node) => visit_node_returns(&mut cb_node, Some(&*node.expression))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.literal))),
        Node::TemplateLiteralTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.head))
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.template_spans))
            }),
        Node::TemplateLiteralTypeSpan(node) => visit_node_returns(&mut cb_node, Some(&*node.type_))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.literal))),
        Node::ComputedPropertyName(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression))
        }
        Node::HeritageClause(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))
        }
        Node::ExpressionWithTypeArguments(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression)).or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
            })
        }
        Node::ExternalModuleReference(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression))
        }
        Node::MissingDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        ),
        Node::CommaListExpression(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::JsxElement(node) => visit_node_returns(&mut cb_node, Some(&*node.opening_element))
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.children))
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.closing_element)))
            }),
        Node::JsxFragment(node) => visit_node_returns(&mut cb_node, Some(&*node.opening_fragment))
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.children))
                    .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.closing_fragment)))
            }),
        Node::JsxSelfClosingElement(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.tag_name)).or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.attributes)))
            })
        }
        Node::JsxOpeningElement(node) => visit_node_returns(&mut cb_node, Some(&*node.tag_name))
            .or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.attributes)))
            }),
        Node::JsxAttributes(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.properties))
        }
        Node::JsxAttribute(node) => visit_node_returns(&mut cb_node, Some(&*node.name))
            .or_else(|| visit_node_returns(&mut cb_node, node.initializer.clone())),
        Node::JsxSpreadAttribute(node) => visit_node_returns(&mut cb_node, Some(&*node.expression)),
        Node::JsxExpression(node) => {
            visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone())
                .or_else(|| visit_node_returns(&mut cb_node, node.expression.clone()))
        }
        Node::JsxClosingElement(node) => visit_node_returns(&mut cb_node, Some(&*node.tag_name)),
        Node::OptionalTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.type_)),
        Node::RestTypeNode(node) => visit_node_returns(&mut cb_node, Some(&*node.type_)),
        Node::JSDocTypeExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.type_)),
        Node::BaseJSDocUnaryType(node) => visit_node_returns(&mut cb_node, node.type_.clone()),
        Node::JSDocFunctionType(node) => {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.parameters()))
                .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
        }
        Node::JSDoc(node) => {
            (if let Some(StringOrNodeArray::NodeArray(comment)) = node.comment.as_ref() {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
            } else {
                None
            })
            .or_else(|| visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.tags.as_deref()))
        }
        Node::JSDocSeeTag(node) => {
            visit_node_returns(&mut cb_node, Some(node.tag_name())).or_else(|| {
                visit_node_returns(&mut cb_node, node.name.clone()).or_else(|| {
                    if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                    } else {
                        None
                    }
                })
            })
        }
        Node::JSDocNameReference(node) => visit_node_returns(&mut cb_node, Some(&*node.name)),
        Node::JSDocMemberName(node) => visit_node_returns(&mut cb_node, Some(&*node.left))
            .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.right))),
        Node::JSDocPropertyLikeTag(node) => visit_node_returns(&mut cb_node, Some(node.tag_name()))
            .or_else(|| {
                if node.is_name_first {
                    visit_node_returns(&mut cb_node, Some(&*node.name)).or_else(|| {
                        visit_node_returns(&mut cb_node, node.type_expression.clone()).or_else(
                            || {
                                if let Some(StringOrNodeArray::NodeArray(comment)) =
                                    node.maybe_comment()
                                {
                                    visit_nodes_returns(
                                        &mut cb_node,
                                        cb_nodes.as_mut(),
                                        Some(comment),
                                    )
                                } else {
                                    None
                                }
                            },
                        )
                    })
                } else {
                    visit_node_returns(&mut cb_node, node.type_expression.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(&*node.name)).or_else(|| {
                            if let Some(StringOrNodeArray::NodeArray(comment)) =
                                node.maybe_comment()
                            {
                                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                            } else {
                                None
                            }
                        })
                    })
                }
            }),
        Node::BaseJSDocTag(node) => visit_node_returns(&mut cb_node, Some(node.tag_name()))
            .or_else(|| {
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                } else {
                    None
                }
            }),
        Node::JSDocImplementsTag(node) => visit_node_returns(&mut cb_node, Some(node.tag_name()))
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.class)).or_else(|| {
                    if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                    } else {
                        None
                    }
                })
            }),
        Node::JSDocAugmentsTag(node) => visit_node_returns(&mut cb_node, Some(node.tag_name()))
            .or_else(|| {
                visit_node_returns(&mut cb_node, Some(&*node.class)).or_else(|| {
                    if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                    } else {
                        None
                    }
                })
            }),
        Node::JSDocTemplateTag(node) => visit_node_returns(&mut cb_node, Some(node.tag_name()))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.constraint.clone()).or_else(|| {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        Some(&node.type_parameters),
                    )
                    .or_else(|| {
                        if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                        } else {
                            None
                        }
                    })
                })
            }),
        Node::JSDocTypedefTag(node) => {
            visit_node_returns(&mut cb_node, Some(node.tag_name())).or_else(|| {
                if matches!(node.type_expression.as_ref(), Some(type_expression) if type_expression.kind() == SyntaxKind::JSDocTypeExpression)
                {
                    visit_node_returns(&mut cb_node, node.type_expression.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.full_name.clone()).or_else(|| {
                            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                            } else {
                                None
                            }
                        })
                    })
                } else {
                    visit_node_returns(&mut cb_node, node.full_name.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, node.type_expression.clone()).or_else(|| {
                            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                            } else {
                                None
                            }
                        })
                    })
                }
            })
        }
        Node::JSDocCallbackTag(node) => {
            visit_node_returns(&mut cb_node, Some(node.tag_name())).or_else(|| {
                visit_node_returns(&mut cb_node, node.full_name.clone()).or_else(|| {
                    visit_node_returns(&mut cb_node, Some(&*node.type_expression)).or_else(|| {
                        if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                        } else {
                            None
                        }
                    })
                })
            })
        }
        Node::BaseJSDocTypeLikeTag(node) => {
            visit_node_returns(&mut cb_node, Some(node.tag_name())).or_else(|| {
                visit_node_returns(&mut cb_node, node.type_expression.clone()).or_else(|| {
                    if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
                    } else {
                        None
                    }
                })
            })
        }
        Node::JSDocSignature(node) => {
            node.maybe_type_parameters().as_ref().and_then(|type_parameters| {
                for_each(type_parameters, |node, _| {
                    cb_node(node)
                })
            }).or_else(|| {
                for_each(&node.parameters, |node, _| {
                    cb_node(node)
                })
            }).or_else(|| {
            visit_node_returns(&mut cb_node, node.type_.clone())
            })
        }
        Node::JSDocLink(node) => {
            visit_node_returns(&mut cb_node, node.name.clone())
        }
        Node::JSDocLinkCode(node) => {
            visit_node_returns(&mut cb_node, node.name.clone())
        }
        Node::JSDocLinkPlain(node) => {
            visit_node_returns(&mut cb_node, node.name.clone())
        }
        Node::JSDocTypeLiteral(node) => {
            maybe_for_each(node.js_doc_property_tags.as_deref(), |node, _| {
                cb_node(node)
            })
        }
        Node::PartiallyEmittedExpression(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression))
        }
        _ => None
    }
}

pub fn try_for_each_child_returns<TReturn, TError>(
    node: Id<Node>,
    mut cb_node: impl FnMut(Id<Node>) -> Result<Option<TReturn>, TError>,
    mut cb_nodes: Option<impl FnMut(&NodeArray) -> Result<Option<TReturn>, TError>>,
) -> Result<Option<TReturn>, TError> {
    if
    /* !node ||*/
    node.kind() <= SyntaxKind::LastToken {
        return Ok(None);
    }
    Ok(match node {
        Node::QualifiedName(node) => try_visit_node_returns(&mut cb_node, Some(&*node.left))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.right)))?,
        Node::TypeParameterDeclaration(node) => try_visit_node_returns(&mut cb_node, Some(node.name()))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.constraint.clone())?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, node.default.clone())?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.expression.clone()))
                })
            })?,
        Node::ShorthandPropertyAssignment(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.question_token.clone())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.exclamation_token.clone())?
                                        .try_or_else(|| {
                                            try_visit_node_returns(
                                                &mut cb_node,
                                                node.equals_token.clone(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_node_returns(
                                                        &mut cb_node,
                                                        node.object_assignment_initializer.clone(),
                                                    )
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::SpreadAssignment(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::ParameterDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.question_token.clone())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_type())?.try_or_else(
                                        || {
                                            try_visit_node_returns(
                                                &mut cb_node,
                                                node.maybe_initializer(),
                                            )
                                        },
                                    )
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::PropertyDeclaration(node) => {
            try_visit_nodes_returns(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_deref(),
            )?
            .try_or_else(|| {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?
                    .try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.question_token.clone())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.exclamation_token.clone())?
                                        .try_or_else(|| {
                                            try_visit_node_returns(&mut cb_node, node.maybe_type())?
                                                .try_or_else(|| {
                                                    try_visit_node_returns(
                                                        &mut cb_node,
                                                        node.maybe_initializer(),
                                                    )
                                                })
                                        })
                                },
                            )
                        })
                    })
            })?
        }
        Node::PropertySignature(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.question_token.clone())?.try_or_else(
                            || {
                                try_visit_node_returns(&mut cb_node, node.maybe_type())?.try_or_else(|| {
                                    try_visit_node_returns(&mut cb_node, node.maybe_initializer())
                                })
                            },
                        )
                    })
                },
            )
        })?,
        Node::PropertyAssignment(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.question_token.clone())?
                            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.initializer)))
                    })
                },
            )
        })?,
        Node::VariableDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.exclamation_token.clone())?.try_or_else(
                            || {
                                try_visit_node_returns(&mut cb_node, node.maybe_type())?.try_or_else(|| {
                                    try_visit_node_returns(&mut cb_node, node.maybe_initializer())
                                })
                            },
                        )
                    })
                },
            )
        })?,
        Node::BindingElement(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.property_name.clone())?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                                try_visit_node_returns(&mut cb_node, node.maybe_initializer())
                            })
                        })
                    })
                },
            )
        })?,
        Node::FunctionTypeNode(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )?
                    .try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        })?,
        Node::ConstructorTypeNode(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )?
                    .try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        })?,
        Node::CallSignatureDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )?
                    .try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        })?,
        Node::ConstructSignatureDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )?
                    .try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        })?,
        Node::IndexSignatureDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_deref(),
                    )?
                    .try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(&node.parameters()),
                        )?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        })?,
        Node::MethodDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::MethodSignature(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )?
                        .try_or_else(|| {
                            try_visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                Some(&node.parameters()),
                            )?
                            .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))
                        })
                    })
                },
            )
        })?,
        Node::ConstructorDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::GetAccessorDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::SetAccessorDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::FunctionExpression(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::FunctionDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                node.maybe_body(),
                                                            )
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::ArrowFunction(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_asterisk_token())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                            try_visit_node_returns(&mut cb_node, node.maybe_question_token())?.try_or_else(
                                || {
                                    try_visit_node_returns(&mut cb_node, node.maybe_exclamation_token().clone())?
                                        .try_or_else(|| {
                                            try_visit_nodes_returns(
                                                &mut cb_node,
                                                cb_nodes.as_mut(),
                                                node.maybe_type_parameters().as_deref(),
                                            )?
                                            .try_or_else(
                                                || {
                                                    try_visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(&node.parameters()),
                                                    )?
                                                    .try_or_else(|| {
                                                        try_visit_node_returns(
                                                            &mut cb_node,
                                                            node.maybe_type(),
                                                        )?
                                                        .try_or_else(|| {
                                                            try_visit_node_returns(
                                                                &mut cb_node,
                                                                Some(
                                                                    &*node
                                                                        .equals_greater_than_token,
                                                                ),
                                                            )?
                                                            .try_or_else(|| {
                                                                try_visit_node_returns(
                                                                    &mut cb_node,
                                                                    node.maybe_body(),
                                                                )
                                                            })
                                                        })
                                                    })
                                                },
                                            )
                                        })
                                },
                            )
                        })
                    })
                },
            )
        })?,
        Node::ClassStaticBlockDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.body)))
        })?,
        Node::TypeReferenceNode(node) => {
            try_visit_node_returns(&mut cb_node, Some(node.type_name.clone()))?.try_or_else(|| {
                try_visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
            })?
        }
        Node::TypePredicateNode(node) => {
            try_visit_node_returns(&mut cb_node, node.asserts_modifier.clone())?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.parameter_name))?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, node.type_.clone()))
            })?
        }
        Node::TypeQueryNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expr_name))?,
        Node::TypeLiteralNode(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))?
        }
        Node::ArrayTypeNode(node) => {
            try_visit_node_returns(&mut cb_node, Some(node.element_type.clone()))?
        }
        Node::TupleTypeNode(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::UnionTypeNode(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))?
        }
        Node::IntersectionTypeNode(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))?
        }
        Node::ConditionalTypeNode(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.check_type))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.extends_type))?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, Some(&*node.true_type))?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.false_type)))
                })
            })?
        }
        Node::InferTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_parameter))?,
        Node::ImportTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.argument))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.qualifier.clone())?.try_or_else(|| {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_deref(),
                    )
                })
            })?,
        Node::ParenthesizedTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?,
        Node::TypeOperatorNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?,
        Node::IndexedAccessTypeNode(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.object_type))?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.index_type)))?
        }
        Node::MappedTypeNode(node) => try_visit_node_returns(&mut cb_node, node.readonly_token.clone())?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.type_parameter))?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, node.name_type.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.question_token.clone())?.try_or_else(
                            || {
                                try_visit_node_returns(&mut cb_node, node.type_.clone())?.try_or_else(|| {
                                    try_visit_nodes_returns(
                                        &mut cb_node,
                                        cb_nodes.as_mut(),
                                        node.members.as_deref(),
                                    )
                                })
                            },
                        )
                    })
                })
            })?,
        Node::LiteralTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.literal))?,
        Node::NamedTupleMember(node) => {
            try_visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone())?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.name))?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, node.question_token.clone())?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.type_)))
                })
            })?
        }
        Node::ObjectBindingPattern(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::ArrayBindingPattern(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::ArrayLiteralExpression(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::ObjectLiteralExpression(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.properties))?
        }
        Node::PropertyAccessExpression(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.question_dot_token.clone())?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.name)))
            })?
        }
        Node::ElementAccessExpression(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.question_dot_token.clone())?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.argument_expression)))
            })?
        }
        Node::CallExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.question_dot_token.clone())?.try_or_else(|| {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_deref(),
                    )?
                    .try_or_else(|| {
                        try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.arguments))
                    })
                })
            })?,
        Node::NewExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| {
                try_visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )?
                .try_or_else(|| {
                    try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.arguments.as_deref())
                })
            })?,
        Node::TaggedTemplateExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.tag))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.question_dot_token.clone())?.try_or_else(|| {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_deref(),
                    )?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.template)))
                })
            })?,
        Node::TypeAssertion(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.expression)))?,
        Node::ParenthesizedExpression(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
        }
        Node::DeleteExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::TypeOfExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::VoidExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::PrefixUnaryExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.operand))?,
        Node::YieldExpression(node) => {
            try_visit_node_returns(&mut cb_node, node.asterisk_token.clone())?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, node.expression.clone()))?
        }
        Node::AwaitExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::PostfixUnaryExpression(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.operand))?
        }
        Node::BinaryExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.left))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.operator_token))?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.right)))
            })?,
        Node::AsExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.type_)))?,
        Node::NonNullExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::MetaProperty(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?,
        Node::ConditionalExpression(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.condition))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.question_token))?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, Some(&*node.when_true))?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(&*node.colon_token))?
                            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.when_false)))
                    })
                })
            })?
        }
        Node::SpreadElement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::Block(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))?
        }
        Node::ModuleBlock(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))?
        }
        Node::SourceFile(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements()))?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(node.end_of_file_token())))?
        }
        Node::VariableStatement(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(node.declaration_list.clone())))
        })?,
        Node::VariableDeclarationList(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.declarations))?
        }
        Node::ExpressionStatement(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
        }
        Node::IfStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.then_statement))?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, node.else_statement.clone()))
            })?,
        Node::DoStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.statement))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.expression)))?,
        Node::WhileStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.statement)))?,
        Node::ForStatement(node) => try_visit_node_returns(&mut cb_node, node.initializer.clone())?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.condition.clone())?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, node.incrementor.clone())?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.statement)))
                })
            })?,
        Node::ForInStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.initializer))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.statement)))
            })?,
        Node::ForOfStatement(node) => try_visit_node_returns(&mut cb_node, node.await_modifier.clone())?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.initializer))?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.statement)))
                })
            })?,
        Node::BreakStatement(node) => try_visit_node_returns(&mut cb_node, node.label.clone())?,
        Node::ContinueStatement(node) => try_visit_node_returns(&mut cb_node, node.label.clone())?,
        Node::ReturnStatement(node) => try_visit_node_returns(&mut cb_node, node.expression.clone())?,
        Node::WithStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.statement)))?,
        Node::SwitchStatement(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.case_block))
            })?
        }
        Node::CaseBlock(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.clauses))?
        }
        Node::CaseClause(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
            })?,
        Node::DefaultClause(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))?
        }
        Node::LabeledStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.label))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.statement)))?,
        Node::ThrowStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::TryStatement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.try_block))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.catch_clause.clone())?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, node.finally_block.clone()))
            })?,
        Node::CatchClause(node) => {
            try_visit_node_returns(&mut cb_node, node.variable_declaration.clone())?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.block)))?
        }
        Node::Decorator(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::ClassDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )?
                        .try_or_else(|| {
                            try_visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses().as_deref(),
                            )?
                            .try_or_else(|| {
                                try_visit_nodes_returns(
                                    &mut cb_node,
                                    cb_nodes.as_mut(),
                                    Some(&node.members()),
                                )
                            })
                        })
                    })
                },
            )
        })?,
        Node::ClassExpression(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_name())?.try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )?
                        .try_or_else(|| {
                            try_visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses().as_deref(),
                            )?
                            .try_or_else(|| {
                                try_visit_nodes_returns(
                                    &mut cb_node,
                                    cb_nodes.as_mut(),
                                    Some(&node.members()),
                                )
                            })
                        })
                    })
                },
            )
        })?,
        Node::InterfaceDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )?
                        .try_or_else(|| {
                            try_visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses().as_deref(),
                            )?
                            .try_or_else(|| {
                                try_visit_nodes_returns(
                                    &mut cb_node,
                                    cb_nodes.as_mut(),
                                    Some(&node.members),
                                )
                            })
                        })
                    })
                },
            )
        })?,
        Node::TypeAliasDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_deref(),
                        )?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(node.type_.clone())))
                    })
                },
            )
        })?,
        Node::EnumDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, Some(node.name()))?.try_or_else(|| {
                        try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))
                    })
                },
            )
        })?,
        Node::EnumMember(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, node.initializer.clone()))?,
        Node::ModuleDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_name())?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, node.body.clone()))
                },
            )
        })?,
        Node::ImportEqualsDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.maybe_name())?
                        .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.module_reference)))
                },
            )
        })?,
        Node::ImportDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.import_clause.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(&*node.module_specifier))?.try_or_else(
                            || try_visit_node_returns(&mut cb_node, node.assert_clause.clone()),
                        )
                    })
                },
            )
        })?,
        Node::ImportClause(node) => try_visit_node_returns(&mut cb_node, node.name.clone())?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, node.named_bindings.clone()))?,
        Node::AssertClause(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::AssertEntry(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.value)))?,
        Node::NamespaceExportDeclaration(node) => {
            try_visit_node_returns(&mut cb_node, node.maybe_name())?
        }
        Node::NamespaceImport(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?,
        Node::NamespaceExport(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?,
        Node::NamedImports(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::NamedExports(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::ExportDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?.try_or_else(
                || {
                    try_visit_node_returns(&mut cb_node, node.export_clause.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.module_specifier.clone())?.try_or_else(
                            || try_visit_node_returns(&mut cb_node, node.assert_clause.clone()),
                        )
                    })
                },
            )
        })?,
        Node::ImportSpecifier(node) => try_visit_node_returns(&mut cb_node, node.property_name.clone())?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.name)))?,
        Node::ExportSpecifier(node) => try_visit_node_returns(&mut cb_node, node.property_name.clone())?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.name)))?,
        Node::ExportAssignment(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?
        .try_or_else(|| {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_deref())?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.expression)))
        })?,
        Node::TemplateExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.head))?
            .try_or_else(|| {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.template_spans))
            })?,
        Node::TemplateSpan(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.literal)))?,
        Node::TemplateLiteralTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.head))?
            .try_or_else(|| {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.template_spans))
            })?,
        Node::TemplateLiteralTypeSpan(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.literal)))?,
        Node::ComputedPropertyName(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
        }
        Node::HeritageClause(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))?
        }
        Node::ExpressionWithTypeArguments(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?.try_or_else(|| {
                try_visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )
            })?
        }
        Node::ExternalModuleReference(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
        }
        Node::MissingDeclaration(node) => try_visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_deref(),
        )?,
        Node::CommaListExpression(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))?
        }
        Node::JsxElement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.opening_element))?
            .try_or_else(|| {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.children))?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.closing_element)))
            })?,
        Node::JsxFragment(node) => try_visit_node_returns(&mut cb_node, Some(&*node.opening_fragment))?
            .try_or_else(|| {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.children))?
                    .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.closing_fragment)))
            })?,
        Node::JsxSelfClosingElement(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.tag_name))?.try_or_else(|| {
                try_visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.attributes)))
            })?
        }
        Node::JsxOpeningElement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.tag_name))?
            .try_or_else(|| {
                try_visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_deref(),
                )?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.attributes)))
            })?,
        Node::JsxAttributes(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.properties))?
        }
        Node::JsxAttribute(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, node.initializer.clone()))?,
        Node::JsxSpreadAttribute(node) => try_visit_node_returns(&mut cb_node, Some(&*node.expression))?,
        Node::JsxExpression(node) => {
            try_visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone())?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, node.expression.clone()))?
        }
        Node::JsxClosingElement(node) => try_visit_node_returns(&mut cb_node, Some(&*node.tag_name))?,
        Node::OptionalTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?,
        Node::RestTypeNode(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?,
        Node::JSDocTypeExpression(node) => try_visit_node_returns(&mut cb_node, Some(&*node.type_))?,
        Node::BaseJSDocUnaryType(node) => try_visit_node_returns(&mut cb_node, node.type_.clone())?,
        Node::JSDocFunctionType(node) => {
            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(&node.parameters()))?
                .try_or_else(|| try_visit_node_returns(&mut cb_node, node.maybe_type()))?
        }
        Node::JSDoc(node) => {
            (if let Some(StringOrNodeArray::NodeArray(comment)) = node.comment.as_ref() {
                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
            } else {
                None
            })
            .try_or_else(|| try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.tags.as_deref()))?
        }
        Node::JSDocSeeTag(node) => {
            try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.name.clone())?.try_or_else(|| -> Result<_, TError> {
                    Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                    } else {
                        None
                    })
                })
            })?
        }
        Node::JSDocNameReference(node) => try_visit_node_returns(&mut cb_node, Some(&*node.name))?,
        Node::JSDocMemberName(node) => try_visit_node_returns(&mut cb_node, Some(&*node.left))?
            .try_or_else(|| try_visit_node_returns(&mut cb_node, Some(&*node.right)))?,
        Node::JSDocPropertyLikeTag(node) => try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?
            .try_or_else(|| {
                if node.is_name_first {
                    try_visit_node_returns(&mut cb_node, Some(&*node.name))?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.type_expression.clone())?.try_or_else(
                            || -> Result<_, TError> {
                                Ok(if let Some(StringOrNodeArray::NodeArray(comment)) =
                                    node.maybe_comment()
                                {
                                    try_visit_nodes_returns(
                                        &mut cb_node,
                                        cb_nodes.as_mut(),
                                        Some(comment),
                                    )?
                                } else {
                                    None
                                })
                            },
                        )
                    })
                } else {
                    try_visit_node_returns(&mut cb_node, node.type_expression.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, Some(&*node.name))?.try_or_else(|| -> Result<_, TError> {
                            Ok(if let Some(StringOrNodeArray::NodeArray(comment)) =
                                node.maybe_comment()
                            {
                                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                            } else {
                                None
                            })
                        })
                    })
                }
            })?,
        Node::BaseJSDocTag(node) => try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?
            .try_or_else(|| -> Result<_, TError> {
                Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                } else {
                    None
                })
            })?,
        Node::JSDocImplementsTag(node) => try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.class))?.try_or_else(|| -> Result<_, TError> {
                    Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                    } else {
                        None
                    })
                })
            })?,
        Node::JSDocAugmentsTag(node) => try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, Some(&*node.class))?.try_or_else(|| -> Result<_, TError> {
                    Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                    } else {
                        None
                    })
                })
            })?,
        Node::JSDocTemplateTag(node) => try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?
            .try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.constraint.clone())?.try_or_else(|| {
                    try_visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        Some(&node.type_parameters),
                    )?
                    .try_or_else(|| -> Result<_, TError> {
                        Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                        } else {
                            None
                        })
                    })
                })
            })?,
        Node::JSDocTypedefTag(node) => {
            try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?.try_or_else(|| {
                if matches!(node.type_expression.as_ref(), Some(type_expression) if type_expression.kind() == SyntaxKind::JSDocTypeExpression)
                {
                    try_visit_node_returns(&mut cb_node, node.type_expression.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.full_name.clone())?.try_or_else(|| -> Result<_, TError> {
                            Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                            } else {
                                None
                            })
                        })
                    })
                } else {
                    try_visit_node_returns(&mut cb_node, node.full_name.clone())?.try_or_else(|| {
                        try_visit_node_returns(&mut cb_node, node.type_expression.clone())?.try_or_else(|| -> Result<_, TError> {
                            Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                                try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                            } else {
                                None
                            })
                        })
                    })
                }
            })?
        }
        Node::JSDocCallbackTag(node) => {
            try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.full_name.clone())?.try_or_else(|| {
                    try_visit_node_returns(&mut cb_node, Some(&*node.type_expression))?.try_or_else(|| {
                        Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                            try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                        } else {
                            None
                        })
                    })
                })
            })?
        }
        Node::BaseJSDocTypeLikeTag(node) => {
            try_visit_node_returns(&mut cb_node, Some(node.tag_name()))?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.type_expression.clone())?.try_or_else(|| -> Result<_, TError> {
                    Ok(if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                        try_visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))?
                    } else {
                        None
                    })
                })
            })?
        }
        Node::JSDocSignature(node) => {
            node.maybe_type_parameters().as_ref().try_and_then(|type_parameters| {
                try_for_each(type_parameters, |node: &Id<Node>, _| {
                    cb_node(node)
                })
            })?.try_or_else(|| {
                try_for_each(&node.parameters, |node: &Id<Node>, _| {
                    cb_node(node)
                })
            })?.try_or_else(|| {
                try_visit_node_returns(&mut cb_node, node.type_.clone())
            })?
        }
        Node::JSDocLink(node) => {
            try_visit_node_returns(&mut cb_node, node.name.clone())?
        }
        Node::JSDocLinkCode(node) => {
            try_visit_node_returns(&mut cb_node, node.name.clone())?
        }
        Node::JSDocLinkPlain(node) => {
            try_visit_node_returns(&mut cb_node, node.name.clone())?
        }
        Node::JSDocTypeLiteral(node) => {
            try_maybe_for_each(node.js_doc_property_tags.as_deref(), |node: &Id<Node>, _| {
                cb_node(node)
            })?
        }
        Node::PartiallyEmittedExpression(node) => {
            try_visit_node_returns(&mut cb_node, Some(&*node.expression))?
        }
        _ => None
    })
}

pub fn for_each_child_bool(
    node: Id<Node>,
    mut cb_node: impl FnMut(Id<Node>) -> bool,
    cb_nodes: Option<impl FnMut(&NodeArray) -> bool>,
) -> bool {
    // match for_each_child_returns(
    //     node,
    //     |node: &Node| if cb_node(node) { Some(()) } else { None },
    //     cb_nodes.map(|cb_nodes| {
    //         |node_array: &NodeArray| if cb_nodes(node_array) { Some(()) } else { None }
    //     }),
    // ) {
    //     Some(_) => true,
    //     None => false,
    // }
    if let Some(mut cb_nodes) = cb_nodes {
        match for_each_child_returns(
            node,
            |node: Id<Node>| if cb_node(node) { Some(()) } else { None },
            Some(|node_array: &NodeArray| if cb_nodes(node_array) { Some(()) } else { None }),
        ) {
            Some(_) => true,
            None => false,
        }
    } else {
        match for_each_child_returns(
            node,
            |node: Id<Node>| if cb_node(node) { Some(()) } else { None },
            Option::<fn(&NodeArray) -> Option<()>>::None,
        ) {
            Some(_) => true,
            None => false,
        }
    }
}

pub fn try_for_each_child_bool<TError>(
    node: Id<Node>,
    mut cb_node: impl FnMut(Id<Node>) -> Result<bool, TError>,
    cb_nodes: Option<impl FnMut(&NodeArray) -> Result<bool, TError>>,
) -> Result<bool, TError> {
    Ok(if let Some(mut cb_nodes) = cb_nodes {
        match try_for_each_child_returns(
            node,
            |node: Id<Node>| -> Result<_, TError> {
                Ok(if cb_node(node)? { Some(()) } else { None })
            },
            Some(|node_array: &NodeArray| -> Result<_, TError> {
                Ok(if cb_nodes(node_array)? {
                    Some(())
                } else {
                    None
                })
            }),
        )? {
            Some(_) => true,
            None => false,
        }
    } else {
        match try_for_each_child_returns(
            node,
            |node: Id<Node>| -> Result<_, TError> {
                Ok(if cb_node(node)? { Some(()) } else { None })
            },
            Option::<fn(&NodeArray) -> Result<Option<()>, TError>>::None,
        )? {
            Some(_) => true,
            None => false,
        }
    })
}
