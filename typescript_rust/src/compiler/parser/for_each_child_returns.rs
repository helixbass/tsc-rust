use super::{visit_node_returns, visit_nodes_returns};
use crate::{
    for_each, maybe_for_each, ClassLikeDeclarationInterface, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasQuestionTokenInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, JSDocTagInterface, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, SignatureDeclarationInterface, StringOrNodeArray, SyntaxKind,
};

pub fn for_each_child_returns<
    TReturn,
    TNodeCallback: FnMut(&Node) -> Option<TReturn>,
    TNodesCallback: FnMut(&NodeArray) -> Option<TReturn>,
>(
    node: &Node,
    mut cb_node: TNodeCallback,
    mut cb_nodes: Option<TNodesCallback>,
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.dot_dot_dot_token.clone()).or_else(|| {
                        visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
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
                node.maybe_decorators().as_ref(),
            )
            .or_else(|| {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref())
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_ref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::ConstructorTypeNode(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_ref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::CallSignatureDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_ref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::ConstructSignatureDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_ref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::IndexSignatureDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_parameters().as_ref(),
                    )
                    .or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            Some(node.parameters()),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
                    })
                },
            )
        }),
        Node::MethodDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_ref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
                                                node.maybe_type_parameters().as_ref(),
                                            )
                                            .or_else(
                                                || {
                                                    visit_nodes_returns(
                                                        &mut cb_node,
                                                        cb_nodes.as_mut(),
                                                        Some(node.parameters()),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref())
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.body)))
        }),
        Node::TypeReferenceNode(node) => {
            visit_node_returns(&mut cb_node, Some(node.type_name.clone())).or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_ref(),
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
                        node.maybe_type_arguments().as_ref(),
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
                                        node.members.as_ref(),
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
                        node.maybe_type_arguments().as_ref(),
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
                    node.maybe_type_arguments().as_ref(),
                )
                .or_else(|| {
                    visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.arguments.as_ref())
                })
            }),
        Node::TaggedTemplateExpression(node) => visit_node_returns(&mut cb_node, Some(&*node.tag))
            .or_else(|| {
                visit_node_returns(&mut cb_node, node.question_dot_token.clone()).or_else(|| {
                    visit_nodes_returns(
                        &mut cb_node,
                        cb_nodes.as_mut(),
                        node.maybe_type_arguments().as_ref(),
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
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(node.statements()))
                .or_else(|| visit_node_returns(&mut cb_node, Some(node.end_of_file_token())))
        }
        Node::VariableStatement(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref())
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_ref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses(),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name()).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_ref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses(),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_ref(),
                        )
                        .or_else(|| {
                            visit_nodes_returns(
                                &mut cb_node,
                                cb_nodes.as_mut(),
                                node.maybe_heritage_clauses(),
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, Some(node.name())).or_else(|| {
                        visit_nodes_returns(
                            &mut cb_node,
                            cb_nodes.as_mut(),
                            node.maybe_type_parameters().as_ref(),
                        )
                        .or_else(|| visit_node_returns(&mut cb_node, Some(node.type_.clone())))
                    })
                },
            )
        }),
        Node::EnumDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name())
                        .or_else(|| visit_node_returns(&mut cb_node, node.body.clone()))
                },
            )
        }),
        Node::ImportEqualsDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
                || {
                    visit_node_returns(&mut cb_node, node.maybe_name())
                        .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.module_reference)))
                },
            )
        }),
        Node::ImportDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref()).or_else(
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
            node.maybe_decorators().as_ref(),
        )
        .or_else(|| {
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.maybe_modifiers().as_ref())
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
                    node.maybe_type_arguments().as_ref(),
                )
            })
        }
        Node::ExternalModuleReference(node) => {
            visit_node_returns(&mut cb_node, Some(&*node.expression))
        }
        Node::MissingDeclaration(node) => visit_nodes_returns(
            &mut cb_node,
            cb_nodes.as_mut(),
            node.maybe_decorators().as_ref(),
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
                    node.maybe_type_arguments().as_ref(),
                )
                .or_else(|| visit_node_returns(&mut cb_node, Some(&*node.attributes)))
            })
        }
        Node::JsxOpeningElement(node) => visit_node_returns(&mut cb_node, Some(&*node.tag_name))
            .or_else(|| {
                visit_nodes_returns(
                    &mut cb_node,
                    cb_nodes.as_mut(),
                    node.maybe_type_arguments().as_ref(),
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
            visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()))
                .or_else(|| visit_node_returns(&mut cb_node, node.maybe_type()))
        }
        Node::JSDoc(node) => {
            (if let Some(StringOrNodeArray::NodeArray(comment)) = node.comment.as_ref() {
                visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), Some(comment))
            } else {
                None
            })
            .or_else(|| visit_nodes_returns(&mut cb_node, cb_nodes.as_mut(), node.tags.as_ref()))
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

pub fn for_each_child_bool<
    TNodeCallback: FnMut(&Node) -> bool,
    TNodesCallback: FnMut(&NodeArray) -> bool,
>(
    node: &Node,
    mut cb_node: TNodeCallback,
    mut cb_nodes: Option<TNodesCallback>,
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
            |node: &Node| if cb_node(node) { Some(()) } else { None },
            Some(|node_array: &NodeArray| if cb_nodes(node_array) { Some(()) } else { None }),
        ) {
            Some(_) => true,
            None => false,
        }
    } else {
        match for_each_child_returns(
            node,
            |node: &Node| if cb_node(node) { Some(()) } else { None },
            Option::<fn(&NodeArray) -> Option<()>>::None,
        ) {
            Some(_) => true,
            None => false,
        }
    }
}
