use gc::Gc;
use id_arena::Id;

use super::{try_visit_node, try_visit_nodes, visit_node, visit_nodes};
use crate::{
    for_each, maybe_for_each, try_for_each, try_maybe_for_each, ClassLikeDeclarationInterface,
    FunctionLikeDeclarationInterface, HasInitializerInterface, HasQuestionTokenInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InterfaceOrClassLikeDeclarationInterface, JSDocTagInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, OptionTry,
    SignatureDeclarationInterface, StringOrNodeArray, SyntaxKind,
    HasArena, InArena, OptionInArena,
};

pub fn for_each_child(
    node: Id<Node>,
    mut cb_node: impl FnMut(Id<Node>),
    mut cb_nodes: Option<impl FnMut(Id<NodeArray>)>,
    arena: &impl HasArena,
) {
    if
    /* !node ||*/
    node.ref_(arena).kind() <= SyntaxKind::LastToken {
        return;
    }
    match &*node.ref_(arena) {
        Node::QualifiedName(node) => {
            visit_node(&mut cb_node, Some(node.left));
            visit_node(&mut cb_node, Some(node.right));
        }
        Node::TypeParameterDeclaration(node) => {
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.constraint.clone());
            visit_node(&mut cb_node, node.default.clone());
            visit_node(&mut cb_node, node.expression.clone());
        }
        Node::ShorthandPropertyAssignment(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.exclamation_token.clone());
            visit_node(&mut cb_node, node.equals_token.clone());
            visit_node(&mut cb_node, node.object_assignment_initializer.clone());
        }
        Node::SpreadAssignment(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::ParameterDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_initializer())
        }
        Node::PropertyDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.exclamation_token.clone());
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_initializer())
        }
        Node::PropertySignature(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_initializer());
        }
        Node::PropertyAssignment(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, Some(node.initializer))
        }
        Node::VariableDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.exclamation_token.clone());
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_initializer())
        }
        Node::BindingElement(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, node.property_name.clone());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_initializer())
        }
        Node::FunctionTypeNode(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::ConstructorTypeNode(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::CallSignatureDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::ConstructSignatureDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::IndexSignatureDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::MethodDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::MethodSignature(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::ConstructorDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::GetAccessorDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::SetAccessorDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::FunctionExpression(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::FunctionDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::ArrowFunction(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, Some(node.equals_greater_than_token));
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::ClassStaticBlockDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.body));
        }
        Node::TypeReferenceNode(node) => {
            visit_node(&mut cb_node, Some(node.type_name.clone()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )
        }
        Node::TypePredicateNode(node) => {
            visit_node(&mut cb_node, node.asserts_modifier.clone());
            visit_node(&mut cb_node, Some(node.parameter_name));
            visit_node(&mut cb_node, node.type_.clone());
        }
        Node::TypeQueryNode(node) => visit_node(&mut cb_node, Some(node.expr_name)),
        Node::TypeLiteralNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members), arena)
        }
        Node::ArrayTypeNode(node) => visit_node(&mut cb_node, Some(node.element_type.clone())),
        Node::TupleTypeNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::UnionTypeNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.types), arena)
        }
        Node::IntersectionTypeNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.types), arena)
        }
        Node::ConditionalTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.check_type));
            visit_node(&mut cb_node, Some(node.extends_type));
            visit_node(&mut cb_node, Some(node.true_type));
            visit_node(&mut cb_node, Some(node.false_type));
        }
        Node::InferTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.type_parameter));
        }
        Node::ImportTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.argument));
            visit_node(&mut cb_node, node.qualifier.clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )
        }
        Node::ParenthesizedTypeNode(node) => visit_node(&mut cb_node, Some(node.type_)),
        Node::TypeOperatorNode(node) => visit_node(&mut cb_node, Some(node.type_)),
        Node::IndexedAccessTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.object_type));
            visit_node(&mut cb_node, Some(node.index_type));
        }
        Node::MappedTypeNode(node) => {
            visit_node(&mut cb_node, node.readonly_token.clone());
            visit_node(&mut cb_node, Some(node.type_parameter));
            visit_node(&mut cb_node, node.name_type.clone());
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.type_.clone());
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.members, arena)
        }
        Node::LiteralTypeNode(node) => visit_node(&mut cb_node, Some(node.literal)),
        Node::NamedTupleMember(node) => {
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, Some(node.name));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, Some(node.type_));
        }
        Node::ObjectBindingPattern(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::ArrayBindingPattern(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::ArrayLiteralExpression(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::ObjectLiteralExpression(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.properties), arena)
        }
        Node::PropertyAccessExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::ElementAccessExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_node(&mut cb_node, Some(node.argument_expression));
        }
        Node::CallExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.arguments), arena);
        }
        Node::NewExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.arguments, arena);
        }
        Node::TaggedTemplateExpression(node) => {
            visit_node(&mut cb_node, Some(node.tag));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.template));
        }
        Node::TypeAssertion(node) => {
            visit_node(&mut cb_node, Some(node.type_));
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::ParenthesizedExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::DeleteExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::TypeOfExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::VoidExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::PrefixUnaryExpression(node) => visit_node(&mut cb_node, Some(node.operand)),
        Node::YieldExpression(node) => {
            visit_node(&mut cb_node, node.asterisk_token.clone());
            visit_node(&mut cb_node, node.expression.clone());
        }
        Node::AwaitExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::PostfixUnaryExpression(node) => visit_node(&mut cb_node, Some(node.operand)),
        Node::BinaryExpression(node) => {
            visit_node(&mut cb_node, Some(node.left));
            visit_node(&mut cb_node, Some(node.operator_token));
            visit_node(&mut cb_node, Some(node.right));
        }
        Node::AsExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.type_));
        }
        Node::NonNullExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::MetaProperty(node) => {
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::ConditionalExpression(node) => {
            visit_node(&mut cb_node, Some(node.condition));
            visit_node(&mut cb_node, Some(node.question_token));
            visit_node(&mut cb_node, Some(node.when_true));
            visit_node(&mut cb_node, Some(node.colon_token));
            visit_node(&mut cb_node, Some(node.when_false));
        }
        Node::SpreadElement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::Block(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena);
        }
        Node::ModuleBlock(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena);
        }
        Node::SourceFile(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements()), arena);
            visit_node(&mut cb_node, Some(node.end_of_file_token()));
        }
        Node::VariableStatement(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.declaration_list.clone()))
        }
        Node::VariableDeclarationList(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.declarations), arena)
        }
        Node::ExpressionStatement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::IfStatement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.then_statement));
            visit_node(&mut cb_node, node.else_statement.clone());
        }
        Node::DoStatement(node) => {
            visit_node(&mut cb_node, Some(node.statement));
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::WhileStatement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.statement));
        }
        Node::ForStatement(node) => {
            visit_node(&mut cb_node, node.initializer.clone());
            visit_node(&mut cb_node, node.condition.clone());
            visit_node(&mut cb_node, node.incrementor.clone());
            visit_node(&mut cb_node, Some(node.statement));
        }
        Node::ForInStatement(node) => {
            visit_node(&mut cb_node, Some(node.initializer));
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.statement));
        }
        Node::ForOfStatement(node) => {
            visit_node(&mut cb_node, node.await_modifier.clone());
            visit_node(&mut cb_node, Some(node.initializer));
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.statement));
        }
        Node::BreakStatement(node) => {
            visit_node(&mut cb_node, node.label.clone());
        }
        Node::ContinueStatement(node) => {
            visit_node(&mut cb_node, node.label.clone());
        }
        Node::ReturnStatement(node) => {
            visit_node(&mut cb_node, node.expression.clone());
        }
        Node::WithStatement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.statement));
        }
        Node::SwitchStatement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.case_block));
        }
        Node::CaseBlock(node) => visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.clauses), arena),
        Node::CaseClause(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena)
        }
        Node::DefaultClause(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena)
        }
        Node::LabeledStatement(node) => {
            visit_node(&mut cb_node, Some(node.label));
            visit_node(&mut cb_node, Some(node.statement));
        }
        Node::ThrowStatement(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::TryStatement(node) => {
            visit_node(&mut cb_node, Some(node.try_block));
            visit_node(&mut cb_node, node.catch_clause.clone());
            visit_node(&mut cb_node, node.finally_block.clone());
        }
        Node::CatchClause(node) => {
            visit_node(&mut cb_node, node.variable_declaration.clone());
            visit_node(&mut cb_node, Some(node.block));
        }
        Node::Decorator(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::ClassDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members()), arena)
        }
        Node::ClassExpression(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members()), arena)
        }
        Node::InterfaceDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
                arena,
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members), arena)
        }
        Node::TypeAliasDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.type_.clone()))
        }
        Node::EnumDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members), arena)
        }
        Node::EnumMember(node) => {
            visit_node(&mut cb_node, Some(node.name));
            visit_node(&mut cb_node, node.initializer.clone());
        }
        Node::ModuleDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.body.clone());
        }
        Node::ImportEqualsDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, Some(node.module_reference));
        }
        Node::ImportDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.import_clause.clone());
            visit_node(&mut cb_node, Some(node.module_specifier));
            visit_node(&mut cb_node, node.assert_clause.clone());
        }
        Node::ImportClause(node) => {
            visit_node(&mut cb_node, node.name.clone());
            visit_node(&mut cb_node, node.named_bindings.clone());
        }
        Node::AssertClause(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::AssertEntry(node) => {
            visit_node(&mut cb_node, Some(node.name));
            visit_node(&mut cb_node, Some(node.value));
        }
        Node::NamespaceExportDeclaration(node) => {
            visit_node(&mut cb_node, node.maybe_name());
        }
        Node::NamespaceImport(node) => {
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::NamespaceExport(node) => {
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::NamedImports(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::NamedExports(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::ExportDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, node.export_clause.clone());
            visit_node(&mut cb_node, node.module_specifier.clone());
            visit_node(&mut cb_node, node.assert_clause.clone());
        }
        Node::ImportSpecifier(node) => {
            visit_node(&mut cb_node, node.property_name.clone());
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::ExportSpecifier(node) => {
            visit_node(&mut cb_node, node.property_name.clone());
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::ExportAssignment(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::TemplateExpression(node) => {
            visit_node(&mut cb_node, Some(node.head));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.template_spans), arena)
        }
        Node::TemplateSpan(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.literal))
        }
        Node::TemplateLiteralTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.head));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.template_spans), arena)
        }
        Node::TemplateLiteralTypeSpan(node) => {
            visit_node(&mut cb_node, Some(node.type_));
            visit_node(&mut cb_node, Some(node.literal))
        }
        Node::ComputedPropertyName(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::HeritageClause(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.types), arena)
        }
        Node::ExpressionWithTypeArguments(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )
        }
        Node::ExternalModuleReference(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::MissingDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            );
        }
        Node::CommaListExpression(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)
        }
        Node::JsxElement(node) => {
            visit_node(&mut cb_node, Some(node.opening_element));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.children), arena);
            visit_node(&mut cb_node, Some(node.closing_element));
        }
        Node::JsxFragment(node) => {
            visit_node(&mut cb_node, Some(node.opening_fragment));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.children), arena);
            visit_node(&mut cb_node, Some(node.closing_fragment));
        }
        Node::JsxSelfClosingElement(node) => {
            visit_node(&mut cb_node, Some(node.tag_name));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.attributes));
        }
        Node::JsxOpeningElement(node) => {
            visit_node(&mut cb_node, Some(node.tag_name));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            );
            visit_node(&mut cb_node, Some(node.attributes));
        }
        Node::JsxAttributes(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.properties), arena);
        }
        Node::JsxAttribute(node) => {
            visit_node(&mut cb_node, Some(node.name));
            visit_node(&mut cb_node, node.initializer.clone());
        }
        Node::JsxSpreadAttribute(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        Node::JsxExpression(node) => {
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, node.expression.clone());
        }
        Node::JsxClosingElement(node) => {
            visit_node(&mut cb_node, Some(node.tag_name));
        }
        Node::OptionalTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.type_));
        }
        Node::RestTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.type_));
        }
        Node::JSDocTypeExpression(node) => {
            visit_node(&mut cb_node, Some(node.type_));
        }
        Node::BaseJSDocUnaryType(node) => {
            visit_node(&mut cb_node, node.type_.clone());
        }
        Node::JSDocFunctionType(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena);
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::JSDoc(node) => {
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.comment.as_ref() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.tags, arena);
        }
        Node::JSDocSeeTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.name.clone());
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::JSDocNameReference(node) => {
            visit_node(&mut cb_node, Some(node.name));
        }
        Node::JSDocMemberName(node) => {
            visit_node(&mut cb_node, Some(node.left));
            visit_node(&mut cb_node, Some(node.right));
        }
        Node::JSDocPropertyLikeTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            if node.is_name_first {
                visit_node(&mut cb_node, Some(node.name));
                visit_node(&mut cb_node, node.type_expression.clone());
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
                }
            } else {
                visit_node(&mut cb_node, node.type_expression.clone());
                visit_node(&mut cb_node, Some(node.name));
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
                }
            }
        }
        Node::BaseJSDocTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::JSDocImplementsTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, Some(node.class));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::JSDocAugmentsTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, Some(node.class));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::JSDocTemplateTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.constraint.clone());
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.type_parameters), arena);
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::JSDocTypedefTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            if matches!(
                node.type_expression,
                Some(type_expression) if type_expression.ref_(arena).kind() == SyntaxKind::JSDocTypeExpression
            ) {
                visit_node(&mut cb_node, node.type_expression.clone());
                visit_node(&mut cb_node, node.full_name.clone());
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
                }
            } else {
                visit_node(&mut cb_node, node.full_name.clone());
                visit_node(&mut cb_node, node.type_expression.clone());
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
                }
            }
        }
        Node::JSDocCallbackTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.full_name.clone());
            visit_node(&mut cb_node, Some(node.type_expression));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::BaseJSDocTypeLikeTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.type_expression.clone());
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena);
            }
        }
        Node::JSDocSignature(node) => {
            node.maybe_type_parameters()
                .map(|type_parameters| {
                    for_each(&*type_parameters.ref_(arena), |&node, _| {
                        cb_node(node);
                        Option::<()>::None
                    })
                });
            for_each(&*node.parameters.ref_(arena), |&node, _| {
                cb_node(node);
                Option::<()>::None
            });
            visit_node(&mut cb_node, node.type_.clone());
        }
        Node::JSDocLink(node) => {
            visit_node(&mut cb_node, node.name.clone());
        }
        Node::JSDocLinkCode(node) => {
            visit_node(&mut cb_node, node.name.clone());
        }
        Node::JSDocLinkPlain(node) => {
            visit_node(&mut cb_node, node.name.clone());
        }
        Node::JSDocTypeLiteral(node) => {
            maybe_for_each(node.js_doc_property_tags.refed(arena).as_deref(), |&node, _| {
                cb_node(node);
                Option::<()>::None
            });
        }
        Node::PartiallyEmittedExpression(node) => {
            visit_node(&mut cb_node, Some(node.expression));
        }
        _ => (),
    }
}

pub fn try_for_each_child<TError>(
    node: Id<Node>,
    mut cb_node: impl FnMut(Id<Node>) -> Result<(), TError>,
    mut cb_nodes: Option<impl FnMut(Id<NodeArray>) -> Result<(), TError>>,
    arena: &impl HasArena,
) -> Result<(), TError> {
    if
    /* !node ||*/
    node.ref_(arena).kind() <= SyntaxKind::LastToken {
        return Ok(());
    }
    match &*node.ref_(arena) {
        Node::QualifiedName(node) => {
            try_visit_node(&mut cb_node, Some(node.left))?;
            try_visit_node(&mut cb_node, Some(node.right))?;
        }
        Node::TypeParameterDeclaration(node) => {
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.constraint.clone())?;
            try_visit_node(&mut cb_node, node.default.clone())?;
            try_visit_node(&mut cb_node, node.expression.clone())?;
        }
        Node::ShorthandPropertyAssignment(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, node.exclamation_token.clone())?;
            try_visit_node(&mut cb_node, node.equals_token.clone())?;
            try_visit_node(&mut cb_node, node.object_assignment_initializer.clone())?;
        }
        Node::SpreadAssignment(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::ParameterDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.dot_dot_dot_token.clone())?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_initializer())?
        }
        Node::PropertyDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, node.exclamation_token.clone())?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_initializer())?
        }
        Node::PropertySignature(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_initializer())?;
        }
        Node::PropertyAssignment(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, Some(node.initializer))?
        }
        Node::VariableDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.exclamation_token.clone())?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_initializer())?
        }
        Node::BindingElement(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.dot_dot_dot_token.clone())?;
            try_visit_node(&mut cb_node, node.property_name.clone())?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.maybe_initializer())?
        }
        Node::FunctionTypeNode(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::ConstructorTypeNode(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::CallSignatureDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::ConstructSignatureDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::IndexSignatureDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::MethodDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::MethodSignature(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::ConstructorDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::GetAccessorDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::SetAccessorDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::FunctionExpression(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::FunctionDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::ArrowFunction(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_asterisk_token())?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.maybe_question_token())?;
            try_visit_node(&mut cb_node, node.maybe_exclamation_token().clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
            try_visit_node(&mut cb_node, Some(node.equals_greater_than_token))?;
            try_visit_node(&mut cb_node, node.maybe_body())?
        }
        Node::ClassStaticBlockDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.body))?;
        }
        Node::TypeReferenceNode(node) => {
            try_visit_node(&mut cb_node, Some(node.type_name.clone()))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?
        }
        Node::TypePredicateNode(node) => {
            try_visit_node(&mut cb_node, node.asserts_modifier.clone())?;
            try_visit_node(&mut cb_node, Some(node.parameter_name))?;
            try_visit_node(&mut cb_node, node.type_.clone())?;
        }
        Node::TypeQueryNode(node) => try_visit_node(&mut cb_node, Some(node.expr_name))?,
        Node::TypeLiteralNode(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members), arena)?
        }
        Node::ArrayTypeNode(node) => try_visit_node(&mut cb_node, Some(node.element_type.clone()))?,
        Node::TupleTypeNode(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::UnionTypeNode(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.types), arena)?
        }
        Node::IntersectionTypeNode(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.types), arena)?
        }
        Node::ConditionalTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.check_type))?;
            try_visit_node(&mut cb_node, Some(node.extends_type))?;
            try_visit_node(&mut cb_node, Some(node.true_type))?;
            try_visit_node(&mut cb_node, Some(node.false_type))?;
        }
        Node::InferTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.type_parameter))?;
        }
        Node::ImportTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.argument))?;
            try_visit_node(&mut cb_node, node.qualifier.clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?
        }
        Node::ParenthesizedTypeNode(node) => try_visit_node(&mut cb_node, Some(node.type_))?,
        Node::TypeOperatorNode(node) => try_visit_node(&mut cb_node, Some(node.type_))?,
        Node::IndexedAccessTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.object_type))?;
            try_visit_node(&mut cb_node, Some(node.index_type))?;
        }
        Node::MappedTypeNode(node) => {
            try_visit_node(&mut cb_node, node.readonly_token.clone())?;
            try_visit_node(&mut cb_node, Some(node.type_parameter))?;
            try_visit_node(&mut cb_node, node.name_type.clone())?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, node.type_.clone())?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.members, arena)?
        }
        Node::LiteralTypeNode(node) => try_visit_node(&mut cb_node, Some(node.literal))?,
        Node::NamedTupleMember(node) => {
            try_visit_node(&mut cb_node, node.dot_dot_dot_token.clone())?;
            try_visit_node(&mut cb_node, Some(node.name))?;
            try_visit_node(&mut cb_node, node.question_token.clone())?;
            try_visit_node(&mut cb_node, Some(node.type_))?;
        }
        Node::ObjectBindingPattern(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::ArrayBindingPattern(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::ArrayLiteralExpression(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::ObjectLiteralExpression(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.properties), arena)?
        }
        Node::PropertyAccessExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, node.question_dot_token.clone())?;
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::ElementAccessExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, node.question_dot_token.clone())?;
            try_visit_node(&mut cb_node, Some(node.argument_expression))?;
        }
        Node::CallExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, node.question_dot_token.clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.arguments), arena)?;
        }
        Node::NewExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.arguments, arena)?;
        }
        Node::TaggedTemplateExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.tag))?;
            try_visit_node(&mut cb_node, node.question_dot_token.clone())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.template))?;
        }
        Node::TypeAssertion(node) => {
            try_visit_node(&mut cb_node, Some(node.type_))?;
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::ParenthesizedExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::DeleteExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::TypeOfExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::VoidExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::PrefixUnaryExpression(node) => try_visit_node(&mut cb_node, Some(node.operand))?,
        Node::YieldExpression(node) => {
            try_visit_node(&mut cb_node, node.asterisk_token.clone())?;
            try_visit_node(&mut cb_node, node.expression.clone())?;
        }
        Node::AwaitExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::PostfixUnaryExpression(node) => try_visit_node(&mut cb_node, Some(node.operand))?,
        Node::BinaryExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.left))?;
            try_visit_node(&mut cb_node, Some(node.operator_token))?;
            try_visit_node(&mut cb_node, Some(node.right))?;
        }
        Node::AsExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.type_))?;
        }
        Node::NonNullExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::MetaProperty(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::ConditionalExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.condition))?;
            try_visit_node(&mut cb_node, Some(node.question_token))?;
            try_visit_node(&mut cb_node, Some(node.when_true))?;
            try_visit_node(&mut cb_node, Some(node.colon_token))?;
            try_visit_node(&mut cb_node, Some(node.when_false))?;
        }
        Node::SpreadElement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::Block(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena)?;
        }
        Node::ModuleBlock(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena)?;
        }
        Node::SourceFile(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements()), arena)?;
            try_visit_node(&mut cb_node, Some(node.end_of_file_token()))?;
        }
        Node::VariableStatement(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.declaration_list.clone()))?
        }
        Node::VariableDeclarationList(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.declarations), arena)?
        }
        Node::ExpressionStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::IfStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.then_statement))?;
            try_visit_node(&mut cb_node, node.else_statement.clone())?;
        }
        Node::DoStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.statement))?;
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::WhileStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.statement))?;
        }
        Node::ForStatement(node) => {
            try_visit_node(&mut cb_node, node.initializer.clone())?;
            try_visit_node(&mut cb_node, node.condition.clone())?;
            try_visit_node(&mut cb_node, node.incrementor.clone())?;
            try_visit_node(&mut cb_node, Some(node.statement))?;
        }
        Node::ForInStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.initializer))?;
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.statement))?;
        }
        Node::ForOfStatement(node) => {
            try_visit_node(&mut cb_node, node.await_modifier.clone())?;
            try_visit_node(&mut cb_node, Some(node.initializer))?;
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.statement))?;
        }
        Node::BreakStatement(node) => {
            try_visit_node(&mut cb_node, node.label.clone())?;
        }
        Node::ContinueStatement(node) => {
            try_visit_node(&mut cb_node, node.label.clone())?;
        }
        Node::ReturnStatement(node) => {
            try_visit_node(&mut cb_node, node.expression.clone())?;
        }
        Node::WithStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.statement))?;
        }
        Node::SwitchStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.case_block))?;
        }
        Node::CaseBlock(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.clauses), arena)?
        }
        Node::CaseClause(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena)?
        }
        Node::DefaultClause(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements), arena)?
        }
        Node::LabeledStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.label))?;
            try_visit_node(&mut cb_node, Some(node.statement))?;
        }
        Node::ThrowStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::TryStatement(node) => {
            try_visit_node(&mut cb_node, Some(node.try_block))?;
            try_visit_node(&mut cb_node, node.catch_clause.clone())?;
            try_visit_node(&mut cb_node, node.finally_block.clone())?;
        }
        Node::CatchClause(node) => {
            try_visit_node(&mut cb_node, node.variable_declaration.clone())?;
            try_visit_node(&mut cb_node, Some(node.block))?;
        }
        Node::Decorator(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::ClassDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members()), arena)?
        }
        Node::ClassExpression(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members()), arena)?
        }
        Node::InterfaceDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
                arena,
            )?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members), arena)?
        }
        Node::TypeAliasDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.type_.clone()))?
        }
        Node::EnumDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.name()))?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.members), arena)?
        }
        Node::EnumMember(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
            try_visit_node(&mut cb_node, node.initializer.clone())?;
        }
        Node::ModuleDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, node.body.clone())?;
        }
        Node::ImportEqualsDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.maybe_name())?;
            try_visit_node(&mut cb_node, Some(node.module_reference))?;
        }
        Node::ImportDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.import_clause.clone())?;
            try_visit_node(&mut cb_node, Some(node.module_specifier))?;
            try_visit_node(&mut cb_node, node.assert_clause.clone())?;
        }
        Node::ImportClause(node) => {
            try_visit_node(&mut cb_node, node.name.clone())?;
            try_visit_node(&mut cb_node, node.named_bindings.clone())?;
        }
        Node::AssertClause(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::AssertEntry(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
            try_visit_node(&mut cb_node, Some(node.value))?;
        }
        Node::NamespaceExportDeclaration(node) => {
            try_visit_node(&mut cb_node, node.maybe_name())?;
        }
        Node::NamespaceImport(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::NamespaceExport(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::NamedImports(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::NamedExports(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::ExportDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, node.export_clause.clone())?;
            try_visit_node(&mut cb_node, node.module_specifier.clone())?;
            try_visit_node(&mut cb_node, node.assert_clause.clone())?;
        }
        Node::ImportSpecifier(node) => {
            try_visit_node(&mut cb_node, node.property_name.clone())?;
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::ExportSpecifier(node) => {
            try_visit_node(&mut cb_node, node.property_name.clone())?;
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::ExportAssignment(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::TemplateExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.head))?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.template_spans), arena)?
        }
        Node::TemplateSpan(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_node(&mut cb_node, Some(node.literal))?
        }
        Node::TemplateLiteralTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.head))?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.template_spans), arena)?
        }
        Node::TemplateLiteralTypeSpan(node) => {
            try_visit_node(&mut cb_node, Some(node.type_))?;
            try_visit_node(&mut cb_node, Some(node.literal))?
        }
        Node::ComputedPropertyName(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::HeritageClause(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.types), arena)?
        }
        Node::ExpressionWithTypeArguments(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?
        }
        Node::ExternalModuleReference(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::MissingDeclaration(node) => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators(),
                arena,
            )?;
        }
        Node::CommaListExpression(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.elements), arena)?
        }
        Node::JsxElement(node) => {
            try_visit_node(&mut cb_node, Some(node.opening_element))?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.children), arena)?;
            try_visit_node(&mut cb_node, Some(node.closing_element))?;
        }
        Node::JsxFragment(node) => {
            try_visit_node(&mut cb_node, Some(node.opening_fragment))?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.children), arena)?;
            try_visit_node(&mut cb_node, Some(node.closing_fragment))?;
        }
        Node::JsxSelfClosingElement(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.attributes))?;
        }
        Node::JsxOpeningElement(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name))?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments(),
                arena,
            )?;
            try_visit_node(&mut cb_node, Some(node.attributes))?;
        }
        Node::JsxAttributes(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.properties), arena)?;
        }
        Node::JsxAttribute(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
            try_visit_node(&mut cb_node, node.initializer.clone())?;
        }
        Node::JsxSpreadAttribute(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        Node::JsxExpression(node) => {
            try_visit_node(&mut cb_node, node.dot_dot_dot_token.clone())?;
            try_visit_node(&mut cb_node, node.expression.clone())?;
        }
        Node::JsxClosingElement(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name))?;
        }
        Node::OptionalTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.type_))?;
        }
        Node::RestTypeNode(node) => {
            try_visit_node(&mut cb_node, Some(node.type_))?;
        }
        Node::JSDocTypeExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.type_))?;
        }
        Node::BaseJSDocUnaryType(node) => {
            try_visit_node(&mut cb_node, node.type_.clone())?;
        }
        Node::JSDocFunctionType(node) => {
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()), arena)?;
            try_visit_node(&mut cb_node, node.maybe_type())?;
        }
        Node::JSDoc(node) => {
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.comment.as_ref() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.tags, arena)?;
        }
        Node::JSDocSeeTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            try_visit_node(&mut cb_node, node.name.clone())?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::JSDocNameReference(node) => {
            try_visit_node(&mut cb_node, Some(node.name))?;
        }
        Node::JSDocMemberName(node) => {
            try_visit_node(&mut cb_node, Some(node.left))?;
            try_visit_node(&mut cb_node, Some(node.right))?;
        }
        Node::JSDocPropertyLikeTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            if node.is_name_first {
                try_visit_node(&mut cb_node, Some(node.name))?;
                try_visit_node(&mut cb_node, node.type_expression.clone())?;
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            } else {
                try_visit_node(&mut cb_node, node.type_expression.clone())?;
                try_visit_node(&mut cb_node, Some(node.name))?;
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            }
        }
        Node::BaseJSDocTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::JSDocImplementsTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            try_visit_node(&mut cb_node, Some(node.class))?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::JSDocAugmentsTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            try_visit_node(&mut cb_node, Some(node.class))?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::JSDocTemplateTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            try_visit_node(&mut cb_node, node.constraint.clone())?;
            try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.type_parameters), arena)?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::JSDocTypedefTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            if matches!(
                node.type_expression,
                Some(type_expression) if type_expression.ref_(arena).kind() == SyntaxKind::JSDocTypeExpression
            ) {
                try_visit_node(&mut cb_node, node.type_expression.clone())?;
                try_visit_node(&mut cb_node, node.full_name.clone())?;
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            } else {
                try_visit_node(&mut cb_node, node.full_name.clone())?;
                try_visit_node(&mut cb_node, node.type_expression.clone())?;
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            }
        }
        Node::JSDocCallbackTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            try_visit_node(&mut cb_node, node.full_name.clone())?;
            try_visit_node(&mut cb_node, Some(node.type_expression))?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::BaseJSDocTypeLikeTag(node) => {
            try_visit_node(&mut cb_node, Some(node.tag_name()))?;
            try_visit_node(&mut cb_node, node.type_expression.clone())?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        Node::JSDocSignature(node) => {
            node.maybe_type_parameters()
                .as_ref()
                .try_map(|type_parameters| {
                    try_for_each(&*type_parameters.ref_(arena), |&node: &Id<Node>, _| -> Result<_, TError> {
                        cb_node(node)?;
                        Ok(Option::<()>::None)
                    })
                })?;
            try_for_each(
                &*node.parameters.ref_(arena),
                |&node: &Id<Node>, _| -> Result<_, TError> {
                    cb_node(node)?;
                    Ok(Option::<()>::None)
                },
            )?;
            try_visit_node(&mut cb_node, node.type_.clone())?;
        }
        Node::JSDocLink(node) => {
            try_visit_node(&mut cb_node, node.name.clone())?;
        }
        Node::JSDocLinkCode(node) => {
            try_visit_node(&mut cb_node, node.name.clone())?;
        }
        Node::JSDocLinkPlain(node) => {
            try_visit_node(&mut cb_node, node.name.clone())?;
        }
        Node::JSDocTypeLiteral(node) => {
            try_maybe_for_each(
                node.js_doc_property_tags,
                |&node: &Id<Node>, _| -> Result<_, TError> {
                    cb_node(node)?;
                    Ok(Option::<()>::None)
                },
            )?;
        }
        Node::PartiallyEmittedExpression(node) => {
            try_visit_node(&mut cb_node, Some(node.expression))?;
        }
        _ => (),
    }

    Ok(())
}
