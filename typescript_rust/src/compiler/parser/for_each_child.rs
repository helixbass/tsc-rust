use id_arena::Id;

use super::{try_visit_node, try_visit_nodes, visit_node, visit_nodes};
use crate::{
    for_each, maybe_for_each, try_for_each, try_maybe_for_each, ClassLikeDeclarationInterface,
    FunctionLikeDeclarationInterface, HasArena, HasInitializerInterface, HasQuestionTokenInterface,
    HasStatementsInterface, HasTypeArgumentsInterface, HasTypeInterface,
    HasTypeParametersInterface, InArena, InterfaceOrClassLikeDeclarationInterface,
    JSDocTagInterface, NamedDeclarationInterface, Node, NodeArray, NodeInterface, OptionInArena,
    OptionTry, SignatureDeclarationInterface, StringOrNodeArray, SyntaxKind,
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_node(&mut cb_node, node.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters(),
                arena,
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
        Node::ObjectLiteralExpression(node) => visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.properties),
            arena,
        ),
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.statements),
                arena,
            );
        }
        Node::ModuleBlock(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.statements),
                arena,
            );
        }
        Node::SourceFile(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.statements()),
                arena,
            );
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
        Node::VariableDeclarationList(node) => visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.declarations),
            arena,
        ),
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
        Node::CaseBlock(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.clauses), arena)
        }
        Node::CaseClause(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.statements),
                arena,
            )
        }
        Node::DefaultClause(node) => visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.statements),
            arena,
        ),
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.template_spans),
                arena,
            )
        }
        Node::TemplateSpan(node) => {
            visit_node(&mut cb_node, Some(node.expression));
            visit_node(&mut cb_node, Some(node.literal))
        }
        Node::TemplateLiteralTypeNode(node) => {
            visit_node(&mut cb_node, Some(node.head));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.template_spans),
                arena,
            )
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.properties),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.parameters()),
                arena,
            );
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
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.type_parameters),
                arena,
            );
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
            node.maybe_type_parameters().map(|type_parameters| {
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
            maybe_for_each(
                node.js_doc_property_tags.refed(arena).as_deref(),
                |&node, _| {
                    cb_node(node);
                    Option::<()>::None
                },
            );
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
    match node.ref_(arena).kind() {
        SyntaxKind::QualifiedName => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_qualified_name().left),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_qualified_name().right),
            )?;
        }
        SyntaxKind::TypeParameterDeclaration => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_parameter_declaration().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_type_parameter_declaration()
                    .constraint
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_type_parameter_declaration()
                    .default
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_type_parameter_declaration()
                    .expression
                    .clone(),
            )?;
        }
        SyntaxKind::ShorthandPropertyAssignment => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_shorthand_property_assignment()
                    .maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_shorthand_property_assignment()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_shorthand_property_assignment()
                    .exclamation_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_shorthand_property_assignment()
                    .equals_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_shorthand_property_assignment()
                    .object_assignment_initializer
                    .clone(),
            )?;
        }
        SyntaxKind::SpreadAssignment => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_spread_assignment().expression),
            )?;
        }
        SyntaxKind::ParameterDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_parameter_declaration()
                    .dot_dot_dot_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_parameter_declaration().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_parameter_declaration()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_parameter_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_parameter_declaration()
                    .maybe_initializer(),
            )?
        }
        SyntaxKind::PropertyDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_property_declaration().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_property_declaration()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_property_declaration()
                    .exclamation_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_property_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_property_declaration()
                    .maybe_initializer(),
            )?
        }
        SyntaxKind::PropertySignature => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_property_signature().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_property_signature()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_property_signature().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_property_signature().maybe_initializer(),
            )?;
        }
        SyntaxKind::PropertyAssignment => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_property_assignment().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_property_assignment()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_property_assignment().initializer),
            )?
        }
        SyntaxKind::VariableDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_variable_declaration().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_variable_declaration()
                    .exclamation_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_variable_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_variable_declaration()
                    .maybe_initializer(),
            )?
        }
        SyntaxKind::BindingElement => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_binding_element()
                    .dot_dot_dot_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_binding_element().property_name.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_binding_element().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_binding_element().maybe_initializer(),
            )?
        }
        SyntaxKind::FunctionTypeNode => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_function_type_node()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_function_type_node().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_type_node().maybe_type(),
            )?;
        }
        SyntaxKind::ConstructorTypeNode => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_constructor_type_node()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_constructor_type_node().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_constructor_type_node().maybe_type(),
            )?;
        }
        SyntaxKind::CallSignatureDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_call_signature_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(
                    node.ref_(arena)
                        .as_call_signature_declaration()
                        .parameters(),
                ),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_call_signature_declaration()
                    .maybe_type(),
            )?;
        }
        SyntaxKind::ConstructSignatureDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_construct_signature_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(
                    node.ref_(arena)
                        .as_construct_signature_declaration()
                        .parameters(),
                ),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_construct_signature_declaration()
                    .maybe_type(),
            )?;
        }
        SyntaxKind::IndexSignatureDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_index_signature_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(
                    node.ref_(arena)
                        .as_index_signature_declaration()
                        .parameters(),
                ),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_index_signature_declaration()
                    .maybe_type(),
            )?;
        }
        SyntaxKind::MethodDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_method_declaration()
                    .maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_method_declaration().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_method_declaration()
                    .maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_method_declaration()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_method_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_method_declaration().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_method_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_method_declaration().maybe_body(),
            )?
        }
        SyntaxKind::MethodSignature => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_method_signature().name()),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_method_signature()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_method_signature().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_method_signature().maybe_type(),
            )?;
        }
        SyntaxKind::ConstructorDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_constructor_declaration()
                    .maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_constructor_declaration().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_constructor_declaration()
                    .maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_constructor_declaration()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_constructor_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_constructor_declaration().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_constructor_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_constructor_declaration().maybe_body(),
            )?
        }
        SyntaxKind::GetAccessorDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_get_accessor_declaration()
                    .maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_get_accessor_declaration().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_get_accessor_declaration()
                    .maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_get_accessor_declaration()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_get_accessor_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_get_accessor_declaration().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_get_accessor_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_get_accessor_declaration().maybe_body(),
            )?
        }
        SyntaxKind::SetAccessorDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_set_accessor_declaration()
                    .maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_set_accessor_declaration().name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_set_accessor_declaration()
                    .maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_set_accessor_declaration()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_set_accessor_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_set_accessor_declaration().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_set_accessor_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_set_accessor_declaration().maybe_body(),
            )?
        }
        SyntaxKind::FunctionExpression => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_function_expression()
                    .maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_expression().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_function_expression()
                    .maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_function_expression()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_function_expression()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_function_expression().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_expression().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_expression().maybe_body(),
            )?
        }
        SyntaxKind::FunctionDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_function_declaration()
                    .maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_declaration().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_function_declaration()
                    .maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_function_declaration()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_function_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_function_declaration().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_declaration().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_function_declaration().maybe_body(),
            )?
        }
        SyntaxKind::ArrowFunction => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_arrow_function().maybe_asterisk_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_arrow_function().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_arrow_function().maybe_question_token(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_arrow_function()
                    .maybe_exclamation_token(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).as_arrow_function().maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_arrow_function().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_arrow_function().maybe_type(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(
                    node.ref_(arena)
                        .as_arrow_function()
                        .equals_greater_than_token,
                ),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_arrow_function().maybe_body(),
            )?
        }
        SyntaxKind::ClassStaticBlockDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_class_static_block_declaration().body),
            )?;
        }
        SyntaxKind::TypeReferenceNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_reference_node().type_name.clone()),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_type_reference_node()
                    .maybe_type_arguments(),
                arena,
            )?
        }
        SyntaxKind::TypePredicateNode => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_type_predicate_node()
                    .asserts_modifier
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_predicate_node().parameter_name),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_type_predicate_node().type_.clone(),
            )?;
        }
        SyntaxKind::TypeQueryNode => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_type_query_node().expr_name),
        )?,
        SyntaxKind::TypeLiteralNode => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_type_literal_node().members),
            arena,
        )?,
        SyntaxKind::ArrayTypeNode => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_array_type_node().element_type.clone()),
        )?,
        SyntaxKind::TupleTypeNode => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_tuple_type_node().elements),
            arena,
        )?,
        SyntaxKind::UnionTypeNode => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_union_type_node().types),
            arena,
        )?,
        SyntaxKind::IntersectionTypeNode => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_intersection_type_node().types),
            arena,
        )?,
        SyntaxKind::ConditionalTypeNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_type_node().check_type),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_type_node().extends_type),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_type_node().true_type),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_type_node().false_type),
            )?;
        }
        SyntaxKind::InferTypeNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_infer_type_node().type_parameter),
            )?;
        }
        SyntaxKind::ImportTypeNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_import_type_node().argument),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_import_type_node().qualifier.clone(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_import_type_node()
                    .maybe_type_arguments(),
                arena,
            )?
        }
        SyntaxKind::ParenthesizedTypeNode => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_parenthesized_type_node().type_),
        )?,
        SyntaxKind::TypeOperatorNode => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_type_operator_node().type_),
        )?,
        SyntaxKind::IndexedAccessTypeNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_indexed_access_type_node().object_type),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_indexed_access_type_node().index_type),
            )?;
        }
        SyntaxKind::MappedTypeNode => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_mapped_type_node()
                    .readonly_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_mapped_type_node().type_parameter),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_mapped_type_node().name_type.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_mapped_type_node()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_mapped_type_node().type_.clone(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).as_mapped_type_node().members,
                arena,
            )?
        }
        SyntaxKind::LiteralTypeNode => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_literal_type_node().literal),
        )?,
        SyntaxKind::NamedTupleMember => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_named_tuple_member()
                    .dot_dot_dot_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_named_tuple_member().name),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_named_tuple_member()
                    .question_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_named_tuple_member().type_),
            )?;
        }
        SyntaxKind::ObjectBindingPattern => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_object_binding_pattern().elements),
            arena,
        )?,
        SyntaxKind::ArrayBindingPattern => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_array_binding_pattern().elements),
            arena,
        )?,
        SyntaxKind::ArrayLiteralExpression => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_array_literal_expression().elements),
            arena,
        )?,
        SyntaxKind::ObjectLiteralExpression => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_object_literal_expression().properties),
            arena,
        )?,
        SyntaxKind::PropertyAccessExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_property_access_expression().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_property_access_expression()
                    .question_dot_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_property_access_expression().name),
            )?;
        }
        SyntaxKind::ElementAccessExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_element_access_expression().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_element_access_expression()
                    .question_dot_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(
                    node.ref_(arena)
                        .as_element_access_expression()
                        .argument_expression,
                ),
            )?;
        }
        SyntaxKind::CallExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_call_expression().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_call_expression()
                    .question_dot_token
                    .clone(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).as_call_expression().maybe_type_arguments(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_call_expression().arguments),
                arena,
            )?;
        }
        SyntaxKind::NewExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_new_expression().expression),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).as_new_expression().maybe_type_arguments(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).as_new_expression().arguments,
                arena,
            )?;
        }
        SyntaxKind::TaggedTemplateExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_tagged_template_expression().tag),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_tagged_template_expression()
                    .question_dot_token
                    .clone(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_tagged_template_expression()
                    .maybe_type_arguments(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_tagged_template_expression().template),
            )?;
        }
        SyntaxKind::TypeAssertion => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_assertion().type_),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_assertion().expression),
            )?;
        }
        SyntaxKind::ParenthesizedExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_parenthesized_expression().expression),
            )?;
        }
        SyntaxKind::DeleteExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_delete_expression().expression),
            )?;
        }
        SyntaxKind::TypeOfExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_of_expression().expression),
            )?;
        }
        SyntaxKind::VoidExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_void_expression().expression),
            )?;
        }
        SyntaxKind::PrefixUnaryExpression => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_prefix_unary_expression().operand),
        )?,
        SyntaxKind::YieldExpression => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_yield_expression()
                    .asterisk_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_yield_expression().expression.clone(),
            )?;
        }
        SyntaxKind::AwaitExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_await_expression().expression),
            )?;
        }
        SyntaxKind::PostfixUnaryExpression => try_visit_node(
            &mut cb_node,
            Some(node.ref_(arena).as_postfix_unary_expression().operand),
        )?,
        SyntaxKind::BinaryExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_binary_expression().left),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_binary_expression().operator_token),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_binary_expression().right),
            )?;
        }
        SyntaxKind::AsExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_as_expression().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_as_expression().type_),
            )?;
        }
        SyntaxKind::NonNullExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_non_null_expression().expression),
            )?;
        }
        SyntaxKind::MetaProperty => {
            try_visit_node(&mut cb_node, Some(node.ref_(arena).as_meta_property().name))?;
        }
        SyntaxKind::ConditionalExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_expression().condition),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_expression().question_token),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_expression().when_true),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_expression().colon_token),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_conditional_expression().when_false),
            )?;
        }
        SyntaxKind::SpreadElement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_spread_element().expression),
            )?;
        }
        SyntaxKind::Block => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_block().statements),
                arena,
            )?;
        }
        SyntaxKind::ModuleBlock => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_module_block().statements),
                arena,
            )?;
        }
        SyntaxKind::SourceFile => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_source_file().statements()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_source_file().end_of_file_token()),
            )?;
        }
        SyntaxKind::VariableStatement => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(
                    node.ref_(arena)
                        .as_variable_statement()
                        .declaration_list
                        .clone(),
                ),
            )?
        }
        SyntaxKind::VariableDeclarationList => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_variable_declaration_list().declarations),
            arena,
        )?,
        SyntaxKind::ExpressionStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_expression_statement().expression),
            )?;
        }
        SyntaxKind::IfStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_if_statement().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_if_statement().then_statement),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_if_statement().else_statement.clone(),
            )?;
        }
        SyntaxKind::DoStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_do_statement().statement),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_do_statement().expression),
            )?;
        }
        SyntaxKind::WhileStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_while_statement().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_while_statement().statement),
            )?;
        }
        SyntaxKind::ForStatement => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_for_statement().initializer.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_for_statement().condition.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_for_statement().incrementor.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_statement().statement),
            )?;
        }
        SyntaxKind::ForInStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_in_statement().initializer),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_in_statement().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_in_statement().statement),
            )?;
        }
        SyntaxKind::ForOfStatement => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_for_of_statement()
                    .await_modifier
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_of_statement().initializer),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_of_statement().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_for_of_statement().statement),
            )?;
        }
        SyntaxKind::BreakStatement => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_break_statement().label.clone(),
            )?;
        }
        SyntaxKind::ContinueStatement => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_continue_statement().label.clone(),
            )?;
        }
        SyntaxKind::ReturnStatement => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_return_statement().expression.clone(),
            )?;
        }
        SyntaxKind::WithStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_with_statement().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_with_statement().statement),
            )?;
        }
        SyntaxKind::SwitchStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_switch_statement().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_switch_statement().case_block),
            )?;
        }
        SyntaxKind::CaseBlock => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_case_block().clauses),
            arena,
        )?,
        SyntaxKind::CaseClause => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_case_clause().expression),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_case_clause().statements),
                arena,
            )?
        }
        SyntaxKind::DefaultClause => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_default_clause().statements),
            arena,
        )?,
        SyntaxKind::LabeledStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_labeled_statement().label),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_labeled_statement().statement),
            )?;
        }
        SyntaxKind::ThrowStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_throw_statement().expression),
            )?;
        }
        SyntaxKind::TryStatement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_try_statement().try_block),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_try_statement().catch_clause.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_try_statement().finally_block.clone(),
            )?;
        }
        SyntaxKind::CatchClause => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_catch_clause()
                    .variable_declaration
                    .clone(),
            )?;
            try_visit_node(&mut cb_node, Some(node.ref_(arena).as_catch_clause().block))?;
        }
        SyntaxKind::Decorator => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_decorator().expression),
            )?;
        }
        SyntaxKind::ClassDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_class_declaration().maybe_name(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_class_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_class_declaration()
                    .maybe_heritage_clauses(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_class_declaration().members()),
                arena,
            )?
        }
        SyntaxKind::ClassExpression => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_class_expression().maybe_name(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_class_expression()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_class_expression()
                    .maybe_heritage_clauses(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_class_expression().members()),
                arena,
            )?
        }
        SyntaxKind::InterfaceDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_interface_declaration().name()),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_interface_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_interface_declaration()
                    .maybe_heritage_clauses(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_interface_declaration().members),
                arena,
            )?
        }
        SyntaxKind::TypeAliasDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_alias_declaration().name()),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_type_alias_declaration()
                    .maybe_type_parameters(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_type_alias_declaration().type_.clone()),
            )?
        }
        SyntaxKind::EnumDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_enum_declaration().name()),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_enum_declaration().members),
                arena,
            )?
        }
        SyntaxKind::EnumMember => {
            try_visit_node(&mut cb_node, Some(node.ref_(arena).as_enum_member().name))?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_enum_member().initializer.clone(),
            )?;
        }
        SyntaxKind::ModuleDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_module_declaration().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_module_declaration().body.clone(),
            )?;
        }
        SyntaxKind::ImportEqualsDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_import_equals_declaration().maybe_name(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(
                    node.ref_(arena)
                        .as_import_equals_declaration()
                        .module_reference,
                ),
            )?;
        }
        SyntaxKind::ImportDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_import_declaration()
                    .import_clause
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_import_declaration().module_specifier),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_import_declaration()
                    .assert_clause
                    .clone(),
            )?;
        }
        SyntaxKind::ImportClause => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_import_clause().name.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_import_clause().named_bindings.clone(),
            )?;
        }
        SyntaxKind::AssertClause => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_assert_clause().elements),
            arena,
        )?,
        SyntaxKind::AssertEntry => {
            try_visit_node(&mut cb_node, Some(node.ref_(arena).as_assert_entry().name))?;
            try_visit_node(&mut cb_node, Some(node.ref_(arena).as_assert_entry().value))?;
        }
        SyntaxKind::NamespaceExportDeclaration => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_namespace_export_declaration()
                    .maybe_name(),
            )?;
        }
        SyntaxKind::NamespaceImport => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_namespace_import().name),
            )?;
        }
        SyntaxKind::NamespaceExport => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_namespace_export().name),
            )?;
        }
        SyntaxKind::NamedImports => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_named_imports().elements),
            arena,
        )?,
        SyntaxKind::NamedExports => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_named_exports().elements),
            arena,
        )?,
        SyntaxKind::ExportDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_export_declaration()
                    .export_clause
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_export_declaration()
                    .module_specifier
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_export_declaration()
                    .assert_clause
                    .clone(),
            )?;
        }
        SyntaxKind::ImportSpecifier => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_import_specifier().property_name.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_import_specifier().name),
            )?;
        }
        SyntaxKind::ExportSpecifier => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_export_specifier().property_name.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_export_specifier().name),
            )?;
        }
        SyntaxKind::ExportAssignment => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_modifiers(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_export_assignment().expression),
            )?;
        }
        SyntaxKind::TemplateExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_template_expression().head),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_template_expression().template_spans),
                arena,
            )?
        }
        SyntaxKind::TemplateSpan => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_template_span().expression),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_template_span().literal),
            )?
        }
        SyntaxKind::TemplateLiteralType => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_template_literal_type_node().head),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(
                    node.ref_(arena)
                        .as_template_literal_type_node()
                        .template_spans,
                ),
                arena,
            )?
        }
        SyntaxKind::TemplateLiteralTypeSpan => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_template_literal_type_span().type_),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_template_literal_type_span().literal),
            )?
        }
        SyntaxKind::ComputedPropertyName => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_computed_property_name().expression),
            )?;
        }
        SyntaxKind::HeritageClause => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_heritage_clause().types),
            arena,
        )?,
        SyntaxKind::ExpressionWithTypeArguments => {
            try_visit_node(
                &mut cb_node,
                Some(
                    node.ref_(arena)
                        .as_expression_with_type_arguments()
                        .expression,
                ),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_expression_with_type_arguments()
                    .maybe_type_arguments(),
                arena,
            )?
        }
        SyntaxKind::ExternalModuleReference => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_external_module_reference().expression),
            )?;
        }
        SyntaxKind::MissingDeclaration => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).maybe_decorators(),
                arena,
            )?;
        }
        SyntaxKind::CommaListExpression => try_visit_nodes(
            &mut cb_node,
            cb_nodes.as_mut(),
            Some(node.ref_(arena).as_comma_list_expression().elements),
            arena,
        )?,
        SyntaxKind::JsxElement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_element().opening_element),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_jsx_element().children),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_element().closing_element),
            )?;
        }
        SyntaxKind::JsxFragment => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_fragment().opening_fragment),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_jsx_fragment().children),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_fragment().closing_fragment),
            )?;
        }
        SyntaxKind::JsxSelfClosingElement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_self_closing_element().tag_name),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_jsx_self_closing_element()
                    .maybe_type_arguments(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_self_closing_element().attributes),
            )?;
        }
        SyntaxKind::JsxOpeningElement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_opening_element().tag_name),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena)
                    .as_jsx_opening_element()
                    .maybe_type_arguments(),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_opening_element().attributes),
            )?;
        }
        SyntaxKind::JsxAttributes => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_jsx_attributes().properties),
                arena,
            )?;
        }
        SyntaxKind::JsxAttribute => {
            try_visit_node(&mut cb_node, Some(node.ref_(arena).as_jsx_attribute().name))?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsx_attribute().initializer.clone(),
            )?;
        }
        SyntaxKind::JsxSpreadAttribute => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_spread_attribute().expression),
            )?;
        }
        SyntaxKind::JsxExpression => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_jsx_expression()
                    .dot_dot_dot_token
                    .clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsx_expression().expression.clone(),
            )?;
        }
        SyntaxKind::JsxClosingElement => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsx_closing_element().tag_name),
            )?;
        }
        SyntaxKind::OptionalTypeNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_optional_type_node().type_),
            )?;
        }
        SyntaxKind::RestTypeNode => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_rest_type_node().type_),
            )?;
        }
        SyntaxKind::JSDocTypeExpression => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_type_expression().type_),
            )?;
        }
        SyntaxKind::BaseJSDocUnaryType => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_base_jsdoc_unary_type().type_.clone(),
            )?;
        }
        SyntaxKind::JSDocFunctionType => {
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_jsdoc_function_type().parameters()),
                arena,
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_function_type().maybe_type(),
            )?;
        }
        SyntaxKind::JSDoc => {
            if let Some(StringOrNodeArray::NodeArray(comment)) =
                node.ref_(arena).as_jsdoc().comment.as_ref()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.ref_(arena).as_jsdoc().tags,
                arena,
            )?;
        }
        SyntaxKind::JSDocSeeTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_see_tag().tag_name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_see_tag().name.clone(),
            )?;
            if let Some(StringOrNodeArray::NodeArray(comment)) =
                node.ref_(arena).as_jsdoc_see_tag().maybe_comment()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::JSDocNameReference => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_name_reference().name),
            )?;
        }
        SyntaxKind::JSDocMemberName => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_member_name().left),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_member_name().right),
            )?;
        }
        SyntaxKind::JSDocPropertyLikeTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_property_like_tag().tag_name()),
            )?;
            if node.ref_(arena).as_jsdoc_property_like_tag().is_name_first {
                try_visit_node(
                    &mut cb_node,
                    Some(node.ref_(arena).as_jsdoc_property_like_tag().name),
                )?;
                try_visit_node(
                    &mut cb_node,
                    node.ref_(arena)
                        .as_jsdoc_property_like_tag()
                        .type_expression
                        .clone(),
                )?;
                if let Some(StringOrNodeArray::NodeArray(comment)) = node
                    .ref_(arena)
                    .as_jsdoc_property_like_tag()
                    .maybe_comment()
                {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            } else {
                try_visit_node(
                    &mut cb_node,
                    node.ref_(arena)
                        .as_jsdoc_property_like_tag()
                        .type_expression
                        .clone(),
                )?;
                try_visit_node(
                    &mut cb_node,
                    Some(node.ref_(arena).as_jsdoc_property_like_tag().name),
                )?;
                if let Some(StringOrNodeArray::NodeArray(comment)) = node
                    .ref_(arena)
                    .as_jsdoc_property_like_tag()
                    .maybe_comment()
                {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            }
        }
        SyntaxKind::BaseJSDocTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_base_jsdoc_tag().tag_name()),
            )?;
            if let Some(StringOrNodeArray::NodeArray(comment)) =
                node.ref_(arena).as_base_jsdoc_tag().maybe_comment()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::JSDocImplementsTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_implements_tag().tag_name()),
            )?;
            try_visit_node(&mut cb_node, Some(node.ref_(arena).class))?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.ref_(arena).maybe_comment() {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::JSDocAugmentsTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_augments_tag().tag_name()),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_augments_tag().class),
            )?;
            if let Some(StringOrNodeArray::NodeArray(comment)) =
                node.ref_(arena).as_jsdoc_augments_tag().maybe_comment()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::JSDocTemplateTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_template_tag().tag_name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_template_tag().constraint.clone(),
            )?;
            try_visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                Some(node.ref_(arena).as_jsdoc_template_tag().type_parameters),
                arena,
            )?;
            if let Some(StringOrNodeArray::NodeArray(comment)) =
                node.ref_(arena).as_jsdoc_template_tag().maybe_comment()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::JSDocTypedefTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_typedef_tag().tag_name()),
            )?;
            if matches!(
                node.ref_(arena).as_jsdoc_typedef_tag().type_expression,
                Some(type_expression) if type_expression.ref_(arena).kind() == SyntaxKind::JSDocTypeExpression
            ) {
                try_visit_node(
                    &mut cb_node,
                    node.ref_(arena)
                        .as_jsdoc_typedef_tag()
                        .type_expression
                        .clone(),
                )?;
                try_visit_node(
                    &mut cb_node,
                    node.ref_(arena).as_jsdoc_typedef_tag().full_name.clone(),
                )?;
                if let Some(StringOrNodeArray::NodeArray(comment)) =
                    node.ref_(arena).as_jsdoc_typedef_tag().maybe_comment()
                {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            } else {
                try_visit_node(
                    &mut cb_node,
                    node.ref_(arena).as_jsdoc_typedef_tag().full_name.clone(),
                )?;
                try_visit_node(
                    &mut cb_node,
                    node.ref_(arena)
                        .as_jsdoc_typedef_tag()
                        .type_expression
                        .clone(),
                )?;
                if let Some(StringOrNodeArray::NodeArray(comment)) =
                    node.ref_(arena).as_jsdoc_typedef_tag().maybe_comment()
                {
                    try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
                }
            }
        }
        SyntaxKind::JSDocCallbackTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_callback_tag().tag_name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_callback_tag().full_name.clone(),
            )?;
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_jsdoc_callback_tag().type_expression),
            )?;
            if let Some(StringOrNodeArray::NodeArray(comment)) =
                node.ref_(arena).as_jsdoc_callback_tag().maybe_comment()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::BaseJSDocTypeLikeTag => {
            try_visit_node(
                &mut cb_node,
                Some(node.ref_(arena).as_base_jsdoc_type_like_tag().tag_name()),
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena)
                    .as_base_jsdoc_type_like_tag()
                    .type_expression
                    .clone(),
            )?;
            if let Some(StringOrNodeArray::NodeArray(comment)) = node
                .ref_(arena)
                .as_base_jsdoc_type_like_tag()
                .maybe_comment()
            {
                try_visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(*comment), arena)?;
            }
        }
        SyntaxKind::JSDocSignature => {
            node.ref_(arena)
                .as_jsdoc_signature()
                .maybe_type_parameters()
                .as_ref()
                .try_map(|type_parameters| {
                    try_for_each(
                        &*type_parameters.ref_(arena),
                        |&node: &Id<Node>, _| -> Result<_, TError> {
                            cb_node(node)?;
                            Ok(Option::<()>::None)
                        },
                    )
                })?;
            try_for_each(
                &*node.ref_(arena).as_jsdoc_signature().parameters.ref_(arena),
                |&node: &Id<Node>, _| -> Result<_, TError> {
                    cb_node(node)?;
                    Ok(Option::<()>::None)
                },
            )?;
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_signature().type_.clone(),
            )?;
        }
        SyntaxKind::JSDocLink => {
            try_visit_node(&mut cb_node, node.ref_(arena).as_jsdoc_link().name.clone())?;
        }
        SyntaxKind::JSDocLinkCode => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_link_code().name.clone(),
            )?;
        }
        SyntaxKind::JSDocLinkPlain => {
            try_visit_node(
                &mut cb_node,
                node.ref_(arena).as_jsdoc_link_plain().name.clone(),
            )?;
        }
        SyntaxKind::JSDocTypeLiteral => {
            try_maybe_for_each(
                node.ref_(arena)
                    .as_jsdoc_type_literal()
                    .js_doc_property_tags
                    .refed(arena)
                    .as_deref(),
                |&node: &Id<Node>, _| -> Result<_, TError> {
                    cb_node(node)?;
                    Ok(Option::<()>::None)
                },
            )?;
        }
        SyntaxKind::PartiallyEmittedExpression => {
            try_visit_node(
                &mut cb_node,
                Some(
                    node.ref_(arena)
                        .as_partially_emitted_expression()
                        .expression,
                ),
            )?;
        }
        _ => (),
    }

    Ok(())
}
