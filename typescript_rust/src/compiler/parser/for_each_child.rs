use super::{visit_node, visit_nodes};
use crate::{
    for_each, ClassLikeDeclarationInterface, FunctionLikeDeclarationInterface,
    HasInitializerInterface, HasQuestionTokenInterface, HasStatementsInterface,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface,
    InterfaceOrClassLikeDeclarationInterface, JSDocTagInterface, NamedDeclarationInterface, Node,
    NodeArray, NodeInterface, SignatureDeclarationInterface, StringOrNodeArray, SyntaxKind,
};

pub fn for_each_child<TNodeCallback: FnMut(&Node), TNodesCallback: FnMut(&NodeArray)>(
    node: &Node,
    mut cb_node: TNodeCallback,
    mut cb_nodes: Option<TNodesCallback>,
) {
    if
    /* !node ||*/
    node.kind() <= SyntaxKind::LastToken {
        return;
    }
    match node {
        Node::QualifiedName(node) => {
            visit_node(&mut cb_node, Some(&*node.left));
            visit_node(&mut cb_node, Some(&*node.right));
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
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.exclamation_token.clone());
            visit_node(&mut cb_node, node.equals_token.clone());
            visit_node(&mut cb_node, node.object_assignment_initializer.clone());
        }
        Node::SpreadAssignment(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::ParameterDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_initializer())
        }
        Node::PropertyDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
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
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
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
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, Some(&*node.initializer))
        }
        Node::VariableDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
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
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
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
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::ConstructorTypeNode(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::CallSignatureDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::ConstructSignatureDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::IndexSignatureDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::MethodDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::MethodSignature(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::ConstructorDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::GetAccessorDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::SetAccessorDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(node.name()));
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::FunctionExpression(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::FunctionDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::ArrowFunction(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_asterisk_token());
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.maybe_question_token());
            visit_node(&mut cb_node, node.maybe_exclamation_token().clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
            visit_node(&mut cb_node, Some(&*node.equals_greater_than_token));
            visit_node(&mut cb_node, node.maybe_body())
        }
        Node::ClassStaticBlockDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(&*node.body));
        }
        Node::TypeReferenceNode(node) => {
            visit_node(&mut cb_node, Some(node.type_name.clone()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            )
        }
        Node::TypePredicateNode(node) => {
            visit_node(&mut cb_node, node.asserts_modifier.clone());
            visit_node(&mut cb_node, Some(&*node.parameter_name));
            visit_node(&mut cb_node, node.type_.clone());
        }
        Node::TypeQueryNode(node) => visit_node(&mut cb_node, Some(&*node.expr_name)),
        Node::TypeLiteralNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))
        }
        Node::ArrayTypeNode(node) => visit_node(&mut cb_node, Some(node.element_type.clone())),
        Node::TupleTypeNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::UnionTypeNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))
        }
        Node::IntersectionTypeNode(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))
        }
        Node::ConditionalTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.check_type));
            visit_node(&mut cb_node, Some(&*node.extends_type));
            visit_node(&mut cb_node, Some(&*node.true_type));
            visit_node(&mut cb_node, Some(&*node.false_type));
        }
        Node::InferTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.type_parameter));
        }
        Node::ImportTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.argument));
            visit_node(&mut cb_node, node.qualifier.clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            )
        }
        Node::ParenthesizedTypeNode(node) => visit_node(&mut cb_node, Some(&*node.type_)),
        Node::TypeOperatorNode(node) => visit_node(&mut cb_node, Some(&*node.type_)),
        Node::IndexedAccessTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.object_type));
            visit_node(&mut cb_node, Some(&*node.index_type));
        }
        Node::MappedTypeNode(node) => {
            visit_node(&mut cb_node, node.readonly_token.clone());
            visit_node(&mut cb_node, Some(&*node.type_parameter));
            visit_node(&mut cb_node, node.name_type.clone());
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, node.type_.clone());
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.members.as_ref())
        }
        Node::LiteralTypeNode(node) => visit_node(&mut cb_node, Some(&*node.literal)),
        Node::NamedTupleMember(node) => {
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, Some(&*node.name));
            visit_node(&mut cb_node, node.question_token.clone());
            visit_node(&mut cb_node, Some(&*node.type_));
        }
        Node::ObjectBindingPattern(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ArrayBindingPattern(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ArrayLiteralExpression(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ObjectLiteralExpression(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.properties))
        }
        Node::PropertyAccessExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::ElementAccessExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_node(&mut cb_node, Some(&*node.argument_expression));
        }
        Node::CallExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.arguments));
        }
        Node::NewExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.arguments.as_ref());
        }
        Node::TaggedTemplateExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.tag));
            visit_node(&mut cb_node, node.question_dot_token.clone());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            );
            visit_node(&mut cb_node, Some(&*node.template));
        }
        Node::TypeAssertion(node) => {
            visit_node(&mut cb_node, Some(&*node.type_));
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::ParenthesizedExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::DeleteExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::TypeOfExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::VoidExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::PrefixUnaryExpression(node) => visit_node(&mut cb_node, Some(&*node.operand)),
        Node::YieldExpression(node) => {
            visit_node(&mut cb_node, node.asterisk_token.clone());
            visit_node(&mut cb_node, node.expression.clone());
        }
        Node::AwaitExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::PostfixUnaryExpression(node) => visit_node(&mut cb_node, Some(&*node.operand)),
        Node::BinaryExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.left));
            visit_node(&mut cb_node, Some(&*node.operator_token));
            visit_node(&mut cb_node, Some(&*node.right));
        }
        Node::AsExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.type_));
        }
        Node::NonNullExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::MetaProperty(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::ConditionalExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.condition));
            visit_node(&mut cb_node, Some(&*node.question_token));
            visit_node(&mut cb_node, Some(&*node.when_true));
            visit_node(&mut cb_node, Some(&*node.colon_token));
            visit_node(&mut cb_node, Some(&*node.when_false));
        }
        Node::SpreadElement(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::Block(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements));
        }
        Node::ModuleBlock(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements));
        }
        Node::SourceFile(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.statements()));
            visit_node(&mut cb_node, Some(node.end_of_file_token()));
        }
        Node::VariableStatement(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.declaration_list.clone()))
        }
        Node::VariableDeclarationList(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.declarations))
        }
        Node::ExpressionStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::IfStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.then_statement));
            visit_node(&mut cb_node, node.else_statement.clone());
        }
        Node::DoStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.statement));
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::WhileStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.statement));
        }
        Node::ForStatement(node) => {
            visit_node(&mut cb_node, node.initializer.clone());
            visit_node(&mut cb_node, node.condition.clone());
            visit_node(&mut cb_node, node.incrementor.clone());
            visit_node(&mut cb_node, Some(&*node.statement));
        }
        Node::ForInStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.initializer));
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.statement));
        }
        Node::ForOfStatement(node) => {
            visit_node(&mut cb_node, node.await_modifier.clone());
            visit_node(&mut cb_node, Some(&*node.initializer));
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.statement));
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
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.statement));
        }
        Node::SwitchStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.case_block));
        }
        Node::CaseBlock(node) => visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.clauses)),
        Node::CaseClause(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
        }
        Node::DefaultClause(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.statements))
        }
        Node::LabeledStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.label));
            visit_node(&mut cb_node, Some(&*node.statement));
        }
        Node::ThrowStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::TryStatement(node) => {
            visit_node(&mut cb_node, Some(&*node.try_block));
            visit_node(&mut cb_node, node.catch_clause.clone());
            visit_node(&mut cb_node, node.finally_block.clone());
        }
        Node::CatchClause(node) => {
            visit_node(&mut cb_node, node.variable_declaration.clone());
            visit_node(&mut cb_node, Some(&*node.block));
        }
        Node::Decorator(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::ClassDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.members()))
        }
        Node::ClassExpression(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.members()))
        }
        Node::InterfaceDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_heritage_clauses(),
            );
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))
        }
        Node::TypeAliasDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_parameters().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.type_.clone()))
        }
        Node::EnumDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(node.name()));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.members))
        }
        Node::EnumMember(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
            visit_node(&mut cb_node, node.initializer.clone());
        }
        Node::ModuleDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, node.body.clone());
        }
        Node::ImportEqualsDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.maybe_name());
            visit_node(&mut cb_node, Some(&*node.module_reference));
        }
        Node::ImportDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.import_clause.clone());
            visit_node(&mut cb_node, Some(&*node.module_specifier));
            visit_node(&mut cb_node, node.assert_clause.clone());
        }
        Node::ImportClause(node) => {
            visit_node(&mut cb_node, node.name.clone());
            visit_node(&mut cb_node, node.named_bindings.clone());
        }
        Node::AssertClause(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::AssertEntry(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
            visit_node(&mut cb_node, Some(&*node.value));
        }
        Node::NamespaceExportDeclaration(node) => {
            visit_node(&mut cb_node, node.maybe_name());
        }
        Node::NamespaceImport(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::NamespaceExport(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::NamedImports(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::NamedExports(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::ExportDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, node.export_clause.clone());
            visit_node(&mut cb_node, node.module_specifier.clone());
            visit_node(&mut cb_node, node.assert_clause.clone());
        }
        Node::ImportSpecifier(node) => {
            visit_node(&mut cb_node, node.property_name.clone());
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::ExportSpecifier(node) => {
            visit_node(&mut cb_node, node.property_name.clone());
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::ExportAssignment(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_modifiers().as_ref(),
            );
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::TemplateExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.head));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.template_spans))
        }
        Node::TemplateSpan(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_node(&mut cb_node, Some(&*node.literal))
        }
        Node::TemplateLiteralTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.head));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.template_spans))
        }
        Node::TemplateLiteralTypeSpan(node) => {
            visit_node(&mut cb_node, Some(&*node.type_));
            visit_node(&mut cb_node, Some(&*node.literal))
        }
        Node::ComputedPropertyName(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::HeritageClause(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.types))
        }
        Node::ExpressionWithTypeArguments(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            )
        }
        Node::ExternalModuleReference(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::MissingDeclaration(node) => {
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_decorators().as_ref(),
            );
        }
        Node::CommaListExpression(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.elements))
        }
        Node::JsxElement(node) => {
            visit_node(&mut cb_node, Some(&*node.opening_element));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.children));
            visit_node(&mut cb_node, Some(&*node.closing_element));
        }
        Node::JsxFragment(node) => {
            visit_node(&mut cb_node, Some(&*node.opening_fragment));
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.children));
            visit_node(&mut cb_node, Some(&*node.closing_fragment));
        }
        Node::JsxSelfClosingElement(node) => {
            visit_node(&mut cb_node, Some(&*node.tag_name));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            );
            visit_node(&mut cb_node, Some(&*node.attributes));
        }
        Node::JsxOpeningElement(node) => {
            visit_node(&mut cb_node, Some(&*node.tag_name));
            visit_nodes(
                &mut cb_node,
                cb_nodes.as_mut(),
                node.maybe_type_arguments().as_ref(),
            );
            visit_node(&mut cb_node, Some(&*node.attributes));
        }
        Node::JsxAttributes(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.properties));
        }
        Node::JsxAttribute(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
            visit_node(&mut cb_node, node.initializer.clone());
        }
        Node::JsxSpreadAttribute(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        Node::JsxExpression(node) => {
            visit_node(&mut cb_node, node.dot_dot_dot_token.clone());
            visit_node(&mut cb_node, node.expression.clone());
        }
        Node::JsxClosingElement(node) => {
            visit_node(&mut cb_node, Some(&*node.tag_name));
        }
        Node::OptionalTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.type_));
        }
        Node::RestTypeNode(node) => {
            visit_node(&mut cb_node, Some(&*node.type_));
        }
        Node::JSDocTypeExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.type_));
        }
        Node::BaseJSDocUnaryType(node) => {
            visit_node(&mut cb_node, node.type_.clone());
        }
        Node::JSDocFunctionType(node) => {
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(node.parameters()));
            visit_node(&mut cb_node, node.maybe_type());
        }
        Node::JSDoc(node) => {
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.comment.as_ref() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), node.tags.as_ref());
        }
        Node::JSDocSeeTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.name.clone());
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::JSDocNameReference(node) => {
            visit_node(&mut cb_node, Some(&*node.name));
        }
        Node::JSDocMemberName(node) => {
            visit_node(&mut cb_node, Some(&*node.left));
            visit_node(&mut cb_node, Some(&*node.right));
        }
        Node::JSDocPropertyLikeTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            if node.is_name_first {
                visit_node(&mut cb_node, Some(&*node.name));
                visit_node(&mut cb_node, node.type_expression.clone());
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
                }
            } else {
                visit_node(&mut cb_node, node.type_expression.clone());
                visit_node(&mut cb_node, Some(&*node.name));
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
                }
            }
        }
        Node::BaseJSDocTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::JSDocImplementsTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, Some(&*node.class));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::JSDocAugmentsTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, Some(&*node.class));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::JSDocTemplateTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.constraint.clone());
            visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(&node.type_parameters));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::JSDocTypedefTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            if matches!(node.type_expression.as_ref(), Some(type_expression) if type_expression.kind() == SyntaxKind::JSDocTypeExpression)
            {
                visit_node(&mut cb_node, node.type_expression.clone());
                visit_node(&mut cb_node, node.full_name.clone());
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
                }
            } else {
                visit_node(&mut cb_node, node.full_name.clone());
                visit_node(&mut cb_node, node.type_expression.clone());
                if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                    visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
                }
            }
        }
        Node::JSDocCallbackTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.full_name.clone());
            visit_node(&mut cb_node, Some(&*node.type_expression));
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::BaseJSDocTypeLikeTag(node) => {
            visit_node(&mut cb_node, Some(node.tag_name()));
            visit_node(&mut cb_node, node.type_expression.clone());
            if let Some(StringOrNodeArray::NodeArray(comment)) = node.maybe_comment() {
                visit_nodes(&mut cb_node, cb_nodes.as_mut(), Some(comment));
            }
        }
        Node::JSDocSignature(node) => {
            node.maybe_type_parameters()
                .as_ref()
                .map(|type_parameters| {
                    for_each(type_parameters, |node, _| {
                        cb_node(node);
                        Option::<()>::None
                    })
                });
            for_each(&node.parameters, |node, _| {
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
        Node::PartiallyEmittedExpression(node) => {
            visit_node(&mut cb_node, Some(&*node.expression));
        }
        _ => (),
    }
}
