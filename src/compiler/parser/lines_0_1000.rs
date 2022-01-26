#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::convert::TryInto;
use std::rc::Rc;

use super::{Parser, ParsingContext};
use crate::{
    create_node_factory, create_scanner, maybe_text_char_at_index, normalize_path,
    object_allocator, BaseNode, BaseNodeFactory, CharacterCodes, Diagnostic, DiagnosticMessage,
    Expression, FunctionLikeDeclarationInterface, HasInitializerInterface, HasTypeInterface,
    HasTypeParametersInterface, Identifier, NamedDeclarationInterface, Node, NodeArray,
    NodeFactory, NodeFactoryFlags, NodeFlags, NodeInterface, Scanner, ScriptTarget,
    SignatureDeclarationInterface, SourceTextAsChars, Statement, SyntaxKind,
    TemplateLiteralLikeNode, TypeElement, TypeNode,
};
use local_macros::ast_type;

bitflags! {
    pub struct SignatureFlags: u32 {
        const None = 0;
        const Yield = 1 << 0;
        const Await = 1 << 1;
        const Type = 1 << 2;
        const IgnoreMissingOpenBrace = 1 << 4;
        const JSDoc = 1 << 5;
    }
}

#[derive(Eq, PartialEq)]
pub(super) enum SpeculationKind {
    TryParse,
    Lookahead,
    Reparse,
}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct ParseBaseNodeFactory {
    NodeConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    TokenConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    IdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    PrivateIdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    SourceFileConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
}

impl ParseBaseNodeFactory {
    pub fn new() -> Self {
        Self {
            NodeConstructor: RefCell::new(None),
            TokenConstructor: RefCell::new(None),
            IdentifierConstructor: RefCell::new(None),
            PrivateIdentifierConstructor: RefCell::new(None),
            SourceFileConstructor: RefCell::new(None),
        }
    }
}

#[allow(non_snake_case)]
impl BaseNodeFactory for ParseBaseNodeFactory {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut SourceFileConstructor = self.SourceFileConstructor.borrow_mut();
        if SourceFileConstructor.is_none() {
            *SourceFileConstructor = Some(object_allocator.get_source_file_constructor());
        }
        (SourceFileConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut IdentifierConstructor = self.IdentifierConstructor.borrow_mut();
        if IdentifierConstructor.is_none() {
            *IdentifierConstructor = Some(object_allocator.get_identifier_constructor());
        }
        (IdentifierConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut PrivateIdentifierConstructor = self.PrivateIdentifierConstructor.borrow_mut();
        if PrivateIdentifierConstructor.is_none() {
            *PrivateIdentifierConstructor =
                Some(object_allocator.get_private_identifier_constructor());
        }
        (PrivateIdentifierConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut TokenConstructor = self.TokenConstructor.borrow_mut();
        if TokenConstructor.is_none() {
            *TokenConstructor = Some(object_allocator.get_token_constructor());
        }
        (TokenConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut NodeConstructor = self.NodeConstructor.borrow_mut();
        if NodeConstructor.is_none() {
            *NodeConstructor = Some(object_allocator.get_node_constructor());
        }
        (NodeConstructor.unwrap())(kind, -1, -1)
    }
}

thread_local! {
    pub static parse_base_node_factory: ParseBaseNodeFactory = ParseBaseNodeFactory::new();
}

thread_local! {
    pub static parse_node_factory: Rc<NodeFactory> = create_node_factory(
        NodeFactoryFlags::NoParenthesizerRules,
        /*parse_base_node_factory.with(|_parse_base_node_factory| _parse_base_node_factory)*/
    );
}

fn visit_node<TNodeRef: Borrow<Node>, TNodeCallback: FnMut(&Node)>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) {
    node.map(|node| cb_node(node.borrow()));
}

fn visit_node_returns<TNodeRef: Borrow<Node>, TReturn, TNodeCallback: FnMut(&Node) -> TReturn>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) -> Option<TReturn> {
    node.map(|node| cb_node(node.borrow()))
}

fn visit_nodes<TNodeCallback: FnMut(&Node), TNodesCallback: FnMut(&NodeArray)>(
    cb_node: &mut TNodeCallback,
    cb_nodes: Option<&mut TNodesCallback>,
    nodes: Option<&NodeArray>,
) {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                cb_nodes(nodes);
            }
            None => {
                for node in nodes.iter() {
                    cb_node(node);
                }
            }
        }
    }
}

// fn visit_nodes_returns<TReturn, TNodeCallback: FnMut(&Node) -> TReturn, TNodesCallback: FnMut(&NodeArray) -> Option<TReturn>>(
//     cb_node: &mut TNodeCallback,
//     cb_nodes: Option<&mut TNodesCallback>,
//     nodes: Option<&NodeArray>,
// ) {
//     if let Some(nodes) = nodes {
//         match cb_nodes {
//             Some(cb_nodes) => {
//                 return cb_nodes(nodes);
//             }
//             None => {
//                 for node in nodes.iter() {
//                     let result = cb_node(node);
//                     if result.is_some() {
//                         return result;
//                     }
//                 }
//             }
//         }
//     }
//     None
// }

pub(crate) fn is_jsdoc_like_text(text: &SourceTextAsChars, start: usize) -> bool {
    matches!(
        maybe_text_char_at_index(text, start + 1),
        Some(CharacterCodes::asterisk)
    ) && matches!(
        maybe_text_char_at_index(text, start + 2),
        Some(CharacterCodes::asterisk)
    ) && !matches!(
        maybe_text_char_at_index(text, start + 3),
        Some(CharacterCodes::slash)
    )
}

pub fn for_each_child<TNodeCallback: FnMut(&Node), TNodesCallback: FnMut(&NodeArray)>(
    node: &Node,
    mut cb_node: TNodeCallback,
    mut cb_nodes: TNodesCallback,
) {
    if node.kind() <= SyntaxKind::LastToken {
        return;
    }
    match node {
        Node::QualifiedName(qualified_name) => {
            visit_node(&mut cb_node, Some(&*qualified_name.left));
            visit_node(&mut cb_node, Some(&*qualified_name.right));
        }
        Node::TypeParameterDeclaration(type_parameter_declaration) => {
            visit_node(&mut cb_node, Some(type_parameter_declaration.name()));
            visit_node(&mut cb_node, type_parameter_declaration.constraint.clone());
            visit_node(&mut cb_node, type_parameter_declaration.default.clone());
            visit_node(&mut cb_node, type_parameter_declaration.expression.clone());
        }
        Node::ParameterDeclaration(parameter_declaration) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                parameter_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                parameter_declaration.maybe_modifiers(),
            );
            visit_node(
                &mut cb_node,
                parameter_declaration.dot_dot_dot_token.clone(),
            );
            visit_node(&mut cb_node, Some(parameter_declaration.name()));
            visit_node(&mut cb_node, parameter_declaration.question_token.clone());
            visit_node(&mut cb_node, parameter_declaration.maybe_type());
            visit_node(&mut cb_node, parameter_declaration.maybe_initializer())
        }
        Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                property_signature.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                property_signature.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(property_signature.name()));
            visit_node(&mut cb_node, property_signature.maybe_type())
        }
        Node::PropertyAssignment(property_assignment) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                property_assignment.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                property_assignment.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(property_assignment.name()));
            visit_node(&mut cb_node, Some(&*property_assignment.initializer))
        }
        Node::VariableDeclaration(variable_declaration) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                variable_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                variable_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(variable_declaration.name()));
            visit_node(&mut cb_node, variable_declaration.maybe_type());
            visit_node(&mut cb_node, variable_declaration.maybe_initializer())
        }
        Node::Statement(Statement::FunctionDeclaration(function_declaration)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                function_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                function_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, function_declaration.maybe_asterisk_token());
            visit_node(&mut cb_node, Some(function_declaration.name()));
            visit_node(&mut cb_node, function_declaration.maybe_question_token());
            visit_node(&mut cb_node, function_declaration.maybe_exclamation_token());
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                function_declaration.maybe_type_parameters(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                Some(function_declaration.parameters()),
            );
            visit_node(&mut cb_node, function_declaration.maybe_type());
            visit_node(&mut cb_node, function_declaration.maybe_body())
        }
        Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
            visit_node(&mut cb_node, Some(type_reference_node.type_name.clone()));
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                type_reference_node.type_arguments.as_ref(),
            )
        }
        Node::TypeNode(TypeNode::ArrayTypeNode(array_type)) => {
            visit_node(&mut cb_node, Some(array_type.element_type.clone()))
        }
        Node::TypeNode(TypeNode::UnionTypeNode(union_type)) => {
            visit_nodes(&mut cb_node, Some(&mut cb_nodes), Some(&union_type.types))
        }
        Node::TypeNode(TypeNode::IntersectionTypeNode(intersection_type)) => visit_nodes(
            &mut cb_node,
            Some(&mut cb_nodes),
            Some(&intersection_type.types),
        ),
        Node::TypeNode(TypeNode::LiteralTypeNode(literal_type)) => {
            visit_node(&mut cb_node, Some(literal_type.literal.clone()))
        }
        Node::Expression(Expression::ArrayLiteralExpression(array_literal_expression)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                Some(&array_literal_expression.elements),
            )
        }
        Node::Expression(Expression::ObjectLiteralExpression(object_literal_expression)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                Some(&object_literal_expression.properties),
            )
        }
        Node::Expression(Expression::PrefixUnaryExpression(prefix_unary_expression)) => {
            visit_node(&mut cb_node, Some(prefix_unary_expression.operand.clone()))
        }
        Node::Statement(Statement::VariableStatement(variable_statement)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                variable_statement.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                variable_statement.maybe_modifiers(),
            );
            visit_node(
                &mut cb_node,
                Some(variable_statement.declaration_list.clone()),
            )
        }
        Node::VariableDeclarationList(variable_declaration_list) => visit_nodes(
            &mut cb_node,
            Some(&mut cb_nodes),
            Some(&variable_declaration_list.declarations),
        ),
        Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                interface_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                interface_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(interface_declaration.name()));
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                interface_declaration.maybe_type_parameters(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                Some(&interface_declaration.members),
            )
        }
        Node::Statement(Statement::TypeAliasDeclaration(type_alias_declaration)) => {
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                type_alias_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                type_alias_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(type_alias_declaration.name()));
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                type_alias_declaration.maybe_type_parameters(),
            );
            visit_node(&mut cb_node, Some(type_alias_declaration.type_.clone()))
        }
        Node::Expression(Expression::TemplateExpression(template_expression)) => {
            visit_node(&mut cb_node, Some(&*template_expression.head));
            visit_nodes(
                &mut cb_node,
                Some(&mut cb_nodes),
                Some(&template_expression.template_spans),
            )
        }
        Node::TemplateSpan(template_span) => {
            visit_node(&mut cb_node, Some(&*template_span.expression));
            visit_node(&mut cb_node, Some(&*template_span.literal))
        }
        _ => unimplemented!(),
    }
}

pub fn create_source_file(file_name: &str, source_text: &str) -> Rc<Node /*SourceFile*/> {
    Parser().parse_source_file(file_name, source_text)
}

#[ast_type(impl_from = false)]
pub enum MissingNode {
    Identifier(Identifier),
    TemplateLiteralLikeNode(TemplateLiteralLikeNode),
}

#[allow(non_snake_case)]
pub(super) struct ParserType {
    pub(super) scanner: RefCell<Scanner>,
    pub(super) NodeConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) IdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) PrivateIdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) TokenConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) SourceFileConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    pub(super) factory: Rc<NodeFactory>,
    pub(super) file_name: Option<String>,
    pub(super) source_text: Option<String>,
    pub(super) parse_diagnostics:
        Option<RefCell<Vec<Rc<Diagnostic /*DiagnosticWithDetachedLocation*/>>>>,
    pub(super) current_token: RefCell<Option<SyntaxKind>>,
    pub(super) parsing_context: Cell<Option<ParsingContext>>,
    pub(super) context_flags: Cell<Option<NodeFlags>>,
    pub(super) top_level: Cell<bool>,
    pub(super) parse_error_before_next_finished_node: Cell<bool>,
}

impl ParserType {
    pub(super) fn new() -> Self {
        ParserType {
            scanner: RefCell::new(create_scanner(
                ScriptTarget::Latest,
                true,
                None,
                None,
                None,
                None,
                None,
            )),
            NodeConstructor: None,
            IdentifierConstructor: None,
            PrivateIdentifierConstructor: None,
            TokenConstructor: None,
            SourceFileConstructor: None,
            factory: create_node_factory(
                NodeFactoryFlags::NoParenthesizerRules
                    | NodeFactoryFlags::NoNodeConverters
                    | NodeFactoryFlags::NoOriginalNode,
            ),
            file_name: None,
            source_text: None,
            parse_diagnostics: None,
            current_token: RefCell::new(None),
            parsing_context: Cell::new(None),
            context_flags: Cell::new(None),
            top_level: Cell::new(true),
            parse_error_before_next_finished_node: Cell::new(false),
        }
    }

    pub(super) fn scanner(&self) -> Ref<Scanner> {
        self.scanner.borrow()
    }

    pub(super) fn scanner_mut(&self) -> RefMut<Scanner> {
        self.scanner.borrow_mut()
    }

    #[allow(non_snake_case)]
    pub(super) fn NodeConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_NodeConstructor(
        &mut self,
        NodeConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn IdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.IdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_IdentifierConstructor(
        &mut self,
        IdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.IdentifierConstructor = Some(IdentifierConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn PrivateIdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.PrivateIdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_PrivateIdentifierConstructor(
        &mut self,
        PrivateIdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.PrivateIdentifierConstructor = Some(PrivateIdentifierConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn TokenConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.TokenConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_TokenConstructor(
        &mut self,
        TokenConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.TokenConstructor = Some(TokenConstructor);
    }

    #[allow(non_snake_case)]
    pub(super) fn SourceFileConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.SourceFileConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    pub(super) fn set_SourceFileConstructor(
        &mut self,
        SourceFileConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.SourceFileConstructor = Some(SourceFileConstructor);
    }

    pub(super) fn file_name(&self) -> &str {
        self.file_name.as_ref().unwrap()
    }

    pub(super) fn set_file_name(&mut self, file_name: String) {
        self.file_name = Some(file_name);
    }

    pub(super) fn source_text(&self) -> &str {
        self.source_text.as_ref().unwrap()
    }

    pub(super) fn set_source_text(&mut self, source_text: String) {
        self.source_text = Some(source_text);
    }

    pub(super) fn parse_diagnostics(&self) -> RefMut<Vec<Rc<Diagnostic>>> {
        self.parse_diagnostics.as_ref().unwrap().borrow_mut()
    }

    pub(super) fn set_parse_diagnostics(&mut self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        self.parse_diagnostics = Some(RefCell::new(parse_diagnostics));
    }

    pub(super) fn current_token(&self) -> SyntaxKind {
        self.current_token.borrow().unwrap()
    }

    pub(super) fn set_current_token(&self, token: SyntaxKind) {
        *self.current_token.borrow_mut() = Some(token);
    }

    pub(super) fn parsing_context(&self) -> ParsingContext {
        self.parsing_context.get().unwrap()
    }

    pub(super) fn set_parsing_context(&self, parsing_context: ParsingContext) {
        self.parsing_context.set(Some(parsing_context));
    }

    pub(super) fn context_flags(&self) -> NodeFlags {
        self.context_flags.get().unwrap()
    }

    pub(super) fn set_context_flags(&self, context_flags: NodeFlags) {
        self.context_flags.set(Some(context_flags));
    }

    pub(super) fn top_level(&self) -> bool {
        self.top_level.get()
    }

    pub(super) fn set_top_level(&self, top_level: bool) {
        self.top_level.set(top_level);
    }

    pub(super) fn parse_error_before_next_finished_node(&self) -> bool {
        self.parse_error_before_next_finished_node.get()
    }

    pub(super) fn set_parse_error_before_next_finished_node(&self, value: bool) {
        self.parse_error_before_next_finished_node.set(value);
    }

    pub(super) fn scan_error(&self, message: &DiagnosticMessage, length: usize) {
        self.parse_error_at_position(
            self.scanner().get_text_pos().try_into().unwrap(),
            length.try_into().unwrap(),
            message,
            None,
        );
    }

    pub(super) fn parse_source_file(
        &mut self,
        file_name: &str,
        source_text: &str,
    ) -> Rc<Node /*SourceFile*/> {
        self.initialize_state(file_name, source_text);
        self.parse_source_file_worker()
    }

    pub(super) fn initialize_state(&mut self, _file_name: &str, _source_text: &str) {
        self.set_NodeConstructor(object_allocator.get_node_constructor());
        self.set_IdentifierConstructor(object_allocator.get_identifier_constructor());
        self.set_PrivateIdentifierConstructor(
            object_allocator.get_private_identifier_constructor(),
        );
        self.set_TokenConstructor(object_allocator.get_token_constructor());
        self.set_SourceFileConstructor(object_allocator.get_source_file_constructor());

        self.set_file_name(normalize_path(_file_name));
        self.set_source_text(_source_text.to_string());

        self.set_parse_diagnostics(vec![]);
        self.set_parsing_context(ParsingContext::None);
        self.set_top_level(true);

        self.set_context_flags(NodeFlags::None);
        self.set_parse_error_before_next_finished_node(false);

        let mut scanner = self.scanner_mut();
        scanner.set_text(
            Some(_source_text.chars().collect()),
            Some(_source_text.to_string()),
            None,
            None,
        );
        // scanner.set_on_error(Some(Box::new(move |message, length| {
        //     self.scan_error(message, length)
        // })));
    }
}
