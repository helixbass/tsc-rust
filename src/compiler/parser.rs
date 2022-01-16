#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::convert::TryInto;
use std::rc::Rc;

use crate::{
    append, attach_file_to_diagnostics, create_detached_diagnostic, create_node_factory,
    create_scanner, get_binary_operator_precedence, get_full_width, is_literal_kind,
    is_modifier_kind, is_template_literal_kind, last_or_undefined, modifiers_to_flags,
    normalize_path, object_allocator, set_text_range_pos_end, some, token_is_identifier_or_keyword,
    token_to_string, ArrayLiteralExpression, BaseNode, BaseNodeFactory, BinaryExpression, Block,
    Debug_, Decorator, Diagnostic, DiagnosticMessage, DiagnosticRelatedInformationInterface,
    Diagnostics, Expression, FunctionDeclaration, HasExpressionInitializerInterface,
    HasTypeInterface, HasTypeParametersInterface, Identifier, InterfaceDeclaration,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralTypeNode, ModifierFlags,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags,
    NodeInterface, ObjectLiteralExpression, OperatorPrecedence, ParameterDeclaration,
    PropertyAssignment, Scanner, SourceFile, Statement, SyntaxKind, TemplateExpression,
    TemplateLiteralLikeNode, TemplateSpan, TokenFlags, TypeAliasDeclaration, TypeElement, TypeNode,
    TypeParameterDeclaration, VariableDeclaration, VariableDeclarationList,
};
use local_macros::{ast_type, enum_unwrapped};

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
enum SpeculationKind {
    TryParse,
    Lookahead,
    Reparse,
}

fn visit_node<TNodeRef: Borrow<Node>, TNodeCallback: FnMut(Option<TNodeRef>)>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) {
    cb_node(node)
}

fn visit_nodes<TNodeCallback: FnMut(Option<Rc<Node>>), TNodesCallback: FnMut(&NodeArray)>(
    cb_node: &mut TNodeCallback,
    cb_nodes: &mut TNodesCallback,
    nodes: Option<&NodeArray>,
) {
    if let Some(nodes) = nodes {
        if true {
            return cb_nodes(nodes);
        }
    }
}

pub fn for_each_child<TNodeCallback: FnMut(Option<Rc<Node>>), TNodesCallback: FnMut(&NodeArray)>(
    node: &Node,
    mut cb_node: TNodeCallback,
    mut cb_nodes: TNodesCallback,
) {
    if node.kind() <= SyntaxKind::LastToken {
        return;
    }
    match node {
        Node::TypeParameterDeclaration(type_parameter_declaration) => {
            visit_node(&mut cb_node, Some(type_parameter_declaration.name()))
        }
        Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                property_signature.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                property_signature.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(property_signature.name()));
            visit_node(&mut cb_node, property_signature.type_())
        }
        Node::PropertyAssignment(property_assignment) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                property_assignment.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                property_assignment.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(property_assignment.name()));
            visit_node(&mut cb_node, Some(property_assignment.initializer.clone()))
        }
        Node::VariableDeclaration(variable_declaration) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                variable_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                variable_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(variable_declaration.name()));
            visit_node(&mut cb_node, variable_declaration.type_());
            visit_node(&mut cb_node, variable_declaration.maybe_initializer())
        }
        Node::TypeNode(TypeNode::TypeReferenceNode(type_reference_node)) => {
            visit_node(&mut cb_node, Some(type_reference_node.type_name.clone()));
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                type_reference_node.type_arguments.as_ref(),
            )
        }
        Node::TypeNode(TypeNode::ArrayTypeNode(array_type)) => {
            visit_node(&mut cb_node, Some(array_type.element_type.clone()))
        }
        Node::TypeNode(TypeNode::UnionTypeNode(union_type)) => {
            visit_nodes(&mut cb_node, &mut cb_nodes, Some(&union_type.types))
        }
        Node::TypeNode(TypeNode::IntersectionTypeNode(intersection_type)) => {
            visit_nodes(&mut cb_node, &mut cb_nodes, Some(&intersection_type.types))
        }
        Node::TypeNode(TypeNode::LiteralTypeNode(literal_type)) => {
            visit_node(&mut cb_node, Some(literal_type.literal.clone()))
        }
        Node::Expression(Expression::ArrayLiteralExpression(array_literal_expression)) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                Some(&array_literal_expression.elements),
            )
        }
        Node::Expression(Expression::ObjectLiteralExpression(object_literal_expression)) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                Some(&object_literal_expression.properties),
            )
        }
        Node::Expression(Expression::PrefixUnaryExpression(prefix_unary_expression)) => {
            visit_node(&mut cb_node, Some(prefix_unary_expression.operand.clone()))
        }
        Node::Statement(Statement::VariableStatement(variable_statement)) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                variable_statement.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                variable_statement.maybe_modifiers(),
            );
            visit_node(
                &mut cb_node,
                Some(variable_statement.declaration_list.clone()),
            )
        }
        Node::VariableDeclarationList(variable_declaration_list) => visit_nodes(
            &mut cb_node,
            &mut cb_nodes,
            Some(&variable_declaration_list.declarations),
        ),
        Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                interface_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                interface_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(interface_declaration.name()));
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                interface_declaration.maybe_type_parameters(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                Some(&interface_declaration.members),
            )
        }
        Node::Statement(Statement::TypeAliasDeclaration(type_alias_declaration)) => {
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                type_alias_declaration.maybe_decorators().as_ref(),
            );
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                type_alias_declaration.maybe_modifiers(),
            );
            visit_node(&mut cb_node, Some(type_alias_declaration.name()));
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                type_alias_declaration.maybe_type_parameters(),
            );
            visit_node(&mut cb_node, Some(type_alias_declaration.type_.clone()))
        }
        Node::Expression(Expression::TemplateExpression(template_expression)) => {
            visit_node(&mut cb_node, Some(template_expression.head.clone()));
            visit_nodes(
                &mut cb_node,
                &mut cb_nodes,
                Some(&template_expression.template_spans),
            )
        }
        Node::TemplateSpan(template_span) => {
            visit_node(&mut cb_node, Some(template_span.expression.clone()));
            visit_node(&mut cb_node, Some(template_span.literal.clone()))
        }
        _ => unimplemented!(),
    }
}

pub fn create_source_file(file_name: &str, source_text: &str) -> Rc<SourceFile> {
    Parser().parse_source_file(file_name, source_text)
}

#[ast_type(impl_from = false)]
pub enum MissingNode {
    Identifier(Identifier),
    TemplateLiteralLikeNode(TemplateLiteralLikeNode),
}

#[allow(non_snake_case)]
struct ParserType {
    scanner: RefCell<Scanner>,
    NodeConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    IdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    TokenConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    SourceFileConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    factory: NodeFactory,
    file_name: Option<String>,
    source_text: Option<String>,
    parse_diagnostics: Option<RefCell<Vec<Rc<Diagnostic /*DiagnosticWithDetachedLocation*/>>>>,
    current_token: RefCell<Option<SyntaxKind>>,
    parsing_context: Cell<Option<ParsingContext>>,
    context_flags: Cell<Option<NodeFlags>>,
    top_level: Cell<bool>,
    parse_error_before_next_finished_node: Cell<bool>,
}

impl ParserType {
    fn new() -> Self {
        ParserType {
            scanner: RefCell::new(create_scanner(true)),
            NodeConstructor: None,
            IdentifierConstructor: None,
            TokenConstructor: None,
            SourceFileConstructor: None,
            factory: create_node_factory(),
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

    fn scanner(&self) -> Ref<Scanner> {
        self.scanner.borrow()
    }

    fn scanner_mut(&self) -> RefMut<Scanner> {
        self.scanner.borrow_mut()
    }

    #[allow(non_snake_case)]
    fn NodeConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.NodeConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_NodeConstructor(&mut self, NodeConstructor: fn(SyntaxKind, isize, isize) -> BaseNode) {
        self.NodeConstructor = Some(NodeConstructor);
    }

    #[allow(non_snake_case)]
    fn IdentifierConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.IdentifierConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_IdentifierConstructor(
        &mut self,
        IdentifierConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.IdentifierConstructor = Some(IdentifierConstructor);
    }

    #[allow(non_snake_case)]
    fn TokenConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.TokenConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_TokenConstructor(&mut self, TokenConstructor: fn(SyntaxKind, isize, isize) -> BaseNode) {
        self.TokenConstructor = Some(TokenConstructor);
    }

    #[allow(non_snake_case)]
    fn SourceFileConstructor(&self) -> fn(SyntaxKind, isize, isize) -> BaseNode {
        self.SourceFileConstructor.unwrap()
    }

    #[allow(non_snake_case)]
    fn set_SourceFileConstructor(
        &mut self,
        SourceFileConstructor: fn(SyntaxKind, isize, isize) -> BaseNode,
    ) {
        self.SourceFileConstructor = Some(SourceFileConstructor);
    }

    fn file_name(&self) -> &str {
        self.file_name.as_ref().unwrap()
    }

    fn set_file_name(&mut self, file_name: String) {
        self.file_name = Some(file_name);
    }

    fn source_text(&self) -> &str {
        self.source_text.as_ref().unwrap()
    }

    fn set_source_text(&mut self, source_text: String) {
        self.source_text = Some(source_text);
    }

    fn parse_diagnostics(&self) -> RefMut<Vec<Rc<Diagnostic>>> {
        self.parse_diagnostics.as_ref().unwrap().borrow_mut()
    }

    fn set_parse_diagnostics(&mut self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        self.parse_diagnostics = Some(RefCell::new(parse_diagnostics));
    }

    fn current_token(&self) -> SyntaxKind {
        self.current_token.borrow().unwrap()
    }

    fn set_current_token(&self, token: SyntaxKind) {
        *self.current_token.borrow_mut() = Some(token);
    }

    fn parsing_context(&self) -> ParsingContext {
        self.parsing_context.get().unwrap()
    }

    fn set_parsing_context(&self, parsing_context: ParsingContext) {
        self.parsing_context.set(Some(parsing_context));
    }

    fn context_flags(&self) -> NodeFlags {
        self.context_flags.get().unwrap()
    }

    fn set_context_flags(&self, context_flags: NodeFlags) {
        self.context_flags.set(Some(context_flags));
    }

    fn top_level(&self) -> bool {
        self.top_level.get()
    }

    fn set_top_level(&self, top_level: bool) {
        self.top_level.set(top_level);
    }

    fn parse_error_before_next_finished_node(&self) -> bool {
        self.parse_error_before_next_finished_node.get()
    }

    fn set_parse_error_before_next_finished_node(&self, value: bool) {
        self.parse_error_before_next_finished_node.set(value);
    }

    fn scan_error(&self, message: &DiagnosticMessage, length: usize) {
        self.parse_error_at_position(
            self.scanner().get_text_pos().try_into().unwrap(),
            length.try_into().unwrap(),
            message,
            None,
        );
    }

    fn parse_source_file(&mut self, file_name: &str, source_text: &str) -> Rc<SourceFile> {
        self.initialize_state(file_name, source_text);
        self.parse_source_file_worker()
    }

    fn initialize_state(&mut self, _file_name: &str, _source_text: &str) {
        self.set_NodeConstructor(object_allocator.get_node_constructor());
        self.set_IdentifierConstructor(object_allocator.get_identifier_constructor());
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
        scanner.set_text(Some(_source_text), None, None);
        // scanner.set_on_error(Some(Box::new(move |message, length| {
        //     self.scan_error(message, length)
        // })));
    }

    fn parse_source_file_worker(&self) -> Rc<SourceFile> {
        self.next_token();

        let statements =
            self.parse_list(ParsingContext::SourceElements, ParserType::parse_statement);
        Debug_.assert(matches!(self.token(), SyntaxKind::EndOfFileToken), None);

        let source_file = self.create_source_file(self.file_name(), statements);
        let source_file = Rc::new(source_file);
        source_file.set_parse_diagnostics(attach_file_to_diagnostics(
            &*self.parse_diagnostics(),
            &source_file,
        ));

        source_file
    }

    fn create_source_file<TNodes: Into<NodeArrayOrVec>>(
        &self,
        file_name: &str,
        statements: TNodes,
    ) -> SourceFile {
        let mut source_file = self.factory.create_source_file(self, statements);

        source_file.text = self.source_text().to_string();
        source_file.set_file_name(file_name.to_string());

        source_file
    }

    fn set_context_flag(&self, val: bool, flag: NodeFlags) {
        if val {
            self.set_context_flags(self.context_flags() | flag);
        } else {
            self.set_context_flags(self.context_flags() & !flag);
        }
    }

    fn set_yield_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::YieldContext);
    }

    fn set_decorator_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::DecoratorContext);
    }

    fn set_await_context(&self, val: bool) {
        self.set_context_flag(val, NodeFlags::AwaitContext);
    }

    fn do_outside_of_context<TReturn>(
        &self,
        context: NodeFlags,
        func: fn(&ParserType) -> TReturn,
    ) -> TReturn {
        let context_flags_to_clear = context & self.context_flags();
        if context_flags_to_clear != NodeFlags::None {
            self.set_context_flag(false, context_flags_to_clear);
            let result = func(self);
            self.set_context_flag(true, context_flags_to_clear);
            return result;
        }

        func(self)
    }

    fn do_inside_of_context<TReturn, TFunc: FnOnce() -> TReturn>(
        &self,
        context: NodeFlags,
        func: TFunc,
    ) -> TReturn {
        let context_flags_to_set = context & !self.context_flags();
        if context_flags_to_set != NodeFlags::None {
            self.set_context_flag(true, context_flags_to_set);
            let result = func();
            self.set_context_flag(false, context_flags_to_set);
            return result;
        }

        func()
    }

    fn allow_in_and<TReturn>(&self, func: fn(&ParserType) -> TReturn) -> TReturn {
        self.do_outside_of_context(NodeFlags::DisallowInContext, func)
    }

    fn do_in_await_context<TReturn, TFunc: FnOnce() -> TReturn>(&self, func: TFunc) -> TReturn {
        self.do_inside_of_context(NodeFlags::AwaitContext, func)
    }

    fn in_context(&self, flags: NodeFlags) -> bool {
        self.context_flags().intersects(flags)
    }

    fn in_yield_context(&self) -> bool {
        self.in_context(NodeFlags::YieldContext)
    }

    fn in_decorator_context(&self) -> bool {
        self.in_context(NodeFlags::DecoratorContext)
    }

    fn in_await_context(&self) -> bool {
        self.in_context(NodeFlags::AwaitContext)
    }

    fn parse_error_at_current_token(&self, message: &DiagnosticMessage, args: Option<Vec<String>>) {
        self.parse_error_at(
            self.scanner().get_token_pos().try_into().unwrap(),
            self.scanner().get_text_pos().try_into().unwrap(),
            message,
            args,
        );
    }

    fn parse_error_at_position(
        &self,
        start: isize,
        length: isize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        {
            let mut parse_diagnostics = self.parse_diagnostics();
            let last_error = last_or_undefined(&*parse_diagnostics);
            if last_error.map_or(true, |last_error| last_error.start() != start) {
                let file_name = self.file_name().to_string();
                parse_diagnostics.push(Rc::new(
                    create_detached_diagnostic(&file_name, start, length, message, args).into(),
                ));
            }
        }

        self.set_parse_error_before_next_finished_node(true);
    }

    fn parse_error_at(
        &self,
        start: isize,
        end: isize,
        message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) {
        self.parse_error_at_position(start, end - start, message, args);
    }

    fn get_node_pos(&self) -> isize {
        self.scanner().get_start_pos().try_into().unwrap()
    }

    fn token(&self) -> SyntaxKind {
        self.current_token()
    }

    fn next_token_without_check(&self) -> SyntaxKind {
        let current_token = self
            .scanner()
            .scan(Some(&|message, length| self.scan_error(message, length)));
        self.set_current_token(current_token);
        self.current_token()
    }

    fn next_token_and<TReturn>(&self, func: fn(&ParserType) -> TReturn) -> TReturn {
        self.next_token();
        func(self)
    }

    fn next_token(&self) -> SyntaxKind {
        self.next_token_without_check()
    }

    fn re_scan_template_token(&self, is_tagged_template: bool) -> SyntaxKind {
        self.set_current_token(self.scanner().re_scan_template_token(
            Some(&|message, length| self.scan_error(message, length)),
            is_tagged_template,
        ));
        self.current_token()
    }

    fn re_scan_template_head_or_no_substitution_template(&self) -> SyntaxKind {
        self.set_current_token(
            self.scanner()
                .re_scan_template_head_or_no_substitution_template(Some(&|message, length| {
                    self.scan_error(message, length)
                })),
        );
        self.current_token()
    }

    fn re_scan_less_than_token(&self) -> SyntaxKind {
        self.set_current_token(self.scanner().re_scan_less_than_token());
        self.current_token()
    }

    fn speculation_helper<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
        speculation_kind: SpeculationKind,
    ) -> Option<TReturn> {
        let save_token = self.current_token();
        let save_parse_diagnostics_length = self.parse_diagnostics().len();
        let save_parse_error_before_next_finished_node =
            self.parse_error_before_next_finished_node();

        let result = if speculation_kind != SpeculationKind::TryParse {
            self.scanner().look_ahead(callback)
        } else {
            self.scanner().try_scan(callback)
        };

        if result.is_none() || speculation_kind != SpeculationKind::TryParse {
            self.set_current_token(save_token);
            if speculation_kind != SpeculationKind::Reparse {
                self.parse_diagnostics()
                    .truncate(save_parse_diagnostics_length);
            }
            self.set_parse_error_before_next_finished_node(
                save_parse_error_before_next_finished_node,
            );
        }

        result
    }

    fn look_ahead<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, SpeculationKind::Lookahead)
    }

    fn look_ahead_bool<TCallback: FnOnce() -> bool>(&self, callback: TCallback) -> bool {
        self.look_ahead(|| if callback() { Some(()) } else { None })
            .is_some()
    }

    fn try_parse<TReturn, TCallback: FnOnce() -> Option<TReturn>>(
        &self,
        callback: TCallback,
    ) -> Option<TReturn> {
        self.speculation_helper(callback, SpeculationKind::TryParse)
    }

    fn try_parse_bool<TCallback: FnOnce() -> bool>(&self, callback: TCallback) -> bool {
        self.try_parse(|| if callback() { Some(()) } else { None })
            .is_some()
    }

    fn is_binding_identifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    fn is_identifier(&self) -> bool {
        if self.token() == SyntaxKind::Identifier {
            return true;
        }

        self.token() > SyntaxKind::LastReservedWord
    }

    fn parse_expected(
        &self,
        kind: SyntaxKind,
        diagnostic_message: Option<&DiagnosticMessage>,
        should_advance: Option<bool>,
    ) -> bool {
        let should_advance = should_advance.unwrap_or(true);
        if self.token() == kind {
            if should_advance {
                self.next_token();
            }
            return true;
        }

        if let Some(diagnostic_message) = diagnostic_message {
            self.parse_error_at_current_token(diagnostic_message, None);
        } else {
            self.parse_error_at_current_token(
                &Diagnostics::_0_expected,
                token_to_string(kind).map(|string| vec![string.to_string()]),
            );
        }
        false
    }

    fn parse_error_for_missing_semicolon_after(&self, node: &Expression) {
        unimplemented!()
    }

    fn parse_optional_token(&self, t: SyntaxKind) -> Option<Node> {
        if self.token() == t {
            return Some(self.parse_token_node().into());
        }
        None
    }

    fn parse_optional(&self, t: SyntaxKind) -> bool {
        if self.token() == t {
            self.next_token();
            return true;
        }
        false
    }

    fn parse_expected_token(
        &self,
        t: SyntaxKind,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Node {
        self.parse_optional_token(t).unwrap_or_else(|| {
            enum_unwrapped!(
                self.create_missing_node(
                    t,
                    false,
                    diagnostic_message.unwrap_or(&Diagnostics::_0_expected),
                    Some(args.unwrap_or_else(|| vec![token_to_string(t).unwrap().to_string()])),
                ),
                [MissingNode, TemplateLiteralLikeNode]
            )
            .into()
        })
    }

    fn parse_token_node(&self) -> BaseNode {
        let pos = self.get_node_pos();
        let kind = self.token();
        self.next_token();
        self.finish_node(self.factory.create_token(self, kind), pos, None)
    }

    fn can_parse_semicolon(&self) -> bool {
        if self.token() == SyntaxKind::SemicolonToken {
            return true;
        }

        self.token() == SyntaxKind::CloseBraceToken
            || self.token() == SyntaxKind::EndOfFileToken
            || self.scanner().has_preceding_line_break()
    }

    fn try_parse_semicolon(&self) -> bool {
        if !self.can_parse_semicolon() {
            return false;
        }

        if self.token() == SyntaxKind::SemicolonToken {
            self.next_token();
        }

        true
    }

    fn parse_semicolon(&self) -> bool {
        self.try_parse_semicolon() || self.parse_expected(SyntaxKind::SemicolonToken, None, None)
    }

    fn create_node_array(
        &self,
        elements: Vec<Node>,
        pos: isize,
        end: Option<isize>,
        has_trailing_comma: Option<bool>,
    ) -> NodeArray {
        let array = self.factory.create_node_array(
            Some(
                elements
                    .into_iter()
                    .map(Node::wrap)
                    .collect::<Vec<Rc<Node>>>(),
            ),
            has_trailing_comma,
        );
        // set_text_range_pos_end(
        //     array,
        //     pos,
        //     end.unwrap_or_else(|| self.scanner().get_start_pos()),
        // );
        array
    }

    fn finish_node<TParsedNode: NodeInterface>(
        &self,
        mut node: TParsedNode,
        pos: isize,
        end: Option<isize>,
    ) -> TParsedNode {
        set_text_range_pos_end(
            &mut node,
            pos,
            end.unwrap_or_else(|| self.scanner().get_start_pos().try_into().unwrap()),
        );

        if self.parse_error_before_next_finished_node() {
            self.set_parse_error_before_next_finished_node(false);
        }

        node
    }

    fn create_missing_node(
        &self,
        kind: SyntaxKind,
        report_at_current_position: bool,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> MissingNode {
        if report_at_current_position {
            self.parse_error_at_position(
                self.scanner().get_start_pos().try_into().unwrap(),
                0,
                diagnostic_message,
                args,
            );
        } else
        /*if diagnostic_message*/
        {
            self.parse_error_at_current_token(&diagnostic_message, args);
        }

        let pos = self.get_node_pos();
        let result = if kind == SyntaxKind::Identifier {
            MissingNode::Identifier(self.factory.create_identifier(self, ""))
        } else if is_template_literal_kind(kind) {
            MissingNode::TemplateLiteralLikeNode(self.factory.create_template_literal_like_node(
                self,
                kind,
                "".to_string(),
                Some("".to_string()),
                None,
            ))
        } else {
            unimplemented!()
        };
        self.finish_node(result, pos, None)
    }

    fn intern_identifier(&self, text: &str) -> String {
        text.to_string()
    }

    fn create_identifier(
        &self,
        is_identifier: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        if is_identifier {
            let pos = self.get_node_pos();
            let text = self.intern_identifier(&self.scanner().get_token_value());
            self.next_token_without_check();
            return self.finish_node(self.factory.create_identifier(self, &text), pos, None);
        }

        let report_at_current_position = self.token() == SyntaxKind::EndOfFileToken;

        let msg_arg = self.scanner().get_token_text();

        let default_message = if false {
            unimplemented!()
        } else {
            Diagnostics::Identifier_expected
        };

        enum_unwrapped!(
            self.create_missing_node(
                SyntaxKind::Identifier,
                report_at_current_position,
                diagnostic_message.unwrap_or(&default_message),
                Some(vec![msg_arg]),
            ),
            [MissingNode, Identifier]
        )
    }

    fn parse_binding_identifier(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        self.create_identifier(
            self.is_binding_identifier(),
            None,
            private_identifier_diagnostic_message,
        )
    }

    fn parse_identifier(
        &self,
        diagnostic_message: Option<&DiagnosticMessage>,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Identifier {
        self.create_identifier(
            self.is_identifier(),
            diagnostic_message,
            private_identifier_diagnostic_message,
        )
    }

    fn parse_identifier_name(&self, diagnostic_message: Option<&DiagnosticMessage>) -> Identifier {
        self.create_identifier(
            token_is_identifier_or_keyword(self.token()),
            diagnostic_message,
            None,
        )
    }

    fn is_literal_property_name(&self) -> bool {
        token_is_identifier_or_keyword(self.token())
            || self.token() == SyntaxKind::StringLiteral
            || self.token() == SyntaxKind::NumericLiteral
    }

    fn parse_property_name_worker(&self) -> Node /*PropertyName*/ {
        self.parse_identifier_name(None).into()
    }

    fn parse_property_name(&self) -> Node /*PropertyName*/ {
        self.parse_property_name_worker()
    }

    fn next_token_is_on_same_line_and_can_follow_modifier(&self) -> bool {
        self.next_token();
        if self.scanner().has_preceding_line_break() {
            return false;
        }
        self.can_follow_modifier()
    }

    fn next_token_can_follow_modifier(&self) -> bool {
        match self.token() {
            SyntaxKind::ConstKeyword => self.next_token() == SyntaxKind::EnumKeyword,
            SyntaxKind::ExportKeyword => {
                self.next_token();
                if self.token() == SyntaxKind::DefaultKeyword {
                    return self.look_ahead_bool(|| self.next_token_can_follow_default_keyword());
                }
                if self.token() == SyntaxKind::TypeKeyword {
                    return self.look_ahead_bool(|| self.next_token_can_follow_export_modifier());
                }
                self.can_follow_export_modifier()
            }
            SyntaxKind::DefaultKeyword => self.next_token_can_follow_default_keyword(),
            SyntaxKind::StaticKeyword | SyntaxKind::GetKeyword | SyntaxKind::SetKeyword => {
                self.next_token();
                self.can_follow_modifier()
            }
            _ => self.next_token_is_on_same_line_and_can_follow_modifier(),
        }
    }

    fn can_follow_export_modifier(&self) -> bool {
        self.token() != SyntaxKind::AsteriskToken
            && self.token() != SyntaxKind::AsKeyword
            && self.token() != SyntaxKind::OpenBraceToken
            && self.can_follow_modifier()
    }

    fn next_token_can_follow_export_modifier(&self) -> bool {
        self.next_token();
        self.can_follow_export_modifier()
    }

    fn parse_any_contextual_modifier(&self) -> bool {
        is_modifier_kind(self.token())
            && self.try_parse_bool(|| self.next_token_can_follow_modifier())
    }

    fn can_follow_modifier(&self) -> bool {
        self.token() == SyntaxKind::OpenBracketToken
            || self.token() == SyntaxKind::OpenBraceToken
            || self.token() == SyntaxKind::AsteriskToken
            || self.token() == SyntaxKind::DotDotDotToken
            || self.is_literal_property_name()
    }

    fn next_token_can_follow_default_keyword(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::ClassKeyword
            || self.token() == SyntaxKind::FunctionKeyword
            || self.token() == SyntaxKind::InterfaceKeyword
            || self.token() == SyntaxKind::AbstractKeyword
                && self.look_ahead_bool(|| self.next_token_is_class_keyword_on_same_line())
            || self.token() == SyntaxKind::AsyncKeyword
                && self.look_ahead_bool(|| self.next_token_is_function_keyword_on_same_line())
    }

    fn is_list_element(&self, kind: ParsingContext) -> bool {
        match kind {
            ParsingContext::SourceElements | ParsingContext::BlockStatements => {
                self.is_start_of_statement()
            }
            ParsingContext::TypeMembers => self.look_ahead_bool(|| self.is_type_member_start()),
            ParsingContext::ObjectLiteralMembers => match self.token() {
                SyntaxKind::OpenBracketToken
                | SyntaxKind::AsteriskToken
                | SyntaxKind::DotDotDotToken
                | SyntaxKind::DotToken => true,
                _ => self.is_literal_property_name(),
            },
            ParsingContext::VariableDeclarations => {
                self.is_binding_identifier_or_private_identifier_or_pattern()
            }
            ParsingContext::TypeParameters => self.is_identifier(),
            ParsingContext::ArrayLiteralMembers => {
                self.token() == SyntaxKind::CommaToken
                    || self.token() == SyntaxKind::DotToken
                    || self.token() == SyntaxKind::DotDotDotToken
                    || self.is_start_of_expression()
            }
            ParsingContext::TypeArguments => {
                self.token() == SyntaxKind::CommaToken || self.is_start_of_type()
            }
            _ => unimplemented!(),
        }
    }

    fn is_list_terminator(&self, kind: ParsingContext) -> bool {
        if self.token() == SyntaxKind::EndOfFileToken {
            return true;
        }
        match kind {
            ParsingContext::BlockStatements
            | ParsingContext::TypeMembers
            | ParsingContext::ObjectLiteralMembers => self.token() == SyntaxKind::CloseBraceToken,
            ParsingContext::VariableDeclarations => self.is_variable_declarator_list_terminator(),
            ParsingContext::TypeParameters => {
                self.token() == SyntaxKind::GreaterThanToken
                    || self.token() == SyntaxKind::OpenParenToken
                    || self.token() == SyntaxKind::OpenBraceToken
                    || self.token() == SyntaxKind::ExtendsKeyword
                    || self.token() == SyntaxKind::ImplementsKeyword
            }
            ParsingContext::ArrayLiteralMembers => self.token() == SyntaxKind::CloseBracketToken,
            ParsingContext::TypeArguments => self.token() != SyntaxKind::CommaToken,
            _ => false,
        }
    }

    fn is_variable_declarator_list_terminator(&self) -> bool {
        if self.can_parse_semicolon() {
            return true;
        }

        false
    }

    fn parse_list<TItem: Into<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
    ) -> NodeArray {
        let mut list = vec![];
        let list_pos = self.get_node_pos();

        while !self.is_list_terminator(kind) {
            if self.is_list_element(kind) {
                list.push(self.parse_list_element(kind, parse_element).into());

                continue;
            }

            unimplemented!()
        }

        self.create_node_array(list, list_pos, None, None)
    }

    fn parse_list_element<TItem: Into<Node>>(
        &self,
        _parsing_context: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
    ) -> TItem {
        parse_element(self)
    }

    fn parse_delimited_list<TItem: Into<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
        consider_semicolon_as_delimiter: Option<bool>,
    ) -> NodeArray {
        let consider_semicolon_as_delimiter = consider_semicolon_as_delimiter.unwrap_or(false);
        let save_parsing_context = self.parsing_context();
        self.set_parsing_context(self.parsing_context() | kind);
        let mut list: Vec<Node> = vec![];
        let list_pos = self.get_node_pos();

        let mut comma_start: isize = -1;
        loop {
            if self.is_list_element(kind) {
                let start_pos = self.scanner().get_start_pos();
                list.push(self.parse_list_element(kind, parse_element).into());
                comma_start = self.scanner().get_token_pos().try_into().unwrap();

                if self.parse_optional(SyntaxKind::CommaToken) {
                    continue;
                }

                comma_start = -1;
                if self.is_list_terminator(kind) {
                    break;
                }

                unimplemented!()
            }

            if self.is_list_terminator(kind) {
                break;
            }

            unimplemented!()
        }

        self.set_parsing_context(save_parsing_context);
        self.create_node_array(list, list_pos, None, Some(comma_start >= 0))
    }

    fn create_missing_list(&self) -> NodeArray {
        let mut list = self.create_node_array(vec![], self.get_node_pos(), None, None);
        list.is_missing_list = true;
        list
    }

    fn parse_bracketed_list<TItem: Into<Node>>(
        &self,
        kind: ParsingContext,
        parse_element: fn(&ParserType) -> TItem,
        open: SyntaxKind,
        close: SyntaxKind,
    ) -> NodeArray {
        if self.parse_expected(open, None, None) {
            let result = self.parse_delimited_list(kind, parse_element, None);
            self.parse_expected(close, None, None);
            return result;
        }

        self.create_missing_list()
    }

    fn parse_entity_name(
        &self,
        allow_reserved_words: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Node /*EntityName*/ {
        let pos = self.get_node_pos();
        let entity: Node = if allow_reserved_words {
            self.parse_identifier_name(diagnostic_message).into()
        } else {
            self.parse_identifier(diagnostic_message, None).into()
        };
        entity
    }

    fn parse_template_spans(&self, is_tagged_template: bool) -> NodeArray /*<TemplateSpan>*/ {
        let pos = self.get_node_pos();
        let mut list = vec![];
        let mut node: TemplateSpan;
        while {
            node = self.parse_template_span(is_tagged_template);
            let is_node_template_middle = node.literal.kind() == SyntaxKind::TemplateMiddle;
            list.push(node.into());
            is_node_template_middle
        } {}
        self.create_node_array(list, pos, None, None)
    }

    fn parse_template_expression(&self, is_tagged_template: bool) -> TemplateExpression {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_expression(
                self,
                self.parse_template_head(is_tagged_template).into(),
                self.parse_template_spans(is_tagged_template),
            ),
            pos,
            None,
        )
    }

    fn parse_literal_of_template_span(&self, is_tagged_template: bool) -> Node /*TemplateMiddle | TemplateTail*/
    {
        if self.token() == SyntaxKind::CloseBraceToken {
            self.re_scan_template_token(is_tagged_template);
            self.parse_template_middle_or_template_tail().into()
        } else {
            self.parse_expected_token(
                SyntaxKind::TemplateTail,
                Some(&Diagnostics::_0_expected),
                Some(vec![token_to_string(SyntaxKind::CloseBraceToken)
                    .unwrap()
                    .to_string()]),
            )
        }
    }

    fn parse_template_span(&self, is_tagged_template: bool) -> TemplateSpan {
        let pos = self.get_node_pos();
        self.finish_node(
            self.factory.create_template_span(
                self,
                self.allow_in_and(ParserType::parse_expression).into(),
                self.parse_literal_of_template_span(is_tagged_template)
                    .into(),
            ),
            pos,
            None,
        )
    }

    fn parse_literal_node(&self) -> LiteralLikeNode {
        self.parse_literal_like_node(self.token())
    }

    fn parse_template_head(&self, is_tagged_template: bool) -> LiteralLikeNode /*TemplateHead*/ {
        if is_tagged_template {
            self.re_scan_template_head_or_no_substitution_template();
        }
        let fragment = self.parse_literal_like_node(self.token());
        Debug_.assert(
            fragment.kind() == SyntaxKind::TemplateHead,
            Some("Template head has wrong token kind"),
        );
        fragment
    }

    fn parse_template_middle_or_template_tail(&self) -> LiteralLikeNode /*TemplateMiddle | TemplateTail*/
    {
        let fragment = self.parse_literal_like_node(self.token());
        Debug_.assert(
            fragment.kind() == SyntaxKind::TemplateMiddle
                || fragment.kind() == SyntaxKind::TemplateTail,
            Some("Template fragment has wrong token kind"),
        );
        fragment
    }

    fn get_template_literal_raw_text(&self, kind: SyntaxKind) -> String {
        let is_last =
            kind == SyntaxKind::NoSubstitutionTemplateLiteral || kind == SyntaxKind::TemplateTail;
        let token_text = self.scanner().get_token_text();
        let token_text_chars = token_text.chars();
        let token_text_chars_len = token_text_chars.clone().count();
        token_text_chars
            .skip(1)
            .take(
                token_text_chars_len
                    - (if self.scanner().is_unterminated() {
                        0
                    } else if is_last {
                        1
                    } else {
                        2
                    })
                    - 1,
            )
            .collect()
    }

    fn parse_literal_like_node(&self, kind: SyntaxKind) -> LiteralLikeNode {
        let pos = self.get_node_pos();
        let mut node: LiteralLikeNode = if is_template_literal_kind(kind) {
            self.factory
                .create_template_literal_like_node(
                    self,
                    kind,
                    self.scanner().get_token_value(),
                    Some(self.get_template_literal_raw_text(kind)),
                    Some(self.scanner().get_token_flags() & TokenFlags::TemplateLiteralLikeFlags),
                )
                .into()
        } else if kind == SyntaxKind::NumericLiteral {
            self.factory
                .create_numeric_literal(
                    self,
                    self.scanner().get_token_value(),
                    Some(self.scanner().get_numeric_literal_flags()),
                )
                .into()
        } else if kind == SyntaxKind::StringLiteral {
            self.factory
                .create_string_literal(
                    self,
                    self.scanner().get_token_value(),
                    None,
                    Some(self.scanner().has_extended_unicode_escape()),
                )
                .into()
        } else if is_literal_kind(kind) {
            self.factory
                .create_literal_like_node(self, kind, self.scanner().get_token_value())
        } else {
            Debug_.fail(None)
        };

        if self.scanner().has_extended_unicode_escape() {
            node.set_has_extended_unicode_escape(Some(true));
        }

        if self.scanner().is_unterminated() {
            node.set_is_unterminated(Some(true));
        }

        self.next_token();
        self.finish_node(node, pos, None)
    }

    fn parse_entity_name_of_type_reference(&self) -> Node /*EntityName*/ {
        self.parse_entity_name(true, Some(&Diagnostics::Type_expected))
    }

    fn parse_type_arguments_of_type_reference(&self) -> Option<NodeArray /*<TypeNode>*/> {
        if !self.scanner().has_preceding_line_break()
            && self.re_scan_less_than_token() == SyntaxKind::LessThanToken
        {
            return Some(self.parse_bracketed_list(
                ParsingContext::TypeArguments,
                ParserType::parse_type,
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ));
        }
        None
    }

    fn parse_type_reference(&self) -> TypeNode {
        let pos = self.get_node_pos();
        let name = self.parse_entity_name_of_type_reference().wrap();
        let type_arguments = self.parse_type_arguments_of_type_reference();
        self.finish_node(
            self.factory
                .create_type_reference_node(self, name, type_arguments)
                .into(),
            pos,
            None,
        )
    }

    fn parse_type_parameter(&self) -> TypeParameterDeclaration {
        let pos = self.get_node_pos();
        let name = self.parse_identifier(None, None);

        let node = self
            .factory
            .create_type_parameter_declaration(self, name.into());
        self.finish_node(node, pos, None)
    }

    fn parse_type_parameters(&self) -> Option<NodeArray /*<TypeParameterDeclaration>*/> {
        if self.token() == SyntaxKind::LessThanToken {
            return Some(self.parse_bracketed_list(
                ParsingContext::TypeParameters,
                ParserType::parse_type_parameter,
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ));
        }
        None
    }

    fn parse_name_of_parameter(&self, modifiers: Option<&NodeArray>) -> Rc<Node> {
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_cannot_be_used_as_parameters,
        ));
        if get_full_width(&*name) == 0
            && !some(
                modifiers.as_ref().map(|modifiers| {
                    let modifiers: &[Rc<Node>] = modifiers;
                    modifiers
                }),
                Option::<fn(&Rc<Node>) -> bool>::None,
            )
            && is_modifier_kind(self.token())
        {
            self.next_token();
        }
        name
    }

    fn parse_parameter_in_outer_await_context(&self) -> ParameterDeclaration {
        self.parse_parameter_worker(true)
    }

    fn parse_parameter(&self) -> ParameterDeclaration {
        self.parse_parameter_worker(false)
    }

    fn parse_parameter_worker(&self, in_outer_await_context: bool) -> ParameterDeclaration {
        let pos = self.get_node_pos();

        let decorators = if in_outer_await_context {
            self.do_in_await_context(|| self.parse_decorators())
        } else {
            self.parse_decorators()
        };

        if self.token() == SyntaxKind::ThisKeyword {
            unimplemented!()
        }

        let saved_top_level = self.top_level();
        self.set_top_level(false);
        let modifiers = self.parse_modifiers(None, None);
        let dot_dot_dot_token = self.parse_optional_token(SyntaxKind::DotDotDotToken);
        let name = self.parse_name_of_parameter(modifiers.as_ref());
        let question_token = self.parse_optional_token(SyntaxKind::QuestionToken);
        let type_annotation = self.parse_type_annotation();
        let initializer = self.parse_initializer();
        let node = self.finish_node(
            self.factory.create_parameter_declaration(
                self,
                decorators,
                modifiers,
                dot_dot_dot_token.map(Into::into),
                name,
                question_token.map(Into::into),
                type_annotation.map(Into::into),
                initializer.map(Into::into),
            ),
            pos,
            None,
        );
        self.set_top_level(saved_top_level);
        node
    }

    fn parse_return_type(&self, return_token: SyntaxKind, is_type: bool) -> Option<TypeNode> {
        if self.should_parse_return_type(return_token, is_type) {
            return Some(self.parse_type_or_type_predicate());
        }
        None
    }

    fn should_parse_return_type(&self, return_token: SyntaxKind, is_type: bool) -> bool {
        if return_token == SyntaxKind::EqualsGreaterThanToken {
            self.parse_expected(return_token, None, None);
            return true;
        } else if self.parse_optional(SyntaxKind::ColonToken) {
            return true;
        } else if is_type && self.token() == SyntaxKind::EqualsGreaterThanToken {
            self.parse_error_at_current_token(
                &Diagnostics::_0_expected,
                Some(vec![token_to_string(SyntaxKind::ColonToken)
                    .unwrap()
                    .to_string()]),
            );
            self.next_token();
            return true;
        }
        false
    }

    fn parse_parameters_worker(&self, flags: SignatureFlags) -> NodeArray /*<ParameterDeclaration>*/
    {
        let saved_yield_context = self.in_yield_context();
        let saved_await_context = self.in_await_context();

        self.set_yield_context(flags.intersects(SignatureFlags::Yield));
        self.set_await_context(flags.intersects(SignatureFlags::Await));

        let parameters = if false {
            unimplemented!()
        } else {
            self.parse_delimited_list(
                ParsingContext::Parameters,
                if saved_await_context {
                    ParserType::parse_parameter_in_outer_await_context
                } else {
                    ParserType::parse_parameter
                },
                None,
            )
        };

        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        parameters
    }

    fn parse_parameters(&self, flags: SignatureFlags) -> NodeArray /*<ParameterDeclaration>*/ {
        if !self.parse_expected(SyntaxKind::OpenParenToken, None, None) {
            return self.create_missing_list();
        }

        let parameters = self.parse_parameters_worker(flags);
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        parameters
    }

    fn parse_type_member_semicolon(&self) {
        if self.parse_optional(SyntaxKind::CommaToken) {
            return;
        }

        self.parse_semicolon();
    }

    fn parse_property_or_method_signature(
        &self,
        pos: isize,
        modifiers: Option<NodeArray>,
    ) -> TypeElement {
        let name = self.parse_property_name();
        let node: TypeElement;
        if false {
            unimplemented!()
        } else {
            let type_ = self.parse_type_annotation();
            node = self
                .factory
                .create_property_signature(self, modifiers, name.wrap(), type_.map(Into::into))
                .into();
        }
        self.parse_type_member_semicolon();
        self.finish_node(node, pos, None)
    }

    fn is_type_member_start(&self) -> bool {
        let mut id_token = false;

        if self.is_literal_property_name() {
            id_token = true;
            self.next_token();
        }

        if id_token {
            return self.token() == SyntaxKind::OpenParenToken
                || self.token() == SyntaxKind::LessThanToken
                || self.token() == SyntaxKind::QuestionToken
                || self.token() == SyntaxKind::ColonToken
                || self.token() == SyntaxKind::CommaToken
                || self.can_parse_semicolon();
        }
        false
    }

    fn parse_type_member(&self) -> TypeElement {
        let pos = self.get_node_pos();
        let modifiers = self.parse_modifiers(None, None);

        self.parse_property_or_method_signature(pos, modifiers)
    }

    fn parse_object_type_members(&self) -> NodeArray /*<TypeElement>*/ {
        let members: NodeArray;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.parse_list(ParsingContext::TypeMembers, ParserType::parse_type_member);
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }

        members
    }

    fn parse_keyword_and_no_dot(&self) -> Option<TypeNode> {
        let node = self.parse_token_node();
        if false {
            None
        } else {
            Some(Into::<KeywordTypeNode>::into(node).into())
        }
    }

    fn parse_literal_type_node(&self, negative: Option<bool>) -> LiteralTypeNode {
        let negative = negative.unwrap_or(false);
        let pos = self.get_node_pos();
        if negative {
            self.next_token();
        }
        let expression: Expression = match self.token() {
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword | SyntaxKind::NullKeyword => {
                self.parse_token_node().into()
            }
            _ => self.parse_literal_like_node(self.token()).into(),
        };
        if negative {
            unimplemented!()
        }
        self.finish_node(
            self.factory
                .create_literal_type_node(self, expression.into()),
            pos,
            None,
        )
    }

    fn parse_non_array_type(&self) -> TypeNode {
        match self.token() {
            SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::BooleanKeyword => self
                .try_parse(|| self.parse_keyword_and_no_dot())
                .unwrap_or_else(|| unimplemented!()),
            SyntaxKind::StringLiteral => self.parse_literal_type_node(None).into(),
            _ => self.parse_type_reference(),
        }
    }

    fn is_start_of_type(&self) -> bool {
        match self.token() {
            SyntaxKind::AnyKeyword
            | SyntaxKind::UnknownKeyword
            | SyntaxKind::StringKeyword
            | SyntaxKind::NumberKeyword
            | SyntaxKind::BigIntKeyword
            | SyntaxKind::BooleanKeyword
            | SyntaxKind::ReadonlyKeyword
            | SyntaxKind::SymbolKeyword
            | SyntaxKind::UniqueKeyword
            | SyntaxKind::VoidKeyword
            | SyntaxKind::UndefinedKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::ThisKeyword
            | SyntaxKind::TypeOfKeyword
            | SyntaxKind::NeverKeyword
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::OpenBracketToken
            | SyntaxKind::LessThanToken
            | SyntaxKind::BarToken
            | SyntaxKind::AmpersandToken
            | SyntaxKind::NewKeyword
            | SyntaxKind::StringLiteral
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::ObjectKeyword
            | SyntaxKind::AsteriskToken
            | SyntaxKind::QuestionToken
            | SyntaxKind::ExclamationToken
            | SyntaxKind::DotDotDotToken
            | SyntaxKind::InferKeyword
            | SyntaxKind::ImportKeyword
            | SyntaxKind::AssertsKeyword
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead => true,
            _ => self.is_identifier(),
        }
    }

    fn parse_postfix_type_or_higher(&self) -> TypeNode {
        let pos = self.get_node_pos();
        let mut type_ = self.parse_non_array_type();
        while !self.scanner().has_preceding_line_break() {
            match self.token() {
                SyntaxKind::ExclamationToken => {
                    unimplemented!()
                }
                SyntaxKind::QuestionToken => {
                    unimplemented!()
                }
                SyntaxKind::OpenBracketToken => {
                    self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
                    if self.is_start_of_type() {
                        unimplemented!()
                    } else {
                        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
                        type_ = self.finish_node(
                            self.factory
                                .create_array_type_node(self, type_.into())
                                .into(),
                            pos,
                            None,
                        );
                    }
                    break;
                }
                _ => {
                    return type_;
                }
            }
        }
        type_
    }

    fn parse_type_operator_or_higher(&self) -> TypeNode {
        // let operator = self.token();
        // match operator {

        // }
        self.parse_postfix_type_or_higher()
    }

    fn parse_function_or_constructor_type_to_error(
        &self,
        is_in_union_type: bool,
    ) -> Option<TypeNode> {
        None
    }

    fn parse_union_or_intersection_type<TReturn: Into<TypeNode>>(
        &self,
        operator: SyntaxKind, /*SyntaxKind.BarToken | SyntaxKind.AmpersandToken*/
        parse_constituent_type: fn(&ParserType) -> TypeNode,
        create_type_node: fn(&NodeFactory, &ParserType, NodeArray) -> TReturn,
    ) -> TypeNode {
        let pos = self.get_node_pos();
        let is_union_type = operator == SyntaxKind::BarToken;
        let has_leading_operator = self.parse_optional(operator);
        let mut type_: Option<TypeNode> = Some(if has_leading_operator {
            self.parse_function_or_constructor_type_to_error(is_union_type)
                .unwrap_or_else(|| parse_constituent_type(self))
        } else {
            parse_constituent_type(self)
        });
        if self.token() == operator || has_leading_operator {
            let mut types: Vec<Node> = vec![type_.take().unwrap().into()];
            while self.parse_optional(operator) {
                types.push(
                    self.parse_function_or_constructor_type_to_error(is_union_type)
                        .unwrap_or_else(|| parse_constituent_type(self))
                        .into(),
                );
            }
            type_ = Some(
                self.finish_node(
                    create_type_node(
                        &self.factory,
                        self,
                        self.create_node_array(types, pos, None, None),
                    )
                    .into(),
                    pos,
                    None,
                ),
            );
        }
        type_.unwrap()
    }

    fn parse_intersection_type_or_higher(&self) -> TypeNode {
        self.parse_union_or_intersection_type(
            SyntaxKind::AmpersandToken,
            ParserType::parse_type_operator_or_higher,
            NodeFactory::create_intersection_type_node,
        )
    }

    fn parse_union_type_or_higher(&self) -> TypeNode {
        self.parse_union_or_intersection_type(
            SyntaxKind::BarToken,
            ParserType::parse_intersection_type_or_higher,
            NodeFactory::create_union_type_node,
        )
    }

    fn parse_type_or_type_predicate(&self) -> TypeNode {
        let pos = self.get_node_pos();
        let type_predicate_variable = if self.is_identifier() {
            self.try_parse(|| self.parse_type_predicate_prefix())
        } else {
            None
        };
        let type_ = self.parse_type();
        if let Some(type_predicate_variable) = type_predicate_variable {
            self.finish_node(
                self.factory
                    .create_type_predicate_node(
                        self,
                        None,
                        type_predicate_variable.into(),
                        Some(type_.into()),
                    )
                    .into(),
                pos,
                None,
            )
        } else {
            type_
        }
    }

    fn parse_type_predicate_prefix(&self) -> Option<Identifier> {
        let id = self.parse_identifier(None, None);
        if self.token() == SyntaxKind::IsKeyword && !self.scanner().has_preceding_line_break() {
            self.next_token();
            return Some(id);
        }
        None
    }

    fn parse_type(&self) -> TypeNode {
        self.do_outside_of_context(NodeFlags::TypeExcludesFlags, ParserType::parse_type_worker)
    }

    fn parse_type_worker(&self) -> TypeNode {
        let pos = self.get_node_pos();
        let type_ = self.parse_union_type_or_higher();
        type_
    }

    fn parse_type_annotation(&self) -> Option<TypeNode> {
        if self.parse_optional(SyntaxKind::ColonToken) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn is_start_of_left_hand_side_expression(&self) -> bool {
        match self.token() {
            SyntaxKind::ThisKeyword
            | SyntaxKind::SuperKeyword
            | SyntaxKind::NullKeyword
            | SyntaxKind::TrueKeyword
            | SyntaxKind::FalseKeyword
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::OpenParenToken
            | SyntaxKind::OpenBracketToken
            | SyntaxKind::OpenBraceToken
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::ClassKeyword
            | SyntaxKind::NewKeyword
            | SyntaxKind::SlashToken
            | SyntaxKind::SlashEqualsToken
            | SyntaxKind::Identifier => true,
            SyntaxKind::ImportKeyword => {
                unimplemented!()
            }
            _ => self.is_identifier(),
        }
    }

    fn is_start_of_expression(&self) -> bool {
        if self.is_start_of_left_hand_side_expression() {
            return true;
        }

        match self.token() {
            SyntaxKind::PlusPlusToken => true,
            _ => {
                if self.is_binary_operator() {
                    return true;
                }

                self.is_identifier()
            }
        }
    }

    fn parse_expression(&self) -> Expression {
        let expr = self.parse_assignment_expression_or_higher();

        expr
    }

    fn parse_initializer(&self) -> Option<Expression> {
        if self.parse_optional(SyntaxKind::EqualsToken) {
            Some(self.parse_assignment_expression_or_higher())
        } else {
            None
        }
    }

    fn parse_assignment_expression_or_higher(&self) -> Expression {
        let expr = self.parse_binary_expression_or_higher(OperatorPrecedence::Lowest);

        self.parse_conditional_expression_rest(expr)
    }

    fn next_token_is_identifier_on_same_line(&self) -> bool {
        self.next_token();
        !self.scanner().has_preceding_line_break() && self.is_identifier()
    }

    fn parse_conditional_expression_rest(&self, left_operand: Expression) -> Expression {
        left_operand
    }

    fn parse_binary_expression_or_higher(&self, precedence: OperatorPrecedence) -> Expression {
        let pos = self.get_node_pos();
        let left_operand = self.parse_unary_expression_or_higher();
        self.parse_binary_expression_rest(precedence, left_operand, pos)
    }

    fn parse_binary_expression_rest(
        &self,
        precedence: OperatorPrecedence,
        mut left_operand: Expression,
        pos: isize,
    ) -> Expression {
        loop {
            let new_precedence = get_binary_operator_precedence(self.token());

            let consume_current_operator = new_precedence > precedence;

            if !consume_current_operator {
                break;
            }

            let operator_token = self.parse_token_node();
            let right = self.parse_binary_expression_or_higher(new_precedence);
            left_operand = self
                .make_binary_expression(left_operand, operator_token, right, pos)
                .into();
        }

        left_operand
    }

    fn is_binary_operator(&self) -> bool {
        get_binary_operator_precedence(self.token()) > OperatorPrecedence::Comma
    }

    fn make_binary_expression<TNode: Into<Node>>(
        &self,
        left: Expression,
        operator_token: TNode,
        right: Expression,
        pos: isize,
    ) -> BinaryExpression {
        self.finish_node(
            self.factory
                .create_binary_expression(self, left, operator_token.into(), right),
            pos,
            None,
        )
    }

    fn parse_unary_expression_or_higher(&self) -> Expression {
        if self.is_update_expression() {
            let update_expression = self.parse_update_expression();
            return update_expression;
        }

        panic!("Unimplemented");
    }

    fn is_update_expression(&self) -> bool {
        match self.token() {
            _ => true,
        }
    }

    fn parse_update_expression(&self) -> Expression {
        if self.token() == SyntaxKind::PlusPlusToken {
            let pos = self.get_node_pos();
            let operator = self.token();
            let operand =
                self.next_token_and(ParserType::parse_left_hand_side_expression_or_higher);
            return self
                .finish_node(
                    self.factory
                        .create_prefix_unary_expression(self, operator, operand),
                    pos,
                    None,
                )
                .into();
        }

        let expression = self.parse_left_hand_side_expression_or_higher();

        expression
    }

    fn parse_left_hand_side_expression_or_higher(&self) -> Expression {
        let expression = self.parse_member_expression_or_higher();

        self.parse_call_expression_rest(expression)
    }

    fn parse_member_expression_or_higher(&self) -> Expression {
        let expression = self.parse_primary_expression();
        self.parse_member_expression_rest(expression)
    }

    fn parse_member_expression_rest(&self, expression: Expression) -> Expression {
        loop {
            return expression;
        }
    }

    fn parse_call_expression_rest(&self, expression: Expression) -> Expression {
        expression
    }

    fn parse_primary_expression(&self) -> Expression {
        match self.token() {
            SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral
            | SyntaxKind::NoSubstitutionTemplateLiteral => return self.parse_literal_node().into(),
            SyntaxKind::TrueKeyword | SyntaxKind::FalseKeyword => {
                return self.parse_token_node().into()
            }
            SyntaxKind::OpenBracketToken => return self.parse_array_literal_expression().into(),
            SyntaxKind::OpenBraceToken => return self.parse_object_literal_expression().into(),
            SyntaxKind::TemplateHead => return self.parse_template_expression(false).into(),
            _ => (),
        }

        self.parse_identifier(Some(&Diagnostics::Expression_expected), None)
            .into()
    }

    fn parse_argument_or_array_literal_element(&self) -> Expression {
        if false {
            unimplemented!()
        } else if false {
            unimplemented!()
        } else {
            self.parse_assignment_expression_or_higher()
        }
    }

    fn parse_array_literal_expression(&self) -> ArrayLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBracketToken, None, None);
        let elements = self.parse_delimited_list(
            ParsingContext::ArrayLiteralMembers,
            ParserType::parse_argument_or_array_literal_element,
            None,
        );
        self.parse_expected(SyntaxKind::CloseBracketToken, None, None);
        self.finish_node(
            self.factory
                .create_array_literal_expression(self, Some(elements)),
            pos,
            None,
        )
    }

    fn parse_object_literal_element(&self) -> Node {
        let pos = self.get_node_pos();

        let token_is_identifier = self.is_identifier();
        let name = self.parse_property_name();

        let node: PropertyAssignment;
        if false {
            unimplemented!()
        } else {
            self.parse_expected(SyntaxKind::ColonToken, None, None);
            let initializer = self.allow_in_and(ParserType::parse_assignment_expression_or_higher);
            node = self
                .factory
                .create_property_assignment(self, name.wrap(), initializer.into());
        }
        self.finish_node(node.into(), pos, None)
    }

    fn parse_object_literal_expression(&self) -> ObjectLiteralExpression {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::OpenBraceToken, None, None);
        let properties = self.parse_delimited_list(
            ParsingContext::ObjectLiteralMembers,
            ParserType::parse_object_literal_element,
            Some(true),
        );
        if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
            unimplemented!()
        }
        self.finish_node(
            self.factory
                .create_object_literal_expression(self, Some(properties)),
            pos,
            None,
        )
    }

    fn parse_optional_binding_identifier(&self) -> Option<Identifier> {
        if self.is_binding_identifier() {
            Some(self.parse_binding_identifier(None))
        } else {
            None
        }
    }

    fn parse_block(
        &self,
        ignore_missing_open_brace: bool,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Block {
        let pos = self.get_node_pos();
        let open_brace_position = self.scanner().get_token_pos();
        if self.parse_expected(SyntaxKind::OpenBraceToken, diagnostic_message, None)
            || ignore_missing_open_brace
        {
            let multi_line = self.scanner().has_preceding_line_break();
            let statements =
                self.parse_list(ParsingContext::BlockStatements, ParserType::parse_statement);
            if !self.parse_expected(SyntaxKind::CloseBraceToken, None, None) {
                unimplemented!()
            }
            let result = self.finish_node(
                self.factory
                    .create_block(self, statements, Some(multi_line)),
                pos,
                None,
            );
            if self.token() == SyntaxKind::EqualsToken {
                unimplemented!()
            }

            return result;
        } else {
            let statements = self.create_missing_list();
            self.finish_node(self.factory.create_block(self, statements, None), pos, None)
        }
    }

    fn parse_function_block(
        &self,
        flags: SignatureFlags,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Block {
        let saved_yield_context = self.in_yield_context();
        self.set_yield_context(flags.intersects(SignatureFlags::Yield));

        let saved_await_context = self.in_await_context();
        self.set_await_context(flags.intersects(SignatureFlags::Await));

        let saved_top_level = self.top_level();
        self.set_top_level(false);

        let save_decorator_context = self.in_decorator_context();
        if save_decorator_context {
            self.set_decorator_context(false);
        }

        let block = self.parse_block(
            flags.intersects(SignatureFlags::IgnoreMissingOpenBrace),
            diagnostic_message,
        );

        if save_decorator_context {
            self.set_decorator_context(true);
        }

        self.set_top_level(saved_top_level);
        self.set_yield_context(saved_yield_context);
        self.set_await_context(saved_await_context);

        block
    }

    fn parse_empty_statement(&self) -> Statement {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::SemicolonToken, None, None);
        self.finish_node(self.factory.create_empty_statement(self).into(), pos, None)
    }

    fn parse_if_statement(&self) -> Statement {
        let pos = self.get_node_pos();
        self.parse_expected(SyntaxKind::IfKeyword, None, None);
        self.parse_expected(SyntaxKind::OpenParenToken, None, None);
        let expression = self.allow_in_and(ParserType::parse_expression);
        self.parse_expected(SyntaxKind::CloseParenToken, None, None);
        let then_statement = self.parse_statement();
        let else_statement = if self.parse_optional(SyntaxKind::ElseKeyword) {
            Some(self.parse_statement())
        } else {
            None
        };
        self.finish_node(
            self.factory
                .create_if_statement(self, expression, then_statement, else_statement)
                .into(),
            pos,
            None,
        )
    }

    fn parse_expression_or_labeled_statement(&self) -> Statement {
        let pos = self.get_node_pos();
        let expression = self.parse_expression();
        let node: Statement = if false {
            unimplemented!()
        } else {
            if !self.try_parse_semicolon() {
                self.parse_error_for_missing_semicolon_after(&expression);
            }
            self.factory
                .create_expression_statement(self, expression)
                .into()
        };
        self.finish_node(node, pos, None)
    }

    fn next_token_is_class_keyword_on_same_line(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::ClassKeyword && !self.scanner().has_preceding_line_break()
    }

    fn next_token_is_function_keyword_on_same_line(&self) -> bool {
        self.next_token();
        self.token() == SyntaxKind::FunctionKeyword && !self.scanner().has_preceding_line_break()
    }

    fn is_declaration(&self) -> bool {
        loop {
            match self.token() {
                SyntaxKind::VarKeyword | SyntaxKind::ConstKeyword => {
                    return true;
                }
                SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                    return self.next_token_is_identifier_on_same_line();
                }
                SyntaxKind::DeclareKeyword => {
                    self.next_token();
                    if self.scanner().has_preceding_line_break() {
                        return false;
                    }
                    continue;
                }
                _ => unimplemented!(),
            }
        }
    }

    fn is_start_of_declaration(&self) -> bool {
        self.look_ahead_bool(|| self.is_declaration())
    }

    fn is_start_of_statement(&self) -> bool {
        match self.token() {
            SyntaxKind::SemicolonToken
            | SyntaxKind::VarKeyword
            | SyntaxKind::FunctionKeyword
            | SyntaxKind::IfKeyword => true,
            SyntaxKind::ConstKeyword => self.is_start_of_declaration(),
            SyntaxKind::DeclareKeyword | SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                true
            }
            _ => self.is_start_of_expression(),
        }
    }

    fn parse_statement(&self) -> Statement {
        match self.token() {
            SyntaxKind::SemicolonToken => return self.parse_empty_statement(),
            SyntaxKind::VarKeyword => {
                return self.parse_variable_statement(self.get_node_pos(), None, None)
            }
            SyntaxKind::FunctionKeyword => {
                return self
                    .parse_function_declaration(self.get_node_pos(), None, None)
                    .into()
            }
            SyntaxKind::OpenBraceToken => return self.parse_block(false, None).into(),
            SyntaxKind::IfKeyword => return self.parse_if_statement(),
            SyntaxKind::ConstKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            SyntaxKind::DeclareKeyword | SyntaxKind::InterfaceKeyword | SyntaxKind::TypeKeyword => {
                if self.is_start_of_declaration() {
                    return self.parse_declaration();
                }
            }
            _ => (),
        }
        self.parse_expression_or_labeled_statement()
    }

    fn is_declare_modifier(&self, modifier: &Node /*Modifier*/) -> bool {
        modifier.kind() == SyntaxKind::DeclareKeyword
    }

    fn parse_declaration(&self) -> Statement {
        let is_ambient = some(
            self.look_ahead(|| {
                self.parse_decorators();
                self.parse_modifiers(None, None)
            })
            .as_ref()
            .map(|node_array| {
                let node_array: &[Rc<Node>] = node_array;
                node_array
            }),
            Some(|modifier: &Rc<Node>| self.is_declare_modifier(&**modifier)),
        );
        if is_ambient {
            let node = self.try_reuse_ambient_declaration();
            if let Some(node) = node {
                return node;
            }
        }

        let pos = self.get_node_pos();
        let decorators = self.parse_decorators();
        let modifiers = self.parse_modifiers(None, None);
        if is_ambient {
            for m in modifiers.as_ref().unwrap() {
                m.set_flags(m.flags() | NodeFlags::Ambient);
            }
            self.do_inside_of_context(NodeFlags::Ambient, move || {
                self.parse_declaration_worker(pos, decorators, modifiers)
            })
        } else {
            self.parse_declaration_worker(pos, decorators, modifiers)
        }
    }

    fn try_reuse_ambient_declaration(&self) -> Option<Statement> {
        None
    }

    fn parse_declaration_worker(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Statement {
        match self.token() {
            SyntaxKind::VarKeyword | SyntaxKind::ConstKeyword => {
                self.parse_variable_statement(pos, decorators, modifiers)
            }
            SyntaxKind::InterfaceKeyword => self
                .parse_interface_declaration(pos, decorators, modifiers)
                .into(),
            SyntaxKind::TypeKeyword => self
                .parse_type_alias_declaration(pos, decorators, modifiers)
                .into(),
            _ => unimplemented!(),
        }
    }

    fn parse_function_block_or_semicolon(
        &self,
        flags: SignatureFlags,
        diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Option<Block> {
        if self.token() != SyntaxKind::OpenBraceToken && self.can_parse_semicolon() {
            self.parse_semicolon();
            return None;
        }

        Some(self.parse_function_block(flags, diagnostic_message))
    }

    fn is_binding_identifier_or_private_identifier_or_pattern(&self) -> bool {
        self.is_binding_identifier()
    }

    fn parse_identifier_or_pattern(
        &self,
        private_identifier_diagnostic_message: Option<&DiagnosticMessage>,
    ) -> Rc<Node> {
        self.parse_binding_identifier(private_identifier_diagnostic_message)
            .into()
    }

    fn parse_variable_declaration_no_exclamation(&self) -> VariableDeclaration {
        self.parse_variable_declaration(false)
    }

    fn parse_variable_declaration_allow_exclamation(&self) -> VariableDeclaration {
        self.parse_variable_declaration(true)
    }

    fn parse_variable_declaration(&self, allow_exclamation: bool) -> VariableDeclaration {
        let pos = self.get_node_pos();
        let name = self.parse_identifier_or_pattern(Some(
            &Diagnostics::Private_identifiers_are_not_allowed_in_variable_declarations,
        ));
        let type_ = self.parse_type_annotation();
        let initializer = if false {
            None
        } else {
            self.parse_initializer()
        };
        let node = self.factory.create_variable_declaration(
            self,
            Some(name),
            type_.map(|type_| type_.into()),
            initializer.map(|initializer| initializer.into()),
        );
        self.finish_node(node, pos, None)
    }

    fn parse_variable_declaration_list(&self) -> VariableDeclarationList {
        let pos = self.get_node_pos();

        let mut flags = NodeFlags::None;
        match self.token() {
            SyntaxKind::VarKeyword => (),
            SyntaxKind::ConstKeyword => {
                flags |= NodeFlags::Const;
            }
            _ => Debug_.fail(None),
        }

        self.next_token();

        let declarations: NodeArray;
        if false {
            unimplemented!()
        } else {
            declarations = self.parse_delimited_list(
                ParsingContext::VariableDeclarations,
                ParserType::parse_variable_declaration_allow_exclamation,
                None,
            );
        }

        self.finish_node(
            self.factory
                .create_variable_declaration_list(self, declarations, Some(flags)),
            pos,
            None,
        )
    }

    fn parse_variable_statement(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Statement {
        let declaration_list = self.parse_variable_declaration_list();
        self.parse_semicolon();
        let mut node = self
            .factory
            .create_variable_statement(self, modifiers, declaration_list);
        node.set_decorators(decorators);
        self.finish_node(node.into(), pos, None)
    }

    fn parse_function_declaration(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> FunctionDeclaration {
        let saved_await_context = self.in_await_context();

        let modifier_flags = modifiers_to_flags(modifiers.as_ref());
        self.parse_expected(SyntaxKind::FunctionKeyword, None, None);
        let asterisk_token = self.parse_optional_token(SyntaxKind::AsteriskToken);
        let name = if modifier_flags.intersects(ModifierFlags::Default) {
            self.parse_optional_binding_identifier()
        } else {
            Some(self.parse_binding_identifier(None))
        };
        let is_generator = if asterisk_token.is_some() {
            SignatureFlags::Yield
        } else {
            SignatureFlags::None
        };
        let is_async = if modifier_flags.intersects(ModifierFlags::Async) {
            SignatureFlags::Await
        } else {
            SignatureFlags::None
        };
        let type_parameters = self.parse_type_parameters();
        if modifier_flags.intersects(ModifierFlags::Export) {
            self.set_await_context(true);
        }
        let parameters = self.parse_parameters(is_generator | is_async);
        let type_ = self.parse_return_type(SyntaxKind::ColonToken, false);
        let body = self.parse_function_block_or_semicolon(
            is_generator | is_async,
            Some(&Diagnostics::or_expected),
        );
        self.set_await_context(saved_await_context);
        let node = self.factory.create_function_declaration(
            self,
            decorators,
            modifiers,
            asterisk_token.map(Into::into),
            name.map(Into::into),
            type_parameters,
            parameters,
            type_.map(Into::into),
            body.map(Into::into),
        );
        self.finish_node(node.into(), pos, None)
    }

    fn try_parse_decorator(&self) -> Option<Decorator> {
        let pos = self.get_node_pos();
        if !self.parse_optional(SyntaxKind::AtToken) {
            return None;
        }
        unimplemented!()
    }

    fn parse_decorators(&self) -> Option<NodeArray /*<Decorator>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Node>> = None;
        loop {
            let decorator = self.try_parse_decorator();
            if decorator.is_none() {
                break;
            }
            let decorator = decorator.unwrap();
            if list.is_none() {
                list = Some(vec![]);
            }
            let list = list.as_mut().unwrap();
            append(list, Some(decorator.into()));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    fn try_parse_modifier(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
        has_seen_static_modifier: Option<bool>,
    ) -> Option<Node /*Modifier*/> {
        let permit_invalid_const_as_modifier = permit_invalid_const_as_modifier.unwrap_or(false);
        let stop_on_start_of_class_static_block =
            stop_on_start_of_class_static_block.unwrap_or(false);
        let has_seen_static_modifier = has_seen_static_modifier.unwrap_or(false);
        let pos = self.get_node_pos();
        let kind = self.token();

        if self.token() == SyntaxKind::ConstKeyword && permit_invalid_const_as_modifier {
            unimplemented!()
        } else if stop_on_start_of_class_static_block
            && self.token() == SyntaxKind::StaticKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_brace())
        {
            return None;
        } else if has_seen_static_modifier && self.token() == SyntaxKind::StaticKeyword {
            return None;
        } else {
            if !self.parse_any_contextual_modifier() {
                return None;
            }
        }

        Some(
            self.finish_node(self.factory.create_token(self, kind), pos, None)
                .into(),
        )
    }

    fn parse_modifiers(
        &self,
        permit_invalid_const_as_modifier: Option<bool>,
        stop_on_start_of_class_static_block: Option<bool>,
    ) -> Option<NodeArray /*<Modifier>*/> {
        let pos = self.get_node_pos();
        let mut list: Option<Vec<Node>> = None;
        let mut has_seen_static = false;
        loop {
            let modifier = self.try_parse_modifier(
                permit_invalid_const_as_modifier,
                stop_on_start_of_class_static_block,
                Some(has_seen_static),
            );
            if modifier.is_none() {
                break;
            }
            let modifier = modifier.unwrap();
            if modifier.kind() == SyntaxKind::StaticKeyword {
                has_seen_static = true;
            }
            if list.is_none() {
                list = Some(vec![]);
            }
            let list = list.as_mut().unwrap();
            append(list, Some(modifier.into()));
        }
        list.map(|list| self.create_node_array(list, pos, None, None))
    }

    fn parse_interface_declaration(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> InterfaceDeclaration {
        self.parse_expected(SyntaxKind::InterfaceKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        let members = self.parse_object_type_members();
        let node = self.factory.create_interface_declaration(
            self,
            decorators,
            modifiers,
            name.into(),
            type_parameters,
            members,
        );
        self.finish_node(node, pos, None)
    }

    fn parse_type_alias_declaration(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> TypeAliasDeclaration {
        self.parse_expected(SyntaxKind::TypeKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        self.parse_expected(SyntaxKind::EqualsToken, None, None);
        let type_ = if false {
            unimplemented!()
        } else {
            self.parse_type()
        };
        self.parse_semicolon();
        let node = self.factory.create_type_alias_declaration(
            self,
            decorators,
            modifiers,
            name.into(),
            type_parameters,
            type_.into(),
        );
        self.finish_node(node, pos, None)
    }

    fn next_token_is_open_brace(&self) -> bool {
        self.next_token() == SyntaxKind::OpenBraceToken
    }
}

impl BaseNodeFactory for ParserType {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        self.SourceFileConstructor()(kind, 0, 0)
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.IdentifierConstructor()(kind, 0, 0)
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        self.TokenConstructor()(kind, 0, 0)
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.NodeConstructor()(kind, 0, 0)
    }
}

// lazy_static! {
//     static ref ParserMut: Mutex<ParserType> = Mutex::new(ParserType::new());
// }

#[allow(non_snake_case)]
fn Parser() -> ParserType {
    ParserType::new()
}
// fn Parser() -> MutexGuard<'static, ParserType> {
//     ParserMut.lock().unwrap()
// }

bitflags! {
    pub struct ParsingContext: u32 {
        const None = 0;
        const SourceElements = 1 << 0;
        const BlockStatements = 1 << 1;
        const TypeMembers = 1 << 4;
        const VariableDeclarations = 1 << 8;
        const ObjectLiteralMembers = 1 << 12;
        const ArrayLiteralMembers = 1 << 15;
        const Parameters = 1 << 16;
        const TypeParameters = 1 << 19;
        const TypeArguments = 1 << 20;
    }
}
