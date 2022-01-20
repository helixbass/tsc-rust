#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

use super::{
    BaseNamedDeclaration, BaseNode, BindingLikeDeclarationInterface, Diagnostic, Expression,
    FunctionDeclaration, HasExpressionInitializerInterface, HasTypeInterface,
    NamedDeclarationInterface, Node, NodeArray, NodeInterface, Path, StringLiteral, Symbol,
    TypeCheckerHost, VariableLikeDeclarationInterface,
};
use local_macros::ast_type;

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct BinaryExpression {
    pub _node: BaseNode,
    pub left: Rc<Node>,
    pub operator_token: Box<Node>,
    pub right: Rc<Node>,
}

impl BinaryExpression {
    pub fn new(
        base_node: BaseNode,
        left: Expression,
        operator_token: Node,
        right: Expression,
    ) -> Self {
        Self {
            _node: base_node,
            left: left.into(),
            operator_token: Box::new(operator_token),
            right: right.into(),
        }
    }
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct BaseLiteralLikeNode {
    _node: BaseNode,
    text: String,
    is_unterminated: Option<bool>,
    has_extended_unicode_escape: Option<bool>,
}

impl BaseLiteralLikeNode {
    pub fn new(base_node: BaseNode, text: String) -> Self {
        Self {
            _node: base_node,
            text,
            is_unterminated: None,
            has_extended_unicode_escape: None,
        }
    }
}

impl LiteralLikeNodeInterface for BaseLiteralLikeNode {
    fn text(&self) -> &str {
        &self.text
    }

    fn is_unterminated(&self) -> Option<bool> {
        self.is_unterminated
    }

    fn set_is_unterminated(&mut self, is_unterminated: Option<bool>) {
        self.is_unterminated = is_unterminated;
    }

    fn has_extended_unicode_escape(&self) -> Option<bool> {
        self.has_extended_unicode_escape
    }

    fn set_has_extended_unicode_escape(&mut self, has_extended_unicode_escape: Option<bool>) {
        self.has_extended_unicode_escape = has_extended_unicode_escape;
    }
}

pub trait LiteralLikeNodeInterface {
    fn text(&self) -> &str;
    fn is_unterminated(&self) -> Option<bool>;
    fn set_is_unterminated(&mut self, is_unterminated: Option<bool>);
    fn has_extended_unicode_escape(&self) -> Option<bool>;
    fn set_has_extended_unicode_escape(&mut self, has_extended_unicode_escape: Option<bool>);
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression", interfaces = "LiteralLikeNodeInterface")]
pub enum LiteralLikeNode {
    StringLiteral(StringLiteral),
    TemplateLiteralLikeNode(TemplateLiteralLikeNode),
    NumericLiteral(NumericLiteral),
    BigIntLiteral(BigIntLiteral),
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct TemplateLiteralLikeNode {
    _literal_like_node: BaseLiteralLikeNode,
    pub raw_text: Option<String>,
    pub template_flags: Option<TokenFlags>,
}

impl TemplateLiteralLikeNode {
    pub fn new(
        literal_like_node: BaseLiteralLikeNode,
        raw_text: Option<String>,
        template_flags: Option<TokenFlags>,
    ) -> Self {
        Self {
            _literal_like_node: literal_like_node,
            raw_text,
            template_flags,
        }
    }
}

impl TemplateLiteralLikeNodeInterface for TemplateLiteralLikeNode {
    fn maybe_raw_text(&self) -> Option<&str> {
        self.raw_text.as_deref()
    }

    fn maybe_template_flags(&self) -> Option<TokenFlags> {
        self.template_flags
    }
}

pub trait TemplateLiteralLikeNodeInterface {
    fn maybe_raw_text(&self) -> Option<&str>;
    fn maybe_template_flags(&self) -> Option<TokenFlags>;
}

bitflags! {
    pub struct TokenFlags: u32 {
        const None = 0;
        const PrecedingLineBreak = 1 << 0;
        const PrecedingJSDocComment = 1 << 1;
        const Unterminated = 1 << 2;
        const ExtendedUnicodeEscape = 1 << 3;
        const Scientific = 1 << 4;
        const Octal = 1 << 5;
        const HexSpecifier = 1 << 6;
        const BinarySpecifier = 1 << 7;
        const OctalSpecifier = 1 << 8;
        const ContainsSeparator = 1 << 9;
        const UnicodeEscape = 1 << 10;
        const ContainsInvalidEscape = 1 << 11;

        const BinaryOrOctalSpecifier = Self::BinarySpecifier.bits | Self::OctalSpecifier.bits;
        const NumericLiteralFlags = Self::Scientific.bits | Self::Octal.bits | Self::HexSpecifier.bits | Self::BinaryOrOctalSpecifier.bits | Self::ContainsSeparator.bits;
        const TemplateLiteralLikeFlags = Self::ContainsInvalidEscape.bits;
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct NumericLiteral {
    _literal_like_node: BaseLiteralLikeNode,
}

impl NumericLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "LiteralLikeNode, Expression",
    interfaces = "LiteralLikeNodeInterface"
)]
pub struct BigIntLiteral {
    _literal_like_node: BaseLiteralLikeNode,
}

impl BigIntLiteral {
    pub fn new(base_literal_like_node: BaseLiteralLikeNode) -> Self {
        Self {
            _literal_like_node: base_literal_like_node,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct TemplateExpression {
    _node: BaseNode,
    pub head: Rc<Node /*TemplateHead*/>,
    pub template_spans: NodeArray, /*<TemplateSpan>*/
}

impl TemplateExpression {
    pub fn new(base_node: BaseNode, head: Rc<Node>, template_spans: NodeArray) -> Self {
        Self {
            _node: base_node,
            head,
            template_spans,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct TemplateSpan {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
    pub literal: Rc<Node /*TemplateMiddle | TemplateTail*/>,
}

impl TemplateSpan {
    pub fn new(base_node: BaseNode, expression: Rc<Node>, literal: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
            literal,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct ArrayLiteralExpression {
    _node: BaseNode,
    pub elements: NodeArray, /*<Expression>*/
}

impl ArrayLiteralExpression {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Expression")]
pub struct ObjectLiteralExpression {
    _node: BaseNode,
    pub properties: NodeArray, /*<ObjectLiteralElementLike>*/
}

impl ObjectLiteralExpression {
    pub fn new(base_node: BaseNode, properties: NodeArray) -> Self {
        Self {
            _node: base_node,
            properties,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub enum Statement {
    FunctionDeclaration(FunctionDeclaration),
    EmptyStatement(EmptyStatement),
    Block(Block),
    VariableStatement(VariableStatement),
    ExpressionStatement(ExpressionStatement),
    IfStatement(IfStatement),
    ReturnStatement(ReturnStatement),
    InterfaceDeclaration(InterfaceDeclaration),
    TypeAliasDeclaration(TypeAliasDeclaration),
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct EmptyStatement {
    pub _node: BaseNode,
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct Block {
    _node: BaseNode,
    pub statements: NodeArray, /*<Statement>*/
    pub(crate) multi_line: Option<bool>,
}

impl Block {
    pub fn new(base_node: BaseNode, statements: NodeArray, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            statements,
            multi_line,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct VariableStatement {
    _node: BaseNode,
    pub declaration_list: Rc</*VariableDeclarationList*/ Node>,
}

impl VariableStatement {
    pub fn new(base_node: BaseNode, declaration_list: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            declaration_list,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct ExpressionStatement {
    _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
}

impl ExpressionStatement {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct IfStatement {
    _node: BaseNode,
    pub expression: Rc</*Expression*/ Node>,
    pub then_statement: Rc</*Statement*/ Node>,
    pub else_statement: Option<Rc</*Statement*/ Node>>,
}

impl IfStatement {
    pub fn new(
        base_node: BaseNode,
        expression: Rc<Node>,
        then_statement: Rc<Node>,
        else_statement: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            expression,
            then_statement,
            else_statement,
        }
    }
}

#[derive(Debug)]
#[ast_type(ancestors = "Statement")]
pub struct ReturnStatement {
    _node: BaseNode,
    pub expression: Option<Rc</*Expression*/ Node>>,
}

impl ReturnStatement {
    pub fn new(base_node: BaseNode, expression: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub enum TypeElement {
    PropertySignature(PropertySignature),
}

#[derive(Debug)]
#[ast_type(ancestors = "TypeElement", interfaces = "NamedDeclarationInterface")]
pub struct PropertySignature {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub type_: Option<Rc<Node /*TypeNode*/>>,
}

impl PropertySignature {
    pub fn new(base_named_declaration: BaseNamedDeclaration, type_: Option<Rc<Node>>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_,
        }
    }
}

impl HasTypeInterface for PropertySignature {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Rc<Node>) {
        self.type_ = Some(type_);
    }
}

impl HasExpressionInitializerInterface for PropertySignature {
    fn maybe_initializer(&self) -> Option<Rc<Node>> {
        None
    }

    fn set_initializer(&mut self, _initializer: Rc<Node>) {
        panic!("Shouldn't call set_initializer() on PropertySignature")
    }
}

impl BindingLikeDeclarationInterface for PropertySignature {}

impl VariableLikeDeclarationInterface for PropertySignature {}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct PropertyAssignment {
    _named_declaration: BaseNamedDeclaration, /*name: PropertyName*/
    pub initializer: Rc<Node /*Expression*/>,
}

// TODO: should implement HasExpressionInitializerInterface for PropertyAssignment? Its initializer
// isn't optional - should maybe change HasExpressionInitializerInterface initializer() ->
// maybe_initializer() and add non-optional initializer()?

impl PropertyAssignment {
    pub fn new(base_named_declaration: BaseNamedDeclaration, initializer: Rc<Node>) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            initializer,
        }
    }
}

pub trait HasTypeParametersInterface {
    fn maybe_type_parameters(&self) -> Option<&NodeArray>;
}

pub trait GenericNamedDeclarationInterface:
    NamedDeclarationInterface + HasTypeParametersInterface
{
}

#[derive(Debug)]
#[ast_type(impl_from = false, interfaces = "NamedDeclarationInterface")]
pub struct BaseGenericNamedDeclaration {
    _named_declaration: BaseNamedDeclaration,
    pub type_parameters: Option<NodeArray /*<TypeParameterDeclaration>*/>,
}

impl BaseGenericNamedDeclaration {
    pub fn new(
        base_named_declaration: BaseNamedDeclaration,
        type_parameters: Option<NodeArray>,
    ) -> Self {
        Self {
            _named_declaration: base_named_declaration,
            type_parameters,
        }
    }
}

impl HasTypeParametersInterface for BaseGenericNamedDeclaration {
    fn maybe_type_parameters(&self) -> Option<&NodeArray> {
        self.type_parameters.as_ref()
    }
}

impl GenericNamedDeclarationInterface for BaseGenericNamedDeclaration {}

#[derive(Debug)]
#[ast_type(
    impl_from = false,
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct BaseInterfaceOrClassLikeDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration,
}

impl BaseInterfaceOrClassLikeDeclaration {
    pub fn new(base_generic_named_declaration: BaseGenericNamedDeclaration) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "Statement",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct InterfaceDeclaration {
    _interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration, /*name: Identifier*/
    pub members: NodeArray,                                                    /*<TypeElement>*/
}

impl InterfaceDeclaration {
    pub fn new(
        base_interface_or_class_like_declaration: BaseInterfaceOrClassLikeDeclaration,
        members: NodeArray,
    ) -> Self {
        Self {
            _interface_or_class_like_declaration: base_interface_or_class_like_declaration,
            members,
        }
    }
}

#[derive(Debug)]
#[ast_type(
    ancestors = "Statement",
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface"
)]
pub struct TypeAliasDeclaration {
    _generic_named_declaration: BaseGenericNamedDeclaration, /*name: Identifier*/
    pub type_: Rc<Node /*TypeNode*/>,
}

impl TypeAliasDeclaration {
    pub fn new(
        base_generic_named_declaration: BaseGenericNamedDeclaration,
        type_: Rc<Node>,
    ) -> Self {
        Self {
            _generic_named_declaration: base_generic_named_declaration,
            type_,
        }
    }
}

pub type SourceTextAsChars = Vec<char>;

pub fn text_len(text: &SourceTextAsChars) -> usize {
    text.len()
}

pub fn maybe_text_char_at_index(text: &SourceTextAsChars, index: usize) -> Option<char> {
    text.get(index).map(|ch| *ch)
}

pub fn text_char_at_index(text: &SourceTextAsChars, index: usize) -> char {
    maybe_text_char_at_index(text, index).unwrap()
}

pub fn text_substring(text: &SourceTextAsChars, start: usize, end: usize) -> String {
    text[start..end].into_iter().collect()
}

pub fn text_str_num_chars(text: &str, start: usize, end: usize) -> usize {
    text[start..end].chars().count()
}

pub trait SourceFileLike {
    fn text(&self) -> &str;
    fn text_as_chars(&self) -> &SourceTextAsChars;
    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>>;
    fn line_map(&self) -> Ref<Vec<usize>>;
    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize>;
}

#[derive(Debug)]
#[ast_type(impl_from = false)]
pub struct SourceFile {
    _node: BaseNode,
    _symbols_without_a_symbol_table_strong_references: RefCell<Vec<Rc<Symbol>>>,
    pub statements: NodeArray,

    file_name: RefCell<String>,
    path: RefCell<Option<Path>>,
    pub text: String,
    pub text_as_chars: SourceTextAsChars,

    parse_diagnostics: RefCell<Option<Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>>>,

    line_map: RefCell<Option<Vec<usize>>>,
}

impl SourceFile {
    pub fn new(
        base_node: BaseNode,
        statements: NodeArray,
        file_name: String,
        text: String,
    ) -> Self {
        Self {
            _node: base_node,
            _symbols_without_a_symbol_table_strong_references: RefCell::new(vec![]),
            statements,
            file_name: RefCell::new(file_name),
            path: RefCell::new(None),
            text,
            text_as_chars: text.chars().collect(),
            parse_diagnostics: RefCell::new(None),
            line_map: RefCell::new(None),
        }
    }

    pub fn file_name(&self) -> Ref<String> {
        self.file_name.borrow()
    }

    pub fn set_file_name(&self, file_name: String) {
        *self.file_name.borrow_mut() = file_name;
    }

    pub fn maybe_path(&self) -> Ref<Option<Path>> {
        self.path.borrow()
    }

    pub fn set_path(&self, path: Path) {
        *self.path.borrow_mut() = Some(path);
    }

    pub fn parse_diagnostics(&self) -> Ref<Vec<Rc<Diagnostic>>> {
        Ref::map(self.parse_diagnostics.borrow(), |option| {
            option.as_ref().unwrap()
        })
    }

    pub fn set_parse_diagnostics(&self, parse_diagnostics: Vec<Rc<Diagnostic>>) {
        *self.parse_diagnostics.borrow_mut() = Some(parse_diagnostics);
    }

    pub fn keep_strong_reference_to_symbol(&self, symbol: Rc<Symbol>) {
        self._symbols_without_a_symbol_table_strong_references
            .borrow_mut()
            .push(symbol);
    }
}

impl SourceFileLike for SourceFile {
    fn text(&self) -> &str {
        &self.text
    }

    fn text_as_chars(&self) -> &SourceTextAsChars {
        &self.text_as_chars
    }

    fn maybe_line_map(&self) -> RefMut<Option<Vec<usize>>> {
        self.line_map.borrow_mut()
    }

    fn line_map(&self) -> Ref<Vec<usize>> {
        Ref::map(self.line_map.borrow(), |line_map| {
            line_map.as_ref().unwrap()
        })
    }

    fn maybe_get_position_of_line_and_character(
        &self,
        line: usize,
        character: usize,
        allow_edits: Option<bool>,
    ) -> Option<usize> {
        None
    }
}

// impl From<Rc<SourceFile>> for Rc<Node> {
//     fn from(source_file: Rc<SourceFile>) -> Self {
//         let rc = Rc::new(Node::SourceFile(source_file));
//         rc.set_node_wrapper(rc.clone());
//         rc
//     }
// }

pub fn rc_source_file_into_rc_node(source_file: Rc<SourceFile>) -> Rc<Node> {
    let rc = Rc::new(Node::SourceFile(source_file));
    rc.set_node_wrapper(rc.clone());
    rc
}

pub trait Program: TypeCheckerHost {
    fn get_syntactic_diagnostics(&mut self) -> Vec<Rc<Diagnostic /*DiagnosticWithLocation*/>>;
    fn get_semantic_diagnostics(&mut self) -> Vec<Rc<Diagnostic>>;
}
