#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::cell::Cell;
use std::rc::Rc;

use super::{
    BaseNamedDeclaration, BaseNode, BaseSignatureDeclaration, HasExpressionInterface,
    HasIsTypeOnlyInterface, HasTypeInterface, NamedDeclarationInterface, Node, NodeArray,
    SyntaxKind, TextRange,
};
use local_macros::ast_type;

#[derive(Debug)]
#[ast_type]
pub struct ExternalModuleReference {
    _node: BaseNode,
    pub expression: Rc<Node /*Expression*/>,
}

impl ExternalModuleReference {
    pub fn new(base_node: BaseNode, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ExternalModuleReference {
    fn expression(&self) -> Rc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportDeclaration {
    _node: BaseNode,
    pub import_clause: Option<Rc<Node /*ImportClause*/>>,
    pub module_specifier: Rc<Node /*Expression*/>,
    pub assert_clause: Option<Rc<Node /*AssertClause*/>>,
}

impl ImportDeclaration {
    pub fn new(
        base_node: BaseNode,
        import_clause: Option<Rc<Node>>,
        module_specifier: Rc<Node>,
        assert_clause: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            import_clause,
            module_specifier,
            assert_clause,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportClause {
    _node: BaseNode,
    pub is_type_only: bool,
    pub name: Option<Rc<Node /*Identifier*/>>,
    pub named_bindings: Option<Rc<Node /*NamedImportBindings*/>>,
}

impl ImportClause {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        name: Option<Rc<Node>>,
        named_bindings: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            is_type_only,
            name,
            named_bindings,
        }
    }
}

impl NamedDeclarationInterface for ImportClause {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

impl HasIsTypeOnlyInterface for ImportClause {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct AssertEntry {
    _node: BaseNode,
    pub name: Rc<Node /*AssertionKey*/>,
    pub value: Rc<Node /*StringLiteral*/>,
}

impl AssertEntry {
    pub fn new(base_node: BaseNode, name: Rc<Node>, value: Rc<Node /*StringLiteral*/>) -> Self {
        Self {
            _node: base_node,
            name,
            value,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct AssertClause {
    _node: BaseNode,
    pub elements: NodeArray, /*<AssertEntry>*/
    pub multi_line: Option<bool>,
}

impl AssertClause {
    pub fn new(base_node: BaseNode, elements: NodeArray, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            elements,
            multi_line,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamespaceImport {
    _node: BaseNode,
    pub name: Rc<Node /*Identifier*/>,
}

impl NamespaceImport {
    pub fn new(base_node: BaseNode, name: Rc<Node /*Identifier*/>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamespaceExport {
    _node: BaseNode,
    pub name: Rc<Node /*Identifier*/>,
}

impl NamespaceExport {
    pub fn new(base_node: BaseNode, name: Rc<Node /*Identifier*/>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "NamedDeclarationInterface")]
pub struct NamespaceExportDeclaration {
    _named_declaration: BaseNamedDeclaration,
}

impl NamespaceExportDeclaration {
    pub fn new(base_named_declaration: BaseNamedDeclaration) -> Self {
        Self {
            _named_declaration: base_named_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportDeclaration {
    _node: BaseNode,
    pub is_type_only: bool,
    pub export_clause: Option<Rc<Node /*NamedExportBindings*/>>,
    pub module_specifier: Option<Rc<Node /*Expression*/>>,
    pub assert_clause: Option<Rc<Node /*AssertClause*/>>,
}

impl ExportDeclaration {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        export_clause: Option<Rc<Node>>,
        module_specifier: Option<Rc<Node>>,
        assert_clause: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            is_type_only,
            export_clause,
            module_specifier,
            assert_clause,
        }
    }
}

impl HasIsTypeOnlyInterface for ExportDeclaration {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamedImports {
    _node: BaseNode,
    pub elements: NodeArray, /*<ImportSpecifier>*/
}

impl NamedImports {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamedExports {
    _node: BaseNode,
    pub elements: NodeArray, /*<ExportSpecifier>*/
}

impl NamedExports {
    pub fn new(base_node: BaseNode, elements: NodeArray) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportSpecifier {
    _node: BaseNode,
    pub property_name: Option<Rc<Node /*Identifier*/>>,
    pub name: Rc<Node /*Identifier*/>,
    pub is_type_only: bool,
}

impl ImportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Rc<Node>>,
        name: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            property_name,
            name,
            is_type_only,
        }
    }
}

impl NamedDeclarationInterface for ImportSpecifier {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = name;
    }
}

impl HasIsTypeOnlyInterface for ImportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportSpecifier {
    _node: BaseNode,
    pub is_type_only: bool,
    pub property_name: Option<Rc<Node /*Identifier*/>>,
    pub name: Rc<Node /*Identifier*/>,
}

impl ExportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Rc<Node>>,
        name: Rc<Node>,
    ) -> Self {
        Self {
            _node: base_node,
            is_type_only,
            property_name,
            name,
        }
    }
}

impl NamedDeclarationInterface for ExportSpecifier {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = name;
    }
}

impl HasIsTypeOnlyInterface for ExportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportAssignment {
    _node: BaseNode,
    pub is_export_equals: Option<bool>,
    pub expression: Rc<Node /*Expression*/>,
}

// TODO: should implement HasExpressionInterface for ExportAssignment?
impl ExportAssignment {
    pub fn new(base_node: BaseNode, is_export_equals: Option<bool>, expression: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            is_export_equals,
            expression,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FileReference {
    pos: Cell<isize>,
    end: Cell<isize>,
    file_name: String,
}

impl TextRange for FileReference {
    fn pos(&self) -> isize {
        self.pos.get()
    }

    fn set_pos(&self, pos: isize) {
        self.pos.set(pos);
    }

    fn end(&self) -> isize {
        self.end.get()
    }

    fn set_end(&self, end: isize) {
        self.end.set(end);
    }
}

pub type CommentKind = SyntaxKind; /*SyntaxKind.SingleLineCommentTrivia | SyntaxKind.MultiLineCommentTrivia*/

#[derive(Clone, Debug)]
pub struct CommentRange {
    pos: Cell<isize>,
    end: Cell<isize>,
    has_trailing_new_line: Option<bool>,
    kind: CommentKind,
}

impl CommentRange {
    pub fn new(
        kind: CommentKind,
        pos: isize,
        end: isize,
        has_trailing_new_line: Option<bool>,
    ) -> Self {
        Self {
            kind,
            pos: Cell::new(pos),
            end: Cell::new(end),
            has_trailing_new_line,
        }
    }
}

impl TextRange for CommentRange {
    fn pos(&self) -> isize {
        self.pos.get()
    }

    fn set_pos(&self, pos: isize) {
        self.pos.set(pos);
    }

    fn end(&self) -> isize {
        self.end.get()
    }

    fn set_end(&self, end: isize) {
        self.end.set(end);
    }
}

// TODO: should eg implement a CommentRangeInterface for CommentRange + SynthesizedComment?
#[derive(Debug)]
pub struct SynthesizedComment {
    has_trailing_new_line: Option<bool>,
    kind: CommentKind,
    text: String,
    has_leading_new_line: Option<bool>,
}

impl TextRange for SynthesizedComment {
    fn pos(&self) -> isize {
        -1
    }

    fn set_pos(&self, pos: isize) {
        panic!("Shouldn't call set_pos() on a SynthesizedComment")
    }

    fn end(&self) -> isize {
        -1
    }

    fn set_end(&self, end: isize) {
        panic!("Shouldn't call set_end() on a SynthesizedComment")
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocTypeExpression {
    _node: BaseNode,
    pub type_: Rc<Node /*TypeNode*/>,
}

impl JSDocTypeExpression {
    pub fn new(base_node: BaseNode, type_: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for JSDocTypeExpression {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocNameReference {
    _node: BaseNode,
    pub name: Rc<Node /*EntityName | JSDocMemberName*/>,
}

impl JSDocNameReference {
    pub fn new(base_node: BaseNode, name: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for JSDocNameReference {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = name;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocMemberName {
    _node: BaseNode,
    pub left: Rc<Node /*EntityName | JSDocMemberName*/>,
    pub right: Rc<Node /*Identifier*/>,
}

impl JSDocMemberName {
    pub fn new(base_node: BaseNode, left: Rc<Node>, right: Rc<Node>) -> Self {
        Self {
            _node: base_node,
            left,
            right,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct BaseJSDocUnaryType {
    _node: BaseNode,
    pub type_: Option<Rc<Node /*TypeNode*/>>,
}

impl BaseJSDocUnaryType {
    pub fn new(base_node: BaseNode, type_: Option<Rc<Node>>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for BaseJSDocUnaryType {
    fn maybe_type(&self) -> Option<Rc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Rc<Node>>) {
        self.type_ = type_;
    }
}

#[derive(Debug)]
#[ast_type(
    interfaces = "NamedDeclarationInterface, HasTypeParametersInterface, GenericNamedDeclarationInterface, HasTypeInterface, SignatureDeclarationInterface"
)]
pub struct JSDocFunctionType {
    _signature_declaration: BaseSignatureDeclaration,
}

impl JSDocFunctionType {
    pub fn new(signature_declaration: BaseSignatureDeclaration) -> Self {
        Self {
            _signature_declaration: signature_declaration,
        }
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDoc {
    _node: BaseNode,
    pub tags: Option<NodeArray /*<JSDocTag>*/>,
    pub comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl JSDoc {
    pub fn new(
        base_node: BaseNode,
        comment: Option<StringOrNodeArray>,
        tags: Option<NodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            tags,
            comment,
        }
    }
}

pub trait JSDocTagInterface {
    fn tag_name(&self) -> Rc<Node /*Identifier*/>;
    fn maybe_comment(&self) -> Option<&StringOrNodeArray /*<JSDocComment>*/>;
}

pub trait JSDocLinkLikeInterface {
    fn maybe_name(&self) -> Option<Rc<Node>>;
    fn text(&self) -> &str;
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLink {
    _node: BaseNode,
    pub name: Option<Rc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLink {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLink {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLinkCode {
    _node: BaseNode,
    pub name: Option<Rc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkCode {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkCode {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLinkPlain {
    _node: BaseNode,
    pub name: Option<Rc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkPlain {
    pub fn new(base_node: BaseNode, name: Option<Rc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkPlain {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocText {
    _node: BaseNode,
    pub text: String,
}

impl JSDocText {
    pub fn new(base_node: BaseNode, text: String) -> Self {
        Self {
            _node: base_node,
            text,
        }
    }
}

#[derive(Debug)]
pub enum StringOrNodeArray {
    String(String),
    NodeArray(NodeArray),
}

impl From<String> for StringOrNodeArray {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<NodeArray> for StringOrNodeArray {
    fn from(value: NodeArray) -> Self {
        Self::NodeArray(value)
    }
}

#[derive(Debug)]
#[ast_type]
pub struct BaseJSDocTag {
    _node: BaseNode,
    tag_name: Rc<Node /*Identifier*/>,
    comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl BaseJSDocTag {
    pub fn new(
        base_node: BaseNode,
        tag_name: Rc<Node>,
        comment: Option<StringOrNodeArray>,
    ) -> Self {
        Self {
            _node: base_node,
            tag_name,
            comment,
        }
    }
}

impl JSDocTagInterface for BaseJSDocTag {
    fn tag_name(&self) -> Rc<Node> {
        self.tag_name.clone()
    }

    fn maybe_comment(&self) -> Option<&StringOrNodeArray> {
        self.comment.as_ref()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocAugmentsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Rc<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocAugmentsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Rc<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocImplementsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Rc<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocImplementsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Rc<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

pub trait JSDocTypeLikeTagInterface: JSDocTagInterface {
    fn maybe_type_expression(&self) -> Option<Rc<Node>>;
    fn type_expression(&self) -> Rc<Node>;
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct BaseJSDocTypeLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
}

impl BaseJSDocTypeLikeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, type_expression: Option<Rc<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            type_expression,
        }
    }
}

impl JSDocTypeLikeTagInterface for BaseJSDocTypeLikeTag {
    fn maybe_type_expression(&self) -> Option<Rc<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Rc<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocTemplateTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub constraint: Option<Rc<Node /*JSDocTypeExpression*/>>,
    pub type_parameters: NodeArray, /*<TypeParameterDeclaration>*/
}

impl JSDocTemplateTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        constraint: Option<Rc<Node>>,
        type_parameters: NodeArray,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            constraint,
            type_parameters,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocSeeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Option<Rc<Node /*JSDocNameReference*/>>,
}

impl JSDocSeeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, name: Option<Rc<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            name,
        }
    }
}

pub trait JSDocTypedefOrCallbackTagInterface: NamedDeclarationInterface {
    fn maybe_full_name(&self) -> Option<Rc<Node>>;
    fn maybe_type_expression(&self) -> Option<Rc<Node>>;
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocTypedefTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Rc<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Rc<Node /*Identifier*/>>,
    pub type_expression: Option<Rc<Node /*JSDocTypeExpression | JSDocTypeLiteral*/>>,
}

impl JSDocTypedefTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        full_name: Option<Rc<Node>>,
        name: Option<Rc<Node>>,
        type_expression: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            full_name,
            name,
            type_expression,
        }
    }
}

impl NamedDeclarationInterface for JSDocTypedefTag {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

impl JSDocTypedefOrCallbackTagInterface for JSDocTypedefTag {
    fn maybe_full_name(&self) -> Option<Rc<Node>> {
        self.full_name.clone()
    }

    fn maybe_type_expression(&self) -> Option<Rc<Node>> {
        self.type_expression.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocCallbackTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Rc<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Rc<Node /*Identifier*/>>,
    pub type_expression: Rc<Node /*JSDocSignature*/>,
}

impl JSDocCallbackTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        type_expression: Rc<Node>,
        full_name: Option<Rc<Node>>,
        name: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            full_name,
            name,
            type_expression,
        }
    }
}

impl NamedDeclarationInterface for JSDocCallbackTag {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = Some(name);
    }
}

impl JSDocTypedefOrCallbackTagInterface for JSDocCallbackTag {
    fn maybe_full_name(&self) -> Option<Rc<Node>> {
        self.full_name.clone()
    }

    fn maybe_type_expression(&self) -> Option<Rc<Node>> {
        Some(self.type_expression.clone())
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocSignature {
    _node: BaseNode,
    pub type_parameters: Option<NodeArray /*<JSDocTemplateTag>*/>,
    pub parameters: NodeArray, /*<JSDocParameterTag>*/
    pub type_: Option<Rc<Node /*JSDocReturnTag*/>>,
}

impl JSDocSignature {
    pub fn new(
        base_node: BaseNode,
        type_parameters: Option<NodeArray>,
        parameters: NodeArray,
        type_: Option<Rc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            type_parameters,
            parameters,
            type_,
        }
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocPropertyLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Rc<Node /*EntityName*/>,
    pub type_expression: Option<Rc<Node /*JSDocTypeExpression*/>>,
    pub is_name_first: bool,
    pub is_bracketed: bool,
}

impl JSDocPropertyLikeTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        type_expression: Option<Rc<Node>>,
        name: Rc<Node>,
        is_name_first: bool,
        is_bracketed: bool,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            name,
            type_expression,
            is_name_first,
            is_bracketed,
        }
    }
}

impl NamedDeclarationInterface for JSDocPropertyLikeTag {
    fn maybe_name(&self) -> Option<Rc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Rc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Rc<Node>) {
        self.name = name;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocTypeLiteral {
    _node: BaseNode,
    pub js_doc_property_tags: Option<NodeArray /*<JSDocPropertyLikeTag>*/>,
    pub is_array_type: bool,
}

impl JSDocTypeLiteral {
    pub fn new(
        base_node: BaseNode,
        js_doc_property_tags: Option<NodeArray>,
        is_array_type: bool,
    ) -> Self {
        Self {
            _node: base_node,
            js_doc_property_tags,
            is_array_type,
        }
    }
}

bitflags! {
    pub struct FlowFlags: u32 {
        const None = 0;
        const Unreachable = 1 << 0;
        const Start = 1 << 1;
        const BranchLabel = 1 << 2;
        const LoopLabel = 1 << 3;
        const Assignment = 1 << 4;
        const TrueCondition = 1 << 5;
        const FalseCondition = 1 << 6;
        const SwitchClause = 1 << 7;
        const ArrayMutation = 1 << 8;
        const Call = 1 << 9;
        const ReduceLabel = 1 << 10;
        const Referenced = 1 << 11;
        const Shared = 1 << 12;

        const Label = Self::BranchLabel.bits | Self::LoopLabel.bits;
        const Condition = Self::TrueCondition.bits | Self::FalseCondition.bits;
    }
}

#[derive(Debug)]
pub enum FlowNode {
    FlowStart(FlowStart),
    FlowLabel(FlowLabel),
    FlowAssignment(FlowAssignment),
    FlowCall(FlowCall),
    FlowCondition(FlowCondition),
    FlowSwitchClause(FlowSwitchClause),
    FlowArrayMutation(FlowArrayMutation),
    FlowReduceLabel(FlowReduceLabel),
}

pub trait FlowNodeBase {
    fn flags(&self) -> FlowFlags;
    fn id(&self) -> Option<usize>;
}

#[derive(Debug)]
pub struct FlowStart {
    flags: FlowFlags,
    id: Option<usize>,
    pub node: Option<
        Rc<
            Node, /*FunctionExpression | ArrowFunction | MethodDeclaration | GetAccessorDeclaration | SetAccessorDeclaration*/
        >,
    >,
}

impl FlowStart {
    pub fn new(flags: FlowFlags, id: Option<usize>, node: Option<Rc<Node>>) -> Self {
        Self { flags, id, node }
    }
}

impl FlowNodeBase for FlowStart {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowStart> for FlowNode {
    fn from(value: FlowStart) -> Self {
        Self::FlowStart(value)
    }
}

#[derive(Debug)]
pub struct FlowLabel {
    flags: FlowFlags,
    id: Option<usize>,
    pub antecedents: Option<Vec<Rc<FlowNode>>>,
}

impl FlowNodeBase for FlowLabel {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowLabel> for FlowNode {
    fn from(value: FlowLabel) -> Self {
        Self::FlowLabel(value)
    }
}

#[derive(Debug)]
pub struct FlowAssignment {
    flags: FlowFlags,
    id: Option<usize>,
    pub node: Rc<Node /*Expression | VariableDeclaration | BindingElement*/>,
    pub antecedent: Rc<FlowNode>,
}

impl FlowNodeBase for FlowAssignment {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowAssignment> for FlowNode {
    fn from(value: FlowAssignment) -> Self {
        Self::FlowAssignment(value)
    }
}

#[derive(Debug)]
pub struct FlowCall {
    flags: FlowFlags,
    id: Option<usize>,
    pub node: Rc<Node /*CallExpression*/>,
    pub antecedent: Rc<FlowNode>,
}

impl FlowNodeBase for FlowCall {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowCall> for FlowNode {
    fn from(value: FlowCall) -> Self {
        Self::FlowCall(value)
    }
}

#[derive(Debug)]
pub struct FlowCondition {
    flags: FlowFlags,
    id: Option<usize>,
    pub node: Rc<Node /*Expression*/>,
    pub antecedent: Rc<FlowNode>,
}

impl FlowNodeBase for FlowCondition {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowCondition> for FlowNode {
    fn from(value: FlowCondition) -> Self {
        Self::FlowCondition(value)
    }
}

#[derive(Debug)]
pub struct FlowSwitchClause {
    flags: FlowFlags,
    id: Option<usize>,
    pub switch_statement: Rc<Node /*SwitchStatement*/>,
    pub clause_start: usize,
    pub clause_end: usize,
    pub antecedent: Rc<FlowNode>,
}

impl FlowNodeBase for FlowSwitchClause {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowSwitchClause> for FlowNode {
    fn from(value: FlowSwitchClause) -> Self {
        Self::FlowSwitchClause(value)
    }
}

#[derive(Debug)]
pub struct FlowArrayMutation {
    flags: FlowFlags,
    id: Option<usize>,
    pub node: Rc<Node /*CallExpression | BinaryExpression*/>,
    pub antecedent: Rc<FlowNode>,
}

impl FlowNodeBase for FlowArrayMutation {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowArrayMutation> for FlowNode {
    fn from(value: FlowArrayMutation) -> Self {
        Self::FlowArrayMutation(value)
    }
}

#[derive(Debug)]
pub struct FlowReduceLabel {
    flags: FlowFlags,
    id: Option<usize>,
    pub target: Rc<FlowNode /*FlowLabel*/>,
    pub antecedents: Vec<Rc<FlowNode>>,
    pub antecedent: Rc<FlowNode>,
}

impl FlowNodeBase for FlowReduceLabel {
    fn flags(&self) -> FlowFlags {
        self.flags
    }

    fn id(&self) -> Option<usize> {
        self.id
    }
}

impl From<FlowReduceLabel> for FlowNode {
    fn from(value: FlowReduceLabel) -> Self {
        Self::FlowReduceLabel(value)
    }
}
