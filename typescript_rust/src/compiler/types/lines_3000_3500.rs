use std::cell::{Cell, RefCell, Ref, RefMut};

use bitflags::bitflags;
use local_macros::{ast_type, enum_unwrapped};

use super::{
    BaseNamedDeclaration, BaseNode, BaseSignatureDeclaration, HasElementsInterface,
    HasExpressionInterface, HasIsTypeOnlyInterface, HasLeftAndRightInterface, HasTypeInterface,
    HasTypeParametersInterface, NamedDeclarationInterface, Node, NodeArray,
    SignatureDeclarationInterface, SyntaxKind, TextRange,
};

#[derive(Debug)]
#[ast_type]
pub struct ExternalModuleReference {
    _node: BaseNode,
    pub expression: Id<Node /*Expression*/>,
}

impl ExternalModuleReference {
    pub fn new(base_node: BaseNode, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ExternalModuleReference {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportDeclaration {
    _node: BaseNode,
    pub import_clause: Option<Id<Node /*ImportClause*/>>,
    pub module_specifier: Id<Node /*Expression*/>,
    pub assert_clause: Option<Id<Node /*AssertClause*/>>,
}

impl ImportDeclaration {
    pub fn new(
        base_node: BaseNode,
        import_clause: Option<Id<Node>>,
        module_specifier: Id<Node>,
        assert_clause: Option<Id<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            import_clause,
            module_specifier,
            assert_clause,
        }
    }
}

impl HasAssertClauseInterface for ImportDeclaration {
    fn maybe_assert_clause(&self) -> Option<Id<Node>> {
        self.assert_clause.clone()
    }
}

pub trait HasModuleSpecifierInterface {
    fn maybe_module_specifier(&self) -> Option<Id<Node>>;
}

impl HasModuleSpecifierInterface for ImportDeclaration {
    fn maybe_module_specifier(&self) -> Option<Id<Node>> {
        Some(self.module_specifier.clone())
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ImportClause {
    _node: BaseNode,
    pub is_type_only: bool,
    pub name: Option<Id<Node /*Identifier*/>>,
    pub named_bindings: Option<Id<Node /*NamedImportBindings*/>>,
}

impl ImportClause {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        name: Option<Id<Node>>,
        named_bindings: Option<Id<Node>>,
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
    fn maybe_name(&self) -> Option<Id<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Id<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Id<Node>) {
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
    pub name: Id<Node /*AssertionKey*/>,
    pub value: Id<Node /*StringLiteral*/>,
}

impl AssertEntry {
    pub fn new(base_node: BaseNode, name: Id<Node>, value: Id<Node /*StringLiteral*/>) -> Self {
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
    pub elements: Id<NodeArray /*<AssertEntry>*/>,
    pub multi_line: Option<bool>,
}

impl AssertClause {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>, multi_line: Option<bool>) -> Self {
        Self {
            _node: base_node,
            elements,
            multi_line,
        }
    }
}

impl HasElementsInterface for AssertClause {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamespaceImport {
    _node: BaseNode,
    pub name: Id<Node /*Identifier*/>,
}

impl NamespaceImport {
    pub fn new(base_node: BaseNode, name: Id<Node /*Identifier*/>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for NamespaceImport {
    fn maybe_name(&self) -> Option<Id<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Id<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = name;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamespaceExport {
    _node: BaseNode,
    pub name: Id<Node /*Identifier*/>,
}

impl NamespaceExport {
    pub fn new(base_node: BaseNode, name: Id<Node /*Identifier*/>) -> Self {
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

pub trait HasAssertClauseInterface {
    fn maybe_assert_clause(&self) -> Option<Id<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct ExportDeclaration {
    _node: BaseNode,
    pub is_type_only: bool,
    pub export_clause: Option<Id<Node /*NamedExportBindings*/>>,
    pub module_specifier: Option<Id<Node /*Expression*/>>,
    pub assert_clause: Option<Id<Node /*AssertClause*/>>,
}

impl ExportDeclaration {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        export_clause: Option<Id<Node>>,
        module_specifier: Option<Id<Node>>,
        assert_clause: Option<Id<Node>>,
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

impl HasAssertClauseInterface for ExportDeclaration {
    fn maybe_assert_clause(&self) -> Option<Id<Node>> {
        self.assert_clause.clone()
    }
}

impl HasModuleSpecifierInterface for ExportDeclaration {
    fn maybe_module_specifier(&self) -> Option<Id<Node>> {
        self.module_specifier.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamedImports {
    _node: BaseNode,
    pub elements: Id<NodeArray>, /*<ImportSpecifier>*/
}

impl NamedImports {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for NamedImports {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct NamedExports {
    _node: BaseNode,
    pub elements: Id<NodeArray>, /*<ExportSpecifier>*/
}

impl NamedExports {
    pub fn new(base_node: BaseNode, elements: Id<NodeArray>) -> Self {
        Self {
            _node: base_node,
            elements,
        }
    }
}

impl HasElementsInterface for NamedExports {
    fn elements(&self) -> Id<NodeArray> {
        self.elements.clone()
    }
}

pub trait HasPropertyNameInterface {
    fn maybe_property_name(&self) -> Option<Id<Node>>;
}

#[derive(Debug)]
#[ast_type]
pub struct ImportSpecifier {
    _node: BaseNode,
    pub property_name: Option<Id<Node /*Identifier*/>>,
    pub name: Id<Node /*Identifier*/>,
    pub is_type_only: bool,
}

impl ImportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Id<Node>>,
        name: Id<Node>,
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
    fn maybe_name(&self) -> Option<Id<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Id<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = name;
    }
}

impl HasIsTypeOnlyInterface for ImportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

impl HasPropertyNameInterface for ImportSpecifier {
    fn maybe_property_name(&self) -> Option<Id<Node>> {
        self.property_name.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportSpecifier {
    _node: BaseNode,
    pub is_type_only: bool,
    pub property_name: Option<Id<Node /*Identifier*/>>,
    pub name: Id<Node /*Identifier*/>,
}

impl ExportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Id<Node>>,
        name: Id<Node>,
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
    fn maybe_name(&self) -> Option<Id<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Id<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = name;
    }
}

impl HasIsTypeOnlyInterface for ExportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

impl HasPropertyNameInterface for ExportSpecifier {
    fn maybe_property_name(&self) -> Option<Id<Node>> {
        self.property_name.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct ExportAssignment {
    _node: BaseNode,
    pub is_export_equals: Option<bool>,
    pub expression: Id<Node /*Expression*/>,
}

// TODO: should implement HasExpressionInterface for ExportAssignment?
impl ExportAssignment {
    pub fn new(base_node: BaseNode, is_export_equals: Option<bool>, expression: Id<Node>) -> Self {
        Self {
            _node: base_node,
            is_export_equals,
            expression,
        }
    }
}

impl HasExpressionInterface for ExportAssignment {
    fn expression(&self) -> Id<Node> {
        self.expression.clone()
    }
}

#[derive(Clone, Debug)]
pub struct FileReference {
    pos: Cell<isize>,
    end: Cell<isize>,
    pub file_name: String,
}

impl FileReference {
    pub fn new(pos: isize, end: isize, file_name: String) -> Self {
        Self {
            pos: Cell::new(pos),
            end: Cell::new(end),
            file_name,
        }
    }
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

#[derive(Clone, Debug)]
pub struct CheckJsDirective {
    pos: Cell<isize>,
    end: Cell<isize>,
    pub enabled: bool,
}

impl CheckJsDirective {
    pub fn new(pos: isize, end: isize, enabled: bool) -> Self {
        Self {
            pos: Cell::new(pos),
            end: Cell::new(end),
            enabled,
        }
    }
}

impl TextRange for CheckJsDirective {
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
    pub has_trailing_new_line: Option<bool>,
    pub kind: CommentKind,
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
    pub has_trailing_new_line: Option<bool>,
    pub kind: CommentKind,
    pub text: String,
    pub has_leading_new_line: Option<bool>,
}

impl TextRange for SynthesizedComment {
    fn pos(&self) -> isize {
        -1
    }

    fn set_pos(&self, _pos: isize) {
        panic!("Shouldn't call set_pos() on a SynthesizedComment")
    }

    fn end(&self) -> isize {
        -1
    }

    fn set_end(&self, _end: isize) {
        panic!("Shouldn't call set_end() on a SynthesizedComment")
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocTypeExpression {
    _node: BaseNode,
    pub type_: Id<Node /*TypeNode*/>,
}

impl JSDocTypeExpression {
    pub fn new(base_node: BaseNode, type_: Id<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for JSDocTypeExpression {
    fn maybe_type(&self) -> Option<Id<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocNameReference {
    _node: BaseNode,
    pub name: Id<Node /*EntityName | JSDocMemberName*/>,
}

impl JSDocNameReference {
    pub fn new(base_node: BaseNode, name: Id<Node>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for JSDocNameReference {
    fn maybe_name(&self) -> Option<Id<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Id<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = name;
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocMemberName {
    _node: BaseNode,
    pub left: Id<Node /*EntityName | JSDocMemberName*/>,
    pub right: Id<Node /*Identifier*/>,
}

impl JSDocMemberName {
    pub fn new(base_node: BaseNode, left: Id<Node>, right: Id<Node>) -> Self {
        Self {
            _node: base_node,
            left,
            right,
        }
    }
}

impl HasLeftAndRightInterface for JSDocMemberName {
    fn left(&self) -> Id<Node> {
        self.left.clone()
    }

    fn right(&self) -> Id<Node> {
        self.right.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct BaseJSDocUnaryType {
    _node: BaseNode,
    pub type_: Option<Id<Node /*TypeNode*/>>,
}

impl BaseJSDocUnaryType {
    pub fn new(base_node: BaseNode, type_: Option<Id<Node>>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for BaseJSDocUnaryType {
    fn maybe_type(&self) -> Option<Id<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
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
    pub tags: Option<Id<NodeArray> /*<JSDocTag>*/>,
    pub comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl JSDoc {
    pub fn new(
        base_node: BaseNode,
        comment: Option<StringOrNodeArray>,
        tags: Option<Id<NodeArray>>,
    ) -> Self {
        Self {
            _node: base_node,
            tags,
            comment,
        }
    }
}

pub trait JSDocTagInterface {
    fn tag_name(&self) -> Id<Node /*Identifier*/>;
    fn maybe_comment(&self) -> Option<&StringOrNodeArray /*<JSDocComment>*/>;
}

pub trait JSDocLinkLikeInterface {
    fn maybe_name(&self) -> Option<Id<Node>>;
    fn text(&self) -> &str;
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocLink {
    _node: BaseNode,
    pub name: Option<Id<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLink {
    pub fn new(base_node: BaseNode, name: Option<Id<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLink {
    fn maybe_name(&self) -> Option<Id<Node>> {
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
    pub name: Option<Id<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkCode {
    pub fn new(base_node: BaseNode, name: Option<Id<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkCode {
    fn maybe_name(&self) -> Option<Id<Node>> {
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
    pub name: Option<Id<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkPlain {
    pub fn new(base_node: BaseNode, name: Option<Id<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkPlain {
    fn maybe_name(&self) -> Option<Id<Node>> {
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

mod _StringOrNodeArrayDeriveTraceScope {
    use super::*;

    #[derive(Clone, Debug)]
    pub enum StringOrNodeArray {
        String(String),
        NodeArray(Id<NodeArray>),
    }
}
pub use _StringOrNodeArrayDeriveTraceScope::StringOrNodeArray;
use id_arena::Id;

impl From<String> for StringOrNodeArray {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<Id<NodeArray>> for StringOrNodeArray {
    fn from(value: Id<NodeArray>) -> Self {
        Self::NodeArray(value)
    }
}

#[derive(Debug)]
#[ast_type]
pub struct BaseJSDocTag {
    _node: BaseNode,
    tag_name: Id<Node /*Identifier*/>,
    comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl BaseJSDocTag {
    pub fn new(
        base_node: BaseNode,
        tag_name: Id<Node>,
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
    fn tag_name(&self) -> Id<Node> {
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
    pub class: Id<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocAugmentsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Id<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

pub trait JSDocHeritageTagInterface: JSDocTagInterface {
    fn class(&self) -> Id<Node>;
}

impl JSDocHeritageTagInterface for JSDocAugmentsTag {
    fn class(&self) -> Id<Node> {
        self.class.clone()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocImplementsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Id<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocImplementsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Id<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

impl JSDocHeritageTagInterface for JSDocImplementsTag {
    fn class(&self) -> Id<Node> {
        self.class.clone()
    }
}

pub trait JSDocTypeLikeTagInterface: JSDocTagInterface {
    fn maybe_type_expression(&self) -> Option<Id<Node>>;
    fn type_expression(&self) -> Id<Node>;
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct BaseJSDocTypeLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
}

impl BaseJSDocTypeLikeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, type_expression: Option<Id<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            type_expression,
        }
    }
}

impl JSDocTypeLikeTagInterface for BaseJSDocTypeLikeTag {
    fn maybe_type_expression(&self) -> Option<Id<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Id<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocTemplateTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub constraint: Option<Id<Node /*JSDocTypeExpression*/>>,
    pub type_parameters: Id<NodeArray>, /*<TypeParameterDeclaration>*/
    type_parameters_for_has_type_parameters_interface: Cell<Option<Id<NodeArray>>>, /*<TypeParameterDeclaration>*/
}

impl JSDocTemplateTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        constraint: Option<Id<Node>>,
        type_parameters: Id<NodeArray>,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            constraint,
            type_parameters: type_parameters.clone(),
            type_parameters_for_has_type_parameters_interface: Cell::new(Some(type_parameters)),
        }
    }
}

impl HasTypeParametersInterface for JSDocTemplateTag {
    fn maybe_type_parameters(&self) -> Option<Id<NodeArray>> {
        self.type_parameters_for_has_type_parameters_interface
            .get()
    }

    fn set_type_parameters(&self, type_parameters: Option<Id<NodeArray>>) {
        self.type_parameters_for_has_type_parameters_interface
            .set(type_parameters);
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocSeeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Option<Id<Node /*JSDocNameReference*/>>,
}

impl JSDocSeeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, name: Option<Id<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            name,
        }
    }
}

pub trait JSDocTypedefOrCallbackTagInterface:
    NamedDeclarationInterface + JSDocTypeLikeTagInterface
{
    fn maybe_full_name(&self) -> Option<Id<Node>>;
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocTypedefTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Id<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Id<Node /*Identifier*/>>,
    pub type_expression: Option<Id<Node /*JSDocTypeExpression | JSDocTypeLiteral*/>>,
}

impl JSDocTypedefTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        full_name: Option<Id<Node>>,
        name: Option<Id<Node>>,
        type_expression: Option<Id<Node>>,
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
    fn maybe_name(&self) -> Option<Id<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Id<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = Some(name);
    }
}

impl JSDocTypedefOrCallbackTagInterface for JSDocTypedefTag {
    fn maybe_full_name(&self) -> Option<Id<Node>> {
        self.full_name.clone()
    }
}

impl JSDocTypeLikeTagInterface for JSDocTypedefTag {
    fn maybe_type_expression(&self) -> Option<Id<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Id<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocCallbackTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Id<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Id<Node /*Identifier*/>>,
    pub type_expression: Id<Node /*JSDocSignature*/>,
}

impl JSDocCallbackTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        type_expression: Id<Node>,
        full_name: Option<Id<Node>>,
        name: Option<Id<Node>>,
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
    fn maybe_name(&self) -> Option<Id<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Id<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = Some(name);
    }
}

impl JSDocTypedefOrCallbackTagInterface for JSDocCallbackTag {
    fn maybe_full_name(&self) -> Option<Id<Node>> {
        self.full_name.clone()
    }
}

impl JSDocTypeLikeTagInterface for JSDocCallbackTag {
    fn maybe_type_expression(&self) -> Option<Id<Node>> {
        Some(self.type_expression.clone())
    }

    fn type_expression(&self) -> Id<Node> {
        self.type_expression.clone()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocSignature {
    _node: BaseNode,
    type_parameters: Cell<Option<Id<NodeArray> /*<JSDocTemplateTag>*/>>,
    pub parameters: Id<NodeArray>, /*<JSDocParameterTag>*/
    pub type_: Option<Id<Node /*JSDocReturnTag*/>>,
}

impl JSDocSignature {
    pub fn new(
        base_node: BaseNode,
        type_parameters: Option<Id<NodeArray>>,
        parameters: Id<NodeArray>,
        type_: Option<Id<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            type_parameters: Cell::new(type_parameters),
            parameters,
            type_,
        }
    }
}

impl SignatureDeclarationInterface for JSDocSignature {
    fn parameters(&self) -> Id<NodeArray> {
        self.parameters.clone()
    }
}

impl NamedDeclarationInterface for JSDocSignature {
    fn maybe_name(&self) -> Option<Id<Node>> {
        None
    }

    fn name(&self) -> Id<Node> {
        panic!("JSDocSignature doesn't have name")
    }

    fn set_name(&mut self, _name: Id<Node>) {
        panic!("Tried to set name of JSDocSignature")
    }
}

impl HasTypeInterface for JSDocSignature {
    fn maybe_type(&self) -> Option<Id<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Id<Node>>) {
        self.type_ = type_;
    }
}

impl HasTypeParametersInterface for JSDocSignature {
    fn maybe_type_parameters(&self) -> Option<Id<NodeArray>> {
        self.type_parameters.get()
    }

    fn set_type_parameters(&self, type_parameters: Option<Id<NodeArray>>) {
        self.type_parameters.set(type_parameters);
    }
}

#[derive(Debug)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocPropertyLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Id<Node /*EntityName*/>,
    pub type_expression: Option<Id<Node /*JSDocTypeExpression*/>>,
    pub is_name_first: bool,
    pub is_bracketed: bool,
}

impl JSDocPropertyLikeTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        type_expression: Option<Id<Node>>,
        name: Id<Node>,
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
    fn maybe_name(&self) -> Option<Id<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Id<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Id<Node>) {
        self.name = name;
    }
}

impl JSDocTypeLikeTagInterface for JSDocPropertyLikeTag {
    fn maybe_type_expression(&self) -> Option<Id<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Id<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug)]
#[ast_type]
pub struct JSDocTypeLiteral {
    _node: BaseNode,
    pub js_doc_property_tags: Option<Id<NodeArray /*<JSDocPropertyLikeTag>*/>>,
    pub is_array_type: bool,
}

impl JSDocTypeLiteral {
    pub fn new(
        base_node: BaseNode,
        js_doc_property_tags: Option<Id<NodeArray>>,
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

impl FlowNode {
    pub fn as_flow_start(&self) -> &FlowStart {
        enum_unwrapped!(self, [FlowNode, FlowStart])
    }

    pub fn as_flow_label(&self) -> &FlowLabel {
        enum_unwrapped!(self, [FlowNode, FlowLabel])
    }

    pub fn as_flow_call(&self) -> &FlowCall {
        enum_unwrapped!(self, [FlowNode, FlowCall])
    }

    pub fn as_flow_switch_clause(&self) -> &FlowSwitchClause {
        enum_unwrapped!(self, [FlowNode, FlowSwitchClause])
    }

    pub fn as_flow_reduce_label(&self) -> &FlowReduceLabel {
        enum_unwrapped!(self, [FlowNode, FlowReduceLabel])
    }

    pub fn as_flow_assignment(&self) -> &FlowAssignment {
        enum_unwrapped!(self, [FlowNode, FlowAssignment])
    }

    pub fn as_flow_array_mutation(&self) -> &FlowArrayMutation {
        enum_unwrapped!(self, [FlowNode, FlowArrayMutation])
    }

    pub fn as_flow_condition(&self) -> &FlowCondition {
        enum_unwrapped!(self, [FlowNode, FlowCondition])
    }

    pub fn as_has_antecedent(&self) -> &dyn HasAntecedentInterface {
        match self {
            Self::FlowAssignment(value) => value,
            Self::FlowCall(value) => value,
            Self::FlowCondition(value) => value,
            Self::FlowSwitchClause(value) => value,
            Self::FlowArrayMutation(value) => value,
            Self::FlowReduceLabel(value) => value,
            _ => panic!("Expected has antecedent"),
        }
    }
}

pub trait FlowNodeBase {
    fn flags(&self) -> FlowFlags;
    fn set_flags(&self, flags: FlowFlags);
    fn maybe_id(&self) -> Option<isize>;
    fn set_id(&self, id: Option<isize>);
}

impl FlowNodeBase for FlowNode {
    fn flags(&self) -> FlowFlags {
        match self {
            Self::FlowStart(flow_node) => flow_node.flags(),
            Self::FlowLabel(flow_node) => flow_node.flags(),
            Self::FlowAssignment(flow_node) => flow_node.flags(),
            Self::FlowCall(flow_node) => flow_node.flags(),
            Self::FlowCondition(flow_node) => flow_node.flags(),
            Self::FlowSwitchClause(flow_node) => flow_node.flags(),
            Self::FlowArrayMutation(flow_node) => flow_node.flags(),
            Self::FlowReduceLabel(flow_node) => flow_node.flags(),
        }
    }

    fn set_flags(&self, flags: FlowFlags) {
        match self {
            Self::FlowStart(flow_node) => flow_node.set_flags(flags),
            Self::FlowLabel(flow_node) => flow_node.set_flags(flags),
            Self::FlowAssignment(flow_node) => flow_node.set_flags(flags),
            Self::FlowCall(flow_node) => flow_node.set_flags(flags),
            Self::FlowCondition(flow_node) => flow_node.set_flags(flags),
            Self::FlowSwitchClause(flow_node) => flow_node.set_flags(flags),
            Self::FlowArrayMutation(flow_node) => flow_node.set_flags(flags),
            Self::FlowReduceLabel(flow_node) => flow_node.set_flags(flags),
        }
    }

    fn maybe_id(&self) -> Option<isize> {
        match self {
            Self::FlowStart(flow_node) => flow_node.maybe_id(),
            Self::FlowLabel(flow_node) => flow_node.maybe_id(),
            Self::FlowAssignment(flow_node) => flow_node.maybe_id(),
            Self::FlowCall(flow_node) => flow_node.maybe_id(),
            Self::FlowCondition(flow_node) => flow_node.maybe_id(),
            Self::FlowSwitchClause(flow_node) => flow_node.maybe_id(),
            Self::FlowArrayMutation(flow_node) => flow_node.maybe_id(),
            Self::FlowReduceLabel(flow_node) => flow_node.maybe_id(),
        }
    }

    fn set_id(&self, id: Option<isize>) {
        match self {
            Self::FlowStart(flow_node) => flow_node.set_id(id),
            Self::FlowLabel(flow_node) => flow_node.set_id(id),
            Self::FlowAssignment(flow_node) => flow_node.set_id(id),
            Self::FlowCall(flow_node) => flow_node.set_id(id),
            Self::FlowCondition(flow_node) => flow_node.set_id(id),
            Self::FlowSwitchClause(flow_node) => flow_node.set_id(id),
            Self::FlowArrayMutation(flow_node) => flow_node.set_id(id),
            Self::FlowReduceLabel(flow_node) => flow_node.set_id(id),
        }
    }
}

#[derive(Debug)]
pub struct FlowStart {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    node: Cell<
        Option<
            Id<
                Node, /*FunctionExpression | ArrowFunction | MethodDeclaration | GetAccessorDeclaration | SetAccessorDeclaration*/
            >,
        >,
    >,
}

impl FlowStart {
    pub fn new(flags: FlowFlags, node: Option<Id<Node>>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Default::default(),
            node: Cell::new(node),
        }
    }
}

impl FlowStart {
    pub fn maybe_node(&self) -> Option<Id<Node>> {
        self.node.get()
    }

    pub fn set_node(&self, node: Option<Id<Node>>) {
        self.node.set(node);
    }
}

impl FlowNodeBase for FlowStart {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl From<FlowStart> for FlowNode {
    fn from(value: FlowStart) -> Self {
        Self::FlowStart(value)
    }
}

#[derive(Debug)]
pub struct FlowLabel {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    antecedents: RefCell<Option<Vec<Id<FlowNode>>>>,
}

impl FlowLabel {
    pub fn new(flags: FlowFlags, antecedents: Option<Vec<Id<FlowNode>>>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            antecedents: RefCell::new(antecedents),
        }
    }

    pub fn maybe_antecedents(&self) -> Ref<Option<Vec<Id<FlowNode>>>> {
        self.antecedents.borrow()
    }

    pub fn maybe_antecedents_mut(&self) -> RefMut<Option<Vec<Id<FlowNode>>>> {
        self.antecedents.borrow_mut()
    }

    pub fn set_antecedents(&self, antecedents: Option<Vec<Id<FlowNode>>>) {
        *self.antecedents.borrow_mut() = antecedents;
    }
}

impl FlowNodeBase for FlowLabel {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl From<FlowLabel> for FlowNode {
    fn from(value: FlowLabel) -> Self {
        Self::FlowLabel(value)
    }
}

pub trait HasAntecedentInterface {
    fn antecedent(&self) -> Id<FlowNode>;
}

#[derive(Debug)]
pub struct FlowAssignment {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Id<Node /*Expression | VariableDeclaration | BindingElement*/>,
    pub antecedent: Id<FlowNode>,
}

impl FlowAssignment {
    pub fn new(flags: FlowFlags, antecedent: Id<FlowNode>, node: Id<Node>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            node,
            antecedent,
        }
    }
}

impl FlowNodeBase for FlowAssignment {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl HasAntecedentInterface for FlowAssignment {
    fn antecedent(&self) -> Id<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowAssignment> for FlowNode {
    fn from(value: FlowAssignment) -> Self {
        Self::FlowAssignment(value)
    }
}

#[derive(Debug)]
pub struct FlowCall {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Id<Node /*CallExpression*/>,
    pub antecedent: Id<FlowNode>,
}

impl FlowCall {
    pub fn new(flags: FlowFlags, antecedent: Id<FlowNode>, node: Id<Node>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            node,
            antecedent,
        }
    }
}

impl FlowNodeBase for FlowCall {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl HasAntecedentInterface for FlowCall {
    fn antecedent(&self) -> Id<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowCall> for FlowNode {
    fn from(value: FlowCall) -> Self {
        Self::FlowCall(value)
    }
}

#[derive(Debug)]
pub struct FlowCondition {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Id<Node /*Expression*/>,
    pub antecedent: Id<FlowNode>,
}

impl FlowCondition {
    pub fn new(flags: FlowFlags, antecedent: Id<FlowNode>, node: Id<Node>) -> Self {
        Self {
            flags: Cell::new(flags),
            antecedent,
            node,
            id: Cell::new(None),
        }
    }
}

impl FlowNodeBase for FlowCondition {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl HasAntecedentInterface for FlowCondition {
    fn antecedent(&self) -> Id<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowCondition> for FlowNode {
    fn from(value: FlowCondition) -> Self {
        Self::FlowCondition(value)
    }
}

#[derive(Debug)]
pub struct FlowSwitchClause {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub switch_statement: Id<Node /*SwitchStatement*/>,
    pub clause_start: usize,
    pub clause_end: usize,
    pub antecedent: Id<FlowNode>,
}

impl FlowSwitchClause {
    pub fn new(
        flags: FlowFlags,
        antecedent: Id<FlowNode>,
        switch_statement: Id<Node>,
        clause_start: usize,
        clause_end: usize,
    ) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            switch_statement,
            clause_start,
            clause_end,
            antecedent,
        }
    }
}

impl FlowNodeBase for FlowSwitchClause {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl HasAntecedentInterface for FlowSwitchClause {
    fn antecedent(&self) -> Id<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowSwitchClause> for FlowNode {
    fn from(value: FlowSwitchClause) -> Self {
        Self::FlowSwitchClause(value)
    }
}

#[derive(Debug)]
pub struct FlowArrayMutation {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Id<Node /*CallExpression | BinaryExpression*/>,
    pub antecedent: Id<FlowNode>,
}

impl FlowArrayMutation {
    pub fn new(flags: FlowFlags, antecedent: Id<FlowNode>, node: Id<Node>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            node,
            antecedent,
        }
    }
}

impl FlowNodeBase for FlowArrayMutation {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl HasAntecedentInterface for FlowArrayMutation {
    fn antecedent(&self) -> Id<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowArrayMutation> for FlowNode {
    fn from(value: FlowArrayMutation) -> Self {
        Self::FlowArrayMutation(value)
    }
}

#[derive(Debug)]
pub struct FlowReduceLabel {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub target: Id<FlowNode /*FlowLabel*/>,
    pub antecedents: Vec<Id<FlowNode>>,
    pub antecedent: Id<FlowNode>,
}

impl FlowReduceLabel {
    pub fn new(
        flags: FlowFlags,
        target: Id<FlowNode>,
        antecedents: Vec<Id<FlowNode>>,
        antecedent: Id<FlowNode>,
    ) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            target,
            antecedents,
            antecedent,
        }
    }
}

impl FlowNodeBase for FlowReduceLabel {
    fn flags(&self) -> FlowFlags {
        self.flags.get()
    }

    fn set_flags(&self, flags: FlowFlags) {
        self.flags.set(flags)
    }

    fn maybe_id(&self) -> Option<isize> {
        self.id.get()
    }

    fn set_id(&self, id: Option<isize>) {
        self.id.set(id);
    }
}

impl HasAntecedentInterface for FlowReduceLabel {
    fn antecedent(&self) -> Id<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowReduceLabel> for FlowNode {
    fn from(value: FlowReduceLabel) -> Self {
        Self::FlowReduceLabel(value)
    }
}
