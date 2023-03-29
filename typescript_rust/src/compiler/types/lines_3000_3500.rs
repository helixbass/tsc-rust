#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use std::cell::Cell;

use super::{
    BaseNamedDeclaration, BaseNode, BaseSignatureDeclaration, HasElementsInterface,
    HasExpressionInterface, HasIsTypeOnlyInterface, HasLeftAndRightInterface, HasTypeInterface,
    HasTypeParametersInterface, NamedDeclarationInterface, Node, NodeArray,
    SignatureDeclarationInterface, SyntaxKind, TextRange,
};
use local_macros::{ast_type, enum_unwrapped};

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ExternalModuleReference {
    _node: BaseNode,
    pub expression: Gc<Node /*Expression*/>,
}

impl ExternalModuleReference {
    pub fn new(base_node: BaseNode, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            expression,
        }
    }
}

impl HasExpressionInterface for ExternalModuleReference {
    fn expression(&self) -> Gc<Node> {
        self.expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ImportDeclaration {
    _node: BaseNode,
    pub import_clause: Option<Gc<Node /*ImportClause*/>>,
    pub module_specifier: Gc<Node /*Expression*/>,
    pub assert_clause: Option<Gc<Node /*AssertClause*/>>,
}

impl ImportDeclaration {
    pub fn new(
        base_node: BaseNode,
        import_clause: Option<Gc<Node>>,
        module_specifier: Gc<Node>,
        assert_clause: Option<Gc<Node>>,
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
    fn maybe_assert_clause(&self) -> Option<Gc<Node>> {
        self.assert_clause.clone()
    }
}

pub trait HasModuleSpecifierInterface {
    fn maybe_module_specifier(&self) -> Option<Gc<Node>>;
}

impl HasModuleSpecifierInterface for ImportDeclaration {
    fn maybe_module_specifier(&self) -> Option<Gc<Node>> {
        Some(self.module_specifier.clone())
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ImportClause {
    _node: BaseNode,
    pub is_type_only: bool,
    pub name: Option<Gc<Node /*Identifier*/>>,
    pub named_bindings: Option<Gc<Node /*NamedImportBindings*/>>,
}

impl ImportClause {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        name: Option<Gc<Node>>,
        named_bindings: Option<Gc<Node>>,
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
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = Some(name);
    }
}

impl HasIsTypeOnlyInterface for ImportClause {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct AssertEntry {
    _node: BaseNode,
    pub name: Gc<Node /*AssertionKey*/>,
    pub value: Gc<Node /*StringLiteral*/>,
}

impl AssertEntry {
    pub fn new(base_node: BaseNode, name: Gc<Node>, value: Gc<Node /*StringLiteral*/>) -> Self {
        Self {
            _node: base_node,
            name,
            value,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
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

impl HasElementsInterface for AssertClause {
    fn elements(&self) -> &NodeArray {
        &self.elements
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct NamespaceImport {
    _node: BaseNode,
    pub name: Gc<Node /*Identifier*/>,
}

impl NamespaceImport {
    pub fn new(base_node: BaseNode, name: Gc<Node /*Identifier*/>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for NamespaceImport {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct NamespaceExport {
    _node: BaseNode,
    pub name: Gc<Node /*Identifier*/>,
}

impl NamespaceExport {
    pub fn new(base_node: BaseNode, name: Gc<Node /*Identifier*/>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

#[derive(Debug, Trace, Finalize)]
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
    fn maybe_assert_clause(&self) -> Option<Gc<Node>>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ExportDeclaration {
    _node: BaseNode,
    pub is_type_only: bool,
    pub export_clause: Option<Gc<Node /*NamedExportBindings*/>>,
    pub module_specifier: Option<Gc<Node /*Expression*/>>,
    pub assert_clause: Option<Gc<Node /*AssertClause*/>>,
}

impl ExportDeclaration {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        export_clause: Option<Gc<Node>>,
        module_specifier: Option<Gc<Node>>,
        assert_clause: Option<Gc<Node>>,
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
    fn maybe_assert_clause(&self) -> Option<Gc<Node>> {
        self.assert_clause.clone()
    }
}

impl HasModuleSpecifierInterface for ExportDeclaration {
    fn maybe_module_specifier(&self) -> Option<Gc<Node>> {
        self.module_specifier.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
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

impl HasElementsInterface for NamedImports {
    fn elements(&self) -> &NodeArray {
        &self.elements
    }
}

#[derive(Debug, Trace, Finalize)]
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

impl HasElementsInterface for NamedExports {
    fn elements(&self) -> &NodeArray {
        &self.elements
    }
}

pub trait HasPropertyNameInterface {
    fn maybe_property_name(&self) -> Option<Gc<Node>>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ImportSpecifier {
    _node: BaseNode,
    pub property_name: Option<Gc<Node /*Identifier*/>>,
    pub name: Gc<Node /*Identifier*/>,
    pub is_type_only: bool,
}

impl ImportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Gc<Node>>,
        name: Gc<Node>,
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
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

impl HasIsTypeOnlyInterface for ImportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

impl HasPropertyNameInterface for ImportSpecifier {
    fn maybe_property_name(&self) -> Option<Gc<Node>> {
        self.property_name.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ExportSpecifier {
    _node: BaseNode,
    pub is_type_only: bool,
    pub property_name: Option<Gc<Node /*Identifier*/>>,
    pub name: Gc<Node /*Identifier*/>,
}

impl ExportSpecifier {
    pub fn new(
        base_node: BaseNode,
        is_type_only: bool,
        property_name: Option<Gc<Node>>,
        name: Gc<Node>,
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
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

impl HasIsTypeOnlyInterface for ExportSpecifier {
    fn is_type_only(&self) -> bool {
        self.is_type_only
    }
}

impl HasPropertyNameInterface for ExportSpecifier {
    fn maybe_property_name(&self) -> Option<Gc<Node>> {
        self.property_name.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct ExportAssignment {
    _node: BaseNode,
    pub is_export_equals: Option<bool>,
    pub expression: Gc<Node /*Expression*/>,
}

// TODO: should implement HasExpressionInterface for ExportAssignment?
impl ExportAssignment {
    pub fn new(base_node: BaseNode, is_export_equals: Option<bool>, expression: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            is_export_equals,
            expression,
        }
    }
}

impl HasExpressionInterface for ExportAssignment {
    fn expression(&self) -> Gc<Node> {
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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocTypeExpression {
    _node: BaseNode,
    pub type_: Gc<Node /*TypeNode*/>,
}

impl JSDocTypeExpression {
    pub fn new(base_node: BaseNode, type_: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for JSDocTypeExpression {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        Some(self.type_.clone())
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_.unwrap();
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocNameReference {
    _node: BaseNode,
    pub name: Gc<Node /*EntityName | JSDocMemberName*/>,
}

impl JSDocNameReference {
    pub fn new(base_node: BaseNode, name: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            name,
        }
    }
}

impl NamedDeclarationInterface for JSDocNameReference {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocMemberName {
    _node: BaseNode,
    pub left: Gc<Node /*EntityName | JSDocMemberName*/>,
    pub right: Gc<Node /*Identifier*/>,
}

impl JSDocMemberName {
    pub fn new(base_node: BaseNode, left: Gc<Node>, right: Gc<Node>) -> Self {
        Self {
            _node: base_node,
            left,
            right,
        }
    }
}

impl HasLeftAndRightInterface for JSDocMemberName {
    fn left(&self) -> Gc<Node> {
        self.left.clone()
    }

    fn right(&self) -> Gc<Node> {
        self.right.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct BaseJSDocUnaryType {
    _node: BaseNode,
    pub type_: Option<Gc<Node /*TypeNode*/>>,
}

impl BaseJSDocUnaryType {
    pub fn new(base_node: BaseNode, type_: Option<Gc<Node>>) -> Self {
        Self {
            _node: base_node,
            type_,
        }
    }
}

impl HasTypeInterface for BaseJSDocUnaryType {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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
    fn tag_name(&self) -> Gc<Node /*Identifier*/>;
    fn maybe_comment(&self) -> Option<&StringOrNodeArray /*<JSDocComment>*/>;
}

pub trait JSDocLinkLikeInterface {
    fn maybe_name(&self) -> Option<Gc<Node>>;
    fn text(&self) -> &str;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocLink {
    _node: BaseNode,
    pub name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLink {
    pub fn new(base_node: BaseNode, name: Option<Gc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLink {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocLinkCode {
    _node: BaseNode,
    pub name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkCode {
    pub fn new(base_node: BaseNode, name: Option<Gc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkCode {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocLinkPlain {
    _node: BaseNode,
    pub name: Option<Gc<Node /*EntityName | JSDocMemberName*/>>,
    pub text: String,
}

impl JSDocLinkPlain {
    pub fn new(base_node: BaseNode, name: Option<Gc<Node>>, text: String) -> Self {
        Self {
            _node: base_node,
            name,
            text,
        }
    }
}

impl JSDocLinkLikeInterface for JSDocLinkPlain {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug, Trace, Finalize)]
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
    use local_macros::Trace;

    #[derive(Debug, Trace, Finalize)]
    pub enum StringOrNodeArray {
        String(String),
        NodeArray(NodeArray),
    }
}
pub use _StringOrNodeArrayDeriveTraceScope::StringOrNodeArray;

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

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct BaseJSDocTag {
    _node: BaseNode,
    tag_name: Gc<Node /*Identifier*/>,
    comment: Option<StringOrNodeArray /*<JSDocComment>*/>,
}

impl BaseJSDocTag {
    pub fn new(
        base_node: BaseNode,
        tag_name: Gc<Node>,
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
    fn tag_name(&self) -> Gc<Node> {
        self.tag_name.clone()
    }

    fn maybe_comment(&self) -> Option<&StringOrNodeArray> {
        self.comment.as_ref()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocAugmentsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Gc<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocAugmentsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Gc<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

pub trait JSDocHeritageTagInterface: JSDocTagInterface {
    fn class(&self) -> Gc<Node>;
}

impl JSDocHeritageTagInterface for JSDocAugmentsTag {
    fn class(&self) -> Gc<Node> {
        self.class.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocImplementsTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub class: Gc<
        Node, /*ExpressionWithTypeArguments & { readonly expression: Identifier | PropertyAccessEntityNameExpression*/
    >,
}

impl JSDocImplementsTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, class: Gc<Node>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            class,
        }
    }
}

impl JSDocHeritageTagInterface for JSDocImplementsTag {
    fn class(&self) -> Gc<Node> {
        self.class.clone()
    }
}

pub trait JSDocTypeLikeTagInterface: JSDocTagInterface {
    fn maybe_type_expression(&self) -> Option<Gc<Node>>;
    fn type_expression(&self) -> Gc<Node>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct BaseJSDocTypeLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
}

impl BaseJSDocTypeLikeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, type_expression: Option<Gc<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            type_expression,
        }
    }
}

impl JSDocTypeLikeTagInterface for BaseJSDocTypeLikeTag {
    fn maybe_type_expression(&self) -> Option<Gc<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Gc<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocTemplateTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub constraint: Option<Gc<Node /*JSDocTypeExpression*/>>,
    pub type_parameters: NodeArray, /*<TypeParameterDeclaration>*/
    type_parameters_for_has_type_parameters_interface: GcCell<Option<NodeArray>>, /*<TypeParameterDeclaration>*/
}

impl JSDocTemplateTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        constraint: Option<Gc<Node>>,
        type_parameters: NodeArray,
    ) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            constraint,
            type_parameters: type_parameters.clone(),
            type_parameters_for_has_type_parameters_interface: GcCell::new(Some(type_parameters)),
        }
    }
}

impl HasTypeParametersInterface for JSDocTemplateTag {
    fn maybe_type_parameters(&self) -> GcCellRef<Option<NodeArray>> {
        self.type_parameters_for_has_type_parameters_interface
            .borrow()
    }

    fn maybe_type_parameters_mut(&self) -> GcCellRefMut<Option<NodeArray>> {
        self.type_parameters_for_has_type_parameters_interface
            .borrow_mut()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocSeeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Option<Gc<Node /*JSDocNameReference*/>>,
}

impl JSDocSeeTag {
    pub fn new(base_jsdoc_tag: BaseJSDocTag, name: Option<Gc<Node>>) -> Self {
        Self {
            _base_jsdoc_tag: base_jsdoc_tag,
            name,
        }
    }
}

pub trait JSDocTypedefOrCallbackTagInterface:
    NamedDeclarationInterface + JSDocTypeLikeTagInterface
{
    fn maybe_full_name(&self) -> Option<Gc<Node>>;
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocTypedefTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Gc<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Gc<Node /*Identifier*/>>,
    pub type_expression: Option<Gc<Node /*JSDocTypeExpression | JSDocTypeLiteral*/>>,
}

impl JSDocTypedefTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        full_name: Option<Gc<Node>>,
        name: Option<Gc<Node>>,
        type_expression: Option<Gc<Node>>,
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
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = Some(name);
    }
}

impl JSDocTypedefOrCallbackTagInterface for JSDocTypedefTag {
    fn maybe_full_name(&self) -> Option<Gc<Node>> {
        self.full_name.clone()
    }
}

impl JSDocTypeLikeTagInterface for JSDocTypedefTag {
    fn maybe_type_expression(&self) -> Option<Gc<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Gc<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocCallbackTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub full_name: Option<Gc<Node /*JSDocNamespaceDeclaration | Identifier*/>>,
    pub name: Option<Gc<Node /*Identifier*/>>,
    pub type_expression: Gc<Node /*JSDocSignature*/>,
}

impl JSDocCallbackTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        type_expression: Gc<Node>,
        full_name: Option<Gc<Node>>,
        name: Option<Gc<Node>>,
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
    fn maybe_name(&self) -> Option<Gc<Node>> {
        self.name.clone()
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone().unwrap()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = Some(name);
    }
}

impl JSDocTypedefOrCallbackTagInterface for JSDocCallbackTag {
    fn maybe_full_name(&self) -> Option<Gc<Node>> {
        self.full_name.clone()
    }
}

impl JSDocTypeLikeTagInterface for JSDocCallbackTag {
    fn maybe_type_expression(&self) -> Option<Gc<Node>> {
        Some(self.type_expression.clone())
    }

    fn type_expression(&self) -> Gc<Node> {
        self.type_expression.clone()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type]
pub struct JSDocSignature {
    _node: BaseNode,
    type_parameters: GcCell<Option<NodeArray /*<JSDocTemplateTag>*/>>,
    pub parameters: NodeArray, /*<JSDocParameterTag>*/
    pub type_: Option<Gc<Node /*JSDocReturnTag*/>>,
}

impl JSDocSignature {
    pub fn new(
        base_node: BaseNode,
        type_parameters: Option<NodeArray>,
        parameters: NodeArray,
        type_: Option<Gc<Node>>,
    ) -> Self {
        Self {
            _node: base_node,
            type_parameters: GcCell::new(type_parameters),
            parameters,
            type_,
        }
    }
}

impl SignatureDeclarationInterface for JSDocSignature {
    fn parameters(&self) -> &NodeArray {
        &self.parameters
    }
}

impl NamedDeclarationInterface for JSDocSignature {
    fn maybe_name(&self) -> Option<Gc<Node>> {
        None
    }

    fn name(&self) -> Gc<Node> {
        panic!("JSDocSignature doesn't have name")
    }

    fn set_name(&mut self, name: Gc<Node>) {
        panic!("Tried to set name of JSDocSignature")
    }
}

impl HasTypeInterface for JSDocSignature {
    fn maybe_type(&self) -> Option<Gc<Node>> {
        self.type_.clone()
    }

    fn set_type(&mut self, type_: Option<Gc<Node>>) {
        self.type_ = type_;
    }
}

impl HasTypeParametersInterface for JSDocSignature {
    fn maybe_type_parameters(&self) -> GcCellRef<Option<NodeArray>> {
        self.type_parameters.borrow()
    }

    fn maybe_type_parameters_mut(&self) -> GcCellRefMut<Option<NodeArray>> {
        self.type_parameters.borrow_mut()
    }
}

#[derive(Debug, Trace, Finalize)]
#[ast_type(interfaces = "JSDocTagInterface")]
pub struct JSDocPropertyLikeTag {
    _base_jsdoc_tag: BaseJSDocTag,
    pub name: Gc<Node /*EntityName*/>,
    pub type_expression: Option<Gc<Node /*JSDocTypeExpression*/>>,
    pub is_name_first: bool,
    pub is_bracketed: bool,
}

impl JSDocPropertyLikeTag {
    pub fn new(
        base_jsdoc_tag: BaseJSDocTag,
        type_expression: Option<Gc<Node>>,
        name: Gc<Node>,
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
    fn maybe_name(&self) -> Option<Gc<Node>> {
        Some(self.name.clone())
    }

    fn name(&self) -> Gc<Node> {
        self.name.clone()
    }

    fn set_name(&mut self, name: Gc<Node>) {
        self.name = name;
    }
}

impl JSDocTypeLikeTagInterface for JSDocPropertyLikeTag {
    fn maybe_type_expression(&self) -> Option<Gc<Node>> {
        self.type_expression.clone()
    }

    fn type_expression(&self) -> Gc<Node> {
        self.type_expression.clone().unwrap()
    }
}

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
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

#[derive(Debug, Trace, Finalize)]
pub struct FlowStart {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    node: GcCell<
        Option<
            Gc<
                Node, /*FunctionExpression | ArrowFunction | MethodDeclaration | GetAccessorDeclaration | SetAccessorDeclaration*/
            >,
        >,
    >,
}

impl FlowStart {
    pub fn new(flags: FlowFlags, node: Option<Gc<Node>>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Default::default(),
            node: Default::default(),
        }
    }
}

impl FlowStart {
    pub fn maybe_node(&self) -> Option<Gc<Node>> {
        self.node.borrow().clone()
    }

    pub fn set_node(&self, node: Option<Gc<Node>>) {
        *self.node.borrow_mut() = node;
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

#[derive(Debug, Trace, Finalize)]
pub struct FlowLabel {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    antecedents: GcCell<Option<Vec<Gc<FlowNode>>>>,
}

impl FlowLabel {
    pub fn new(flags: FlowFlags, antecedents: Option<Vec<Gc<FlowNode>>>) -> Self {
        Self {
            flags: Cell::new(flags),
            id: Cell::new(None),
            antecedents: GcCell::new(antecedents),
        }
    }

    pub fn maybe_antecedents(&self) -> GcCellRef<Option<Vec<Gc<FlowNode>>>> {
        self.antecedents.borrow()
    }

    pub fn maybe_antecedents_mut(&self) -> GcCellRefMut<Option<Vec<Gc<FlowNode>>>> {
        self.antecedents.borrow_mut()
    }

    pub fn set_antecedents(&self, antecedents: Option<Vec<Gc<FlowNode>>>) {
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
    fn antecedent(&self) -> Gc<FlowNode>;
}

#[derive(Debug, Trace, Finalize)]
pub struct FlowAssignment {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Gc<Node /*Expression | VariableDeclaration | BindingElement*/>,
    pub antecedent: Gc<FlowNode>,
}

impl FlowAssignment {
    pub fn new(flags: FlowFlags, antecedent: Gc<FlowNode>, node: Gc<Node>) -> Self {
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
    fn antecedent(&self) -> Gc<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowAssignment> for FlowNode {
    fn from(value: FlowAssignment) -> Self {
        Self::FlowAssignment(value)
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct FlowCall {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Gc<Node /*CallExpression*/>,
    pub antecedent: Gc<FlowNode>,
}

impl FlowCall {
    pub fn new(flags: FlowFlags, antecedent: Gc<FlowNode>, node: Gc<Node>) -> Self {
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
    fn antecedent(&self) -> Gc<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowCall> for FlowNode {
    fn from(value: FlowCall) -> Self {
        Self::FlowCall(value)
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct FlowCondition {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Gc<Node /*Expression*/>,
    pub antecedent: Gc<FlowNode>,
}

impl FlowCondition {
    pub fn new(flags: FlowFlags, antecedent: Gc<FlowNode>, node: Gc<Node>) -> Self {
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
    fn antecedent(&self) -> Gc<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowCondition> for FlowNode {
    fn from(value: FlowCondition) -> Self {
        Self::FlowCondition(value)
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct FlowSwitchClause {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub switch_statement: Gc<Node /*SwitchStatement*/>,
    pub clause_start: usize,
    pub clause_end: usize,
    pub antecedent: Gc<FlowNode>,
}

impl FlowSwitchClause {
    pub fn new(
        flags: FlowFlags,
        antecedent: Gc<FlowNode>,
        switch_statement: Gc<Node>,
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
    fn antecedent(&self) -> Gc<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowSwitchClause> for FlowNode {
    fn from(value: FlowSwitchClause) -> Self {
        Self::FlowSwitchClause(value)
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct FlowArrayMutation {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub node: Gc<Node /*CallExpression | BinaryExpression*/>,
    pub antecedent: Gc<FlowNode>,
}

impl FlowArrayMutation {
    pub fn new(flags: FlowFlags, antecedent: Gc<FlowNode>, node: Gc<Node>) -> Self {
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
    fn antecedent(&self) -> Gc<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowArrayMutation> for FlowNode {
    fn from(value: FlowArrayMutation) -> Self {
        Self::FlowArrayMutation(value)
    }
}

#[derive(Debug, Trace, Finalize)]
pub struct FlowReduceLabel {
    #[unsafe_ignore_trace]
    flags: Cell<FlowFlags>,
    #[unsafe_ignore_trace]
    id: Cell<Option<isize>>,
    pub target: Gc<FlowNode /*FlowLabel*/>,
    pub antecedents: Vec<Gc<FlowNode>>,
    pub antecedent: Gc<FlowNode>,
}

impl FlowReduceLabel {
    pub fn new(
        flags: FlowFlags,
        target: Gc<FlowNode>,
        antecedents: Vec<Gc<FlowNode>>,
        antecedent: Gc<FlowNode>,
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
    fn antecedent(&self) -> Gc<FlowNode> {
        self.antecedent.clone()
    }
}

impl From<FlowReduceLabel> for FlowNode {
    fn from(value: FlowReduceLabel) -> Self {
        Self::FlowReduceLabel(value)
    }
}
