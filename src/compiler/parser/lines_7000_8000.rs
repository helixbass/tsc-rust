#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::rc::Rc;

use super::ParserType;
use crate::{
    for_each, for_each_child_returns, is_export_assignment, is_export_declaration,
    is_external_module_reference, is_import_declaration, is_import_equals_declaration,
    is_meta_property, some, BaseNode, BaseNodeFactory, Diagnostic, InterfaceDeclaration, Node,
    NodeArray, NodeFlags, NodeInterface, SyntaxKind, TypeAliasDeclaration,
};

impl ParserType {
    pub(super) fn parse_heritage_clauses(&self) -> Option<NodeArray /*<HeritageClause>*/> {
        None
    }

    pub(super) fn try_parse_type_arguments(&self) -> Option<NodeArray /*<TypeNode>*/> {
        unimplemented!()
    }

    pub(super) fn is_heritage_clause(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::ExtendsKeyword | SyntaxKind::ImplementsKeyword
        )
    }

    pub(super) fn parse_interface_declaration(
        &self,
        pos: isize,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> InterfaceDeclaration {
        self.parse_expected(SyntaxKind::InterfaceKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        let heritage_clauses = self.parse_heritage_clauses();
        let members = self.parse_object_type_members();
        let node = self.factory.create_interface_declaration(
            self,
            decorators,
            modifiers,
            name.wrap(),
            type_parameters,
            heritage_clauses,
            members,
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn parse_type_alias_declaration(
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
            name.wrap(),
            type_parameters,
            type_.wrap(),
        );
        self.finish_node(node, pos, None)
    }

    pub(super) fn next_token_is_open_paren(&self) -> bool {
        self.next_token() == SyntaxKind::OpenParenToken
    }

    pub(super) fn next_token_is_open_brace(&self) -> bool {
        self.next_token() == SyntaxKind::OpenBraceToken
    }

    pub(super) fn next_token_is_slash(&self) -> bool {
        self.next_token() == SyntaxKind::SlashToken
    }

    pub(super) fn set_external_module_indicator(&self, source_file: &Node /*SourceFile*/) {
        let source_file_as_source_file = source_file.as_source_file();
        source_file_as_source_file.set_external_module_indicator(
            for_each(&source_file_as_source_file.statements, |statement, _| {
                self.is_an_external_module_indicator_node(statement)
            })
            .or_else(|| self.get_import_meta_if_necessary(source_file)),
        );
    }

    pub(super) fn is_an_external_module_indicator_node(&self, node: &Node) -> Option<Rc<Node>> {
        if self.has_modifier_of_kind(node, SyntaxKind::ExportKeyword)
            || is_import_equals_declaration(node)
                && is_external_module_reference(
                    &node.as_import_equals_declaration().module_reference,
                )
            || is_import_declaration(node)
            || is_export_assignment(node)
            || is_export_declaration(node)
        {
            Some(node.node_wrapper())
        } else {
            None
        }
    }

    pub(super) fn get_import_meta_if_necessary(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Option<Rc<Node>> {
        if source_file
            .flags()
            .intersects(NodeFlags::PossiblyContainsImportMeta)
        {
            self.walk_tree_for_external_module_indicators(source_file)
        } else {
            None
        }
    }

    pub(super) fn walk_tree_for_external_module_indicators(&self, node: &Node) -> Option<Rc<Node>> {
        if self.is_import_meta(node) {
            Some(node.node_wrapper())
        } else {
            for_each_child_returns(
                node,
                |child| self.walk_tree_for_external_module_indicators(child),
                Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
            )
        }
    }

    pub(super) fn has_modifier_of_kind(&self, node: &Node, kind: SyntaxKind) -> bool {
        let modifiers = node.maybe_modifiers();
        let modifiers: Option<&[Rc<Node>]> = modifiers.as_ref().map(|node_array| {
            let slice_ref: &[Rc<Node>] = node_array;
            slice_ref
        });
        some(modifiers, Some(|m: &Rc<Node>| m.kind() == kind))
    }

    pub(super) fn is_import_meta(&self, node: &Node) -> bool {
        if !is_meta_property(node) {
            return false;
        }
        let node_as_meta_property = node.as_meta_property();
        node_as_meta_property.keyword_token == SyntaxKind::ImportKeyword
            && node_as_meta_property
                .name
                .as_identifier()
                .escaped_text
                .eq_str("meta")
    }

    pub fn JSDocParser_parse_jsdoc_type_expression_for_tests(
        &self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedJSDocTypeExpression> {
        unimplemented!()
    }

    pub fn JSDocParser_parse_isolated_jsdoc_comment(
        &self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedIsolatedJSDocComment> {
        unimplemented!()
    }

    pub fn JSDocParser_parse_jsdoc_comment<TNode: NodeInterface>(
        &self,
        parent: &TNode,
        start: usize,
        length: usize,
    ) -> Option<Rc<Node /*JSDoc*/>> {
        unimplemented!()
    }
}

pub struct ParsedJSDocTypeExpression {
    pub js_doc_type_expression: Rc<Node /*JSDocTypeExpression*/>,
    pub diagnostics: Vec<Rc<Diagnostic>>,
}

pub struct ParsedIsolatedJSDocComment {
    pub js_doc: Rc<Node /*JSDoc*/>,
    pub diagnostics: Vec<Rc<Diagnostic>>,
}

impl BaseNodeFactory for ParserType {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.SourceFileConstructor()(kind, 0, 0))
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.IdentifierConstructor()(kind, 0, 0))
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.PrivateIdentifierConstructor()(kind, 0, 0))
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.TokenConstructor()(kind, 0, 0))
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        self.count_node(self.NodeConstructor()(kind, 0, 0))
    }
}

// lazy_static! {
//     static ref ParserMut: Mutex<ParserType> = Mutex::new(ParserType::new());
// }

#[allow(non_snake_case)]
pub(super) fn Parser() -> ParserType {
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
        const SwitchClauses = 1 << 2;
        const SwitchClauseStatements = 1 << 3;
        const TypeMembers = 1 << 4;
        const ClassMembers = 1 << 5;
        const EnumMembers = 1 << 6;
        const HeritageClauseElement = 1 << 7;
        const VariableDeclarations = 1 << 8;
        const ObjectBindingElements = 1 << 9;
        const ArrayBindingElements = 1 << 10;
        const ArgumentExpressions = 1 << 11;
        const ObjectLiteralMembers = 1 << 12;
        const JsxAttributes = 1 << 13;
        const JsxChildren = 1 << 14;
        const ArrayLiteralMembers = 1 << 15;
        const Parameters = 1 << 16;
        const JSDocParameters = 1 << 17;
        const RestProperties = 1 << 18;
        const TypeParameters = 1 << 19;
        const TypeArguments = 1 << 20;
        const TupleElementTypes = 1 << 21;
        const HeritageClauses = 1 << 22;
        const ImportOrExportSpecifiers = 1 << 23;
        const AssertEntries = 1 << 24;
        const Count = 1 << 25;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum Tristate {
    False,
    True,
    Unknown,
}
