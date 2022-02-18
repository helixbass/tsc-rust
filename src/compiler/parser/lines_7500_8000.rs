#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::convert::TryInto;
use std::rc::Rc;

use super::ParserType;
use crate::{
    add_related_info, attach_file_to_diagnostics, create_detached_diagnostic, for_each,
    for_each_child_returns, is_export_assignment, is_export_declaration, is_export_modifier,
    is_external_module_reference, is_import_declaration, is_import_equals_declaration, is_keyword,
    is_meta_property, last_or_undefined, some, token_is_identifier_or_keyword, AssertClause,
    AssertEntry, BaseNode, BaseNodeFactory, Debug_, Diagnostic,
    DiagnosticRelatedInformationInterface, Diagnostics, EnumDeclaration, EnumMember,
    ExportAssignment, ExportDeclaration, ExpressionWithTypeArguments, ExternalModuleReference,
    HeritageClause, ImportClause, ImportEqualsDeclaration, InterfaceDeclaration, ModuleBlock,
    ModuleDeclaration, NamespaceExport, NamespaceExportDeclaration, NamespaceImport, Node,
    NodeArray, NodeFlags, NodeInterface, ScriptKind, ScriptTarget, SyntaxKind,
    TypeAliasDeclaration,
};

impl ParserType {
    pub(super) fn parse_export_assignment(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ExportAssignment {
        let saved_await_context = self.in_await_context();
        self.set_await_context(true);
        let mut is_export_equals: Option<bool> = None;
        if self.parse_optional(SyntaxKind::EqualsToken) {
            is_export_equals = Some(true);
        } else {
            self.parse_expected(SyntaxKind::DefaultKeyword, None, None);
        }
        let expression = self.parse_assignment_expression_or_higher();
        self.parse_semicolon();
        self.set_await_context(saved_await_context);
        let node = self.factory.create_export_assignment(
            self,
            decorators,
            modifiers,
            is_export_equals,
            expression,
        );
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
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
        &mut self,
        content: String,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Option<ParsedJSDocTypeExpression> {
        self.initialize_state(
            "file.js",
            content.clone(),
            ScriptTarget::Latest,
            None,
            ScriptKind::JS,
        );
        self.scanner_mut().set_text(
            Some(content.chars().collect()),
            Some(content),
            start,
            length,
        );
        self.set_current_token(
            self.scanner()
                .scan(Some(&|message, length| self.scan_error(message, length))),
        );
        let js_doc_type_expression = self.JSDocParser_parse_jsdoc_type_expression(None);

        let source_file = self.create_source_file(
            "file.js",
            ScriptTarget::Latest,
            ScriptKind::JS,
            false,
            vec![],
            self.factory
                .create_token(self, SyntaxKind::EndOfFileToken)
                .into(),
            NodeFlags::None,
        );
        let diagnostics = attach_file_to_diagnostics(&*self.parse_diagnostics(), &source_file);
        {
            let maybe_js_doc_diagnostics = self.maybe_js_doc_diagnostics();
            if let Some(js_doc_diagnostics) = &*maybe_js_doc_diagnostics {
                source_file
                    .as_source_file()
                    .set_js_doc_diagnostics(attach_file_to_diagnostics(
                        js_doc_diagnostics,
                        &source_file,
                    ));
            }
        }

        self.clear_state();

        /*jsDocTypeExpression ?*/
        Some(ParsedJSDocTypeExpression {
            js_doc_type_expression,
            diagnostics,
        })
    }

    pub fn JSDocParser_parse_jsdoc_type_expression(
        &self,
        may_omit_braces: Option<bool>,
    ) -> Rc<Node /*JSDocTypeExpression*/> {
        let may_omit_braces = may_omit_braces.unwrap_or(false);
        let pos = self.get_node_pos();
        let has_brace = if may_omit_braces {
            self.parse_optional(SyntaxKind::OpenBraceToken)
        } else {
            self.parse_expected(SyntaxKind::OpenBraceToken, None, None)
        };
        let type_ = self.do_inside_of_context(NodeFlags::JSDoc, || self.parse_jsdoc_type());
        if !may_omit_braces || has_brace {
            self.parse_expected_jsdoc(SyntaxKind::CloseBraceToken);
        }

        let result = Into::<Rc<Node>>::into(
            self.factory
                .create_jsdoc_type_expression(self, type_.wrap()),
        );
        self.fixup_parent_references(&result);
        self.finish_node_ref(&*result, pos, None);
        result
    }

    pub fn JSDocParser_parse_jsdoc_name_reference(&self) -> Rc<Node /*JSDocNameReference*/> {
        let pos = self.get_node_pos();
        let has_brace = self.parse_optional(SyntaxKind::OpenBraceToken);
        let p2 = self.get_node_pos();
        let mut entity_name: Rc<Node /*EntityName | JSDocMemberName*/> =
            self.parse_entity_name(false, None).wrap();
        while self.token() == SyntaxKind::PrivateIdentifier {
            self.re_scan_hash_token();
            self.next_token_jsdoc();
            entity_name = self
                .finish_node(
                    self.factory.create_jsdoc_member_name(
                        self,
                        entity_name,
                        self.parse_identifier(None, None).wrap(),
                    ),
                    p2,
                    None,
                )
                .into();
        }
        if has_brace {
            self.parse_expected_jsdoc(SyntaxKind::CloseBraceToken);
        }

        let result =
            Into::<Rc<Node>>::into(self.factory.create_jsdoc_name_reference(self, entity_name));
        self.fixup_parent_references(&result);
        self.finish_node_ref(&*result, pos, None);
        result
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
