#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::rc::Rc;

use super::ParserType;
use crate::{
    for_each, for_each_child_returns, is_export_assignment, is_export_declaration,
    is_export_modifier, is_external_module_reference, is_import_declaration,
    is_import_equals_declaration, is_meta_property, some, BaseNode, BaseNodeFactory, Debug_,
    Diagnostic, EnumDeclaration, EnumMember, ExportAssignment, ExportDeclaration,
    ExpressionWithTypeArguments, HeritageClause, InterfaceDeclaration, ModuleBlock,
    ModuleDeclaration, NamespaceExportDeclaration, Node, NodeArray, NodeFlags, NodeInterface,
    SyntaxKind, TypeAliasDeclaration,
};

impl ParserType {
    pub(super) fn parse_class_declaration_or_expression(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        kind: SyntaxKind, /*ClassLikeDeclaration["kind"]*/
    ) -> Node /*ClassLikeDeclaration*/ {
        let saved_await_context = self.in_await_context();
        self.parse_expected(SyntaxKind::ClassKeyword, None, None);

        let name: Option<Rc<Node>> = self
            .parse_name_of_class_declaration_or_expression()
            .map(Node::wrap);
        let type_parameters = self.parse_type_parameters();
        if some(
            modifiers.as_ref().map(|node_array| {
                let node_array: &[Rc<Node>] = node_array;
                node_array
            }),
            Some(|modifier: &Rc<Node>| is_export_modifier(modifier)),
        ) {
            self.set_await_context(true);
        }
        let heritage_clauses = self.parse_heritage_clauses();

        let members: NodeArray;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.parse_class_members();
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }
        self.set_await_context(saved_await_context);
        let node: Node = if kind == SyntaxKind::ClassDeclaration {
            self.factory
                .create_class_declaration(
                    self,
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .into()
        } else {
            self.factory
                .create_class_expression(
                    self,
                    decorators,
                    modifiers,
                    name,
                    type_parameters,
                    heritage_clauses,
                    members,
                )
                .into()
        };
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_name_of_class_declaration_or_expression(
        &self,
    ) -> Option<Node /*Identifier*/> {
        if self.is_binding_identifier() && !self.is_implements_clause() {
            Some(self.create_identifier(self.is_binding_identifier(), None, None))
        } else {
            None
        }
    }

    pub(super) fn is_implements_clause(&self) -> bool {
        self.token() == SyntaxKind::ImplementsKeyword
            && self.look_ahead_bool(|| self.next_token_is_identifier_or_keyword())
    }

    pub(super) fn parse_heritage_clauses(&self) -> Option<NodeArray /*<HeritageClause>*/> {
        if self.is_heritage_clause() {
            return Some(self.parse_list(ParsingContext::HeritageClauses, &mut || {
                self.parse_heritage_clause().into()
            }));
        }

        None
    }

    pub(super) fn parse_heritage_clause(&self) -> HeritageClause {
        let pos = self.get_node_pos();
        let tok = self.token();
        Debug_.assert(
            matches!(
                tok,
                SyntaxKind::ExtendsKeyword | SyntaxKind::ImplementsKeyword
            ),
            None,
        );
        self.next_token();
        let types = self.parse_delimited_list(
            ParsingContext::HeritageClauseElement,
            || self.parse_expression_with_type_arguments().into(),
            None,
        );
        self.finish_node(
            self.factory.create_heritage_clause(self, tok, types),
            pos,
            None,
        )
    }

    pub(super) fn parse_expression_with_type_arguments(&self) -> ExpressionWithTypeArguments {
        let pos = self.get_node_pos();
        let expression = self.parse_left_hand_side_expression_or_higher();
        let type_arguments = self.try_parse_type_arguments();
        self.finish_node(
            self.factory
                .create_expression_with_type_arguments(self, expression, type_arguments),
            pos,
            None,
        )
    }

    pub(super) fn try_parse_type_arguments(&self) -> Option<NodeArray /*<TypeNode>*/> {
        if self.token() == SyntaxKind::LessThanToken {
            Some(self.parse_bracketed_list(
                ParsingContext::TypeArguments,
                || self.parse_type().wrap(),
                SyntaxKind::LessThanToken,
                SyntaxKind::GreaterThanToken,
            ))
        } else {
            None
        }
    }

    pub(super) fn is_heritage_clause(&self) -> bool {
        matches!(
            self.token(),
            SyntaxKind::ExtendsKeyword | SyntaxKind::ImplementsKeyword
        )
    }

    pub(super) fn parse_class_members(&self) -> NodeArray /*<ClassElement>*/ {
        self.parse_list(ParsingContext::ClassMembers, &mut || {
            self.parse_class_element().wrap()
        })
    }

    pub(super) fn parse_interface_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
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
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_type_alias_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> TypeAliasDeclaration {
        self.parse_expected(SyntaxKind::TypeKeyword, None, None);
        let name = self.parse_identifier(None, None);
        let type_parameters = self.parse_type_parameters();
        self.parse_expected(SyntaxKind::EqualsToken, None, None);
        let type_ = if self.token() == SyntaxKind::IntrinsicKeyword {
            self.try_parse(|| self.parse_keyword_and_no_dot())
                .unwrap_or_else(|| self.parse_type())
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
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_enum_member(&self) -> EnumMember {
        let pos = self.get_node_pos();
        let has_jsdoc = self.has_preceding_jsdoc_comment();
        let name: Rc<Node> = self.parse_property_name().wrap();
        let initializer = self.allow_in_and(|| self.parse_initializer());
        self.with_jsdoc(
            self.finish_node(
                self.factory.create_enum_member(self, name, initializer),
                pos,
                None,
            ),
            has_jsdoc,
        )
    }

    pub(super) fn parse_enum_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> EnumDeclaration {
        self.parse_expected(SyntaxKind::EnumKeyword, None, None);
        let name: Rc<Node> = self.parse_identifier(None, None).wrap();
        let members: NodeArray;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            members = self.do_outside_of_yield_and_await_context(|| {
                self.parse_delimited_list(
                    ParsingContext::EnumMembers,
                    || self.parse_enum_member().into(),
                    None,
                )
            });
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            members = self.create_missing_list();
        }
        let node =
            self.factory
                .create_enum_declaration(self, decorators, modifiers, name, Some(members));
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_module_block(&self) -> ModuleBlock {
        let pos = self.get_node_pos();
        let statements: NodeArray;
        if self.parse_expected(SyntaxKind::OpenBraceToken, None, None) {
            statements = self.parse_list(ParsingContext::BlockStatements, &mut || {
                self.parse_statement()
            });
            self.parse_expected(SyntaxKind::CloseBraceToken, None, None);
        } else {
            statements = self.create_missing_list();
        }
        self.finish_node(
            self.factory.create_module_block(self, Some(statements)),
            pos,
            None,
        )
    }

    pub(super) fn parse_module_or_namespace_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
        flags: NodeFlags,
    ) -> ModuleDeclaration {
        let namespace_flag = flags & NodeFlags::Namespace;
        let name: Rc<Node> = self.parse_identifier(None, None).wrap();
        let body: Rc<Node> = if self.parse_optional(SyntaxKind::DotToken) {
            self.parse_module_or_namespace_declaration(
                self.get_node_pos(),
                false,
                None,
                None,
                NodeFlags::NestedNamespace | namespace_flag,
            )
            .into()
        } else {
            self.parse_module_block().into()
        };
        let node = self.factory.create_module_declaration(
            self,
            decorators,
            modifiers,
            name,
            Some(body),
            Some(flags),
        );
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_ambient_external_module_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ModuleDeclaration {
        let mut flags = NodeFlags::None;
        let name: Rc<Node>;
        if self.token() == SyntaxKind::GlobalKeyword {
            name = self.parse_identifier(None, None).wrap();
            flags |= NodeFlags::GlobalAugmentation;
        } else {
            name = self.parse_literal_node().wrap();
            let name_as_literal_like_node = name.as_literal_like_node();
            name_as_literal_like_node
                .set_text(self.intern_identifier(&name_as_literal_like_node.text()));
        }
        let mut body: Option<Rc<Node>> = None;
        if self.token() == SyntaxKind::OpenBraceToken {
            body = Some(self.parse_module_block().into());
        } else {
            self.parse_semicolon();
        }
        let node = self.factory.create_module_declaration(
            self,
            decorators,
            modifiers,
            name,
            body,
            Some(flags),
        );
        self.with_jsdoc(self.finish_node(node, pos, None), has_jsdoc)
    }

    pub(super) fn parse_module_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ModuleDeclaration {
        let mut flags = NodeFlags::None;
        if self.token() == SyntaxKind::GlobalKeyword {
            return self
                .parse_ambient_external_module_declaration(pos, has_jsdoc, decorators, modifiers);
        } else if self.parse_optional(SyntaxKind::NamespaceKeyword) {
            flags |= NodeFlags::Namespace;
        } else {
            self.parse_expected(SyntaxKind::ModuleKeyword, None, None);
            if self.token() == SyntaxKind::StringLiteral {
                return self.parse_ambient_external_module_declaration(
                    pos, has_jsdoc, decorators, modifiers,
                );
            }
        }
        self.parse_module_or_namespace_declaration(pos, has_jsdoc, decorators, modifiers, flags)
    }

    pub(super) fn is_external_module_reference(&self) -> bool {
        self.token() == SyntaxKind::RequireKeyword
            && self.look_ahead_bool(|| self.next_token_is_open_paren())
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

    pub(super) fn parse_namespace_export_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> NamespaceExportDeclaration {
        unimplemented!()
    }

    pub(super) fn parse_import_declaration_or_import_equals_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> Node /*ImportEqualsDeclaration | ImportDeclaration*/ {
        unimplemented!()
    }

    pub(super) fn parse_export_declaration(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ExportDeclaration {
        unimplemented!()
    }

    pub(super) fn parse_export_assignment(
        &self,
        pos: isize,
        has_jsdoc: bool,
        decorators: Option<NodeArray>,
        modifiers: Option<NodeArray>,
    ) -> ExportAssignment {
        unimplemented!()
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
