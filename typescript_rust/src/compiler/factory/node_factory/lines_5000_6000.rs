#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;
use std::rc::Rc;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    is_outer_expression, BaseNodeFactory, BaseUnparsedNode, Bundle, EnumMember, FileReference,
    HasStatementsInterface, InputFiles, LanguageVariant, NamedDeclarationInterface, Node,
    NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, OuterExpressionKinds,
    PropertyAssignment, ScriptKind, ScriptTarget, ShorthandPropertyAssignment, SourceFile,
    SpreadAssignment, StrOrRcNode, StringOrRcNode, SyntaxKind, SyntheticExpression, TransformFlags,
    Type, UnparsedPrepend, UnparsedPrologue, UnparsedSource, UnparsedTextLike,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_property_assignment<'name, TName: Into<StrOrRcNode<'name>>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: TName, /*PropertyName*/
        initializer: Gc<Node /*Expression*/>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            Some(name),
        );
        let mut node = PropertyAssignment::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(base_factory, &initializer),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.name()))
                | propagate_child_flags(Some(&*node.initializer)),
        );
        node
    }

    pub fn create_shorthand_property_assignment<'name, TName: Into<StrOrRcNode<'name>>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: TName, /*Identifier*/
        object_assignment_initializer: Option<Gc<Node /*Expression*/>>,
    ) -> ShorthandPropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::ShorthandPropertyAssignment,
            Option::<NodeArray>::None,
            Option::<NodeArray>::None,
            Some(name),
        );
        let mut node = ShorthandPropertyAssignment::new(
            node,
            object_assignment_initializer.map(|object_assignment_initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(
                        base_factory,
                        &object_assignment_initializer,
                    )
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(node.object_assignment_initializer.clone())
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub fn create_spread_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
    ) -> SpreadAssignment {
        let node = self.create_base_node(base_factory, SyntaxKind::SpreadAssignment);
        let mut node = SpreadAssignment::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(base_factory, &expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression.clone()))
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsObjectRestOrSpread,
        );
        node
    }

    pub fn create_enum_member<'name, TName: Into<StrOrRcNode<'name>>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: TName, /*Identifier*/
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> EnumMember {
        let node = self.create_base_node(base_factory, SyntaxKind::EnumMember);
        let mut node = EnumMember::new(
            node,
            self.as_name(base_factory, Some(name)).unwrap(),
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(base_factory, &initializer)
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(&*node.name))
                | propagate_child_flags(node.initializer.clone())
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn create_source_file<TStatements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: TStatements,
        end_of_file_token: Gc<Node /*EndOfFileToken*/>,
        flags: NodeFlags,
    ) -> SourceFile {
        let node = base_factory.create_base_source_file_node(SyntaxKind::SourceFile);
        let node = SourceFile::new(
            node,
            self.create_node_array(Some(statements), None),
            end_of_file_token,
            "".to_string(),
            "".to_string(),
            ScriptTarget::ES3,
            LanguageVariant::Standard,
            ScriptKind::Unknown,
            false,
            false,
        );
        node.set_flags(node.flags() | flags);
        node.add_transform_flags(
            propagate_children_flags(Some(node.statements()))
                | propagate_child_flags(Some(node.end_of_file_token())),
        );
        node
    }

    pub fn update_source_file<TStatements: Into<NodeArrayOrVec>>(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*SourceFile*/
        statements: TStatements,
        is_declaration_file: Option<bool>,
        referenced_files: Option<Vec<FileReference>>,
        type_reference_directives: Option<Vec<FileReference>>,
        has_no_default_lib: Option<bool>,
        lib_reference_directives: Option<Vec<FileReference>>,
    ) -> Gc<Node /*SourceFile*/> {
        // TODO
        // let node_as_source_file = node.as_source_file();
        // let is_declaration_file = is_declaration_file.unwrap_or_else(|| node_as_source_file.is_declaration_file());
        // let referenced_files = referenced_files.unwrap_or_else(|| node_as_source_file.referenced_files().clone());
        // let type_reference_directives = type_reference_directives.unwrap_or_else(|| node_as_source_file.type_reference_directives().clone());
        // let has_no_default_lib = has_no_default_lib.unwrap_or_else(|| node_as_source_file.has_no_default_lib());
        // let lib_reference_directives = lib_reference_directives.unwrap_or_else(|| node_as_source_file.lib_reference_directives().clone());
        // let statements = statements.into();
        // let statements_as_vec = match statements.clone() {
        //     NodeArrayOrVec::NodeArray(statements) => statements.to_vec(),
        //     NodeArrayOrVec::Vec(statements) => statements,
        // };
        //     if !(node_as_source_file.statements.len() == statements_as_vec.len()
        //         && node_as_source_file.statements.iter().enumerate().all(
        //             |(index, node_statement)| Rc::ptr_eq(node_statement, &statements_as_vec[index]),
        //         ))
        // {
        //     // self.update(
        //     //     self.create_call_expression(
        //     //         base_factory,
        //     //         expression.node_wrapper(),
        //     //         type_arguments.map(|type_arguments| type_arguments.to_vec()),
        //     //         Some(arguments_array.to_vec()),
        //     //     )
        //     //     .into(),
        //     //     node,
        //     // )
        // } else {
        //     node.node_wrapper()
        // }
        node.node_wrapper()
    }

    pub fn create_bundle(
        &self,
        base_factory: &TBaseNodeFactory,
        source_files: Vec<Gc<Node /*<SourceFile>*/>>,
        prepends: Option<Vec<Gc<Node /*<UnparsedSource | InputFiles>*/>>>,
    ) -> Bundle {
        let prepends = prepends.unwrap_or_else(|| vec![]);
        let node = self.create_base_node(base_factory, SyntaxKind::Bundle);
        Bundle::new(node, prepends, source_files)
    }

    pub fn create_unparsed_source(
        &self,
        base_factory: &TBaseNodeFactory,
        prologues: Vec<Gc<Node /*<UnparsedPrologue>*/>>,
        synthetic_references: Option<Vec<Gc<Node /*<UnparsedSyntheticReference*/>>>,
        texts: Vec<Gc<Node /*<UnparsedSourceText>*/>>,
    ) -> UnparsedSource {
        let node = self.create_base_node(base_factory, SyntaxKind::UnparsedSource);
        UnparsedSource::new(
            node,
            prologues,
            synthetic_references,
            texts,
            "".to_owned(),
            "".to_owned(),
            vec![],
            vec![],
        )
        // node.getLineAndCharacterOfPosition = pos => getLineAndCharacterOfPosition(node, pos);
    }

    fn create_base_unparsed_node(
        &self,
        base_factory: &TBaseNodeFactory,
        kind: SyntaxKind,
        data: Option<String>,
    ) -> BaseUnparsedNode {
        let node = self.create_base_node(base_factory, kind);
        BaseUnparsedNode::new(node, data)
    }

    fn create_unparsed_prologue(
        &self,
        base_factory: &TBaseNodeFactory,
        data: Option<String>,
    ) -> UnparsedPrologue {
        let node = self.create_base_unparsed_node(base_factory, SyntaxKind::UnparsedPrologue, data);
        UnparsedPrologue::new(node)
    }

    fn create_unparsed_prepend(
        &self,
        base_factory: &TBaseNodeFactory,
        data: Option<String>,
        texts: Vec<Gc<Node /*UnparsedTextLike*/>>,
    ) -> UnparsedPrepend {
        let node = self.create_base_unparsed_node(base_factory, SyntaxKind::UnparsedPrepend, data);
        UnparsedPrepend::new(node, texts)
    }

    fn create_unparsed_text_like(
        &self,
        base_factory: &TBaseNodeFactory,
        data: Option<String>,
        internal: bool,
    ) -> UnparsedTextLike {
        let node = self.create_base_unparsed_node(
            base_factory,
            if internal {
                SyntaxKind::UnparsedInternalText
            } else {
                SyntaxKind::UnparsedText
            },
            data,
        );
        UnparsedTextLike::new(node)
    }

    pub fn create_input_files(&self, base_factory: &TBaseNodeFactory) -> InputFiles {
        let node = self.create_base_node(base_factory, SyntaxKind::InputFiles);
        InputFiles::new(node, "".to_owned(), "".to_owned())
    }

    pub fn create_synthetic_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        type_: Gc<Type>,
        is_spread: Option<bool>,
        tuple_name_source: Option<Gc<Node /*ParameterDeclaration | NamedTupleMember*/>>,
    ) -> SyntheticExpression {
        let is_spread = is_spread.unwrap_or(false);
        let node = self.create_base_node(base_factory, SyntaxKind::SyntheticExpression);
        let node = SyntheticExpression::new(node, is_spread, type_, tuple_name_source);
        node
    }

    pub fn clone_node(&self, base_factory: &TBaseNodeFactory, node: &Node) -> Gc<Node> {
        // unimplemented!()
        // TODO: this is definitely not right
        node.node_wrapper()
    }

    fn is_ignorable_paren(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub fn restore_outer_expressions<TOuterExpression: Borrow<Node>>(
        &self,
        outer_expression: Option<TOuterExpression /*Expression*/>,
        inner_expression: &Node, /*Expression*/
        kinds: Option<OuterExpressionKinds>,
    ) -> Gc<Node /*Expression*/> {
        let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
        if let Some(outer_expression) = outer_expression.filter(|outer_expression| {
            let outer_expression = outer_expression.borrow();
            is_outer_expression(outer_expression, Some(kinds))
                && !self.is_ignorable_paren(outer_expression)
        }) {
            unimplemented!()
        }
        inner_expression.node_wrapper()
    }

    pub fn get_declaration_name<TNode: Borrow<Node>>(
        &self,
        node: Option<TNode /*Declaration*/>,
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }
}
