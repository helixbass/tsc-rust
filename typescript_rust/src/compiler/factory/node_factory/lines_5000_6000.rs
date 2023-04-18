#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    are_option_gcs_equal, every, has_node_array_changed, is_outer_expression,
    is_statement_or_block, set_original_node, single_or_undefined, BaseNodeFactory,
    BaseUnparsedNode, Bundle, CommaListExpression, Debug_, EnumMember, FileReference,
    HasInitializerInterface, HasStatementsInterface, InputFiles, LanguageVariant,
    NamedDeclarationInterface, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags,
    NodeInterface, OuterExpressionKinds, PartiallyEmittedExpression, PropertyAssignment,
    ReadonlyTextRange, ScriptKind, ScriptTarget, ShorthandPropertyAssignment, SourceFile,
    SpreadAssignment, StrOrRcNode, SyntaxKind, SyntheticExpression, TransformFlags, Type,
    UnparsedPrepend, UnparsedPrologue, UnparsedSource, UnparsedTextLike, VisitResult,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub fn create_property_assignment<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: impl Into<StrOrRcNode<'name> /*PropertyName*/>,
        initializer: Gc<Node /*Expression*/>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::PropertyAssignment,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
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

    pub(super) fn finish_update_property_assignment(
        &self,
        mut updated: PropertyAssignment,
        original: &Node, /*PropertyAssignment*/
    ) -> Gc<Node> {
        if let Some(original_decorators) = original.maybe_decorators() {
            updated.set_decorators(Some(original_decorators));
        }
        if let Some(original_modifiers) = original.maybe_modifiers() {
            updated.set_modifiers(Some(original_modifiers));
        }
        let original_as_property_assignment = original.as_property_assignment();
        if let Some(original_question_token) =
            original_as_property_assignment.question_token.as_ref()
        {
            updated.question_token = Some(original_question_token.clone());
        }
        if let Some(original_exclamation_token) =
            original_as_property_assignment.exclamation_token.as_ref()
        {
            updated.exclamation_token = Some(original_exclamation_token.clone());
        }
        self.update(updated.wrap(), original)
    }

    pub fn update_property_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,    /*PropertyAssignment*/
        name: Gc<Node>, /*PropertyName*/
        initializer: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_property_assignment = node.as_property_assignment();
        if !Gc::ptr_eq(&node_as_property_assignment.name(), &name)
            || !Gc::ptr_eq(
                &node_as_property_assignment.maybe_initializer().unwrap(),
                &initializer,
            )
        {
            self.finish_update_property_assignment(
                self.create_property_assignment(base_factory, name, initializer),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_shorthand_property_assignment<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier*/
        object_assignment_initializer: Option<Gc<Node /*Expression*/>>,
    ) -> ShorthandPropertyAssignment {
        let node = self.create_base_named_declaration(
            base_factory,
            SyntaxKind::ShorthandPropertyAssignment,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
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

    pub(super) fn finish_update_shorthand_property_assignment(
        &self,
        mut updated: ShorthandPropertyAssignment,
        original: &Node, /*ShorthandPropertyAssignment*/
    ) -> Gc<Node> {
        if let Some(original_decorators) = original.maybe_decorators() {
            updated.set_decorators(Some(original_decorators));
        }
        if let Some(original_modifiers) = original.maybe_modifiers() {
            updated.set_modifiers(Some(original_modifiers));
        }
        let original_as_shorthand_property_assignment = original.as_shorthand_property_assignment();
        if let Some(original_equals_token) = original_as_shorthand_property_assignment
            .equals_token
            .as_ref()
        {
            updated.equals_token = Some(original_equals_token.clone());
        }
        if let Some(original_question_token) = original_as_shorthand_property_assignment
            .question_token
            .as_ref()
        {
            updated.question_token = Some(original_question_token.clone());
        }
        if let Some(original_exclamation_token) = original_as_shorthand_property_assignment
            .exclamation_token
            .as_ref()
        {
            updated.exclamation_token = Some(original_exclamation_token.clone());
        }
        self.update(updated.wrap(), original)
    }

    pub fn update_shorthand_property_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node,    /*ShorthandPropertyAssignment*/
        name: Gc<Node>, /*Identifier*/
        object_assignment_initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_shorthand_property_assignment = node.as_shorthand_property_assignment();
        if !Gc::ptr_eq(&node_as_shorthand_property_assignment.name(), &name)
            || !are_option_gcs_equal(
                node_as_shorthand_property_assignment
                    .object_assignment_initializer
                    .as_ref(),
                object_assignment_initializer.as_ref(),
            )
        {
            self.finish_update_shorthand_property_assignment(
                self.create_shorthand_property_assignment(
                    base_factory,
                    name,
                    object_assignment_initializer,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
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

    pub fn update_spread_assignment(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*SpreadAssignment*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_spread_assignment = node.as_spread_assignment();
        if !Gc::ptr_eq(&node_as_spread_assignment.expression, &expression) {
            self.update(
                self.create_spread_assignment(base_factory, expression)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_enum_member<'name>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier*/
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

    pub fn update_enum_member(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*EnumMember*/
        name: Gc<Node>,
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        let node_as_enum_member = node.as_enum_member();
        if !Gc::ptr_eq(&node_as_enum_member.name, &name)
            || !are_option_gcs_equal(
                node_as_enum_member.initializer.as_ref(),
                initializer.as_ref(),
            )
        {
            self.update(
                self.create_enum_member(base_factory, name, initializer)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_source_file(
        &self,
        base_factory: &TBaseNodeFactory,
        statements: impl Into<NodeArrayOrVec>,
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
            propagate_children_flags(Some(&node.statements()))
                | propagate_child_flags(Some(node.end_of_file_token())),
        );
        node
    }

    pub fn update_source_file(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*SourceFile*/
        statements: impl Into<NodeArrayOrVec>,
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
        source_files: Vec<Option<Gc<Node /*<SourceFile>*/>>>,
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

    pub fn create_partially_emitted_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        expression: Gc<Node /*Expression*/>,
        original: Option<Gc<Node>>,
    ) -> PartiallyEmittedExpression {
        unimplemented!()
    }

    pub fn update_partially_emitted_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*PartiallyEmittedExpression*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_partially_emitted_expression = node.as_partially_emitted_expression();
        if !Gc::ptr_eq(
            &node_as_partially_emitted_expression.expression,
            &expression,
        ) {
            self.update(
                self.create_partially_emitted_expression(
                    base_factory,
                    expression,
                    node_as_partially_emitted_expression.maybe_original(),
                )
                .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_comma_list_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        elements: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> CommaListExpression {
        unimplemented!()
    }

    pub fn update_comma_list_expression(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*CommaListExpression*/
        elements: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> Gc<Node> {
        let node_as_comma_list_expression = node.as_comma_list_expression();
        let elements = elements.into();
        if has_node_array_changed(&node_as_comma_list_expression.elements, &elements) {
            self.update(
                self.create_comma_list_expression(base_factory, elements)
                    .wrap(),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    pub fn clone_node(&self, base_factory: &TBaseNodeFactory, node: &Node) -> Gc<Node> {
        // if (node === undefined) {
        //     return node;
        //  }

        let clone = node.clone().wrap();
        clone.set_pos(-1);
        clone.set_end(-1);

        clone.set_flags(clone.flags() | (node.flags() & !NodeFlags::Synthesized));
        clone.set_transform_flags(node.transform_flags());
        set_original_node(clone.clone(), Some(node.node_wrapper()));

        clone
    }

    pub fn create_void_zero(&self, base_factory: &TBaseNodeFactory) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_function_call_call(
        &self,
        base_factory: &TBaseNodeFactory,
        target: Gc<Node /*Expression*/>,
        this_arg: Gc<Node /*Expression*/>,
        arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_global_method_call(
        &self,
        base_factory: &TBaseNodeFactory,
        global_object_name: String,
        method_name: String,
        arguments_list: Vec<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    fn is_ignorable_paren(&self, node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub fn restore_outer_expressions(
        &self,
        outer_expression: Option<impl Borrow<Node> /*Expression*/>,
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

    pub fn restore_enclosing_label(
        &self,
        base_factory: &TBaseNodeFactory,
        node: &Node, /*Statement*/
        outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        after_restore_label_callback: Option<impl FnMut(&Node /*LabeledStatement*/)>,
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub fn inline_expressions(
        &self,
        base_factory: &TBaseNodeFactory,
        expressions: &[Gc<Node /*Expression*/>],
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn get_declaration_name(
        &self,
        node: Option<impl Borrow<Node> /*Declaration*/>,
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub fn copy_prologue(
        &self,
        base_factory: &TBaseNodeFactory,
        source: &[Gc<Node /*Statement*/>],
        target: &mut Vec<Gc<Node /*Statement*/>>,
        ensure_use_strict: Option<bool>,
        visitor: Option<impl FnMut(&Node) -> VisitResult /*<Node>*/>,
    ) -> usize {
        unimplemented!()
    }

    pub fn lift_to_block(
        &self,
        base_factory: &TBaseNodeFactory,
        nodes: &[Gc<Node>],
    ) -> Gc<Node /*Statement*/> {
        Debug_.assert(
            every(nodes, |node: &Gc<Node>, _| is_statement_or_block(node)),
            Some("Cannot lift nodes to a Block."),
        );
        single_or_undefined(Some(nodes))
            .cloned()
            .unwrap_or_else(|| {
                self.create_block(base_factory, nodes.to_owned(), None)
                    .wrap()
            })
    }
}
