use std::{borrow::Borrow, cell::RefCell, io, rc::Rc};

use gc::{Finalize, Gc, Trace};
use local_macros::generate_node_factory_method_wrapper;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    are_option_gcs_equal, are_option_rcs_equal, every, has_node_array_changed,
    is_binary_expression, is_comma_list_expression, is_comma_token, is_outer_expression,
    is_parse_tree_node, is_statement_or_block, node_is_synthesized, same_flat_map_rc_node,
    set_original_node, single_or_undefined, BaseNodeFactory, BaseUnparsedNode, Bundle, CallBinding,
    CommaListExpression, Debug_, EnumMember, FileReference, HasInitializerInterface,
    HasStatementsInterface, InputFiles, LanguageVariant, ModifierFlags, NamedDeclarationInterface,
    Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, OuterExpressionKinds,
    PartiallyEmittedExpression, PropertyAssignment, PropertyDescriptorAttributes,
    ReadonlyTextRange, ScriptKind, ScriptTarget, ShorthandPropertyAssignment, SingleOrVec,
    SourceFile, SpreadAssignment, StrOrRcNode, SyntaxKind, SyntheticExpression, TransformFlags,
    Type, UnparsedPrepend, UnparsedPrologue, UnparsedSource, UnparsedTextLike, VisitResult,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize> NodeFactory<TBaseNodeFactory> {
    #[generate_node_factory_method_wrapper]
    pub fn create_property_assignment_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name> /*PropertyName*/>,
        initializer: Gc<Node /*Expression*/>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            SyntaxKind::PropertyAssignment,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(name),
        );
        let node = PropertyAssignment::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(&initializer),
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
                self.create_property_assignment_raw(name, initializer),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_shorthand_property_assignment_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier*/
        object_assignment_initializer: Option<Gc<Node /*Expression*/>>,
    ) -> ShorthandPropertyAssignment {
        let node = self.create_base_named_declaration(
            SyntaxKind::ShorthandPropertyAssignment,
            Option::<Gc<NodeArray>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(name),
        );
        let node = ShorthandPropertyAssignment::new(
            node,
            object_assignment_initializer.map(|object_assignment_initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(&object_assignment_initializer)
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
                self.create_shorthand_property_assignment_raw(name, object_assignment_initializer),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_spread_assignment_raw(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> SpreadAssignment {
        let node = self.create_base_node(SyntaxKind::SpreadAssignment);
        let node = SpreadAssignment::new(
            node,
            self.parenthesizer_rules()
                .parenthesize_expression_for_disallowed_comma(&expression),
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
        node: &Node, /*SpreadAssignment*/
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        let node_as_spread_assignment = node.as_spread_assignment();
        if !Gc::ptr_eq(&node_as_spread_assignment.expression, &expression) {
            self.update(self.create_spread_assignment(expression), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_enum_member_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier*/
        initializer: Option<Gc<Node /*Expression*/>>,
    ) -> EnumMember {
        let node = self.create_base_node(SyntaxKind::EnumMember);
        let node = EnumMember::new(
            node,
            self.as_name(Some(name)).unwrap(),
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .parenthesize_expression_for_disallowed_comma(&initializer)
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
            self.update(self.create_enum_member(name, initializer), node)
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_source_file_raw(
        &self,
        statements: impl Into<NodeArrayOrVec>,
        end_of_file_token: Gc<Node /*EndOfFileToken*/>,
        flags: NodeFlags,
    ) -> SourceFile {
        let node = self
            .base_factory
            .create_base_source_file_node(SyntaxKind::SourceFile);
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

    pub fn clone_source_file_with_changes(
        &self,
        source: &Node, /*SourceFile*/
        statements: NodeArrayOrVec,
        is_declaration_file: bool,
        referenced_files: Rc<RefCell<Vec<FileReference>>>,
        type_reference_directives: Rc<RefCell<Vec<FileReference>>>,
        has_no_default_lib: bool,
        lib_reference_directives: Rc<RefCell<Vec<FileReference>>>,
    ) -> Gc<Node> {
        let source_as_source_file = source.as_source_file();
        let mut node = source_as_source_file.clone();
        node.set_pos(-1);
        node.set_end(-1);
        node.set_id(0);
        node.set_modifier_flags_cache(ModifierFlags::None);
        node.set_parent(None);
        node.set_flags(NodeFlags::None);
        node.set_transform_flags(TransformFlags::None);
        node.set_original(None);
        node.set_emit_node(None);
        self.base_factory.update_cloned_node(&node);

        node.set_statements(self.create_node_array(Some(statements), None));
        node.set_end_of_file_token(source_as_source_file.end_of_file_token());
        node.set_is_declaration_file(is_declaration_file);
        node.set_referenced_files(referenced_files);
        node.set_type_reference_directives(type_reference_directives);
        node.set_has_no_default_lib(has_no_default_lib);
        node.set_lib_reference_directives(lib_reference_directives);
        node.set_transform_flags(
            propagate_children_flags(Some(&node.statements()))
                | propagate_child_flags(Some(node.end_of_file_token())),
        );
        node.set_implied_node_format(source_as_source_file.maybe_implied_node_format());
        node.wrap()
    }

    pub fn update_source_file(
        &self,
        node: &Node, /*SourceFile*/
        statements: impl Into<NodeArrayOrVec>,
        is_declaration_file: Option<bool>,
        referenced_files: Option<Rc<RefCell<Vec<FileReference>>>>,
        type_reference_directives: Option<Rc<RefCell<Vec<FileReference>>>>,
        has_no_default_lib: Option<bool>,
        lib_reference_directives: Option<Rc<RefCell<Vec<FileReference>>>>,
    ) -> Gc<Node> {
        let node_as_source_file = node.as_source_file();
        let is_declaration_file =
            is_declaration_file.unwrap_or_else(|| node_as_source_file.is_declaration_file());
        let referenced_files =
            referenced_files.unwrap_or_else(|| node_as_source_file.referenced_files());
        let type_reference_directives = type_reference_directives
            .unwrap_or_else(|| node_as_source_file.type_reference_directives());
        let has_no_default_lib =
            has_no_default_lib.unwrap_or_else(|| node_as_source_file.has_no_default_lib());
        let lib_reference_directives = lib_reference_directives
            .unwrap_or_else(|| node_as_source_file.lib_reference_directives());
        let statements = statements.into();
        if has_node_array_changed(&node_as_source_file.statements(), &statements)
            || node_as_source_file.is_declaration_file() != is_declaration_file
            || !are_option_rcs_equal(
                node_as_source_file.maybe_referenced_files().as_ref(),
                Some(&referenced_files),
            )
            || !are_option_rcs_equal(
                node_as_source_file
                    .maybe_type_reference_directives()
                    .as_ref(),
                Some(&type_reference_directives),
            )
            || node_as_source_file.has_no_default_lib() != has_no_default_lib
            || !are_option_rcs_equal(
                node_as_source_file
                    .maybe_lib_reference_directives()
                    .as_ref(),
                Some(&lib_reference_directives),
            )
        {
            self.update(
                self.clone_source_file_with_changes(
                    node,
                    statements,
                    is_declaration_file,
                    referenced_files,
                    type_reference_directives,
                    has_no_default_lib,
                    lib_reference_directives,
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_bundle_raw(
        &self,
        source_files: Vec<Option<Gc<Node /*<SourceFile>*/>>>,
        prepends: Option<Vec<Gc<Node /*<UnparsedSource | InputFiles>*/>>>,
    ) -> Bundle {
        let prepends = prepends.unwrap_or_else(|| vec![]);
        let node = self.create_base_node(SyntaxKind::Bundle);
        Bundle::new(node, prepends, source_files)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_unparsed_source_raw(
        &self,
        prologues: Vec<Gc<Node /*<UnparsedPrologue>*/>>,
        synthetic_references: Option<Vec<Gc<Node /*<UnparsedSyntheticReference*/>>>,
        texts: Vec<Gc<Node /*<UnparsedSourceText>*/>>,
    ) -> UnparsedSource {
        let node = self.create_base_node(SyntaxKind::UnparsedSource);
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
        kind: SyntaxKind,
        data: Option<String>,
    ) -> BaseUnparsedNode {
        let node = self.create_base_node(kind);
        BaseUnparsedNode::new(node, data)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_unparsed_prologue_raw(&self, data: Option<String>) -> UnparsedPrologue {
        let node = self.create_base_unparsed_node(SyntaxKind::UnparsedPrologue, data);
        UnparsedPrologue::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_unparsed_prepend_raw(
        &self,
        data: Option<String>,
        texts: Vec<Gc<Node /*UnparsedTextLike*/>>,
    ) -> UnparsedPrepend {
        let node = self.create_base_unparsed_node(SyntaxKind::UnparsedPrepend, data);
        UnparsedPrepend::new(node, texts)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_unparsed_text_like_raw(
        &self,
        data: Option<String>,
        internal: bool,
    ) -> UnparsedTextLike {
        let node = self.create_base_unparsed_node(
            if internal {
                SyntaxKind::UnparsedInternalText
            } else {
                SyntaxKind::UnparsedText
            },
            data,
        );
        UnparsedTextLike::new(node)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_input_files_raw(&self) -> InputFiles {
        let node = self.create_base_node(SyntaxKind::InputFiles);
        InputFiles::new(node, "".to_owned(), "".to_owned())
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_synthetic_expression_raw(
        &self,
        type_: Gc<Type>,
        is_spread: Option<bool>,
        tuple_name_source: Option<Gc<Node /*ParameterDeclaration | NamedTupleMember*/>>,
    ) -> SyntheticExpression {
        let is_spread = is_spread.unwrap_or(false);
        let node = self.create_base_node(SyntaxKind::SyntheticExpression);
        let node = SyntheticExpression::new(node, is_spread, type_, tuple_name_source);
        node
    }

    pub fn create_not_emitted_statement(&self, _original: Gc<Node>) -> Gc<Node> {
        unimplemented!()
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_partially_emitted_expression_raw(
        &self,
        _expression: Gc<Node /*Expression*/>,
        _original: Option<Gc<Node>>,
    ) -> PartiallyEmittedExpression {
        unimplemented!()
    }

    pub fn update_partially_emitted_expression(
        &self,
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
                    expression,
                    node_as_partially_emitted_expression.maybe_original(),
                ),
                node,
            )
        } else {
            node.node_wrapper()
        }
    }

    fn flatten_comma_elements(
        &self,
        node: &Node, /*Expression*/
    ) -> SingleOrVec<Gc<Node /*Expression*/>> {
        if node_is_synthesized(node)
            && !is_parse_tree_node(node)
            && node.maybe_original().is_none()
            && node.maybe_emit_node().is_none()
            && node.maybe_id().is_none()
        {
            if is_comma_list_expression(node) {
                return node.as_comma_list_expression().elements.to_vec().into();
            }
            if is_binary_expression(node) {
                let node_as_binary_expression = node.as_binary_expression();
                if is_comma_token(&node_as_binary_expression.operator_token) {
                    return vec![
                        node_as_binary_expression.left.clone(),
                        node_as_binary_expression.right.clone(),
                    ]
                    .into();
                }
            }
        }
        node.node_wrapper().into()
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_comma_list_expression_raw(
        &self,
        elements: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> CommaListExpression {
        let elements = elements.into();
        let node = self.create_base_node(SyntaxKind::CommaListExpression);
        let node = CommaListExpression::new(
            node,
            self.create_node_array(
                Some(same_flat_map_rc_node(elements, |element: &Gc<Node>, _| {
                    self.flatten_comma_elements(element)
                })),
                None,
            ),
        );
        node.set_transform_flags(
            node.transform_flags() | propagate_children_flags(Some(&node.elements)),
        );
        node
    }

    pub fn update_comma_list_expression(
        &self,
        node: &Node, /*CommaListExpression*/
        elements: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> Gc<Node> {
        let node_as_comma_list_expression = node.as_comma_list_expression();
        let elements = elements.into();
        if has_node_array_changed(&node_as_comma_list_expression.elements, &elements) {
            self.update(self.create_comma_list_expression(elements), node)
        } else {
            node.node_wrapper()
        }
    }

    pub fn create_end_of_declaration_marker(&self, _original: Gc<Node>) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_merge_declaration_marker(&self, _original: Gc<Node>) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_synthetic_reference_expression(
        &self,
        _expression: Gc<Node>, /*Expression*/
        _this_arg: Gc<Node>,   /*Expression*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn clone_node(&self, node: &Node) -> Gc<Node> {
        // if (node === undefined) {
        //     return node;
        //  }

        let clone = node.clone().wrap();
        clone.set_pos(-1);
        clone.set_end(-1);
        clone.set_id(0);
        clone.set_modifier_flags_cache(ModifierFlags::None);
        clone.set_parent(None);
        self.base_factory.update_cloned_node(&*clone);

        clone.set_flags(clone.flags() | (node.flags() & !NodeFlags::Synthesized));
        clone.set_transform_flags(node.transform_flags());
        set_original_node(clone.clone(), Some(node.node_wrapper()));

        clone
    }

    pub fn create_immediately_invoked_arrow_function(
        &self,
        _statements: impl Into<NodeArrayOrVec>,
        _param: Option<Gc<Node /*ParameterDeclaration*/>>,
        _param_value: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_void_zero(&self) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_export_default(&self, _expression: Gc<Node /*Expression*/>) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_external_module_export(&self, _export_name: Gc<Node /*Identifier*/>) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_type_check(
        &self,
        _value: Gc<Node /*Expression*/>,
        _tag: &str, /*TypeOfTag*/
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_function_bind_call(
        &self,
        _target: Gc<Node /*Expression*/>,
        _this_arg: Gc<Node /*Expression*/>,
        _arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_function_call_call(
        &self,
        _target: Gc<Node /*Expression*/>,
        _this_arg: Gc<Node /*Expression*/>,
        _arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_function_apply_call(
        &self,
        _target: Gc<Node /*Expression*/>,
        _this_arg: Gc<Node /*Expression*/>,
        _arguments_expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_global_method_call(
        &self,
        _global_object_name: String,
        _method_name: String,
        _arguments_list: Vec<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_array_concat_call(
        &self,
        _array: Gc<Node /*Expression*/>,
        _arguments_list: Vec<Gc<Node /*Expression*/>>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_object_define_property_call<'property_name>(
        &self,
        _target: Gc<Node /*Expression*/>,
        _property_name: impl Into<StrOrRcNode<'property_name>>,
        _attributes: Gc<Node /*Expression*/>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn create_reflect_get_call(
        &self,
        _target: Gc<Node /*Expression*/>,
        _property_key: Gc<Node /*Expression*/>,
        _receiver: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node /*CallExpression*/> {
        unimplemented!()
    }

    pub fn create_reflect_set_call(
        &self,
        _target: Gc<Node /*Expression*/>,
        _property_key: Gc<Node /*Expression*/>,
        _value: Gc<Node /*Expression*/>,
        _receiver: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node /*CallExpression*/> {
        unimplemented!()
    }

    pub fn create_property_descriptor(
        &self,
        _attributes: PropertyDescriptorAttributes,
        _single_line: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    fn is_ignorable_paren(&self, _node: &Node /*Expression*/) -> bool {
        unimplemented!()
    }

    pub fn restore_outer_expressions(
        &self,
        outer_expression: Option<impl Borrow<Node> /*Expression*/>,
        inner_expression: &Node, /*Expression*/
        kinds: Option<OuterExpressionKinds>,
    ) -> Gc<Node /*Expression*/> {
        let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
        if let Some(_outer_expression) = outer_expression.filter(|outer_expression| {
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
        _node: &Node, /*Statement*/
        _outermost_labeled_statement: Option<impl Borrow<Node /*LabeledStatement*/>>,
        _after_restore_label_callback: Option<impl FnMut(&Node /*LabeledStatement*/)>,
    ) -> Gc<Node /*Statement*/> {
        unimplemented!()
    }

    pub fn create_call_binding(
        &self,
        _expression: &Node, /*Expression*/
        _record_temp_variable: impl FnMut(&Node /*Identifier*/),
        _language_version: Option<ScriptTarget>,
        cache_identifiers: Option<bool>,
    ) -> CallBinding {
        let _cache_identifiers = cache_identifiers.unwrap_or_default();
        unimplemented!()
    }

    pub fn create_assignment_target_wrapper(
        &self,
        _param_name: Gc<Node /*Identifier*/>,
        _expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*LeftHandSideExpression*/> {
        unimplemented!()
    }

    pub fn inline_expressions(&self, _expressions: &[Gc<Node /*Expression*/>]) -> Gc<Node> {
        unimplemented!()
    }

    pub fn get_internal_name(
        &self,
        _node: &Node, /*Declaration*/
        _allow_comments: Option<bool>,
        _allow_source_maps: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn get_local_name(
        &self,
        _node: &Node, /*Declaration*/
        _allow_comments: Option<bool>,
        _allow_source_maps: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub fn get_export_name(
        &self,
        _node: &Node, /*Declaration*/
        _allow_comments: Option<bool>,
        _allow_source_maps: Option<bool>,
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub fn get_declaration_name(
        &self,
        _node: Option<impl Borrow<Node> /*Declaration*/>,
        _allow_comments: Option<bool>,
        _allow_source_maps: Option<bool>,
    ) -> Gc<Node /*Identifier*/> {
        unimplemented!()
    }

    pub fn get_namespace_member_name(
        &self,
        _ns: &Node,   /*Identifier*/
        _name: &Node, /*Identifier*/
        _allow_comments: Option<bool>,
        _allow_source_maps: Option<bool>,
    ) -> Gc<Node /*PropertyAccessExpression*/> {
        unimplemented!()
    }

    pub fn get_external_module_or_namespace_export_name(
        &self,
        _ns: Option<impl Borrow<Node> /*Identifier*/>,
        _node: &Node, /*Declaration*/
        _allow_comments: Option<bool>,
        _allow_source_maps: Option<bool>,
    ) -> Gc<Node /*Identifier | PropertyAccessExpression*/> {
        unimplemented!()
    }

    pub fn copy_prologue(
        &self,
        source: &[Gc<Node /*Statement*/>],
        target: &mut Vec<Gc<Node /*Statement*/>>,
        ensure_use_strict: Option<bool>,
        visitor: Option<impl FnMut(&Node) -> VisitResult /*<Node>*/>,
    ) -> usize {
        self.try_copy_prologue(
            source,
            target,
            ensure_use_strict,
            visitor.map(|mut visitor| move |a: &Node| Ok(visitor(a))),
        )
        .unwrap()
    }

    pub fn try_copy_prologue(
        &self,
        _source: &[Gc<Node /*Statement*/>],
        _target: &mut Vec<Gc<Node /*Statement*/>>,
        _ensure_use_strict: Option<bool>,
        _visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult /*<Node>*/>>,
    ) -> io::Result<usize> {
        unimplemented!()
    }

    pub fn copy_standard_prologue(
        &self,
        _source: &[Gc<Node /*Statement*/>],
        _target: &mut Vec<Gc<Node /*Statement*/>>,
        _ensure_use_strict: Option<bool>,
    ) -> usize {
        unimplemented!()
    }

    pub fn copy_custom_prologue(
        &self,
        source: &[Gc<Node /*Statement*/>],
        target: &mut Vec<Gc<Node /*Statement*/>>,
        statement_offset: Option<usize>,
        visitor: Option<impl FnMut(&Node) -> VisitResult /*<Node>*/>,
        filter: Option<impl FnMut(&Node) -> bool>,
    ) -> Option<usize> {
        self.try_copy_custom_prologue(
            source,
            target,
            statement_offset,
            visitor.map(|mut visitor| move |node: &Node| Ok(visitor(node))),
            filter,
        )
        .unwrap()
    }

    pub fn try_copy_custom_prologue(
        &self,
        _source: &[Gc<Node /*Statement*/>],
        _target: &mut Vec<Gc<Node /*Statement*/>>,
        _statement_offset: Option<usize>,
        _visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult /*<Node>*/>>,
        _filter: Option<impl FnMut(&Node) -> bool>,
    ) -> io::Result<Option<usize>> {
        unimplemented!()
    }

    pub fn lift_to_block(&self, nodes: &[Gc<Node>]) -> Gc<Node /*Statement*/> {
        Debug_.assert(
            every(nodes, |node: &Gc<Node>, _| is_statement_or_block(node)),
            Some("Cannot lift nodes to a Block."),
        );
        single_or_undefined(Some(nodes))
            .cloned()
            .unwrap_or_else(|| self.create_block(nodes.to_owned(), None))
    }

    pub fn merge_lexical_environment(
        &self,
        _statements: impl Into<NodeArrayOrVec>,
        _declarations: Option<&[Gc<Node /*Statement*/>]>,
    ) -> NodeArrayOrVec {
        unimplemented!()
    }
}
