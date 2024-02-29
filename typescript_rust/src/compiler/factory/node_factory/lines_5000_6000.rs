use std::{cell::RefCell, collections::HashMap, io, rc::Rc};

use id_arena::Id;
use local_macros::generate_node_factory_method_wrapper;

use super::{propagate_child_flags, propagate_children_flags};
use crate::{
    are_option_rcs_equal, every, has_node_array_changed, is_binary_expression,
    is_comma_list_expression, is_comma_token, is_custom_prologue, is_hoisted_function,
    is_hoisted_variable_statement, is_outer_expression, is_parse_tree_node, is_prologue_directive,
    is_statement_or_block, node_is_synthesized, same_flat_map_id_node, set_original_node,
    single_or_undefined, BaseUnparsedNode, Bundle, CallBinding, CommaListExpression, Debug_,
    EnumMember, FileReference, HasInitializerInterface, HasStatementsInterface, InputFiles,
    LanguageVariant, LiteralLikeNodeInterface, ModifierFlags, NamedDeclarationInterface, Node,
    NodeArray, NodeArrayExt, NodeArrayOrVec, NodeExt, NodeFactory, NodeFlags, NodeInterface,
    NonEmpty, OuterExpressionKinds, PartiallyEmittedExpression, PropertyAssignment,
    PropertyDescriptorAttributes, ReadonlyTextRange, ScriptKind, ScriptTarget,
    ShorthandPropertyAssignment, SingleOrVec, SourceFile, SpreadAssignment, StrOrRcNode,
    SyntaxKind, SyntheticExpression, TransformFlags, Type, UnparsedPrepend, UnparsedPrologue,
    UnparsedSource, UnparsedTextLike, VisitResult, _d, find_use_strict_prologue, get_comment_range,
    get_emit_flags, get_name_of_declaration, get_source_map_range, get_synthetic_leading_comments,
    get_synthetic_trailing_comments, has_syntactic_modifier, is_call_chain,
    is_element_access_expression, is_generated_identifier, is_identifier, is_labeled_statement,
    is_parenthesized_expression, is_property_access_expression, is_statement, is_string_literal,
    is_super_keyword, is_super_property, reduce_left_no_initial_value, released,
    return_ok_default_if_none, set_emit_flags, set_text_range, skip_outer_expressions,
    skip_parentheses, try_visit_node, EmitFlags, HasArena, InArena, MapOrDefault, Matches,
    NumberOrRcNode, OptionInArena, OptionTry, ReadonlyTextRangeConcrete,
    SyntheticReferenceExpression, VecExt,
};

impl NodeFactory {
    #[generate_node_factory_method_wrapper]
    pub fn create_property_assignment_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name> /*PropertyName*/>,
        initializer: Id<Node /*Expression*/>,
    ) -> PropertyAssignment {
        let node = self.create_base_named_declaration(
            SyntaxKind::PropertyAssignment,
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            Some(name),
        );
        let node = PropertyAssignment::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_for_disallowed_comma(initializer),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.name()), self)
                | propagate_child_flags(Some(node.initializer), self),
        );
        node
    }

    pub(super) fn finish_update_property_assignment(
        &self,
        mut updated: PropertyAssignment,
        original: Id<Node>, /*PropertyAssignment*/
    ) -> Id<Node> {
        if let Some(original_decorators) = original.ref_(self).maybe_decorators() {
            updated.set_decorators(Some(original_decorators));
        }
        if let Some(original_modifiers) = original.ref_(self).maybe_modifiers() {
            updated.set_modifiers(Some(original_modifiers));
        }
        let original_ref = original.ref_(self);
        let original_as_property_assignment = original_ref.as_property_assignment();
        if let Some(original_question_token) = original_as_property_assignment.question_token {
            updated.question_token = Some(original_question_token.clone());
        }
        if let Some(original_exclamation_token) = original_as_property_assignment.exclamation_token
        {
            updated.exclamation_token = Some(original_exclamation_token.clone());
        }
        self.update(updated.alloc(self.arena()), original)
    }

    pub fn update_property_assignment(
        &self,
        node: Id<Node>, /*PropertyAssignment*/
        name: Id<Node>, /*PropertyName*/
        initializer: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_property_assignment = node_ref.as_property_assignment();
        if node_as_property_assignment.name() != name
            || node_as_property_assignment.maybe_initializer().unwrap() != initializer
        {
            self.finish_update_property_assignment(
                self.create_property_assignment_raw(name, initializer),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_shorthand_property_assignment_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier*/
        object_assignment_initializer: Option<Id<Node /*Expression*/>>,
    ) -> ShorthandPropertyAssignment {
        let node = self.create_base_named_declaration(
            SyntaxKind::ShorthandPropertyAssignment,
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            Some(name),
        );
        let node = ShorthandPropertyAssignment::new(
            node,
            object_assignment_initializer.map(|object_assignment_initializer| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_expression_for_disallowed_comma(object_assignment_initializer)
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(node.object_assignment_initializer.clone(), self)
                | TransformFlags::ContainsES2015,
        );
        node
    }

    pub(super) fn finish_update_shorthand_property_assignment(
        &self,
        mut updated: ShorthandPropertyAssignment,
        original: Id<Node>, /*ShorthandPropertyAssignment*/
    ) -> Id<Node> {
        if let Some(original_decorators) = original.ref_(self).maybe_decorators() {
            updated.set_decorators(Some(original_decorators));
        }
        if let Some(original_modifiers) = original.ref_(self).maybe_modifiers() {
            updated.set_modifiers(Some(original_modifiers));
        }
        let original_ref = original.ref_(self);
        let original_as_shorthand_property_assignment =
            original_ref.as_shorthand_property_assignment();
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
        self.update(updated.alloc(self.arena()), original)
    }

    pub fn update_shorthand_property_assignment(
        &self,
        node: Id<Node>, /*ShorthandPropertyAssignment*/
        name: Id<Node>, /*Identifier*/
        object_assignment_initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_shorthand_property_assignment = node_ref.as_shorthand_property_assignment();
        if node_as_shorthand_property_assignment.name() != name
            || node_as_shorthand_property_assignment.object_assignment_initializer
                != object_assignment_initializer
        {
            self.finish_update_shorthand_property_assignment(
                self.create_shorthand_property_assignment_raw(name, object_assignment_initializer),
                node,
            )
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_spread_assignment_raw(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> SpreadAssignment {
        let node = self.create_base_node(SyntaxKind::SpreadAssignment);
        let node = SpreadAssignment::new(
            node,
            self.parenthesizer_rules()
                .ref_(self)
                .parenthesize_expression_for_disallowed_comma(expression),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.expression.clone()), self)
                | TransformFlags::ContainsES2018
                | TransformFlags::ContainsObjectRestOrSpread,
        );
        node
    }

    pub fn update_spread_assignment(
        &self,
        node: Id<Node>, /*SpreadAssignment*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_spread_assignment = node_ref.as_spread_assignment();
        if node_as_spread_assignment.expression != expression {
            self.update(self.create_spread_assignment(expression), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_enum_member_raw<'name>(
        &self,
        name: impl Into<StrOrRcNode<'name>>, /*Identifier*/
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> EnumMember {
        let node = self.create_base_node(SyntaxKind::EnumMember);
        let node = EnumMember::new(
            node,
            self.as_name(Some(name)).unwrap(),
            initializer.map(|initializer| {
                self.parenthesizer_rules()
                    .ref_(self)
                    .parenthesize_expression_for_disallowed_comma(initializer)
            }),
        );
        node.add_transform_flags(
            propagate_child_flags(Some(node.name), self)
                | propagate_child_flags(node.initializer.clone(), self)
                | TransformFlags::ContainsTypeScript,
        );
        node
    }

    pub fn update_enum_member(
        &self,
        node: Id<Node>, /*EnumMember*/
        name: Id<Node>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_enum_member = node_ref.as_enum_member();
        if node_as_enum_member.name != name || node_as_enum_member.initializer != initializer {
            self.update(self.create_enum_member(name, initializer), node)
        } else {
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_source_file_raw(
        &self,
        statements: impl Into<NodeArrayOrVec>,
        end_of_file_token: Id<Node /*EndOfFileToken*/>,
        flags: NodeFlags,
    ) -> SourceFile {
        let node = self
            .base_factory
            .ref_(self)
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
            propagate_children_flags(Some(&node.statements().ref_(self)))
                | propagate_child_flags(Some(node.end_of_file_token()), self),
        );
        node
    }

    pub fn clone_source_file_with_changes(
        &self,
        source: Id<Node>, /*SourceFile*/
        statements: NodeArrayOrVec,
        is_declaration_file: bool,
        referenced_files: Rc<RefCell<Vec<FileReference>>>,
        type_reference_directives: Rc<RefCell<Vec<FileReference>>>,
        has_no_default_lib: bool,
        lib_reference_directives: Rc<RefCell<Vec<FileReference>>>,
    ) -> Id<Node> {
        let mut node = source.ref_(self).as_source_file().clone();
        node.set_pos(-1);
        node.set_end(-1);
        node.set_id(0);
        node.set_modifier_flags_cache(ModifierFlags::None);
        node.set_parent(None);
        node.set_flags(NodeFlags::None);
        node.set_transform_flags(TransformFlags::None);
        node.set_original(None);
        node.set_emit_node(None);
        self.base_factory
            .ref_(self)
            .update_cloned_node(node.base_node());

        node.set_statements(self.create_node_array(Some(statements), None));
        node.set_end_of_file_token(source.ref_(self).as_source_file().end_of_file_token());
        node.set_is_declaration_file(is_declaration_file);
        node.set_referenced_files(referenced_files);
        node.set_type_reference_directives(type_reference_directives);
        node.set_has_no_default_lib(has_no_default_lib);
        node.set_lib_reference_directives(lib_reference_directives);
        node.set_transform_flags(
            propagate_children_flags(Some(&node.statements().ref_(self)))
                | propagate_child_flags(Some(node.end_of_file_token()), self),
        );
        node.set_implied_node_format(
            source
                .ref_(self)
                .as_source_file()
                .maybe_implied_node_format(),
        );
        node.alloc(self.arena())
    }

    pub fn update_source_file(
        &self,
        node: Id<Node>, /*SourceFile*/
        statements: impl Into<NodeArrayOrVec>,
        is_declaration_file: Option<bool>,
        referenced_files: Option<Rc<RefCell<Vec<FileReference>>>>,
        type_reference_directives: Option<Rc<RefCell<Vec<FileReference>>>>,
        has_no_default_lib: Option<bool>,
        lib_reference_directives: Option<Rc<RefCell<Vec<FileReference>>>>,
    ) -> Id<Node> {
        let is_declaration_file = is_declaration_file
            .unwrap_or_else(|| node.ref_(self).as_source_file().is_declaration_file());
        let referenced_files =
            referenced_files.unwrap_or_else(|| node.ref_(self).as_source_file().referenced_files());
        let type_reference_directives = type_reference_directives
            .unwrap_or_else(|| node.ref_(self).as_source_file().type_reference_directives());
        let has_no_default_lib = has_no_default_lib
            .unwrap_or_else(|| node.ref_(self).as_source_file().has_no_default_lib());
        let lib_reference_directives = lib_reference_directives
            .unwrap_or_else(|| node.ref_(self).as_source_file().lib_reference_directives());
        let statements = statements.into();
        if has_node_array_changed(node.ref_(self).as_source_file().statements(), &statements)
            || node.ref_(self).as_source_file().is_declaration_file() != is_declaration_file
            || !are_option_rcs_equal(
                node.ref_(self)
                    .as_source_file()
                    .maybe_referenced_files()
                    .as_ref(),
                Some(&referenced_files),
            )
            || !are_option_rcs_equal(
                node.ref_(self)
                    .as_source_file()
                    .maybe_type_reference_directives()
                    .as_ref(),
                Some(&type_reference_directives),
            )
            || node.ref_(self).as_source_file().has_no_default_lib() != has_no_default_lib
            || !are_option_rcs_equal(
                node.ref_(self)
                    .as_source_file()
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
            node
        }
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_bundle_raw(
        &self,
        source_files: Vec<Option<Id<Node /*<SourceFile>*/>>>,
        prepends: Option<Vec<Id<Node /*<UnparsedSource | InputFiles>*/>>>,
    ) -> Bundle {
        let prepends = prepends.unwrap_or_else(|| vec![]);
        let node = self.create_base_node(SyntaxKind::Bundle);
        Bundle::new(node, prepends, source_files)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_unparsed_source_raw(
        &self,
        prologues: Vec<Id<Node /*<UnparsedPrologue>*/>>,
        synthetic_references: Option<Vec<Id<Node /*<UnparsedSyntheticReference*/>>>,
        texts: Vec<Id<Node /*<UnparsedSourceText>*/>>,
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
        texts: Vec<Id<Node /*UnparsedTextLike*/>>,
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
        InputFiles::new(node, "".to_owned(), "".to_owned(), self)
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_synthetic_expression_raw(
        &self,
        type_: Id<Type>,
        is_spread: Option<bool>,
        tuple_name_source: Option<Id<Node /*ParameterDeclaration | NamedTupleMember*/>>,
    ) -> SyntheticExpression {
        let is_spread = is_spread.unwrap_or(false);
        let node = self.create_base_node(SyntaxKind::SyntheticExpression);
        let node = SyntheticExpression::new(node, is_spread, type_, tuple_name_source);
        node
    }

    pub fn create_not_emitted_statement(&self, original: Id<Node>) -> Id<Node> {
        let node = self
            .create_base_node(SyntaxKind::NotEmittedStatement)
            .alloc(self.arena());
        node.ref_(self).set_original(Some(original.clone()));
        set_text_range(&*node.ref_(self), Some(&*original.ref_(self)));
        node
    }

    #[generate_node_factory_method_wrapper]
    pub fn create_partially_emitted_expression_raw(
        &self,
        expression: Id<Node /*Expression*/>,
        original: Option<Id<Node>>,
    ) -> PartiallyEmittedExpression {
        let node = self.create_base_node(SyntaxKind::PartiallyEmittedExpression);
        let node = PartiallyEmittedExpression::new(node, expression);
        node.set_original(original.clone());
        node.set_transform_flags(
            node.transform_flags()
                | propagate_child_flags(Some(node.expression), self)
                | TransformFlags::ContainsTypeScript,
        );
        set_text_range(&node, original.refed(self).as_deref());
        node
    }

    pub fn update_partially_emitted_expression(
        &self,
        node: Id<Node>, /*PartiallyEmittedExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_partially_emitted_expression = node_ref.as_partially_emitted_expression();
        if node_as_partially_emitted_expression.expression != expression {
            self.update(
                self.create_partially_emitted_expression(
                    expression,
                    node_as_partially_emitted_expression.maybe_original(),
                ),
                node,
            )
        } else {
            node
        }
    }

    fn flatten_comma_elements(
        &self,
        node: Id<Node>, /*Expression*/
    ) -> SingleOrVec<Id<Node /*Expression*/>> {
        if node_is_synthesized(&*node.ref_(self))
            && !is_parse_tree_node(&node.ref_(self))
            && node.ref_(self).maybe_original().is_none()
            && node.ref_(self).maybe_emit_node().is_none()
            && node.ref_(self).maybe_id().is_none()
        {
            if is_comma_list_expression(&node.ref_(self)) {
                return node
                    .ref_(self)
                    .as_comma_list_expression()
                    .elements
                    .ref_(self)
                    .to_vec()
                    .into();
            }
            if is_binary_expression(&node.ref_(self)) {
                let node_ref = node.ref_(self);
                let node_as_binary_expression = node_ref.as_binary_expression();
                if is_comma_token(&node_as_binary_expression.operator_token.ref_(self)) {
                    return vec![
                        node_as_binary_expression.left.clone(),
                        node_as_binary_expression.right.clone(),
                    ]
                    .into();
                }
            }
        }
        node.into()
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
                Some(same_flat_map_id_node(
                    elements,
                    |element: Id<Node>, _| self.flatten_comma_elements(element),
                    self,
                )),
                None,
            ),
        );
        node.set_transform_flags(
            node.transform_flags() | propagate_children_flags(Some(&node.elements.ref_(self))),
        );
        node
    }

    pub fn update_comma_list_expression(
        &self,
        node: Id<Node>, /*CommaListExpression*/
        elements: impl Into<NodeArrayOrVec /*<Expression>*/>,
    ) -> Id<Node> {
        let node_ref = node.ref_(self);
        let node_as_comma_list_expression = node_ref.as_comma_list_expression();
        let elements = elements.into();
        if has_node_array_changed(node_as_comma_list_expression.elements, &elements) {
            self.update(self.create_comma_list_expression(elements), node)
        } else {
            node
        }
    }

    pub fn create_end_of_declaration_marker(&self, original: Id<Node>) -> Id<Node> {
        let node = self
            .create_base_node(SyntaxKind::EndOfDeclarationMarker)
            .alloc(self.arena());
        node.ref_(self)
            .set_emit_node(Some(self.alloc_emit_node(_d())));
        node.ref_(self).set_original(Some(original));
        node
    }

    pub fn create_merge_declaration_marker(&self, original: Id<Node>) -> Id<Node> {
        let node = self
            .create_base_node(SyntaxKind::MergeDeclarationMarker)
            .alloc(self.arena());
        node.ref_(self)
            .set_emit_node(Some(self.alloc_emit_node(_d())));
        node.ref_(self).set_original(Some(original));
        node
    }

    pub fn create_synthetic_reference_expression(
        &self,
        expression: Id<Node>, /*Expression*/
        this_arg: Id<Node>,   /*Expression*/
    ) -> Id<Node> {
        let node = self.create_base_node(SyntaxKind::SyntheticReferenceExpression);
        let node = SyntheticReferenceExpression::new(node, expression, this_arg);
        node.set_transform_flags(
            node.transform_flags()
                | propagate_child_flags(Some(node.expression), self)
                | propagate_child_flags(Some(node.this_arg), self),
        );
        node.alloc(self.arena())
    }

    pub fn clone_node(&self, node: Id<Node>) -> Id<Node> {
        // if (node === undefined) {
        //     return node;
        //  }

        let clone = self.alloc_node(released!((*node.ref_(self)).clone()));
        clone.ref_(self).set_pos(-1);
        clone.ref_(self).set_end(-1);
        clone.ref_(self).set_id(0);
        clone
            .ref_(self)
            .set_modifier_flags_cache(ModifierFlags::None);
        clone.ref_(self).set_parent(None);
        self.base_factory
            .ref_(self)
            .update_cloned_node(clone.ref_(self).base_node());

        clone.ref_(self).set_flags(
            clone.ref_(self).flags() | (node.ref_(self).flags() & !NodeFlags::Synthesized),
        );
        clone
            .ref_(self)
            .set_transform_flags(node.ref_(self).transform_flags());
        set_original_node(clone, Some(node), self);

        clone
    }

    pub fn create_immediately_invoked_arrow_function(
        &self,
        statements: impl Into<NodeArrayOrVec>,
        param: Option<Id<Node /*ParameterDeclaration*/>>,
        param_value: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        self.create_call_expression(
            self.create_arrow_function(
                Option::<Id<NodeArray>>::None,
                Option::<Id<NodeArray>>::None,
                param.map_or_default(|param| vec![param]),
                None,
                None,
                self.create_block(statements, Some(true)),
            ),
            Option::<Id<NodeArray>>::None,
            Some(param_value.map_or_default(|param_value| vec![param_value])),
        )
    }

    pub fn create_void_zero(&self) -> Id<Node> {
        self.create_void_expression(self.create_numeric_literal("0".to_owned(), None))
    }

    pub fn create_export_default(&self, expression: Id<Node /*Expression*/>) -> Id<Node> {
        self.create_export_assignment(
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            Some(false),
            expression,
        )
    }

    pub fn create_external_module_export(&self, export_name: Id<Node /*Identifier*/>) -> Id<Node> {
        self.create_export_declaration(
            Option::<Id<NodeArray>>::None,
            Option::<Id<NodeArray>>::None,
            false,
            Some(self.create_named_exports(vec![self.create_export_specifier(
                false,
                Option::<Id<Node>>::None,
                export_name,
            )])),
            None,
            None,
        )
    }

    pub fn create_type_check(
        &self,
        value: Id<Node /*Expression*/>,
        tag: &str, /*TypeOfTag*/
    ) -> Id<Node> {
        if tag == "undefined" {
            self.create_strict_equality(value, self.create_void_zero())
        } else {
            self.create_strict_equality(
                self.create_type_of_expression(value),
                self.create_string_literal(tag.to_owned(), None, None),
            )
        }
    }

    pub(super) fn create_method_call<'method_name>(
        &self,
        object: Id<Node /*Expression*/>,
        method_name: impl Into<StrOrRcNode<'method_name>>,
        arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Id<Node> {
        if is_call_chain(&object.ref_(self)) {
            return self.create_call_chain(
                self.create_property_access_chain(object, None, method_name),
                None,
                Option::<Id<NodeArray>>::None,
                Some(arguments_list),
            );
        }
        self.create_call_expression(
            self.create_property_access_expression(object, method_name),
            Option::<Id<NodeArray>>::None,
            Some(arguments_list),
        )
    }

    pub fn create_function_bind_call(
        &self,
        target: Id<Node /*Expression*/>,
        this_arg: Id<Node /*Expression*/>,
        arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Id<Node> {
        let arguments_list = arguments_list.into();
        self.create_method_call(
            target,
            "bind",
            vec![this_arg].and_extend(arguments_list.ref_(self)),
        )
    }

    pub fn create_function_call_call(
        &self,
        target: Id<Node /*Expression*/>,
        this_arg: Id<Node /*Expression*/>,
        arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Id<Node> {
        let arguments_list = arguments_list.into();
        self.create_method_call(
            target,
            "call",
            vec![this_arg].and_extend(arguments_list.ref_(self)),
        )
    }

    pub fn create_function_apply_call(
        &self,
        target: Id<Node /*Expression*/>,
        this_arg: Id<Node /*Expression*/>,
        arguments_expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        self.create_method_call(target, "apply", vec![this_arg, arguments_expression])
    }

    pub fn create_global_method_call(
        &self,
        global_object_name: &str,
        method_name: &str,
        arguments_list: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Id<Node> {
        self.create_method_call(
            self.create_identifier(global_object_name),
            method_name,
            arguments_list,
        )
    }

    pub fn create_array_slice_call(
        &self,
        array: Id<Node /*Expression*/>,
        start: Option<impl Into<NumberOrRcNode>>,
    ) -> Id<Node> {
        self.create_method_call(
            array,
            "slice",
            start.map_or_default(|start| vec![self.as_expression(start.into())]),
        )
    }

    pub fn create_array_concat_call(
        &self,
        array: Id<Node /*Expression*/>,
        arguments_list: Vec<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        self.create_method_call(array, "concat", arguments_list)
    }

    pub fn create_object_define_property_call<'property_name>(
        &self,
        target: Id<Node /*Expression*/>,
        property_name: impl Into<StrOrRcNode<'property_name>>,
        attributes: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        let property_name = property_name.into();
        self.create_global_method_call(
            "Object",
            "defineProperty",
            vec![target, self.as_expression(property_name), attributes],
        )
    }

    pub fn create_reflect_get_call(
        &self,
        target: Id<Node /*Expression*/>,
        property_key: Id<Node /*Expression*/>,
        receiver: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node /*CallExpression*/> {
        self.create_global_method_call(
            "Reflect",
            "get",
            receiver.map_or_else(
                || vec![target.clone(), property_key.clone()],
                |receiver| vec![target.clone(), property_key.clone(), receiver],
            ),
        )
    }

    pub fn create_reflect_set_call(
        &self,
        target: Id<Node /*Expression*/>,
        property_key: Id<Node /*Expression*/>,
        value: Id<Node /*Expression*/>,
        receiver: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node /*CallExpression*/> {
        self.create_global_method_call(
            "Reflect",
            "set",
            receiver.map_or_else(
                || vec![target.clone(), property_key.clone(), value.clone()],
                |receiver| {
                    vec![
                        target.clone(),
                        property_key.clone(),
                        value.clone(),
                        receiver,
                    ]
                },
            ),
        )
    }

    pub(super) fn try_add_property_assignment(
        &self,
        properties: &mut Vec<Id<Node /*PropertyAssignment*/>>,
        property_name: &str,
        expression: Option<Id<Node>>,
    ) -> bool {
        if let Some(expression) = expression {
            properties.push(self.create_property_assignment(property_name, expression));
            return true;
        }
        false
    }

    pub fn create_property_descriptor(
        &self,
        attributes: PropertyDescriptorAttributes,
        single_line: Option<bool>,
    ) -> Id<Node> {
        let mut properties: Vec<Id<Node /*PropertyAssignment*/>> = _d();
        self.try_add_property_assignment(
            &mut properties,
            "enumerable",
            self.maybe_as_expression(attributes.enumerable.clone()),
        );
        self.try_add_property_assignment(
            &mut properties,
            "configurable",
            self.maybe_as_expression(attributes.configurable.clone()),
        );

        let mut is_data = self.try_add_property_assignment(
            &mut properties,
            "writable",
            self.maybe_as_expression(attributes.writable.clone()),
        );
        is_data =
            self.try_add_property_assignment(&mut properties, "value", attributes.value.clone())
                || is_data;

        let mut is_accessor =
            self.try_add_property_assignment(&mut properties, "get", attributes.get.clone());
        is_accessor =
            self.try_add_property_assignment(&mut properties, "set", attributes.set.clone())
                || is_accessor;

        Debug_.assert(!(is_data && is_accessor), Some("A PropertyDescriptor may not be both an accessor descriptor and a data descriptor."));
        self.create_object_literal_expression(Some(properties), Some(single_line != Some(true)))
    }

    pub(super) fn update_outer_expression(
        &self,
        outer_expression: Id<Node>, /*OuterExpression*/
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node> {
        match outer_expression.ref_(self).kind() {
            SyntaxKind::ParenthesizedExpression => {
                self.update_parenthesized_expression(outer_expression, expression)
            }
            SyntaxKind::TypeAssertionExpression => self.update_type_assertion(
                outer_expression,
                outer_expression
                    .ref_(self)
                    .as_type_assertion()
                    .type_
                    .clone(),
                expression,
            ),
            SyntaxKind::AsExpression => self.update_as_expression(
                outer_expression,
                expression,
                outer_expression.ref_(self).as_as_expression().type_.clone(),
            ),
            SyntaxKind::NonNullExpression => {
                self.update_non_null_expression(outer_expression, expression)
            }
            SyntaxKind::PartiallyEmittedExpression => {
                self.update_partially_emitted_expression(outer_expression, expression)
            }
            _ => unreachable!(),
        }
    }

    fn is_ignorable_paren(&self, node: Id<Node> /*Expression*/) -> bool {
        is_parenthesized_expression(&node.ref_(self))
            && node_is_synthesized(&*node.ref_(self))
            && node_is_synthesized(&ReadonlyTextRangeConcrete::from_text_range(
                &*get_source_map_range(&node.ref_(self), self).ref_(self),
            ))
            && node_is_synthesized(&ReadonlyTextRangeConcrete::from(get_comment_range(
                node, self,
            )))
            && !get_synthetic_leading_comments(node, self).is_non_empty()
            && !get_synthetic_trailing_comments(node, self).is_non_empty()
    }

    pub fn restore_outer_expressions(
        &self,
        outer_expression: Option<Id<Node> /*Expression*/>,
        inner_expression: Id<Node>, /*Expression*/
        kinds: Option<OuterExpressionKinds>,
    ) -> Id<Node /*Expression*/> {
        let kinds = kinds.unwrap_or(OuterExpressionKinds::All);
        if let Some(outer_expression) = outer_expression.filter(|&outer_expression| {
            is_outer_expression(outer_expression, Some(kinds), self)
                && !self.is_ignorable_paren(outer_expression)
        }) {
            return self.update_outer_expression(
                outer_expression,
                self.restore_outer_expressions(
                    Some(outer_expression.ref_(self).as_has_expression().expression()),
                    inner_expression,
                    None,
                ),
            );
        }
        inner_expression
    }

    pub fn restore_enclosing_label(
        &self,
        node: Id<Node>, /*Statement*/
        outermost_labeled_statement: Option<Id<Node /*LabeledStatement*/>>,
        after_restore_label_callback: Option<impl FnMut(Id<Node> /*LabeledStatement*/)>,
    ) -> Id<Node /*Statement*/> {
        if outermost_labeled_statement.is_none() {
            return node;
        }
        let outermost_labeled_statement = outermost_labeled_statement.unwrap();
        let outermost_labeled_statement_ref = outermost_labeled_statement.ref_(self);
        let outermost_labeled_statement_as_labeled_statement =
            outermost_labeled_statement_ref.as_labeled_statement();
        let updated = self.update_labeled_statement(
            outermost_labeled_statement,
            outermost_labeled_statement_as_labeled_statement
                .label
                .clone(),
            if is_labeled_statement(
                &outermost_labeled_statement_as_labeled_statement
                    .statement
                    .ref_(self),
            ) {
                self.restore_enclosing_label(
                    node,
                    Some(outermost_labeled_statement_as_labeled_statement.statement),
                    Option::<fn(Id<Node>)>::None,
                )
            } else {
                node
            },
        );
        if let Some(mut after_restore_label_callback) = after_restore_label_callback {
            after_restore_label_callback(outermost_labeled_statement);
        }
        updated
    }

    pub(super) fn should_be_captured_in_temp_variable(
        &self,
        node: Id<Node>, /*Expression*/
        cache_identifiers: bool,
    ) -> bool {
        let target = skip_parentheses(node, None, self);
        match target.ref_(self).kind() {
            SyntaxKind::Identifier => cache_identifiers,
            SyntaxKind::ThisKeyword
            | SyntaxKind::NumericLiteral
            | SyntaxKind::BigIntLiteral
            | SyntaxKind::StringLiteral => false,
            SyntaxKind::ArrayLiteralExpression => {
                let target_ref = target.ref_(self);
                let elements = target_ref.as_array_literal_expression().elements;
                if elements.ref_(self).is_empty() {
                    return false;
                }
                true
            }
            SyntaxKind::ObjectLiteralExpression => !target
                .ref_(self)
                .as_object_literal_expression()
                .properties
                .ref_(self)
                .is_empty(),
            _ => true,
        }
    }

    pub fn create_call_binding(
        &self,
        expression: Id<Node>, /*Expression*/
        mut record_temp_variable: impl FnMut(Id<Node> /*Identifier*/),
        language_version: Option<ScriptTarget>,
        cache_identifiers: Option<bool>,
    ) -> CallBinding {
        let cache_identifiers = cache_identifiers.unwrap_or_default();
        let callee = skip_outer_expressions(expression, Some(OuterExpressionKinds::All), self);
        let this_arg: Id<Node /*Expression*/>;
        let target: Id<Node /*LeftHandSideExpression*/>;
        if is_super_property(callee, self) {
            this_arg = self.create_this();
            target = callee.clone();
        } else if is_super_keyword(&callee.ref_(self)) {
            this_arg = self.create_this();
            target = if language_version
                .matches(|language_version| language_version < ScriptTarget::ES2015)
            {
                self.create_identifier("_super")
                    .set_text_range(Some(&*callee.ref_(self)), self)
            } else {
                callee.clone()
            };
        } else if get_emit_flags(callee, self).intersects(EmitFlags::HelperName) {
            this_arg = self.create_void_zero();
            target = self
                .parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(callee);
        } else if is_property_access_expression(&callee.ref_(self)) {
            let callee_ref = callee.ref_(self);
            let callee_as_property_access_expression = callee_ref.as_property_access_expression();
            if self.should_be_captured_in_temp_variable(
                callee_as_property_access_expression.expression,
                cache_identifiers,
            ) {
                this_arg = self.create_temp_variable(
                    Some(|node: Id<Node>| {
                        record_temp_variable(node);
                    }),
                    None,
                );
                target = self
                    .create_property_access_expression(
                        self.create_assignment(
                            this_arg.clone(),
                            callee_as_property_access_expression.expression.clone(),
                        )
                        .set_text_range(
                            Some(&*callee_as_property_access_expression.expression.ref_(self)),
                            self,
                        ),
                        callee_as_property_access_expression.name.clone(),
                    )
                    .set_text_range(Some(&*callee.ref_(self)), self);
            } else {
                this_arg = callee_as_property_access_expression.expression.clone();
                target = callee.clone();
            }
        } else if is_element_access_expression(&callee.ref_(self)) {
            let callee_ref = callee.ref_(self);
            let callee_as_element_access_expression = callee_ref.as_element_access_expression();
            if self.should_be_captured_in_temp_variable(
                callee_as_element_access_expression.expression,
                cache_identifiers,
            ) {
                this_arg = self.create_temp_variable(
                    Some(|node: Id<Node>| {
                        record_temp_variable(node);
                    }),
                    None,
                );
                target = self
                    .create_element_access_expression(
                        self.create_assignment(
                            this_arg.clone(),
                            callee_as_element_access_expression.expression.clone(),
                        )
                        .set_text_range(
                            Some(&*callee_as_element_access_expression.expression.ref_(self)),
                            self,
                        ),
                        callee_as_element_access_expression
                            .argument_expression
                            .clone(),
                    )
                    .set_text_range(Some(&*callee.ref_(self)), self);
            } else {
                this_arg = callee_as_element_access_expression.expression.clone();
                target = callee.clone();
            }
        } else {
            this_arg = self.create_void_zero();
            target = self
                .parenthesizer_rules()
                .ref_(self)
                .parenthesize_left_side_of_access(expression);
        }

        CallBinding { target, this_arg }
    }

    pub fn create_assignment_target_wrapper(
        &self,
        param_name: Id<Node /*Identifier*/>,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*LeftHandSideExpression*/> {
        self.create_property_access_expression(
            self.create_parenthesized_expression(self.create_object_literal_expression(
                Some(vec![self.create_set_accessor_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    "value",
                    vec![self.create_parameter_declaration(
                        Option::<Id<NodeArray>>::None,
                        Option::<Id<NodeArray>>::None,
                        None,
                        Some(param_name),
                        None,
                        None,
                        None,
                    )],
                    Some(
                        self.create_block(vec![self.create_expression_statement(expression)], None),
                    ),
                )]),
                None,
            )),
            "value",
        )
    }

    pub fn inline_expressions(&self, expressions: &[Id<Node /*Expression*/>]) -> Id<Node> {
        if expressions.len() > 10 {
            self.create_comma_list_expression(expressions.to_owned())
        } else {
            reduce_left_no_initial_value(
                expressions,
                |accumulator: Id<Node>, next: &Id<Node>, _| {
                    self.create_comma(accumulator, next.clone())
                },
                None,
                None,
            )
        }
    }

    pub(super) fn get_name(
        &self,
        node: Option<Id<Node /*Declaration*/>>,
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
        emit_flags: Option<EmitFlags>,
    ) -> Id<Node> {
        let mut emit_flags = emit_flags.unwrap_or_default();
        let node_name = get_name_of_declaration(node, self);
        if let Some(node_name) = node_name.filter(|node_name| {
            is_identifier(&node_name.ref_(self)) && !is_generated_identifier(&node_name.ref_(self))
        }) {
            let name = self
                .clone_node(node_name)
                .set_text_range(Some(&*node_name.ref_(self)), self)
                .and_set_parent(node_name.ref_(self).maybe_parent(), self);
            emit_flags |= get_emit_flags(node_name, self);
            if allow_source_maps != Some(true) {
                emit_flags |= EmitFlags::NoSourceMap;
            }
            if allow_comments != Some(true) {
                emit_flags |= EmitFlags::NoComments;
            }
            if emit_flags != EmitFlags::None {
                set_emit_flags(name, emit_flags, self);
            }
            return name;
        }
        self.get_generated_name_for_node(node, None)
    }

    pub fn get_internal_name(
        &self,
        node: Id<Node>, /*Declaration*/
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Id<Node> {
        self.get_name(
            Some(node),
            allow_comments,
            allow_source_maps,
            Some(EmitFlags::LocalName | EmitFlags::InternalName),
        )
    }

    pub fn get_local_name(
        &self,
        node: Id<Node>, /*Declaration*/
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Id<Node> {
        self.get_name(
            Some(node),
            allow_comments,
            allow_source_maps,
            Some(EmitFlags::LocalName),
        )
    }

    pub fn get_export_name(
        &self,
        node: Id<Node>, /*Declaration*/
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Id<Node /*Identifier*/> {
        self.get_name(
            Some(node),
            allow_comments,
            allow_source_maps,
            Some(EmitFlags::ExportName),
        )
    }

    pub fn get_declaration_name(
        &self,
        node: Option<Id<Node> /*Declaration*/>,
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Id<Node /*Identifier*/> {
        self.get_name(node, allow_comments, allow_source_maps, None)
    }

    pub fn get_namespace_member_name(
        &self,
        ns: Id<Node>,   /*Identifier*/
        name: Id<Node>, /*Identifier*/
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Id<Node /*PropertyAccessExpression*/> {
        let qualified_name = self
            .create_property_access_expression(
                ns,
                if node_is_synthesized(&*name.ref_(self)) {
                    name
                } else {
                    self.clone_node(name)
                },
            )
            .set_text_range(Some(&*name.ref_(self)), self);
        let mut emit_flags: EmitFlags = _d();
        if allow_source_maps != Some(true) {
            emit_flags |= EmitFlags::NoSourceMap;
        }
        if allow_comments != Some(true) {
            emit_flags |= EmitFlags::NoComments;
        }
        if emit_flags != EmitFlags::None {
            set_emit_flags(qualified_name, emit_flags, self);
        }
        qualified_name
    }

    pub fn get_external_module_or_namespace_export_name(
        &self,
        ns: Option<Id<Node> /*Identifier*/>,
        node: Id<Node>, /*Declaration*/
        allow_comments: Option<bool>,
        allow_source_maps: Option<bool>,
    ) -> Id<Node /*Identifier | PropertyAccessExpression*/> {
        if let Some(ns) = ns.filter(|_| has_syntactic_modifier(node, ModifierFlags::Export, self)) {
            return self.get_namespace_member_name(
                ns,
                self.get_name(Some(node), None, None, None),
                allow_comments,
                allow_source_maps,
            );
        }
        self.get_export_name(node, allow_comments, allow_source_maps)
    }

    pub fn copy_prologue(
        &self,
        source: &[Id<Node /*Statement*/>],
        target: &mut Vec<Id<Node /*Statement*/>>,
        ensure_use_strict: Option<bool>,
        visitor: Option<impl FnMut(Id<Node>) -> VisitResult /*<Node>*/>,
    ) -> usize {
        self.try_copy_prologue(
            source,
            target,
            ensure_use_strict,
            visitor.map(|mut visitor| move |a: Id<Node>| Ok(visitor(a))),
        )
        .unwrap()
    }

    pub fn try_copy_prologue(
        &self,
        source: &[Id<Node /*Statement*/>],
        target: &mut Vec<Id<Node /*Statement*/>>,
        ensure_use_strict: Option<bool>,
        visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult /*<Node>*/>>,
    ) -> io::Result<usize> {
        let offset = self.copy_standard_prologue(source, target, ensure_use_strict);
        Ok(self
            .try_copy_custom_prologue(
                source,
                target,
                Some(offset),
                visitor,
                Option::<fn(Id<Node>) -> bool>::None,
            )?
            .unwrap())
    }

    pub fn is_use_strict_prologue(&self, node: Id<Node> /*ExpressionStatement*/) -> bool {
        let node_ref = node.ref_(self);
        let node_as_expression_statement = node_ref.as_expression_statement();
        is_string_literal(&node_as_expression_statement.expression.ref_(self))
            && *node_as_expression_statement
                .expression
                .ref_(self)
                .as_string_literal()
                .text()
                == "use strict"
    }

    pub fn create_use_strict_prologue(&self) -> Id<Node> {
        self.create_expression_statement(self.create_string_literal(
            "use strict".to_owned(),
            None,
            None,
        ))
        .start_on_new_line(self)
    }

    pub fn copy_standard_prologue(
        &self,
        source: &[Id<Node /*Statement*/>],
        target: &mut Vec<Id<Node /*Statement*/>>,
        ensure_use_strict: Option<bool>,
    ) -> usize {
        Debug_.assert(target.is_empty(), Some("Prologue directives should be at the first statement in the target statements array"));
        let mut found_use_strict = false;
        let mut statement_offset = 0;
        let num_statements = source.len();
        while statement_offset < num_statements {
            let statement = source[statement_offset];
            if is_prologue_directive(statement, self) {
                if self.is_use_strict_prologue(statement) {
                    found_use_strict = true;
                }
                target.push(statement.clone());
            } else {
                break;
            }
            statement_offset += 1;
        }
        if ensure_use_strict == Some(true) && !found_use_strict {
            target.push(self.create_use_strict_prologue());
        }
        statement_offset
    }

    pub fn copy_custom_prologue(
        &self,
        source: &[Id<Node /*Statement*/>],
        target: &mut Vec<Id<Node /*Statement*/>>,
        statement_offset: Option<usize>,
        visitor: Option<impl FnMut(Id<Node>) -> VisitResult /*<Node>*/>,
        filter: Option<impl FnMut(Id<Node>) -> bool>,
    ) -> Option<usize> {
        self.try_copy_custom_prologue(
            source,
            target,
            statement_offset,
            visitor.map(|mut visitor| move |node: Id<Node>| Ok(visitor(node))),
            filter,
        )
        .unwrap()
    }

    pub fn try_copy_custom_prologue(
        &self,
        source: &[Id<Node /*Statement*/>],
        target: &mut Vec<Id<Node /*Statement*/>>,
        statement_offset: Option<usize>,
        mut visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult /*<Node>*/>>,
        mut filter: Option<impl FnMut(Id<Node>) -> bool>,
    ) -> io::Result<Option<usize>> {
        let mut filter_or_default = |node: Id<Node>| {
            if let Some(filter) = filter.as_mut() {
                filter(node)
            } else {
                true
            }
        };
        let mut statement_offset = return_ok_default_if_none!(statement_offset);
        for (index, statement) in source.iter().enumerate().skip(statement_offset) {
            let statement = *statement;
            statement_offset = index;
            if get_emit_flags(statement, self).intersects(EmitFlags::CustomPrologue)
                && filter_or_default(statement)
            {
                target.push(visitor.as_mut().try_map_or_else(
                    || Ok(statement.clone()),
                    |visitor| {
                        try_visit_node(
                            statement,
                            Some(|node: Id<Node>| visitor(node)),
                            Some(|node: Id<Node>| is_statement(node, self)),
                            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                        )
                    },
                )?);
            } else {
                break;
            }
        }
        Ok(Some(statement_offset))
    }

    pub fn ensure_use_strict(
        &self,
        statements: Id<NodeArray>, /*<Statement>*/
    ) -> Id<NodeArray /*<Statement>*/> {
        let found_use_strict = find_use_strict_prologue(&statements.ref_(self), self);

        if found_use_strict.is_none() {
            return self
                .create_node_array(
                    Some(released!(vec![self.create_use_strict_prologue()]
                        .and_extend(statements.ref_(self).iter().cloned()))),
                    None,
                )
                .set_text_range(Some(&*statements.ref_(self)), self);
        }

        statements
    }

    pub fn lift_to_block(&self, nodes: &[Id<Node>]) -> Id<Node /*Statement*/> {
        Debug_.assert(
            every(nodes, |node: &Id<Node>, _| {
                is_statement_or_block(&node.ref_(self))
            }),
            Some("Cannot lift nodes to a Block."),
        );
        single_or_undefined(Some(nodes))
            .cloned()
            .unwrap_or_else(|| self.create_block(nodes.to_owned(), None))
    }

    pub(super) fn find_span_end<TItem>(
        &self,
        array: &[TItem],
        mut test: impl FnMut(&TItem) -> bool,
        start: usize,
    ) -> usize {
        let mut i = start;
        while i < array.len() && test(&array[i]) {
            i += 1;
        }
        i
    }

    pub fn merge_lexical_environment(
        &self,
        statements: impl Into<NodeArrayOrVec>,
        declarations: Option<&[Id<Node /*Statement*/>]>,
    ) -> NodeArrayOrVec {
        let statements = statements.into();
        if !declarations.is_non_empty() {
            return statements;
        }
        let declarations = declarations.unwrap();

        let left_standard_prologue_end = self.find_span_end(
            &*statements.ref_(self),
            |&node: &Id<Node>| is_prologue_directive(node, self),
            0,
        );
        let left_hoisted_functions_end = self.find_span_end(
            &*statements.ref_(self),
            |&node: &Id<Node>| is_hoisted_function(node, self),
            left_standard_prologue_end,
        );
        let left_hoisted_variables_end = self.find_span_end(
            &*statements.ref_(self),
            |&node: &Id<Node>| is_hoisted_variable_statement(node, self),
            left_hoisted_functions_end,
        );

        let right_standard_prologue_end = self.find_span_end(
            declarations,
            |&node: &Id<Node>| is_prologue_directive(node, self),
            0,
        );
        let right_hoisted_functions_end = self.find_span_end(
            declarations,
            |&node: &Id<Node>| is_hoisted_function(node, self),
            right_standard_prologue_end,
        );
        let right_hoisted_variables_end = self.find_span_end(
            declarations,
            |&node: &Id<Node>| is_hoisted_variable_statement(node, self),
            right_hoisted_functions_end,
        );
        let right_custom_prologue_end = self.find_span_end(
            declarations,
            |&node: &Id<Node>| is_custom_prologue(node, self),
            right_hoisted_variables_end,
        );
        Debug_.assert(
            right_custom_prologue_end == declarations.len(),
            Some("Expected declarations to be a valid standard or custom prologues"),
        );

        let mut left = (*statements.ref_(self)).to_owned();

        if right_custom_prologue_end > right_hoisted_variables_end {
            left.splice(
                left_hoisted_variables_end..left_hoisted_variables_end,
                declarations[right_hoisted_variables_end..right_custom_prologue_end]
                    .into_iter()
                    .cloned(),
            );
        }

        if right_hoisted_variables_end > right_hoisted_functions_end {
            left.splice(
                left_hoisted_functions_end..left_hoisted_functions_end,
                declarations[right_hoisted_functions_end..right_hoisted_variables_end]
                    .into_iter()
                    .cloned(),
            );
        }

        if right_hoisted_functions_end > right_standard_prologue_end {
            left.splice(
                left_standard_prologue_end..left_standard_prologue_end,
                declarations[right_standard_prologue_end..right_hoisted_functions_end]
                    .into_iter()
                    .cloned(),
            );
        }

        if right_standard_prologue_end > 0 {
            if left_standard_prologue_end == 0 {
                left.splice(
                    0..0,
                    declarations[0..right_standard_prologue_end]
                        .into_iter()
                        .cloned(),
                );
            } else {
                let mut left_prologues: HashMap<String, bool> = _d();
                for left_prologue in statements
                    .ref_(self)
                    .iter()
                    .take(left_standard_prologue_end)
                {
                    left_prologues.insert(
                        left_prologue
                            .ref_(self)
                            .as_expression_statement()
                            .expression
                            .ref_(self)
                            .as_string_literal()
                            .text()
                            .clone(),
                        true,
                    );
                }
                for right_prologue in declarations.iter().take(right_standard_prologue_end).rev() {
                    if !left_prologues.contains_key(
                        &*right_prologue
                            .ref_(self)
                            .as_expression_statement()
                            .expression
                            .ref_(self)
                            .as_string_literal()
                            .text(),
                    ) {
                        left.insert(0, right_prologue.clone());
                    }
                }
            }
        }

        match statements {
            NodeArrayOrVec::NodeArray(statements) => self
                .create_node_array(Some(left), Some(statements.ref_(self).has_trailing_comma))
                .set_text_range(Some(&*statements.ref_(self)), self)
                .into(),
            NodeArrayOrVec::Vec(statements) => statements.into(),
        }
    }
}
