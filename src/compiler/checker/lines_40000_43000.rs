#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{DeclarationMeaning, EmitResolverCreateResolver, UnusedKind};
use crate::{
    get_property_name_for_property_name_node, get_text_of_node, is_array_literal_expression,
    is_object_literal_expression, skip_parentheses, Debug_, DiagnosticMessage, Diagnostics,
    ExternalEmitHelpers, NodeArray, NodeCheckFlags, SyntaxKind, __String, bind_source_file,
    for_each, is_external_or_common_js_module, CancellationTokenDebuggable, Diagnostic,
    EmitResolverDebuggable, IndexInfo, Node, NodeInterface, StringOrNumber, Symbol, SymbolFlags,
    Type, TypeChecker,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Rc<Node>]>) -> bool {
        unimplemented!()
    }

    pub(super) fn check_source_element<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    pub(super) fn check_source_element_worker(&self, node: &Node) {
        match node {
            Node::PropertySignature(_) => self.check_property_signature(node),
            Node::TypeReferenceNode(_) => self.check_type_reference_node(node),
            Node::KeywordTypeNode(_) | Node::LiteralTypeNode(_) => (),
            Node::ArrayTypeNode(_) => self.check_array_type(node),
            Node::UnionTypeNode(_) => self.check_union_or_intersection_type(node),
            Node::FunctionDeclaration(_) => self.check_function_declaration(node),
            Node::Block(_) => self.check_block(node),
            Node::VariableStatement(_) => self.check_variable_statement(node),
            Node::ExpressionStatement(_) => self.check_expression_statement(node),
            Node::IfStatement(_) => self.check_if_statement(node),
            Node::ReturnStatement(_) => self.check_return_statement(node),
            Node::VariableDeclaration(_) => self.check_variable_declaration(node),
            Node::InterfaceDeclaration(_) => self.check_interface_declaration(node),
            Node::TypeAliasDeclaration(_) => self.check_type_alias_declaration(node),
            _ => unimplemented!("{:?}", node.kind()),
        };
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: &Node, /*JSDocVariadicType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_node_deferred(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_source_file(&self, source_file: &Node /*SourceFile*/) {
        self.check_source_file_worker(source_file)
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        unimplemented!()
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>> {
        unimplemented!()
    }

    pub(super) fn check_source_file_worker(&self, node: &Node /*SourceFile*/) {
        if true {
            for_each(&node.as_source_file().statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    pub(super) fn get_diagnostics_worker(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self
            .diagnostics()
            .get_diagnostics(Some(&source_file.as_source_file().file_name()));

        semantic_diagnostics
    }

    pub(super) fn get_symbols_in_scope_(
        &self,
        location_in: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_type_declaration(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_type_reference_identifier(&self, node: &Node /*EntityName*/) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_enclosing_class<TReturn, TCallback: FnMut(&Node) -> Option<TReturn>>(
        &self,
        node: &Node,
        callback: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: &Node,
        class_declaration: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: &Node, /*EntityName*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: &Node,
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        unimplemented!()
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TNode: Borrow<Node>>(
        &self,
        location: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags {
        unimplemented!()
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn is_function_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn create_resolver(&self) -> Rc<dyn EmitResolverDebuggable> {
        Rc::new(EmitResolverCreateResolver::new())
    }

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(&*file, self.compiler_options.clone());
            println!("post-binding: {:#?}", file);
        }

        for file in self.host.get_source_files() {
            if !is_external_or_common_js_module(&file) {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
        }

        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) {
        unimplemented!()
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_disallowed_trailing_comma(
        &self,
        list: Option<&NodeArray>,
        diag: Option<&'static DiagnosticMessage>,
    ) -> bool {
        let diag = diag.unwrap_or(&Diagnostics::Trailing_comma_not_allowed);
        unimplemented!()
    }

    pub(super) fn check_grammar_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration | MethodSignature*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_index_signature(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_type_arguments(
        &self,
        node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_tagged_template_chain(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_arguments(
        &self,
        args: Option<&NodeArray /*<Expression>*/>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_computed_property_name(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_generator(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_invalid_question_mark<TQuestionToken: Borrow<Node>>(
        &self,
        question_token: Option<TQuestionToken /*QuestionToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_invalid_exclamation_token<TExclamationToken: Borrow<Node>>(
        &self,
        exclamation_token: Option<TExclamationToken /*ExclamationToken*/>,
        message: &DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        in_destructuring: bool,
    ) -> bool {
        let mut seen: HashMap<__String, DeclarationMeaning> = HashMap::new();

        for prop in &node.as_object_literal_expression().properties {
            if prop.kind() == SyntaxKind::SpreadAssignment {
                if in_destructuring {
                    let expression =
                        skip_parentheses(&prop.as_spread_assignment().expression, None);
                    if is_array_literal_expression(&expression)
                        || is_object_literal_expression(&expression)
                    {
                        return self.grammar_error_on_node(
                            &prop.as_spread_assignment().expression,
                            &Diagnostics::A_rest_element_cannot_contain_a_binding_pattern,
                            None,
                        );
                    }
                }
                continue;
            }
            let name = prop.as_named_declaration().name();
            if name.kind() == SyntaxKind::ComputedPropertyName {
                self.check_grammar_computed_property_name(&name);
            }

            if prop.kind() == SyntaxKind::ShorthandPropertyAssignment
                && !in_destructuring
                && prop
                    .as_shorthand_property_assignment()
                    .object_assignment_initializer
                    .is_some()
            {
                return self.grammar_error_on_node(
                    prop.as_shorthand_property_assignment().equals_token.as_ref().unwrap(),
                    &Diagnostics::Did_you_mean_to_use_a_Colon_An_can_only_follow_a_property_name_when_the_containing_object_literal_is_part_of_a_destructuring_pattern,
                    None,
                );
            }

            if name.kind() == SyntaxKind::PrivateIdentifier {
                self.grammar_error_on_node(
                    &name,
                    &Diagnostics::Private_identifiers_are_not_allowed_outside_class_bodies,
                    None,
                );
            }

            if let Some(prop_modifiers) = prop.maybe_modifiers().as_ref() {
                for mod_ in prop_modifiers {
                    if mod_.kind() != SyntaxKind::AsyncKeyword
                        || prop.kind() != SyntaxKind::MethodDeclaration
                    {
                        self.grammar_error_on_node(
                            mod_,
                            &Diagnostics::_0_modifier_cannot_be_used_here,
                            Some(vec![get_text_of_node(mod_, None).into_owned()]),
                        );
                    }
                }
            }

            let current_kind: DeclarationMeaning;
            match prop.kind() {
                SyntaxKind::ShorthandPropertyAssignment => {
                    self.check_grammar_for_invalid_exclamation_token(
                        prop.as_shorthand_property_assignment().exclamation_token.as_deref(),
                        &Diagnostics::A_definite_assignment_assertion_is_not_permitted_in_this_context,
                    );
                    self.check_grammar_for_invalid_question_mark(
                        prop.as_has_question_token().maybe_question_token(),
                        &Diagnostics::An_object_member_cannot_be_declared_optional,
                    );
                    if name.kind() == SyntaxKind::NumericLiteral {
                        self.check_grammar_numeric_literal(&name);
                    }
                    current_kind = DeclarationMeaning::PropertyAssignment;
                }
                SyntaxKind::PropertyAssignment => {
                    self.check_grammar_for_invalid_question_mark(
                        prop.as_has_question_token().maybe_question_token(),
                        &Diagnostics::An_object_member_cannot_be_declared_optional,
                    );
                    if name.kind() == SyntaxKind::NumericLiteral {
                        self.check_grammar_numeric_literal(&name);
                    }
                    current_kind = DeclarationMeaning::PropertyAssignment;
                }
                SyntaxKind::MethodDeclaration => {
                    current_kind = DeclarationMeaning::Method;
                }
                SyntaxKind::GetAccessor => {
                    current_kind = DeclarationMeaning::GetAccessor;
                }
                SyntaxKind::SetAccessor => {
                    current_kind = DeclarationMeaning::SetAccessor;
                }
                _ => Debug_.assert_never(
                    prop,
                    Some(&format!("Unexpected syntax kind:{:?}", prop.kind())),
                ),
            }

            if !in_destructuring {
                let effective_name = get_property_name_for_property_name_node(&name);
                if effective_name.is_none() {
                    continue;
                }
                let effective_name = effective_name.unwrap();

                let existing_kind = seen.get(&effective_name).copied();
                match existing_kind {
                    None => {
                        seen.insert(effective_name, current_kind);
                    }
                    Some(existing_kind) => {
                        if current_kind.intersects(DeclarationMeaning::PropertyAssignmentOrMethod)
                            && existing_kind
                                .intersects(DeclarationMeaning::PropertyAssignmentOrMethod)
                        {
                            self.grammar_error_on_node(
                                &name,
                                &Diagnostics::Duplicate_identifier_0,
                                Some(vec![get_text_of_node(&name, None).into_owned()]),
                            );
                        } else if current_kind.intersects(DeclarationMeaning::GetOrSetAccessor)
                            && existing_kind.intersects(DeclarationMeaning::GetOrSetAccessor)
                        {
                            if existing_kind != DeclarationMeaning::GetOrSetAccessor
                                && current_kind != existing_kind
                            {
                                seen.insert(effective_name, current_kind | existing_kind);
                            } else {
                                return self.grammar_error_on_node(
                                    &name,
                                    &Diagnostics::An_object_literal_cannot_have_multiple_get_Slashset_accessors_with_the_same_name,
                                    None,
                                );
                            }
                        } else {
                            return self.grammar_error_on_node(
                                &name,
                                &Diagnostics::An_object_literal_cannot_have_property_and_accessor_with_the_same_name,
                                None
                            );
                        }
                    }
                }
            }
        }
        false
    }
}
